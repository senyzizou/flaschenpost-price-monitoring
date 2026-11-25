# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING - FLASCHENPOST PREPARATION
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261027

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) SEPARATE CREDENTIALS AND BOUNDED HTTP REQUESTS
# <-----------------------------------------------------------------------------

fp_dav_http = \(get = Sys.getenv, perform = NULL) {
  names = c("FP_UKEBOX_WEBDAV_URL", "FP_UKEBOX_USERNAME",
    "FP_UKEBOX_APP_PASSWORD")
  values = vapply(names, get, "")
  if (any(!nzchar(trimws(values)))) stop(
    "Missing Flaschenpost WebDAV environment variables.", call. = F)
  base = sub("/+$", "", unname(values[1]))
  if (!identical(base, "https://box.uke.de/remote.php/webdav")) stop(
    "Unexpected Flaschenpost WebDAV endpoint.", call. = F)
  if (is.null(perform)) perform = \(req, output) {
    response = req_perform(req, path = output)
    resp_status(response)
  }
  state = new.env(parent = emptyenv())
  state$verified = F
  state$owns_lock = F
  lock = paste0(fp_dav_base, "/writer_lock")
  send = \(method, path, input = NULL, output = NULL, probe = F) {
    ancestors = c("APM/flaschenpost", "APM/flaschenpost/validation",
      "APM/flaschenpost/validation/w36", fp_dav_base)
    if (!(method == "MKCOL" && path %in% ancestors)) fp_dav_path(path)
    if (method == "PUT_NEW" && !probe && !state$verified) stop(
      "WebDAV conditional writes have not been verified.")
    if (method == "MKCOL_LOCK" && !state$verified) stop(
      "WebDAV lock semantics have not been verified.")
    if (method %in% c("MKCOL_LOCK", "DELETE_LOCK") && path != lock) stop(
      "Unexpected WebDAV lock path.")
    if (method == "DELETE_LOCK" && !state$owns_lock) stop(
      "This process does not own the remote lock.")
    if (!method %in% c("GET", "PROPFIND", "MKCOL", "PUT_NEW",
        "MKCOL_LOCK", "DELETE_LOCK", "DELETE_PROBE")) stop(
      "Unsupported Flaschenpost WebDAV method.")
    if (method == "DELETE_PROBE" && !probe) stop("Probe cleanup is private.")
    actual = switch(method, PUT_NEW = "PUT", MKCOL_LOCK = "MKCOL",
      DELETE_LOCK = "DELETE", DELETE_PROBE = "DELETE", method)
    req = request(paste0(base, "/", path))
    req = req_auth_basic(req, unname(values[2]), unname(values[3]))
    req = req_method(req, actual)
    req = req_timeout(req, 120)
    req = req_options(req, followlocation = F, maxredirs = 0L,
      connecttimeout = 30, ssl_verifypeer = T, ssl_verifyhost = 2L)
    req = req_error(req, is_error = \(resp) F)
    if (method == "PROPFIND") req = req_headers(req, Depth = "1")
    if (method == "PUT_NEW") req = req_headers(req, `If-None-Match` = "*")
    if (!is.null(input)) req = req_body_file(req, input)
    status = tryCatch(suppressWarnings(perform(req, output)),
      error = \(e) NA_integer_)
    if (length(status) != 1L || is.na(status)) stop(
      "Flaschenpost WebDAV transport failure.", call. = F)
    if (!status %in% c(200L, 201L, 204L, 207L, 404L, 405L, 409L, 412L)) stop(
      "Flaschenpost WebDAV stopped at HTTP ", status, ".", call. = F)
    if (method == "MKCOL_LOCK" && status == 201L) state$owns_lock = T
    if (method == "DELETE_LOCK" && status %in% c(200L, 204L)) {
      state$owns_lock = F
    }
    as.integer(status)
  }

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) EXPLICIT CAPABILITY PROBE; NEVER RUN BY CONSTRUCTION OR OFFLINE PREFLIGHT
# <-----------------------------------------------------------------------------

  verify = \() {
    state$verified = F
    token = paste0("probe_", basename(tempfile()), "_", Sys.getpid())
    area = paste0(fp_dav_base, "/capabilities/", token)
    local = tempfile("fp_dav_probe_")
    downloaded = tempfile("fp_dav_probe_read_")
    on.exit(unlink(c(local, downloaded)), add = T)
    owned = character()
    on.exit({
      failed = F
      for (path in rev(owned)) {
        status = tryCatch(send("DELETE_PROBE", path, probe = T),
          error = \(e) NA_integer_)
        if (is.na(status) || !status %in% c(200L, 204L, 404L)) failed = T
      }
      if (failed) {
        state$verified = F
        stop("Flaschenpost capability probe needs private cleanup.", call. = F)
      }
    }, add = T)
    for (path in c("APM/flaschenpost", "APM/flaschenpost/validation",
        "APM/flaschenpost/validation/w36", fp_dav_base,
        paste0(fp_dav_base, "/capabilities"))) {
      if (!send("MKCOL", path) %in% c(201L, 405L)) stop(
        "Cannot prepare the separate capability area.")
    }
    if (send("MKCOL", area) != 201L) stop("Probe area already exists.")
    owned = c(owned, area)
    probe_lock = paste0(area, "/lock")
    if (send("MKCOL", probe_lock) != 201L) stop("MKCOL probe failed.")
    owned = c(owned, probe_lock)
    if (send("MKCOL", probe_lock) != 405L) stop(
      "Server does not preserve an existing MKCOL lock.")
    path = paste0(area, "/conditional.txt")
    writeLines("APM Flaschenpost capability value A", local)
    expected = fp_hash(local)
    if (send("PUT_NEW", path, input = local, probe = T) != 201L) stop(
      "Conditional create probe failed.")
    owned = c(owned, path)
    writeLines("APM Flaschenpost capability value B", local)
    status = send("PUT_NEW", path, input = local, probe = T)
    if (send("GET", path, output = downloaded) != 200L ||
        !identical(fp_hash(downloaded), expected) || status != 412L) stop(
      "Server ignores conditional PUT; publication remains disabled.")
    listing = tempfile("fp_dav_probe_listing_")
    on.exit(unlink(listing), add = T)
    if (send("PROPFIND", area, output = listing) != 207L) stop(
      "Server does not support the required directory listing.")
    tryCatch(suppressWarnings(read_xml(listing, options = "NONET")),
      error = \(e) stop("Invalid WebDAV capability listing.", call. = F))
    state$verified = T
    invisible(list(conditional_put = T, exclusive_mkcol = T,
      propfind = T, namespace = fp_dav_base))
  }
  transport = \(method, path, input = NULL, output = NULL) {
    send(method, path, input, output)
  }
  attr(transport, "verify_capabilities") = verify
  attr(transport, "capabilities_verified") = \() state$verified
  transport
}
