# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261102

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) DIRECTORY-BACKED MOCK OF THE WEBDAV TRANSPORT (NO NETWORK)
# <-----------------------------------------------------------------------------

# Behaves like the real contract of fp_rm_http: PUT_NEW never overwrites
# (412), listings are WebDAV-like XML, errors are raised as exceptions.
# Fault hooks: fail_put_after, corrupt (GET returns changed bytes), hide
# (GET answers 404), offline (every call fails).
mock_remote = \(dir, fail_put_after = Inf, corrupt = character(),
  hide = character(), offline = F) {
  dir.create(dir, recursive = T, showWarnings = F)
  state = new.env(parent = emptyenv())
  state$log = list()
  state$puts = 0L
  state$fail_put_after = fail_put_after
  state$corrupt = corrupt
  state$hide = hide
  state$offline = offline
  tr = \(method, path, input = NULL, output = NULL) {
    if (state$offline) stop("injected transport failure: offline")
    state$log[[length(state$log) + 1L]] = c(method, path)
    full = file.path(dir, path)
    if (method == "GET") {
      if (path %in% state$hide || !file.exists(full) || dir.exists(full)) {
        return(404L)
      }
      file.copy(full, output, overwrite = T)
      if (path %in% state$corrupt) cat("x", file = output, append = T)
      return(200L)
    }
    if (method == "PUT_NEW") {
      if (file.exists(full)) return(412L)
      if (state$puts >= state$fail_put_after) {
        stop("injected transport failure: upload interrupted")
      }
      dir.create(dirname(full), recursive = T, showWarnings = F)
      stopifnot(file.copy(input, full))
      state$puts = state$puts + 1L
      return(201L)
    }
    if (method %in% c("MKCOL", "MKCOL_LOCK")) {
      if (dir.exists(full)) return(405L)
      dir.create(full, recursive = T)
      return(201L)
    }
    if (method == "DELETE_LOCK") {
      unlink(full, recursive = T)
      return(204L)
    }
    if (method == "PROPFIND") {
      if (!dir.exists(full)) return(404L)
      kids = list.files(full)
      xml = paste0("<d:multistatus>", paste0("<d:response><d:href>/",
        path, "/", kids, "</d:href></d:response>", collapse = ""),
        "</d:multistatus>")
      writeLines(xml, output)
      return(207L)
    }
    stop("unsupported method in mock: ", method)
  }
  attr(tr, "state") = state
  tr
}

remote_puts = \(tr) attr(tr, "state")$puts
remote_files = \(dir) sort(list.files(dir, recursive = T))
