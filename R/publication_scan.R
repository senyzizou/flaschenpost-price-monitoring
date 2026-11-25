# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING - FLASCHENPOST PREPARATION
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261028

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) FAIL-CLOSED FILE INSPECTION INCLUDING SERIALIZED AND COMPRESSED DATA
# <-----------------------------------------------------------------------------

fp_ci_privacy = \(files) {
  blocked_names = c("street", "house_number", "housenumber", "address",
    "delivery_address", "deliveryaddress", "customer_id", "customerid",
    "session", "session_id", "sessionid", "cookie", "cookies", "token",
    "access_token", "refresh_token", "password", "secret", "email", "phone")
  inspect = \(object) {
    if (is.function(object) || is.environment(object) || is.language(object) ||
        typeof(object) %in% c("externalptr", "weakref", "raw")) stop(
      "Unsupported executable or opaque serialized object.")
    if (length(names(object)) &&
        any(tolower(names(object)) %in% blocked_names)) stop(
      "Sensitive structured field in package data.")
    if (is.character(object)) {
      fp_preflight_privacy(character(), text = object)
      pattern = paste0("(?i)(/", "home/|[a-z]:[/\\\\]Users[/\\\\]|",
        "ghp_[a-z0-9]{20,}|github_pat_[a-z0-9_]{20,}|",
        "[a-z0-9._%+-]+@[a-z0-9.-]+[.][a-z]{2,})")
      if (any(grepl(pattern, object, perl = T))) stop(
        "Sensitive user path, token or contact value in package data.")
    }
    if (is.list(object)) for (value in object) inspect(value)
    attrs = attributes(object)
    if (length(attrs)) for (value in attrs) inspect(value)
    invisible(T)
  }
  kinds = character()
  for (file in files) {
    if (!file.exists(file) || dir.exists(file) || nzchar(Sys.readlink(file))) {
      stop("Missing file or symlink in package.")
    }
    extension = if (basename(file) == ".gitignore") "txt" else {
      tolower(tools::file_ext(file))
    }
    if (extension %in% c("rds", "rda", "rdata")) {
      if (extension != "rds") stop("RData workspaces are not releasable.")
      inspect(readRDS(file))
      kinds = c(kinds, "rds")
      next
    }
    zipped = extension == "gz"
    inner = if (zipped) tolower(tools::file_ext(sub("[.]gz$", "", file))) else
      extension
    if (!inner %in% c("r", "csv", "json", "md", "yml", "yaml", "txt",
        "log")) stop("Unreviewed binary or archive format in package.")
    connection = if (zipped) gzfile(file, "rb") else file(file, "rb")
    bytes = tryCatch(readBin(connection, "raw", 16L * 1024L * 1024L + 1L),
      finally = close(connection))
    if (length(bytes) > 16L * 1024L * 1024L || any(bytes == as.raw(0))) stop(
      "Oversized, opaque or binary text payload in package.")
    text = rawToChar(bytes)
    if (!validUTF8(text)) stop("Non-UTF-8 package text.")
    fp_preflight_privacy(character(), text = text)
    if (inner == "json") inspect(fromJSON(text, simplifyVector = F)) else
      if (inner == "csv") {
        inspect(read.csv(text = text, colClasses = "character",
          check.names = F, stringsAsFactors = F))
      } else inspect(text)
    kinds = c(kinds, if (zipped) paste0(inner, ".gz") else inner)
  }
  invisible(list(status = "passed", files = length(files),
    formats = as.list(table(kinds)), opaque_files_released = 0L))
}
