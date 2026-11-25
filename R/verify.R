# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261030

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) EVIDENCE LOG FOR WAREHOUSE ASSIGNMENTS (NO ADDRESSES)
# <-----------------------------------------------------------------------------

# The log holds only location ids and warehouse ids. Street, house number and
# postal code stay in the private table outside the project. The session
# counter in config/multi_warehouse.json is never changed by this code.

rp_vf_columns = \() {
  c("session_id", "location_id", "warehouse_id_expected",
    "warehouse_id_observed", "observed_via", "client_state_reset",
    "started_utc", "operator", "note")
}

rp_vf_path = \(root) {
  file.path(root, "data", "verification", "warehouse_verification_log.csv")
}

rp_vf_read = \(path) {
  if (!file.exists(path)) {
    return(as.data.frame(setNames(
      rep(list(character()), length(rp_vf_columns())), rp_vf_columns()
    )))
  }
  read.csv(path, colClasses = "character", na.strings = character())
}

# location id -> warehouse id from the documented validation locations
rp_vf_locations = \(root) {
  loc = jsonlite::fromJSON(
    file.path(root, "config", "validation_locations.json"),
    simplifyVector = T
  )$locations
  setNames(as.character(loc$warehouse_id), loc$location_id)
}

rp_vf_validate = \(log, mw, root) {
  problems = character()
  bad_cols = intersect(tolower(names(log)),
    tolower(mw$privacy$forbidden_columns))
  if (length(bad_cols)) {
    problems = c(problems, paste0("forbidden column: ", bad_cols))
  }
  missing = setdiff(rp_vf_columns(), names(log))
  if (length(missing)) {
    problems = c(problems, paste0("missing column: ", missing))
  }
  if (length(problems)) return(list(ok = F, problems = problems,
    valid = log[0, , drop = F]))
  locs = rp_vf_locations(root)
  ts = suppressWarnings(as.POSIXct(log$started_utc,
    format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  expected_ok = log$location_id %in% names(locs) &
    locs[log$location_id] == log$warehouse_id_expected
  row_ok = !duplicated(log$session_id) & nzchar(log$session_id) &
    expected_ok & !is.na(ts) &
    log$client_state_reset %in% c("TRUE", "FALSE") &
    grepl("^[0-9]+$", log$warehouse_id_observed)
  if (any(!row_ok)) {
    problems = c(problems, paste0("invalid row: ", log$session_id[!row_ok]))
  }
  # valid evidence = assignment confirmed AND independent client state
  valid = row_ok & log$warehouse_id_observed == log$warehouse_id_expected &
    log$client_state_reset == "TRUE"
  mismatch = row_ok & log$warehouse_id_observed != log$warehouse_id_expected
  if (any(mismatch)) {
    problems = c(problems, paste0("assignment mismatch: ",
      log$session_id[mismatch]))
  }
  list(ok = !length(problems), problems = problems,
    valid = log[valid, , drop = F])
}

# Sessions the evidence supports: the documented session of 2026-10-29
# plus valid, distinct, client-state-reset sessions from the log.
rp_vf_supported = \(log, mw, root, warehouse_id) {
  v = rp_vf_validate(log, mw, root)$valid
  1L + sum(v$warehouse_id_expected == as.character(warehouse_id))
}

rp_vf_append = \(root, mw, entry) {
  path = rp_vf_path(root)
  dir.create(dirname(path), recursive = T, showWarnings = F)
  log = rp_vf_read(path)
  new = rbind(log, entry[rp_vf_columns()])
  chk = rp_vf_validate(new, mw, root)
  if (any(grepl("^invalid row", chk$problems))) {
    stop("Entry rejected: ", paste(chk$problems, collapse = "; "),
      call. = F)
  }
  write.csv(new, path, row.names = F, na = "")
  invisible(chk)
}
