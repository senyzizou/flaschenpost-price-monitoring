# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  FLASCHENPOST PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261125

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) VERIFY THE PRIVATE BASELINE
# <-----------------------------------------------------------------------------

fp_hist_verify = \(bundle) {
  file_index = read.csv(file.path(bundle, "SHA256.csv"),
    colClasses = "character")
  actual = list.files(bundle, recursive = T)
  paths = file.path(bundle, file_index$file)
  stopifnot(!anyDuplicated(file_index$file),
    setequal(c(file_index$file, "SHA256.csv"), actual),
    !any(grepl("(^|/)[.]{1,2}(/|$)", file_index$file)),
    !any(nzchar(Sys.readlink(paths))),
    identical(unname(vapply(paths, fp_vs_hash, "")), file_index$sha256))
  store = file.path(bundle, "versioned_store")
  stopifnot(identical(names(fp_vs_history(store)), sprintf("c%04d", 1:5)),
    identical(names(fp_an_supplements(store)), sprintf("s%04d", 1:2)))
  runs = read.csv(file.path(bundle, "runs.csv"), colClasses = "character")
  counts = vapply(runs$id, \(id) {
    stopifnot(grepl("^[A-Za-z0-9_-]+$", id))
    dir = file.path(bundle, "runs", id)
    files = sort(list.files(file.path(dir, "raw"), full.names = T))
    calls = read.csv(file.path(dir, "calls.csv"), colClasses = "character")
    observed = unname(vapply(files, fp_vs_hash, ""))
    stopifnot(identical(observed, calls$sha256))
    length(files)
  }, 1L)
  stopifnot(nrow(runs) == 5L, !anyDuplicated(runs$id), sum(counts) == 419L)
  list(store = store, runs = runs, raw_files = sum(counts))
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) TRANSFER BASELINE AND VALIDATE A FRESH RESTORE
# <-----------------------------------------------------------------------------

fp_hist_seed = \(bundle, transport) {
  seed = fp_hist_verify(bundle)
  fp_rm_with_lock(transport, \() {
    for (i in seq_len(nrow(seed$runs))) {
      row = seed$runs[i, ]
      raw = file.path(bundle, "runs", row$id, "raw")
      fp_rm_archive_raw(transport, raw, row$date, row$warehouse_id, row$id)
    }
    fp_rm_publish_store(transport, seed$store, locked = T)
    fresh = tempfile("fp_restore_")
    on.exit(unlink(fresh, recursive = T), add = T)
    fp_rm_restore(transport, fresh)
    files = list.files(seed$store, recursive = T)
    stopifnot(setequal(files, list.files(fresh, recursive = T)),
      identical(unname(vapply(file.path(fresh, files), fp_vs_hash, "")),
        unname(vapply(file.path(seed$store, files), fp_vs_hash, ""))))
    invisible(seed$raw_files)
  })
}
