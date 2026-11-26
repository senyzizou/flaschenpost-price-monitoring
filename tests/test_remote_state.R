# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261102

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 0) ESSENTIALS
# <-----------------------------------------------------------------------------

args = commandArgs(trailingOnly = F)
script_arg = args[grep("^--file=", args)]
script_dir = if (length(script_arg)) {
  dirname(sub("^--file=", "", script_arg))
} else dirname(sys.frame(1)$ofile)
root = normalizePath(file.path(script_dir, ".."),
  mustWork = T)
setwd(root)

tally = new.env(parent = emptyenv())
tally$pass = 0L
tally$fail = 0L
check = \(label, expr) {
  ok = tryCatch(isTRUE(expr), error = \(e) {
    message("  ERROR: ", conditionMessage(e))
    F
  })
  if (ok) {
    tally$pass = tally$pass + 1L
  } else {
    tally$fail = tally$fail + 1L
    message("FAIL: ", label)
  }
}

library(jsonlite)
source("scripts/bootstrap.R")
load_regional(root)

tmp_root = tempfile("fp_remote_state_")
dir.create(tmp_root, recursive = T)
on.exit(unlink(tmp_root, recursive = T), add = T)

fixtures = file.path(root, "tests", "fixtures")

cat("\n=== REMOTE STATE AND ABORT SCENARIOS ===\n")

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) MOCK TRANSPORT
# <-----------------------------------------------------------------------------

make_mock_transport = \() {
  state = new.env(parent = emptyenv())
  state$files = list()
  state$dirs = character()
  state$call_log = character()
  state$fail_on_path = NULL

  \(method, path, input = NULL, output = NULL) {
    state$call_log = c(state$call_log,
      paste(method, path))
    if (!is.null(state$fail_on_path) &&
        grepl(state$fail_on_path, path)) {
      state$fail_on_path = NULL
      stop("Simulated transport failure", call. = F)
    }
    switch(method,
      "MKCOL" = {
        existed = path %in% state$dirs
        state$dirs = unique(c(state$dirs, path))
        if (existed) 405L else 201L
      },
      "MKCOL_LOCK" = {
        if (path %in% state$dirs) 405L else {
          state$dirs = c(state$dirs, path)
          201L
        }
      },
      "DELETE_LOCK" = {
        prefix = paste0(path, "/")
        child_keys = grep(
          paste0("^", gsub("[.]", "\\\\.",
            prefix)),
          names(state$files), value = T)
        for (k in child_keys)
          state$files[[k]] = NULL
        state$dirs = setdiff(state$dirs, path)
        204L
      },
      "PUT_NEW" = {
        if (path %in% names(state$files)) 412L
        else {
          state$files[[path]] = readBin(
            input, "raw", file.size(input))
          201L
        }
      },
      "GET" = {
        if (path %in% names(state$files)) {
          if (!is.null(output)) {
            writeBin(state$files[[path]], output)
          }
          200L
        } else 404L
      },
      "PROPFIND" = {
        prefix = paste0(path, "/")
        children = grep(
          paste0("^", gsub("[.]", "\\\\.",
            prefix)),
          c(names(state$files), state$dirs),
          value = T)
        if (!length(children) &&
            !path %in% state$dirs) {
          return(404L)
        }
        if (!is.null(output)) {
          hrefs = paste0(
            "<href>", c(path, children),
            "</href>", collapse = "\n")
          writeLines(hrefs, output)
        }
        207L
      },
      stop("Unknown method: ", method, call. = F)
    )
  }
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) HELPER: BUILD OR EXTEND A LOCAL STORE
# <-----------------------------------------------------------------------------

make_test_store = \(n_commits = 1L,
  base_date = "2026-11-10", ancestor = NULL) {
  store = tempfile("store_", tmpdir = tmp_root)
  existing = 0L
  if (is.null(ancestor)) {
    fp_vs_init(store)
  } else {
    existing = length(fp_vs_history(ancestor))
    stopifnot(dir.create(store), all(file.copy(
      list.files(ancestor, all.files = T, no.. = T, full.names = T),
      store, recursive = T
    )))
  }
  fix = file.path(fixtures, "synthetic_w36.csv")
  raw = read.csv(fix, stringsAsFactors = F,
    colClasses = "character")
  for (i in existing + seq_len(n_commits)) {
    date = as.character(
      as.Date(base_date) + (i - 1L))
    wid = c(36L, 26L, 37L, 32L)[
      ((i - 1L) %% 4L) + 1L]
    raw$run_id = paste0("test_run_", i)
    raw$observed_at = paste0(date, "T10:00:00Z")
    raw$daily_date = date
    raw$warehouse_id = as.character(wid)
    raw$retailer = "flaschenpost"
    raw$product_key = fp_vs_product_key(raw$sku)
    raw$observation_key = paste(
      "flaschenpost", paste0("w", wid),
      raw$sku, date, sep = "|")
    raw$listing_state = "listed"
    raw$availability_state = ifelse(
      raw$available == "TRUE",
      "available", "unavailable")
    raw$listing_metadata_file = NA_character_
    raw$raw_sha256 = NA_character_
    raw$collection_method = "api"
    raw$article_status = "active"
    raw$sales_status = "available"
    raw$regional_price = ifelse(
      is.na(raw$regional_price),
      "FALSE", raw$regional_price)
    raw$quality_status = "analysis_ready"
    for (col in fp_vs_tracked_columns()) {
      if (!col %in% names(raw))
        raw[[col]] = NA_character_
    }
    for (col in fp_vs_observation_columns()) {
      if (!col %in% names(raw))
        raw[[col]] = NA_character_
    }
    shard = tempfile(fileext = ".csv.gz")
    fp_vs_write_gz(raw, shard)
    receipt = tempfile(fileext = ".json")
    write_json(list(
      date = date,
      run_id = paste0("test_run_", i),
      n_products = nrow(raw),
      warehouse_id = wid,
      observations = list(
        sha256 = fp_vs_hash(shard),
        path = "observations.csv.gz"
      )
    ), receipt, auto_unbox = T, pretty = T,
      na = "null")
    fp_vs_ingest(store, shard, receipt, wid)
  }
  store
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) SCENARIO: CLEAN PUBLISH AND RE-PUBLISH
# <-----------------------------------------------------------------------------

cat("-- 3) clean publish --\n")

store1 = make_test_store(2L)
t1 = make_mock_transport()

check("clean publish succeeds", {
  pub = fp_rm_publish_store(t1, store1)
  identical(pub$status, "published") &&
    length(pub$published) == 2L
})

check("re-publish is idempotent", {
  pub = fp_rm_publish_store(t1, store1)
  identical(pub$status, "nothing_to_publish")
})

check("markers written for both commits", {
  mk1 = fp_rm_read_marker(t1, "commit", "c0001")
  mk2 = fp_rm_read_marker(t1, "commit", "c0002")
  !is.null(mk1) && !is.null(mk2)
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) SCENARIO: ABORT AFTER LOCAL COMMIT
# <-----------------------------------------------------------------------------

cat("-- 4) abort after local commit --\n")

store2 = make_test_store(1L)
t2 = make_mock_transport()
fp_rm_publish_store(t2, store2)

store2b = make_test_store(ancestor = store2)
check("sync detects unpublished local commit", {
  cmp = fp_rm_compare(t2, store2b)
  nrow(cmp$local_only) == 1L
})

check("re-publish after abort succeeds", {
  pub = fp_rm_publish_store(t2, store2b)
  identical(pub$status, "published") &&
    length(pub$published) == 1L
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) SCENARIO: ABORT DURING UPLOAD
# <-----------------------------------------------------------------------------

cat("-- 5) abort during upload --\n")

store3 = make_test_store(2L)
t3 = make_mock_transport()

env3 = environment(t3)
env3$state$fail_on_path = "commits/c0001"

check("upload failure stops publish", {
  res = tryCatch(
    fp_rm_publish_store(t3, store3),
    error = \(e) list(status = "failed"))
  identical(res$status, "failed")
})

check("no marker for failed unit", {
  mk = fp_rm_read_marker(t3, "commit", "c0001")
  is.null(mk)
})

check("retry after failure completes", {
  pub = fp_rm_publish_store(t3, store3)
  identical(pub$status, "published")
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 6) SCENARIO: ABORT AFTER UPLOAD BEFORE REMOTE HEAD
# <-----------------------------------------------------------------------------

cat("-- 6) post-upload abort --\n")

store4 = make_test_store(1L)
t4 = make_mock_transport()
fp_rm_publish_store(t4, store4)

store4b = make_test_store(ancestor = store4)

# Manually upload unit without marker
dir4 = file.path(store4b, "commits", "c0002")
files4 = list.files(dir4)
base4 = paste0(fp_rm_base, "/commits/c0002")
for (f in files4) {
  fp_rm_put_immutable(t4,
    file.path(dir4, f),
    paste0(base4, "/", f))
}

check("unit exists but no marker", {
  mk = fp_rm_read_marker(t4, "commit", "c0002")
  is.null(mk)
})

check("publish writes missing marker", {
  pub = fp_rm_publish_store(t4, store4b)
  identical(pub$status, "published")
})

check("marker now exists after recovery", {
  mk = fp_rm_read_marker(t4, "commit", "c0002")
  !is.null(mk)
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 7) SCENARIO: REPEATED SAME-DAY UPLOAD
# <-----------------------------------------------------------------------------

cat("-- 7) same-day repeated upload --\n")

store5 = make_test_store(1L)
t5 = make_mock_transport()
fp_rm_publish_store(t5, store5)

check("second identical publish is noop", {
  pub = fp_rm_try_publish(t5, store5)
  pub$status == "nothing_to_publish"
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 8) SCENARIO: RECOVERY AFTER INCOMPLETE DAY
# <-----------------------------------------------------------------------------

cat("-- 8) incomplete day recovery --\n")

store6a = make_test_store(1L)
t6 = make_mock_transport()
fp_rm_publish_store(t6, store6a)

store6b = make_test_store(2L, ancestor = store6a)

check("two unpublished commits detected", {
  cmp = fp_rm_compare(t6, store6b)
  nrow(cmp$local_only) == 2L
})

check("incremental publish covers both", {
  pub = fp_rm_publish_store(t6, store6b)
  length(pub$published) == 2L
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 9) SCENARIO: REMOTE HEAD POINTS TO MISSING FILES
# <-----------------------------------------------------------------------------

cat("-- 9) corrupt remote detection --\n")

store7 = make_test_store(2L)
t7 = make_mock_transport()
fp_rm_publish_store(t7, store7)

env7 = environment(t7)
mk_key = paste0(fp_rm_base,
  "/markers/commits/c0002.json")
old_mk = rawToChar(env7$state$files[[mk_key]])
mk_obj = fromJSON(old_mk)
mk_obj$manifest_sha256 = paste0(
  rep("0", 64), collapse = "")
new_mk = toJSON(mk_obj,
  auto_unbox = T, pretty = T)
env7$state$files[[mk_key]] = charToRaw(new_mk)

check("compare detects integrity conflict", {
  cmp = fp_rm_compare(t7, store7)
  length(cmp$conflicts) >= 1L
})

check("publish refuses on conflict", {
  res = fp_rm_try_publish(t7, store7)
  identical(res$status, "failed") &&
    grepl("[Cc]onflict", res$error)
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 10) SCENARIO: MULTIPLE WAREHOUSE COMMITS SAME DAY
# <-----------------------------------------------------------------------------

cat("-- 10) multi-warehouse day --\n")

store8 = file.path(tmp_root, "store_multi_wh")
fp_vs_init(store8)

fix = file.path(fixtures, "synthetic_w36.csv")
for (wid in c(36L, 26L, 37L)) {
  raw = read.csv(fix, stringsAsFactors = F,
    colClasses = "character")
  raw$run_id = paste0("multi_", wid)
  raw$observed_at = "2026-11-10T10:00:00Z"
  raw$daily_date = "2026-11-10"
  raw$warehouse_id = as.character(wid)
  raw$retailer = "flaschenpost"
  raw$product_key = fp_vs_product_key(raw$sku)
  raw$observation_key = paste(
    "flaschenpost", paste0("w", wid),
    raw$sku, "2026-11-10", sep = "|")
  raw$listing_state = "listed"
  raw$availability_state = ifelse(
    raw$available == "TRUE",
    "available", "unavailable")
  raw$listing_metadata_file = NA_character_
  raw$raw_sha256 = NA_character_
  raw$collection_method = "api"
  raw$article_status = "active"
  raw$sales_status = "available"
  raw$regional_price = ifelse(
    is.na(raw$regional_price),
    "FALSE", raw$regional_price)
  raw$quality_status = "analysis_ready"
  for (col in fp_vs_tracked_columns()) {
    if (!col %in% names(raw))
      raw[[col]] = NA_character_
  }
  for (col in fp_vs_observation_columns()) {
    if (!col %in% names(raw))
      raw[[col]] = NA_character_
  }
  shard = tempfile(fileext = ".csv.gz")
  fp_vs_write_gz(raw, shard)
  receipt = tempfile(fileext = ".json")
  write_json(list(
    date = "2026-11-10",
    run_id = paste0("multi_", wid),
    n_products = nrow(raw),
    warehouse_id = wid,
    observations = list(
      sha256 = fp_vs_hash(shard),
      path = "observations.csv.gz"
    )
  ), receipt, auto_unbox = T, pretty = T,
    na = "null")
  fp_vs_ingest(store8, shard, receipt, wid)
}

t8 = make_mock_transport()
check("three same-day commits published", {
  pub = fp_rm_publish_store(t8, store8)
  length(pub$published) == 3L
})

check("all three markers present", {
  all(vapply(c("c0001", "c0002", "c0003"), \(id) {
    !is.null(fp_rm_read_marker(t8, "commit", id))
  }, T))
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 11) SCENARIO: WRITER LOCK LIFECYCLE
# <-----------------------------------------------------------------------------

cat("-- 11) writer lock lifecycle --\n")

t9 = make_mock_transport()

check("no lock initially", {
  probe = fp_rm_lock_probe(t9)
  !probe$locked
})

check("break with no lock is noop", {
  res = fp_rm_break_lock(t9)
  identical(res$status, "no_lock")
})

fp_rm_publish_store(t9,
  make_test_store(1L, "2026-11-20"))

check("lock released after publish", {
  probe = fp_rm_lock_probe(t9)
  !probe$locked
})

# Simulate orphaned lock
env9 = environment(t9)
env9$state$dirs = c(env9$state$dirs,
  fp_rm_lock_path)
info_tmp = tempfile("lock_info_")
write_json(list(
  acquired_utc = format(
    Sys.time() - 4 * 3600,
    "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  runner_id = "dead_runner_999",
  hostname = "ci-runner-old"
), info_tmp, auto_unbox = T, pretty = T)
env9$state$files[[fp_rm_lock_info_path]] =
  readBin(info_tmp, "raw", file.size(info_tmp))
unlink(info_tmp)

check("orphaned lock detected", {
  probe = fp_rm_lock_probe(t9)
  probe$locked && probe$stale
})

check("stale lock age > 3h", {
  probe = fp_rm_lock_probe(t9)
  probe$age_hours > 3
})

check("fresh lock not broken without confirm", {
  env9b = environment(t9)
  fresh_info = tempfile("lock_fresh_")
  write_json(list(
    acquired_utc = format(
      Sys.time(),
      "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    runner_id = "active_runner",
    hostname = "ci-runner"
  ), fresh_info, auto_unbox = T, pretty = T)
  env9b$state$files[[fp_rm_lock_info_path]] =
    readBin(fresh_info, "raw",
      file.size(fresh_info))
  unlink(fresh_info)
  res = fp_rm_break_lock(t9)
  identical(res$status,
    "active_lock_not_broken")
})

check("stale lock broken automatically", {
  stale_info = tempfile("lock_stale_")
  write_json(list(
    acquired_utc = format(
      Sys.time() - 5 * 3600,
      "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    runner_id = "dead_runner",
    hostname = "ci-old"
  ), stale_info, auto_unbox = T, pretty = T)
  env9$state$files[[fp_rm_lock_info_path]] =
    readBin(stale_info, "raw",
      file.size(stale_info))
  unlink(stale_info)
  res = fp_rm_break_lock(t9)
  identical(res$status, "broken") &&
    res$was_stale
})

check("lock cleared after break", {
  probe = fp_rm_lock_probe(t9)
  !probe$locked
})

check("publish works after break", {
  t10 = make_mock_transport()
  pub = fp_rm_publish_store(t10,
    make_test_store(1L, "2026-11-25"))
  identical(pub$status, "published")
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# RESULT
# <-----------------------------------------------------------------------------

cat(sprintf(
  "\nRemote state tests: %d PASS, %d FAIL\n",
  tally$pass, tally$fail))
if (tally$fail > 0L) {
  stop(tally$fail, " test(s) failed", call. = F)
}
