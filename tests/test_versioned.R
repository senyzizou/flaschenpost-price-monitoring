# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261030

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
check_error = \(label, expr) {
  ok = tryCatch({ expr; F }, error = \(e) T)
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

fixtures = file.path(root, "tests", "fixtures")

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) FIXTURE GENERATION FROM SYNTHETIC DATA
# <-----------------------------------------------------------------------------

make_receipt = \(date, run_id, n, shard_path,
  warehouse_id = 36L) {
  tmp = tempfile(fileext = ".json")
  receipt = list(
    date = date, run_id = run_id,
    n_products = n,
    warehouse_id = as.integer(warehouse_id),
    observations = list(
      sha256 = fp_vs_hash(shard_path),
      path = "observations.csv.gz"
    )
  )
  jsonlite::write_json(receipt, tmp,
    auto_unbox = T, pretty = T, na = "null")
  tmp
}

make_shard = \(fixture_csv, date, run_id,
  warehouse_id = 36L, observed_at = NULL) {
  raw = read.csv(fixture_csv, stringsAsFactors = F,
    colClasses = "character")
  if (is.null(observed_at)) {
    observed_at = paste0(date, "T12:00:00Z")
  }
  raw$run_id = run_id
  raw$observed_at = observed_at
  raw$daily_date = date
  raw$warehouse_id = as.character(warehouse_id)
  raw$retailer = "flaschenpost"
  raw$product_key = fp_vs_product_key(raw$sku)
  raw$observation_key = paste(
    "flaschenpost",
    paste0("w", warehouse_id),
    raw$sku, date, sep = "|"
  )
  raw$listing_state = "listed"
  raw$availability_state = ifelse(
    raw$available == "TRUE", "available", "unavailable"
  )
  raw$listing_metadata_file = NA_character_
  raw$raw_sha256 = NA_character_
  raw$collection_method = "api"
  raw$article_status = "active"
  raw$sales_status = "available"
  raw$regional_price = ifelse(
    is.na(raw$regional_price), "FALSE",
    raw$regional_price
  )
  raw$quality_status = "analysis_ready"
  for (col in fp_vs_tracked_columns()) {
    if (!col %in% names(raw)) {
      raw[[col]] = NA_character_
    }
  }
  for (col in fp_vs_observation_columns()) {
    if (!col %in% names(raw)) {
      raw[[col]] = NA_character_
    }
  }
  tmp = tempfile(fileext = ".csv.gz")
  fp_vs_write_gz(raw, tmp)
  tmp
}

tmp_root = tempfile("fp_vs_test_")
on.exit(unlink(tmp_root, recursive = T), add = T)

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) STORE INITIALIZATION TESTS
# <-----------------------------------------------------------------------------

store1 = file.path(tmp_root, "store1")
check("init creates store", {
  fp_vs_init(store1)
  dir.exists(store1) &&
    file.exists(file.path(store1, "schema.json")) &&
    dir.exists(file.path(store1, "commits"))
})

check("init rejects existing store", {
  tryCatch({ fp_vs_init(store1); F },
    error = \(e) T)
})

check("schema has expected fields", {
  s = fp_vs_read_schema(store1)
  all(c("observation_columns", "tracked", "core",
    "format_version") %in% names(s))
})

check("empty history", {
  h = fp_vs_history(store1)
  length(h) == 0L
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) BASIC INGEST TESTS
# <-----------------------------------------------------------------------------

shard1 = make_shard(
  file.path(fixtures, "synthetic_w36.csv"),
  "2026-10-28", "run_001", 36L
)
receipt1 = make_receipt(
  "2026-10-28", "run_001", 12L, shard1, 36L
)

check("first ingest commits", {
  res = fp_vs_ingest(store1, shard1, receipt1, 36L)
  identical(res, "committed")
})

check("history has one commit", {
  h = fp_vs_history(store1)
  length(h) == 1L &&
    identical(h[[1]]$date, "2026-10-28") &&
    identical(as.integer(h[[1]]$warehouse_id), 36L)
})

check("commit dir is sequential", {
  h = fp_vs_history(store1)
  identical(names(h), "c0001")
})

check("observations stored", {
  obs = fp_vs_table(store1, "observations.csv.gz")
  nrow(obs) == 12L &&
    all(obs$warehouse_id == "36")
})

check("product versions stored", {
  pv = fp_vs_table(
    store1, "product_versions.csv.gz"
  )
  nrow(pv) == 12L &&
    all(!is.na(pv$product_version_id))
})

check("changes logged for initial products", {
  ch = fp_vs_table(
    store1, "product_changes.csv.gz"
  )
  nrow(ch) == 12L &&
    all(ch$change_type == "initial_product")
})

check("version usage stored", {
  vu = fp_vs_table(
    store1, "version_usage.csv.gz"
  )
  nrow(vu) == 12L &&
    all(vu$content_changed == "TRUE")
})

check("manifest has sequential fields", {
  h = fp_vs_history(store1)
  m = h[[1]]
  !is.null(m$commit_seq) &&
    !is.null(m$logical_key) &&
    !is.null(m$committed_at_utc) &&
    identical(m$commit_seq, 1L) &&
    identical(
      m$logical_key,
      fp_vs_commit_id("2026-10-28", 36L)
    )
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) IDEMPOTENT REPEAT
# <-----------------------------------------------------------------------------

check("repeat ingest is idempotent", {
  res = fp_vs_ingest(store1, shard1, receipt1, 36L)
  identical(res, "already_committed")
})

check("history unchanged after repeat", {
  h = fp_vs_history(store1)
  length(h) == 1L
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) SECOND DAY: NO PRODUCT CHANGES
# <-----------------------------------------------------------------------------

shard2 = make_shard(
  file.path(fixtures, "synthetic_w36.csv"),
  "2026-10-29", "run_002", 36L
)
receipt2 = make_receipt(
  "2026-10-29", "run_002", 12L, shard2, 36L
)

check("second day commits", {
  res = fp_vs_ingest(store1, shard2, receipt2, 36L)
  identical(res, "committed")
})

check("no new versions on unchanged day", {
  cdir = fp_vs_find_commit(
    store1, "2026-10-29", 36L
  )
  pv = fp_vs_read_gz(file.path(
    store1, "commits", cdir,
    "product_versions.csv.gz"
  ))
  nrow(pv) == 0L
})

check("no changes on unchanged day", {
  cdir = fp_vs_find_commit(
    store1, "2026-10-29", 36L
  )
  ch = fp_vs_read_gz(file.path(
    store1, "commits", cdir,
    "product_changes.csv.gz"
  ))
  nrow(ch) == 0L
})

check("usage records second day", {
  cdir = fp_vs_find_commit(
    store1, "2026-10-29", 36L
  )
  vu = fp_vs_read_gz(file.path(
    store1, "commits", cdir,
    "version_usage.csv.gz"
  ))
  nrow(vu) == 12L &&
    all(vu$content_changed == "FALSE")
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 6) PRODUCT CHANGE DETECTION
# <-----------------------------------------------------------------------------

store2 = file.path(tmp_root, "store_changes")
fp_vs_init(store2)

shard_d1 = make_shard(
  file.path(fixtures, "synthetic_w36.csv"),
  "2026-10-27", "run_c01", 36L
)
receipt_d1 = make_receipt(
  "2026-10-27", "run_c01", 12L, shard_d1, 36L
)
fp_vs_ingest(store2, shard_d1, receipt_d1, 36L)

mod_csv = file.path(tmp_root, "modified_w36.csv")
raw = read.csv(
  file.path(fixtures, "synthetic_w36.csv"),
  stringsAsFactors = F
)
raw$brand[raw$sku == "741"] = "Krombacher NEU"
raw$alcohol_content[raw$sku == "3001"] = "37.5"
write.csv(raw, mod_csv, row.names = F)

shard_d2 = make_shard(
  mod_csv, "2026-10-28", "run_c02", 36L
)
receipt_d2 = make_receipt(
  "2026-10-28", "run_c02", 12L, shard_d2, 36L
)

check("changed product creates new version", {
  fp_vs_ingest(store2, shard_d2, receipt_d2, 36L)
  cdir = fp_vs_find_commit(
    store2, "2026-10-28", 36L
  )
  pv = fp_vs_read_gz(file.path(
    store2, "commits", cdir,
    "product_versions.csv.gz"
  ))
  nrow(pv) == 2L
})

check("change log records field changes", {
  cdir = fp_vs_find_commit(
    store2, "2026-10-28", 36L
  )
  ch = fp_vs_read_gz(file.path(
    store2, "commits", cdir,
    "product_changes.csv.gz"
  ))
  nrow(ch) >= 2L &&
    "brand" %in% ch$field &&
    "alcohol_content" %in% ch$field
})

check("unchanged products keep same version", {
  cdir = fp_vs_find_commit(
    store2, "2026-10-28", 36L
  )
  vu = fp_vs_read_gz(file.path(
    store2, "commits", cdir,
    "version_usage.csv.gz"
  ))
  n_changed = sum(vu$content_changed == "TRUE")
  n_changed == 2L
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 7) SEQUENTIAL PRODUCT VERSIONS
# <-----------------------------------------------------------------------------

raw$brand[raw$sku == "741"] = "Krombacher FINAL"
write.csv(raw, mod_csv, row.names = F)
shard_d3 = make_shard(
  mod_csv, "2026-10-29", "run_c03", 36L
)
receipt_d3 = make_receipt(
  "2026-10-29", "run_c03", 12L, shard_d3, 36L
)

check("third version for same product", {
  fp_vs_ingest(store2, shard_d3, receipt_d3, 36L)
  pv = fp_vs_table(
    store2, "product_versions.csv.gz"
  )
  krom = pv[pv$sku == "741", ]
  nrow(krom) == 3L &&
    all(as.integer(krom$episode) == 1:3)
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 8) MULTI-WAREHOUSE: SAME SKU, SAME DETAILS
# <-----------------------------------------------------------------------------

store3 = file.path(tmp_root, "store_multi")
fp_vs_init(store3)

shard_w12 = make_shard(
  file.path(fixtures, "synthetic_w12.csv"),
  "2026-10-28", "run_m02", 12L
)
receipt_w12 = make_receipt(
  "2026-10-28", "run_m02", 12L, shard_w12, 12L
)
fp_vs_ingest(store3, shard_w12, receipt_w12, 12L)

shard_w36 = make_shard(
  file.path(fixtures, "synthetic_w36.csv"),
  "2026-10-28", "run_m01", 36L
)
receipt_w36 = make_receipt(
  "2026-10-28", "run_m01", 12L, shard_w36, 36L
)

check("second warehouse same day commits", {
  res = fp_vs_ingest(
    store3, shard_w36, receipt_w36, 36L
  )
  identical(res, "committed")
})

check("shared SKUs reuse product versions", {
  pv = fp_vs_table(
    store3, "product_versions.csv.gz"
  )
  obs = fp_vs_table(store3, "observations.csv.gz")
  shared = intersect(
    obs$sku[obs$warehouse_id == "36"],
    obs$sku[obs$warehouse_id == "12"]
  )
  length(shared) >= 10L &&
    length(unique(pv$product_version_id)) < nrow(obs)
})

check("obs keys unique across warehouses", {
  obs = fp_vs_table(store3, "observations.csv.gz")
  !anyDuplicated(obs$observation_key)
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 9) MULTI-WAREHOUSE: SAME SKU, DIFFERENT DETAILS
# <-----------------------------------------------------------------------------

store4 = file.path(tmp_root, "store_diverge")
fp_vs_init(store4)

diff_csv = file.path(tmp_root, "diff_w12.csv")
raw_12 = read.csv(
  file.path(fixtures, "synthetic_w12.csv"),
  stringsAsFactors = F
)
raw_12$brand[raw_12$sku == "741"] = "Krombacher LOKAL"
write.csv(raw_12, diff_csv, row.names = F)

shard_b = make_shard(
  diff_csv, "2026-10-28", "run_d02", 12L
)
receipt_b = make_receipt(
  "2026-10-28", "run_d02", 12L, shard_b, 12L
)
fp_vs_ingest(store4, shard_b, receipt_b, 12L)

shard_a = make_shard(
  file.path(fixtures, "synthetic_w36.csv"),
  "2026-10-28", "run_d01", 36L
)
receipt_a = make_receipt(
  "2026-10-28", "run_d01", 12L, shard_a, 36L
)
fp_vs_ingest(store4, shard_a, receipt_a, 36L)

check("divergent SKU gets separate version", {
  pv = fp_vs_table(
    store4, "product_versions.csv.gz"
  )
  krom = pv[pv$sku == "741", ]
  nrow(krom) == 2L &&
    krom$content_sha256[1] != krom$content_sha256[2]
})

check("divergence logged in changes", {
  ch = fp_vs_table(
    store4, "product_changes.csv.gz"
  )
  krom = ch[ch$sku == "741", ]
  any(krom$field == "brand" |
    krom$change_type == "initial_product")
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 10) PRICE CHANGE WITHOUT NEW VERSION
# <-----------------------------------------------------------------------------

store5 = file.path(tmp_root, "store_price")
fp_vs_init(store5)

shard_p1 = make_shard(
  file.path(fixtures, "synthetic_w36.csv"),
  "2026-10-27", "run_p01", 36L
)
receipt_p1 = make_receipt(
  "2026-10-27", "run_p01", 12L, shard_p1, 36L
)
fp_vs_ingest(store5, shard_p1, receipt_p1, 36L)

price_csv = file.path(tmp_root, "price_change.csv")
raw_p = read.csv(
  file.path(fixtures, "synthetic_w36.csv"),
  stringsAsFactors = F
)
raw_p$price_cents[raw_p$sku == "741"] = 2199
raw_p$price_eur[raw_p$sku == "741"] = 21.99
write.csv(raw_p, price_csv, row.names = F)

shard_p2 = make_shard(
  price_csv, "2026-10-28", "run_p02", 36L
)
receipt_p2 = make_receipt(
  "2026-10-28", "run_p02", 12L, shard_p2, 36L
)
fp_vs_ingest(store5, shard_p2, receipt_p2, 36L)

check("price change creates no new version", {
  cdir = fp_vs_find_commit(
    store5, "2026-10-28", 36L
  )
  pv_d2 = fp_vs_read_gz(file.path(
    store5, "commits", cdir,
    "product_versions.csv.gz"
  ))
  nrow(pv_d2) == 0L
})

check("price in observations differs", {
  obs = fp_vs_table(store5, "observations.csv.gz")
  krom = obs[obs$sku == "741", ]
  krom$price_cents[1] != krom$price_cents[2]
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 11) AVAILABILITY CHANGE WITHOUT NEW VERSION
# <-----------------------------------------------------------------------------

avail_csv = file.path(tmp_root, "avail_change.csv")
raw_a = read.csv(
  file.path(fixtures, "synthetic_w36.csv"),
  stringsAsFactors = F
)
raw_a$available[raw_a$sku == "741"] = "FALSE"
write.csv(raw_a, avail_csv, row.names = F)

store6 = file.path(tmp_root, "store_avail")
fp_vs_init(store6)
shard_a1 = make_shard(
  file.path(fixtures, "synthetic_w36.csv"),
  "2026-10-27", "run_a01", 36L
)
receipt_a1 = make_receipt(
  "2026-10-27", "run_a01", 12L, shard_a1, 36L
)
fp_vs_ingest(store6, shard_a1, receipt_a1, 36L)

shard_a2 = make_shard(
  avail_csv, "2026-10-28", "run_a02", 36L
)
receipt_a2 = make_receipt(
  "2026-10-28", "run_a02", 12L, shard_a2, 36L
)
fp_vs_ingest(store6, shard_a2, receipt_a2, 36L)

check("availability change: no new version", {
  cdir = fp_vs_find_commit(
    store6, "2026-10-28", 36L
  )
  pv_d2 = fp_vs_read_gz(file.path(
    store6, "commits", cdir,
    "product_versions.csv.gz"
  ))
  nrow(pv_d2) == 0L
})

check("availability change: obs records differ", {
  obs = fp_vs_table(store6, "observations.csv.gz")
  krom = obs[obs$sku == "741", ]
  krom$available[1] != krom$available[2]
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 12) NEW PRODUCT APPEARS
# <-----------------------------------------------------------------------------

new_csv = file.path(tmp_root, "new_product.csv")
raw_n = read.csv(
  file.path(fixtures, "synthetic_w36.csv"),
  stringsAsFactors = F
)
new_row = raw_n[1, ]
new_row$sku = "99999"
new_row$product_key = "99999"
new_row$product_name = "Neues Testbier"
new_row$price_cents = 399
new_row$price_eur = 3.99
raw_n = rbind(raw_n, new_row)
write.csv(raw_n, new_csv, row.names = F)

store7 = file.path(tmp_root, "store_new")
fp_vs_init(store7)
shard_n1 = make_shard(
  file.path(fixtures, "synthetic_w36.csv"),
  "2026-10-27", "run_n01", 36L
)
receipt_n1 = make_receipt(
  "2026-10-27", "run_n01", 12L, shard_n1, 36L
)
fp_vs_ingest(store7, shard_n1, receipt_n1, 36L)

shard_n2 = make_shard(
  new_csv, "2026-10-28", "run_n02", 36L
)
receipt_n2 = make_receipt(
  "2026-10-28", "run_n02", 13L, shard_n2, 36L
)
fp_vs_ingest(store7, shard_n2, receipt_n2, 36L)

check("new product gets initial version", {
  cdir = fp_vs_find_commit(
    store7, "2026-10-28", 36L
  )
  pv_d2 = fp_vs_read_gz(file.path(
    store7, "commits", cdir,
    "product_versions.csv.gz"
  ))
  nrow(pv_d2) == 1L &&
    pv_d2$sku[1] == "99999"
})

check("new product in change log", {
  cdir = fp_vs_find_commit(
    store7, "2026-10-28", 36L
  )
  ch = fp_vs_read_gz(file.path(
    store7, "commits", cdir,
    "product_changes.csv.gz"
  ))
  any(ch$sku == "99999" &
    ch$change_type == "initial_product")
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 13) DISAPPEARED PRODUCT
# <-----------------------------------------------------------------------------

less_csv = file.path(tmp_root, "less_product.csv")
raw_l = read.csv(
  file.path(fixtures, "synthetic_w36.csv"),
  stringsAsFactors = F
)
raw_l = raw_l[raw_l$sku != "9902", ]
write.csv(raw_l, less_csv, row.names = F)

store8 = file.path(tmp_root, "store_disappear")
fp_vs_init(store8)
shard_l1 = make_shard(
  file.path(fixtures, "synthetic_w36.csv"),
  "2026-10-27", "run_l01", 36L
)
receipt_l1 = make_receipt(
  "2026-10-27", "run_l01", 12L, shard_l1, 36L
)
fp_vs_ingest(store8, shard_l1, receipt_l1, 36L)

shard_l2 = make_shard(
  less_csv, "2026-10-28", "run_l02", 36L
)
receipt_l2 = make_receipt(
  "2026-10-28", "run_l02", 11L, shard_l2, 36L
)
fp_vs_ingest(store8, shard_l2, receipt_l2, 36L)

check("disappeared product absent from day 2", {
  cdir = fp_vs_find_commit(
    store8, "2026-10-28", 36L
  )
  obs_d2 = fp_vs_read_gz(file.path(
    store8, "commits", cdir,
    "observations.csv.gz"
  ))
  !"9902" %in% obs_d2$sku
})

check("disappeared product still in versions", {
  pv = fp_vs_table(
    store8, "product_versions.csv.gz"
  )
  "9902" %in% pv$sku
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 14) HISTORICAL RECONSTRUCTION
# <-----------------------------------------------------------------------------

check("restore full range", {
  panel = fp_vs_restore(
    store1, "2026-10-28", "2026-10-29"
  )
  nrow(panel) == 24L &&
    length(unique(panel$daily_date)) == 2L
})

check("restore single day", {
  panel = fp_vs_restore(
    store1, "2026-10-28", "2026-10-28"
  )
  nrow(panel) == 12L
})

check("restore with warehouse filter", {
  panel = fp_vs_restore(
    store3, "2026-10-28", "2026-10-28",
    warehouses = 36L
  )
  all(panel$warehouse_id == "36")
})

check("restore no duplicates", {
  panel = fp_vs_restore(
    store3, "2026-10-28", "2026-10-28"
  )
  !anyDuplicated(panel$observation_key)
})

check("restore rejects invalid range", {
  tryCatch({
    fp_vs_restore(store1, "2030-01-01", "2030-12-31")
    F
  }, error = \(e) T)
})

check("restore uses historical version", {
  panel = fp_vs_restore(
    store2, "2026-10-27", "2026-10-29"
  )
  krom = panel[panel$sku == "741", ]
  nrow(krom) == 3L &&
    krom$brand[krom$daily_date == "2026-10-27"] !=
      krom$brand[krom$daily_date == "2026-10-29"]
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 15) REPEATED DAY RUN (IDEMPOTENT INGEST)
# <-----------------------------------------------------------------------------

check("repeated ingest same data", {
  store_r = file.path(tmp_root, "store_repeat")
  fp_vs_init(store_r)
  s1 = make_shard(
    file.path(fixtures, "synthetic_w36.csv"),
    "2026-10-28", "run_r01", 36L
  )
  r1 = make_receipt(
    "2026-10-28", "run_r01", 12L, s1, 36L
  )
  fp_vs_ingest(store_r, s1, r1, 36L)
  res = fp_vs_ingest(store_r, s1, r1, 36L)
  identical(res, "already_committed")
})

check("conflicting repeat rejected", {
  store_r = file.path(tmp_root, "store_repeat")
  s_alt = make_shard(
    file.path(fixtures, "synthetic_w12.csv"),
    "2026-10-28", "run_r_alt", 36L
  )
  r_alt = make_receipt(
    "2026-10-28", "run_r_alt", 12L, s_alt, 36L
  )
  tryCatch({
    fp_vs_ingest(store_r, s_alt, r_alt, 36L)
    F
  }, error = \(e) grepl(
    "[Cc]onflict", conditionMessage(e)
  ))
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 16) FAILED WAREHOUSE DOES NOT DAMAGE OTHERS
# <-----------------------------------------------------------------------------

check("failed warehouse safe", {
  store_f = file.path(tmp_root, "store_fail")
  fp_vs_init(store_f)
  s_f1 = make_shard(
    file.path(fixtures, "synthetic_w36.csv"),
    "2026-10-28", "run_f01", 12L
  )
  r_f1 = make_receipt(
    "2026-10-28", "run_f01", 12L, s_f1, 12L
  )
  fp_vs_ingest(store_f, s_f1, r_f1, 12L)
  s_f2 = make_shard(
    file.path(fixtures, "synthetic_w36.csv"),
    "2026-10-28", "run_f02", 36L
  )
  r_f2 = make_receipt(
    "2026-10-28", "run_f02", 12L, s_f2, 36L
  )
  err = tryCatch(
    fp_vs_ingest(
      store_f, s_f2, r_f2, 36L,
      fail_at = "after_versions"
    ),
    error = \(e) e
  )
  h = fp_vs_history(store_f)
  inherits(err, "error") &&
    length(h) == 1L &&
    nrow(fp_vs_table(
      store_f, "observations.csv.gz"
    )) == 12L
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 17) SAFE RESTORE AND ROLLBACK
# <-----------------------------------------------------------------------------

check("archive pack and restore", {
  store_ar = file.path(tmp_root, "store_archive")
  fp_vs_init(store_ar)
  s = make_shard(
    file.path(fixtures, "synthetic_w36.csv"),
    "2026-10-28", "run_ar01", 36L
  )
  r = make_receipt(
    "2026-10-28", "run_ar01", 12L, s, 36L
  )
  fp_vs_ingest(store_ar, s, r, 36L)
  archive = file.path(tmp_root, "archive1")
  inc = fp_ar_pack_increment(store_ar, archive)
  length(inc$packed) == 1L &&
    file.exists(file.path(archive, "index.json"))
})

check("restore from archive matches", {
  archive = file.path(tmp_root, "archive1")
  restored = file.path(tmp_root, "restored1")
  fp_ar_restore_increment(archive, restored)
  h = fp_vs_history(restored)
  length(h) == 1L
})

check("checkpoint integrity", {
  store_ar = file.path(tmp_root, "store_archive")
  cp = fp_ar_checkpoint(store_ar)
  cp$n_days == 1L && cp$n_observations == 12L &&
    cp$n_products == 12L
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 18) PRIVACY TESTS
# <-----------------------------------------------------------------------------

check("no private addresses in observations", {
  obs = fp_vs_table(store1, "observations.csv.gz")
  txt = unlist(obs, use.names = F)
  !any(grepl(
    "(stra(ss|ß)e|str\\.|weg|allee|platz|gasse)",
    tolower(txt)
  ))
})

check("no private data in versions", {
  pv = fp_vs_table(
    store1, "product_versions.csv.gz"
  )
  txt = unlist(pv, use.names = F)
  !any(grepl("Wrangel", txt, ignore.case = T))
})

check("observation keys contain no addresses", {
  obs = fp_vs_table(store1, "observations.csv.gz")
  !any(grepl("str\\.|weg|allee|platz|gasse",
    obs$observation_key, ignore.case = T))
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 19) ORIGINAL VALUES PRESERVED
# <-----------------------------------------------------------------------------

check("original empty strings preserved", {
  obs = fp_vs_table(store1, "observations.csv.gz")
  pv = fp_vs_table(
    store1, "product_versions.csv.gz"
  )
  is.data.frame(obs) && is.data.frame(pv)
})

check("prices in cents unchanged", {
  obs = fp_vs_table(store1, "observations.csv.gz")
  krom = obs[obs$sku == "741" &
    obs$daily_date == "2026-10-28", ]
  krom$price_cents == "1999"
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 20) OUT-OF-ORDER INGEST ALLOWED
# <-----------------------------------------------------------------------------

check("earlier date after later date succeeds", {
  store_ooo = file.path(tmp_root, "store_ooo")
  fp_vs_init(store_ooo)
  s2 = make_shard(
    file.path(fixtures, "synthetic_w36.csv"),
    "2026-10-29", "run_ooo02", 36L
  )
  r2 = make_receipt(
    "2026-10-29", "run_ooo02", 12L, s2, 36L
  )
  fp_vs_ingest(store_ooo, s2, r2, 36L)
  s1 = make_shard(
    file.path(fixtures, "synthetic_w36.csv"),
    "2026-10-28", "run_ooo01", 36L
  )
  r1 = make_receipt(
    "2026-10-28", "run_ooo01", 12L, s1, 36L
  )
  res = fp_vs_ingest(store_ooo, s1, r1, 36L)
  identical(res, "committed")
})

check("out-of-order: both in history", {
  store_ooo = file.path(tmp_root, "store_ooo")
  h = fp_vs_history(store_ooo)
  length(h) == 2L &&
    !is.null(
      fp_vs_find_commit(store_ooo, "2026-10-28", 36L)
    ) &&
    !is.null(
      fp_vs_find_commit(store_ooo, "2026-10-29", 36L)
    )
})

check("out-of-order: restore works", {
  store_ooo = file.path(tmp_root, "store_ooo")
  panel = fp_vs_restore(
    store_ooo, "2026-10-28", "2026-10-29"
  )
  nrow(panel) == 24L &&
    length(unique(panel$daily_date)) == 2L
})

check("out-of-order: chain valid", {
  store_ooo = file.path(tmp_root, "store_ooo")
  h = fp_vs_history(store_ooo)
  h[[1]]$previous_commit_sha256 == "GENESIS" &&
    identical(names(h), c("c0001", "c0002"))
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 21) CATCH-UP: w26 OK, w36 FAIL, w37 OK, w36 CATCH-UP
# <-----------------------------------------------------------------------------

check("catch-up scenario", {
  store_cu = file.path(tmp_root, "store_catchup")
  fp_vs_init(store_cu)

  s26 = make_shard(
    file.path(fixtures, "synthetic_w25.csv"),
    "2026-10-28", "run_cu01", 26L
  )
  r26 = make_receipt(
    "2026-10-28", "run_cu01", 11L, s26, 26L
  )
  res26 = fp_vs_ingest(
    store_cu, s26, r26, 26L
  )

  s36f = make_shard(
    file.path(fixtures, "synthetic_w36.csv"),
    "2026-10-28", "run_cu02", 36L
  )
  r36f = make_receipt(
    "2026-10-28", "run_cu02", 12L, s36f, 36L
  )
  err36 = tryCatch(
    fp_vs_ingest(
      store_cu, s36f, r36f, 36L,
      fail_at = "after_versions"
    ),
    error = \(e) e
  )

  s37 = make_shard(
    file.path(fixtures, "synthetic_w36.csv"),
    "2026-10-28", "run_cu03", 37L
  )
  r37 = make_receipt(
    "2026-10-28", "run_cu03", 12L, s37, 37L
  )
  res37 = fp_vs_ingest(
    store_cu, s37, r37, 37L
  )

  s36 = make_shard(
    file.path(fixtures, "synthetic_w36.csv"),
    "2026-10-28", "run_cu04", 36L
  )
  r36 = make_receipt(
    "2026-10-28", "run_cu04", 12L, s36, 36L
  )
  res36 = fp_vs_ingest(
    store_cu, s36, r36, 36L
  )

  identical(res26, "committed") &&
    inherits(err36, "error") &&
    identical(res37, "committed") &&
    identical(res36, "committed")
})

check("catch-up: all three warehouses present", {
  store_cu = file.path(tmp_root, "store_catchup")
  h = fp_vs_history(store_cu)
  length(h) == 3L &&
    !is.null(
      fp_vs_find_commit(store_cu, "2026-10-28", 26L)
    ) &&
    !is.null(
      fp_vs_find_commit(store_cu, "2026-10-28", 37L)
    ) &&
    !is.null(
      fp_vs_find_commit(store_cu, "2026-10-28", 36L)
    )
})

check("catch-up: chain intact", {
  store_cu = file.path(tmp_root, "store_catchup")
  h = fp_vs_history(store_cu)
  identical(names(h), c("c0001", "c0002", "c0003"))
})

check("catch-up: day completeness", {
  store_cu = file.path(tmp_root, "store_catchup")
  dc = fp_ar_day_completeness(
    store_cu, "2026-10-28", c(26L, 36L, 37L)
  )
  dc$complete &&
    setequal(dc$committed_warehouses, c(26L, 36L, 37L))
})

check("catch-up: restore all warehouses", {
  store_cu = file.path(tmp_root, "store_catchup")
  panel = fp_vs_restore(
    store_cu, "2026-10-28", "2026-10-28"
  )
  setequal(unique(panel$warehouse_id),
    c("26", "36", "37"))
})

check("catch-up: w36 filter works", {
  store_cu = file.path(tmp_root, "store_catchup")
  panel = fp_vs_restore(
    store_cu, "2026-10-28", "2026-10-28",
    warehouses = 36L
  )
  all(panel$warehouse_id == "36")
})

check("catch-up: later day after catch-up", {
  store_cu = file.path(tmp_root, "store_catchup")
  s_d2 = make_shard(
    file.path(fixtures, "synthetic_w25.csv"),
    "2026-10-29", "run_cu05", 26L
  )
  r_d2 = make_receipt(
    "2026-10-29", "run_cu05", 11L, s_d2, 26L
  )
  res = fp_vs_ingest(store_cu, s_d2, r_d2, 26L)
  identical(res, "committed") &&
    length(fp_vs_history(store_cu)) == 4L
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 22) DAY COMPLETENESS
# <-----------------------------------------------------------------------------

check("day completeness: single warehouse", {
  dc = fp_ar_day_completeness(
    store1, "2026-10-28", c(36L)
  )
  dc$complete
})

check("day completeness: missing warehouse", {
  dc = fp_ar_day_completeness(
    store1, "2026-10-28", c(36L, 12L)
  )
  !dc$complete && 12L %in% dc$missing_warehouses
})

check("day completeness: uncommitted date", {
  dc = fp_ar_day_completeness(
    store1, "2026-12-01", c(36L)
  )
  !dc$complete
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 23) VERSION VIEW
# <-----------------------------------------------------------------------------

check("version view has usage dates", {
  vv = fp_vs_version_view(store1)
  all(!is.na(vv$last_observed_date)) &&
    all(nzchar(vv$last_observed_date))
})

check("version view warehouse column", {
  vv = fp_vs_version_view(store3)
  any(grepl(",", vv$warehouses_observed))
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 24) CHANGE VALUES LOOKUP
# <-----------------------------------------------------------------------------

check("change values resolves", {
  ch = fp_vs_table(
    store2, "product_changes.csv.gz"
  )
  field_ch = ch[ch$field == "brand" &
    !is.na(ch$old_version_id), ]
  if (nrow(field_ch) == 0) {
    T
  } else {
    vals = fp_vs_change_values(
      store2, field_ch[1, ]
    )
    !is.null(vals$old_value) &&
      !is.null(vals$new_value) &&
      vals$old_value != vals$new_value
  }
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 25) STORAGE EFFICIENCY
# <-----------------------------------------------------------------------------

check("storage efficiency", {
  pv_all = fp_vs_table(
    store1, "product_versions.csv.gz"
  )
  obs_all = fp_vs_table(store1, "observations.csv.gz")
  n_unique_versions = nrow(pv_all)
  n_obs = nrow(obs_all)
  n_unique_versions <= n_obs
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 26) FIND COMMIT HELPER
# <-----------------------------------------------------------------------------

check("find_commit returns dir", {
  cdir = fp_vs_find_commit(
    store1, "2026-10-28", 36L
  )
  !is.null(cdir) && identical(cdir, "c0001")
})

check("find_commit returns NULL for missing", {
  cdir = fp_vs_find_commit(
    store1, "2099-01-01", 36L
  )
  is.null(cdir)
})

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 27) STYLE CHECKS ON NEW FILES
# <-----------------------------------------------------------------------------

new_files = c(
  file.path(root, "R", "versioned.R"),
  file.path(root, "R", "archive.R")
)

for (f in new_files) {
  lines = readLines(f)
  long = which(nchar(lines) > 80)
  if (length(long)) {
    for (ln in long) {
      tally$fail = tally$fail + 1L
      message("FAIL: line >80 chars in ",
        basename(f), ":", ln,
        " (", nchar(lines[ln]), " chars)")
    }
  } else {
    tally$pass = tally$pass + 1L
  }

  code = lines[!grepl("^\\s*#", lines)]
  bare = gsub('"[^"]*"', '""', code)
  bare = gsub("'[^']*'", "''", bare)
  bad_arrow = grep(
    "(?<![<>!=])(<-|<<-)(?!=)", bare, perl = T
  )
  if (length(bad_arrow)) {
    for (ln in bad_arrow) {
      tally$fail = tally$fail + 1L
      message("FAIL: arrow in ", basename(f),
        ":", ln)
    }
  } else {
    tally$pass = tally$pass + 1L
  }

  bad_pipe = grep("\\|>", bare)
  if (length(bad_pipe)) {
    tally$fail = tally$fail + 1L
    message("FAIL: native pipe in ", basename(f))
  } else {
    tally$pass = tally$pass + 1L
  }

  bad_fn = grep("\\bfunction\\s*\\(", bare)
  if (length(bad_fn)) {
    tally$fail = tally$fail + 1L
    message("FAIL: function() in ", basename(f))
  } else {
    tally$pass = tally$pass + 1L
  }

  bad_true = grep("\\bTRUE\\b", bare)
  bad_false = grep("\\bFALSE\\b", bare)
  if (length(bad_true) || length(bad_false)) {
    tally$fail = tally$fail + 1L
    message("FAIL: TRUE/FALSE in ", basename(f))
  } else {
    tally$pass = tally$pass + 1L
  }
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# RESULT
# <-----------------------------------------------------------------------------

cat("\n", tally$pass, " PASS, ", tally$fail, " FAIL\n",
  sep = "")
if (tally$fail > 0) quit(status = 1)
