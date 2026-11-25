# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261030

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) COLUMN GROUPS: PRODUCT IDENTITY VS. ANALYTICAL ANNOTATIONS
# <-----------------------------------------------------------------------------

# Product identity = what the shop says about the product. Classification =
# what the project derives from it with versioned rules. A pure change of
# the classification never creates a product version.
fp_an_class_columns = \() {
  c("price_category", "category_state", "inclusion_state",
    "classification_version", "price_analysis_version", "tax_class",
    "tax_basis", "product_type", "category_rule", "refinement_decision",
    "project_status", "abv_filter_min_0_5", "abv_filter_gt_0_5")
}

# Main (analysis_ready) and sensitivity (analysis_ready_gt_0_5) decision.
fp_an_class_extra = \() c("analysis_ready", "analysis_ready_gt_0_5")

fp_an_detail_columns = \() {
  c("description_text", "detail_description_text", "ingredients_text")
}

# Inferred sale-volume fields depend on sibling products and prices of the
# run (brand siblings, shown base price). They are not product content.
fp_an_volume_columns = \() {
  c("pack_count", "unit_volume_l", "total_volume_l", "volume_status")
}

fp_an_identity_columns = \(tracked) {
  setdiff(tracked, c(fp_an_class_columns(), fp_an_volume_columns()))
}

fp_an_files = \() {
  c("product_details.csv.gz", "detail_links.csv.gz",
    "classification_versions.csv.gz", "volume_resolutions.csv.gz",
    "version_assessments.csv.gz")
}

fp_an_detail_id = \(x) {
  cols = intersect(fp_an_detail_columns(), names(x))
  if (!length(cols)) return(rep(NA_character_, nrow(x)))
  vapply(seq_len(nrow(x)), \(i) {
    v = unlist(x[i, cols, drop = F], use.names = F)
    if (all(is.na(v))) NA_character_ else fp_vs_content_hash(v)
  }, character(1))
}

fp_an_class_hash = \(x) {
  cols = c(fp_an_class_columns(), fp_an_class_extra())
  for (cl in setdiff(cols, names(x))) x[[cl]] = NA_character_
  vapply(seq_len(nrow(x)), \(i) {
    fp_vs_content_hash(unlist(x[i, cols, drop = F], use.names = F))
  }, character(1))
}

fp_an_bind = \(rows) {
  rows = rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) return(NULL)
  cols = unique(unlist(lapply(rows, names)))
  rows = lapply(rows, \(r) {
    for (cl in setdiff(cols, names(r))) r[[cl]] = NA_character_
    r[cols]
  })
  out = do.call(rbind, rows)
  rownames(out) = NULL
  out
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) SUPPLEMENTS: IMMUTABLE ADDITIONS TO EXISTING COMMITS
# <-----------------------------------------------------------------------------

fp_an_supplement_dirs = \(root) {
  base = file.path(root, "supplements")
  if (!dir.exists(base)) return(character())
  sort(list.dirs(base, recursive = F, full.names = T))
}

fp_an_validate_supplement = \(path, previous, root) {
  m = jsonlite::read_json(
    file.path(path, "manifest.json"), simplifyVector = T
  )
  stopifnot(
    identical(m$previous_supplement_sha256, previous),
    identical(m$schema_sha256, fp_vs_hash(file.path(root, "schema.json")))
  )
  for (i in seq_len(nrow(m$files))) {
    p = file.path(path, m$files$file[i])
    if (!file.exists(p) || file.size(p) != m$files$bytes[i] ||
        fp_vs_hash(p) != m$files$sha256[i]) {
      stop("Corrupt or missing supplement file: ", p, call. = F)
    }
  }
  targets = m$target_commits
  for (i in seq_len(nrow(targets))) {
    mf = file.path(root, "commits", targets$commit_id[i], "manifest.json")
    if (!file.exists(mf) || fp_vs_hash(mf) != targets$manifest_sha256[i]) {
      stop("Supplement target commit changed: ", targets$commit_id[i],
        call. = F)
    }
  }
  m
}

fp_an_supplements = \(root) {
  dirs = fp_an_supplement_dirs(root)
  previous = "GENESIS"
  out = vector("list", length(dirs))
  for (i in seq_along(dirs)) {
    out[[i]] = fp_an_validate_supplement(dirs[i], previous, root)
    previous = fp_vs_hash(file.path(dirs[i], "manifest.json"))
  }
  names(out) = basename(dirs)
  out
}

# Rows of one annotation table from commits and supplements, in order.
fp_an_table = \(root, filename, commit_ids = NULL) {
  history = fp_vs_history(root)
  cids = if (is.null(commit_ids)) names(history) else commit_ids
  rows = list()
  order = 0L
  for (cid in cids) {
    p = file.path(root, "commits", cid, filename)
    order = order + 1L
    if (!file.exists(p)) next
    r = fp_vs_read_gz(p)
    r$source_unit = cid
    r$source_order = as.character(order)
    rows[[length(rows) + 1L]] = r
  }
  if (is.null(commit_ids)) {
    for (sid in names(fp_an_supplements(root))) {
      p = file.path(root, "supplements", sid, filename)
      order = order + 1L
      if (!file.exists(p)) next
      r = fp_vs_read_gz(p)
      r$source_unit = sid
      r$source_order = as.character(order)
      rows[[length(rows) + 1L]] = r
    }
  }
  fp_an_bind(rows)
}

# Latest row per key that is valid on or before `as_of`.
fp_an_effective = \(tbl, key, as_of = NULL) {
  if (is.null(tbl) || !nrow(tbl)) return(NULL)
  if (!is.null(as_of)) tbl = tbl[tbl$valid_from_date <= as_of, , drop = F]
  if (!nrow(tbl)) return(NULL)
  o = order(tbl$valid_from_date, as.integer(tbl$source_order))
  tbl = tbl[o, , drop = F]
  tbl[!duplicated(tbl[[key]], fromLast = T), , drop = F]
}

fp_an_write_supplement = \(root, tables, kind, targets, provenance) {
  stopifnot(
    length(tables) > 0L,
    all(names(tables) %in% fp_an_files()),
    is.data.frame(targets)
  )
  lock = file.path(root, ".writer_lock")
  if (!dir.create(lock, showWarnings = F)) {
    stop("Writer lock exists", call. = F)
  }
  on.exit(unlink(lock, recursive = T), add = T)
  existing = fp_an_supplements(root)
  seq_no = length(existing) + 1L
  previous = if (!length(existing)) "GENESIS" else {
    fp_vs_hash(file.path(root, "supplements", tail(names(existing), 1L),
      "manifest.json"))
  }
  dir.create(file.path(root, "transactions"), showWarnings = F)
  pending = tempfile("pending_s_", tmpdir = file.path(root, "transactions"))
  dir.create(pending)
  for (nm in names(tables)) {
    fp_vs_write_gz(tables[[nm]], file.path(pending, nm))
  }
  files = sort(list.files(pending))
  records = data.frame(
    file = files, bytes = file.size(file.path(pending, files)),
    sha256 = vapply(file.path(pending, files), fp_vs_hash, ""),
    stringsAsFactors = F
  )
  rownames(records) = NULL
  targets$manifest_sha256 = vapply(targets$commit_id, \(cid) {
    fp_vs_hash(file.path(root, "commits", cid, "manifest.json"))
  }, "")
  manifest = list(
    supplement_seq = seq_no, kind = kind,
    created_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    target_commits = targets, provenance = provenance,
    schema_sha256 = fp_vs_hash(file.path(root, "schema.json")),
    previous_supplement_sha256 = previous, files = records
  )
  fp_vs_write_json(manifest, file.path(pending, "manifest.json"))
  fp_an_validate_supplement(pending, previous, root)
  dest_base = file.path(root, "supplements")
  dir.create(dest_base, showWarnings = F)
  target = file.path(dest_base, sprintf("s%04d", seq_no))
  if (dir.exists(target) || !file.rename(pending, target)) {
    stop("Supplement commit failed; pending retained", call. = F)
  }
  invisible(basename(target))
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) VERSION PLAN FOR ONE DAILY RUN
# <-----------------------------------------------------------------------------

# Decides per SKU: unchanged, shared with another warehouse, or a new
# product version. Classification and first capture of details never count
# as product changes; they become annotation rows with provenance.
fp_an_plan = \(root, x, tracked, versions, prior_usage, wid, date,
  run_id) {
  n = nrow(x)
  id_cols = fp_an_identity_columns(tracked)
  has_det = any(fp_an_detail_columns() %in% names(x))
  det_new = if (has_det) fp_an_detail_id(x) else rep(NA_character_, n)
  row_hash = \(cols) vapply(seq_len(n), \(i) {
    fp_vs_content_hash(unlist(x[i, cols, drop = F], use.names = F))
  }, character(1))
  h_prod = row_hash(id_cols)
  h_full = row_hash(tracked)
  identity = vapply(seq_len(n), \(i) {
    fp_vs_content_hash(c(h_prod[i], det_new[i]))
  }, character(1))

  links = fp_an_table(root, "detail_links.csv.gz")
  det_eff = fp_an_effective(links, "product_version_id")
  det_of = \(vids) {
    if (is.null(det_eff)) rep(NA_character_, length(vids)) else {
      det_eff$detail_id[match(vids, det_eff$product_version_id)]
    }
  }
  stored_prod = \(vids) {
    at = match(vids, versions$product_version_id)
    vapply(seq_along(vids), \(k) {
      if (is.na(at[k])) NA_character_ else {
        fp_vs_content_hash(unlist(versions[at[k], id_cols], use.names = F))
      }
    }, character(1))
  }
  compatible = \(i, h_old, d_old) {
    !is.na(h_old) && h_prod[i] == h_old &&
      (is.na(det_new[i]) || is.na(d_old) || det_new[i] == d_old)
  }

  wid_chr = as.character(wid)
  own_vid = rep(NA_character_, n)
  base_vid = rep(NA_character_, n)
  per_wh = NULL
  if (!is.null(prior_usage) && nrow(prior_usage)) {
    last_use = \(u) u[!duplicated(u$sku, fromLast = T), ]
    own_last = last_use(prior_usage[prior_usage$warehouse_id == wid_chr, ])
    any_last = last_use(prior_usage)
    own_vid = own_last$product_version_id[match(x$sku, own_last$sku)]
    any_vid = any_last$product_version_id[match(x$sku, any_last$sku)]
    base_vid = ifelse(is.na(own_vid), any_vid, own_vid)
    per_wh = prior_usage[!duplicated(
      paste(prior_usage$sku, prior_usage$warehouse_id), fromLast = T
    ), ]
  }
  old_at = if (is.null(versions)) rep(NA_integer_, n) else {
    match(base_vid, versions$product_version_id)
  }
  base_prod = stored_prod(base_vid)
  base_det = det_of(base_vid)
  same_base = vapply(seq_len(n), \(i) {
    !is.na(old_at[i]) && compatible(i, base_prod[i], base_det[i])
  }, logical(1))

  reuse_vid = rep(NA_character_, n)
  if (!is.null(per_wh)) {
    cand_prod = stored_prod(per_wh$product_version_id)
    cand_det = det_of(per_wh$product_version_id)
    for (i in which(!same_base)) {
      cand = which(per_wh$sku == x$sku[i])
      for (k in cand) {
        if (compatible(i, cand_prod[k], cand_det[k])) {
          reuse_vid[i] = per_wh$product_version_id[k]
          break
        }
      }
    }
  }
  reuse = !same_base & !is.na(reuse_vid)
  created = !same_base & !reuse
  switched = reuse & !is.na(own_vid)
  changed = created | switched

  ids = if (is.null(versions)) rep(NA_character_, n) else {
    versions$product_version_id[old_at]
  }
  ids[reuse] = reuse_vid[reuse]
  episodes = if (is.null(versions)) rep(0L, n) else {
    as.integer(versions$episode[old_at])
  }
  episodes[is.na(episodes)] = 0L
  for (i in which(created)) {
    prior = if (is.na(base_vid[i]) || is.na(old_at[i])) "GENESIS" else {
      base_vid[i]
    }
    ids[i] = fp_vs_version_id(x$product_key[i], prior, identity[i])
  }

  new_v = x[which(created),
    unique(c("retailer", "sku", "product_key", tracked)), drop = F]
  new_v$product_version_id = ids[created]
  new_v$content_sha256 = h_full[created]
  new_v$identity_sha256 = identity[created]
  new_v$episode = as.character(episodes[created] + 1L)
  new_v$first_observed_date = x$daily_date[created]
  new_v$first_observed_at = x$observed_at[created]
  new_v$creation_run_id = x$run_id[created]
  new_v$source_warehouse_id = x$warehouse_id[created]
  new_v$raw_sha256 = x$raw_sha256[created]
  rownames(new_v) = NULL
  if (!is.null(versions) && nrow(new_v)) {
    new_v = new_v[!new_v$product_version_id %in%
      versions$product_version_id, , drop = F]
  }

  # --- details: shared content-addressed texts and links per version
  details_tbl = fp_an_table(root, "product_details.csv.gz")
  has_d = !is.na(det_new)
  linked = det_of(ids)
  add_link = has_d & is.na(linked)
  link_rows = NULL
  if (any(add_link)) {
    link_rows = data.frame(
      product_version_id = ids[add_link], sku = x$sku[add_link],
      detail_id = det_new[add_link], valid_from_date = date,
      provenance = ifelse(created[add_link], "live_collector",
        "technical_adoption"),
      change_type = ifelse(created[add_link], "new_version",
        "first_capture"),
      recorded_in_run = run_id, stringsAsFactors = F
    )
  }
  det_rows = NULL
  fresh = has_d & !det_new %in% details_tbl$detail_id & !duplicated(det_new)
  if (any(fresh)) {
    det_rows = x[fresh, intersect(fp_an_detail_columns(), names(x)),
      drop = F]
    det_rows = cbind(detail_id = det_new[fresh], det_rows,
      first_seen_date = date, recorded_in_run = run_id,
      stringsAsFactors = F)
    rownames(det_rows) = NULL
  }

  # --- classification: separate versioned annotation per product version
  cls = fp_an_table(root, "classification_versions.csv.gz")
  cls_eff = fp_an_effective(cls, "product_version_id", as_of = date)
  cols_c = c(fp_an_class_columns(), fp_an_class_extra())
  present = intersect(cols_c, names(x))
  # A classification exists only if the category rules ran for the row.
  has_c = if ("category_state" %in% names(x)) {
    !is.na(x$category_state)
  } else rep(F, n)
  c_hash = fp_an_class_hash(x)
  old_c = if (is.null(cls_eff)) rep(NA_character_, n) else {
    cls_eff$class_hash[match(ids, cls_eff$product_version_id)]
  }
  add_c = has_c & (is.na(old_c) | old_c != c_hash)
  class_rows = NULL
  if (any(add_c)) {
    at_c = if (is.null(cls_eff)) rep(NA_integer_, n) else {
      match(ids, cls_eff$product_version_id)
    }
    ne = \(a, b) is.na(a) != is.na(b) | (!is.na(a) & !is.na(b) & a != b)
    rules_changed = !is.na(at_c) & (
      ne(cls_eff$classification_version[at_c], x$classification_version) |
        ne(cls_eff$price_analysis_version[at_c], x$price_analysis_version)
    )
    kind = ifelse(is.na(old_c), ifelse(created, "new_product_version",
      "first_classification"),
      ifelse(rules_changed, "rules_version_change", "reclassification"))
    full = x[, c("sku", "product_key"), drop = F]
    for (cl in cols_c) full[[cl]] = if (cl %in% names(x)) x[[cl]] else {
      NA_character_
    }
    class_rows = cbind(
      classification_id = vapply(which(add_c), \(i) {
        fp_vs_content_hash(c(ids[i], c_hash[i]))
      }, ""),
      product_version_id = ids[add_c], full[add_c, , drop = F],
      class_hash = c_hash[add_c], valid_from_date = date,
      provenance = "live_collector", retroactive = "FALSE",
      change_type = kind[add_c], recorded_in_run = run_id,
      stringsAsFactors = F
    )
    rownames(class_rows) = NULL
  }

  # --- inferred volume fields: annotation per product version and warehouse
  vol_cols = fp_an_volume_columns()
  vol_ok = all(vol_cols %in% names(x))
  vol_hash = if (vol_ok) vapply(seq_len(n), \(i) {
    fp_vs_content_hash(unlist(x[i, vol_cols], use.names = F))
  }, "") else rep(NA_character_, n)
  vol_key = paste(ids, wid_chr, sep = "|")
  vol_tbl = fp_an_table(root, "volume_resolutions.csv.gz")
  vol_eff = fp_an_effective(vol_tbl, "resolution_key", as_of = date)
  snap_hash = rep(NA_character_, n)
  if (vol_ok && !is.null(versions)) {
    at_s = match(ids, versions$product_version_id)
    for (i in which(!is.na(at_s) & !is.na(x$sku))) {
      snap_hash[i] = fp_vs_content_hash(
        unlist(versions[at_s[i], vol_cols], use.names = F)
      )
    }
  }
  old_v = if (is.null(vol_eff)) rep(NA_character_, n) else {
    vol_eff$volume_hash[match(vol_key, vol_eff$resolution_key)]
  }
  ref_v = ifelse(is.na(old_v), snap_hash, old_v)
  add_v = vol_ok & !is.na(ref_v) & ref_v != vol_hash
  vol_rows = NULL
  if (any(add_v)) {
    vol_rows = cbind(
      volume_resolution_id = vapply(which(add_v), \(i) {
        fp_vs_content_hash(c(vol_key[i], vol_hash[i]))
      }, ""),
      resolution_key = vol_key[add_v], product_version_id = ids[add_v],
      sku = x$sku[add_v], product_key = x$product_key[add_v],
      warehouse_id = wid_chr, x[add_v, vol_cols, drop = F],
      volume_hash = vol_hash[add_v], valid_from_date = date,
      provenance = "live_collector", change_type = "resolution_change",
      recorded_in_run = run_id, stringsAsFactors = F
    )
    rownames(vol_rows) = NULL
  }

  # --- product change log (identity columns and detail texts only)
  change_rows = list()
  n_change = 0L
  det_old_of = \(vid) {
    d = det_of(vid)
    if (is.na(d) || is.null(details_tbl)) return(NULL)
    details_tbl[match(d, details_tbl$detail_id), , drop = F]
  }
  for (i in which(changed)) {
    initial = is.na(old_at[i])
    old_vals = if (initial) rep(NA_character_, length(id_cols)) else {
      unlist(versions[old_at[i], id_cols], use.names = F)
    }
    current = unlist(x[i, id_cols], use.names = F)
    diff_idx = if (initial) integer() else which(
      is.na(old_vals) != is.na(current) |
        (!is.na(old_vals) & !is.na(current) & old_vals != current)
    )
    mk = \(field, old_id, kind) {
      data.frame(
        retailer = "flaschenpost", sku = x$sku[i],
        product_key = x$product_key[i], warehouse_id = wid_chr,
        date = date, observed_at = x$observed_at[i], run_id = run_id,
        field = field, old_version_id = old_id,
        new_version_id = ids[i], change_type = kind,
        stringsAsFactors = F
      )
    }
    if (initial) {
      n_change = n_change + 1L
      change_rows[[n_change]] = mk(
        "__initial_product__", NA_character_, "initial_product"
      )
    }
    old_id = if (initial) NA_character_ else {
      versions$product_version_id[old_at[i]]
    }
    for (j in diff_idx) {
      seen = any(!is.na(versions[[id_cols[j]]][
        versions$product_key == x$product_key[i]
      ]))
      n_change = n_change + 1L
      change_rows[[n_change]] = mk(
        id_cols[j], old_id,
        fp_vs_change_type(old_vals[j], current[j], previously_known = seen)
      )
    }
    old_d = if (initial) NULL else det_old_of(old_id)
    if (!is.null(old_d) && has_d[i]) {
      for (cl in intersect(fp_an_detail_columns(), names(x))) {
        a = old_d[[cl]][1]
        b = x[[cl]][i]
        if (is.na(a) != is.na(b) || (!is.na(a) && a != b)) {
          n_change = n_change + 1L
          change_rows[[n_change]] = mk(
            cl, old_id, fp_vs_change_type(a, b, previously_known = !is.na(a))
          )
        }
      }
    }
  }
  list(
    ids = ids, created = created, switched = switched, changed = changed,
    old_at = old_at, new_v = new_v, change_rows = change_rows,
    tables = list(
      "product_details.csv.gz" = det_rows,
      "detail_links.csv.gz" = link_rows,
      "classification_versions.csv.gz" = class_rows,
      "volume_resolutions.csv.gz" = vol_rows
    ),
    counts = list(
      first_capture_links = sum(add_link & !created),
      classification_rows = if (is.null(class_rows)) 0L else nrow(class_rows),
      volume_rows = if (is.null(vol_rows)) 0L else nrow(vol_rows)
    )
  )
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) RECONSTRUCTION FROM ARCHIVED COLLECTOR RUNS
# <-----------------------------------------------------------------------------

# Same string form as a live run would write: round trip through CSV.
fp_an_as_stored = \(df) {
  tmp = tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  write.csv(df, tmp, row.names = F, na = "")
  out = read.csv(tmp, colClasses = "character", na.strings = character())
  for (cl in names(out)) out[[cl]][!nzchar(out[[cl]])] = NA_character_
  out
}

fp_an_inputs = \(run_dir) {
  files = c("variants.csv", "texts.csv", "categories.csv", "meta.json")
  files = files[file.exists(file.path(run_dir, files))]
  data.frame(
    file = files,
    sha256 = vapply(file.path(run_dir, files), fp_vs_hash, ""),
    stringsAsFactors = F, row.names = NULL
  )
}

# `assign_fn(variants, texts, categories)` is the frozen project classifier.
fp_an_reconstruct = \(run_dir, assign_fn) {
  v = read.csv(file.path(run_dir, "variants.csv"), stringsAsFactors = F,
    colClasses = c(subcategory_ids = "character", sku = "character",
      product_key = "character"))
  tx = read.csv(file.path(run_dir, "texts.csv"), stringsAsFactors = F,
    colClasses = c(sku = "character"))
  ct = read.csv(file.path(run_dir, "categories.csv"), stringsAsFactors = F)
  a = assign_fn(v, tx, ct)
  cols = c(fp_an_class_columns(), fp_an_class_extra())
  # ABV filters are set by the collector, not by the category rules.
  for (cl in setdiff(cols, names(a))) {
    if (cl %in% names(v)) a[[cl]] = v[[cl]][match(a$sku, v$sku)]
  }
  cls = fp_an_as_stored(a[, c("sku", intersect(cols, names(a)))])
  for (cl in setdiff(cols, names(cls))) cls[[cl]] = NA_character_
  txt = fp_an_as_stored(tx[, c("sku", intersect(fp_an_detail_columns(),
    names(tx)))])
  list(class = cls, texts = txt, inputs = fp_an_inputs(run_dir))
}

# Adds classification and details to an existing commit without touching it.
fp_an_backfill = \(root, commit_id, run_dir, assign_fn, rules) {
  mf = jsonlite::read_json(
    file.path(root, "commits", commit_id, "manifest.json"),
    simplifyVector = T
  )
  obs = fp_vs_read_gz(file.path(root, "commits", commit_id,
    "observations.csv.gz"))
  rec = fp_an_reconstruct(run_dir, assign_fn)
  stopifnot(setequal(obs$sku, rec$class$sku))
  at = match(rec$class$sku, obs$sku)
  vid = obs$product_version_id[at]
  c_hash = fp_an_class_hash(rec$class)
  cls_old = fp_an_effective(
    fp_an_table(root, "classification_versions.csv.gz"),
    "product_version_id", as_of = mf$date
  )
  old_c = if (is.null(cls_old)) rep(NA_character_, length(vid)) else {
    cls_old$class_hash[match(vid, cls_old$product_version_id)]
  }
  keep = is.na(old_c) | old_c != c_hash
  run_id = basename(dirname(run_dir))
  class_rows = NULL
  if (any(keep)) {
    class_rows = cbind(
      classification_id = vapply(which(keep), \(i) {
        fp_vs_content_hash(c(vid[i], c_hash[i]))
      }, ""),
      product_version_id = vid[keep], sku = rec$class$sku[keep],
      product_key = obs$product_key[at][keep],
      rec$class[keep, c(fp_an_class_columns(), fp_an_class_extra())],
      class_hash = c_hash[keep], valid_from_date = mf$date,
      provenance = "archive_reconstruction", retroactive = "TRUE",
      change_type = "first_classification", recorded_in_run = run_id,
      recorded_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"),
      stringsAsFactors = F
    )
    rownames(class_rows) = NULL
  }
  d_id = fp_an_detail_id(rec$texts)
  links_old = fp_an_effective(
    fp_an_table(root, "detail_links.csv.gz"), "product_version_id"
  )
  have = if (is.null(links_old)) rep(NA_character_, length(vid)) else {
    links_old$detail_id[match(vid, links_old$product_version_id)]
  }
  add_l = !is.na(d_id) & is.na(have)
  link_rows = NULL
  det_rows = NULL
  if (any(add_l)) {
    link_rows = data.frame(
      product_version_id = vid[add_l], sku = rec$class$sku[add_l],
      detail_id = d_id[add_l], valid_from_date = mf$date,
      provenance = "archive_reconstruction",
      change_type = "first_capture", recorded_in_run = run_id,
      stringsAsFactors = F
    )
    known = fp_an_table(root, "product_details.csv.gz")
    fresh = add_l & !d_id %in% known$detail_id & !duplicated(d_id)
    if (any(fresh)) {
      det_rows = cbind(
        detail_id = d_id[fresh],
        rec$texts[fresh, intersect(fp_an_detail_columns(),
          names(rec$texts)), drop = F],
        first_seen_date = mf$date, recorded_in_run = run_id,
        stringsAsFactors = F
      )
      rownames(det_rows) = NULL
    }
  }
  tables = list(
    "product_details.csv.gz" = det_rows,
    "detail_links.csv.gz" = link_rows,
    "classification_versions.csv.gz" = class_rows
  )
  tables = tables[!vapply(tables, is.null, logical(1))]
  if (!length(tables)) return(invisible("nothing_to_add"))
  fp_an_write_supplement(
    root, tables, "classification_and_details_backfill",
    data.frame(commit_id = commit_id, stringsAsFactors = F),
    list(
      method = paste0(
        "frozen classifier applied offline to archived collector ",
        "output; no HTTP; not part of the original commit"
      ),
      analytical_note = paste0(
        "retroactive analytical addition; the classification was not ",
        "available on the collection day and is not part of the ",
        "original commit"
      ),
      classifier_rules = rules,
      source_run = run_id,
      input_files = rec$inputs,
      observation_date = mf$date,
      recorded_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
  )
}

# Recomputes the classification and compares it with a stored supplement.
fp_an_verify_backfill = \(root, supplement_id, run_dir, assign_fn) {
  rec = fp_an_reconstruct(run_dir, assign_fn)
  m = fp_an_supplements(root)[[supplement_id]]
  p = file.path(root, "supplements", supplement_id)
  stored = fp_vs_read_gz(file.path(p, "classification_versions.csv.gz"))
  now = fp_an_class_hash(rec$class)
  list(
    rows = nrow(stored),
    class_hash_equal = identical(
      stored$class_hash[order(stored$sku)],
      now[match(sort(stored$sku), rec$class$sku)]
    ),
    inputs_equal = identical(
      m$provenance$input_files$sha256, rec$inputs$sha256
    )
  )
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) CONFIRMATION OF A RETROACTIVE CLASSIFICATION BY A LIVE RUN
# <-----------------------------------------------------------------------------

# Versions whose effective classification is retroactive (supplement) get a
# non-retroactive confirmation row when the classifier of a later live run
# yields exactly the same decision. Mismatches are returned, never merged.
fp_an_confirm_classification = \(root, commit_id, run_dir) {
  mf = jsonlite::read_json(
    file.path(root, "commits", commit_id, "manifest.json"),
    simplifyVector = T
  )
  obs = fp_vs_read_gz(file.path(root, "commits", commit_id,
    "observations.csv.gz"))
  cols = c(fp_an_class_columns(), fp_an_class_extra())
  v = read.csv(file.path(run_dir, "variants.csv"), colClasses = "character",
    na.strings = character())
  live = fp_an_as_stored(v[, c("sku", intersect(cols, names(v)))])
  for (cl in setdiff(cols, names(live))) live[[cl]] = NA_character_
  live = live[match(obs$sku, live$sku), ]
  c_hash = fp_an_class_hash(live)
  eff = fp_an_effective(
    fp_an_table(root, "classification_versions.csv.gz"),
    "product_version_id", as_of = mf$date
  )
  at = match(obs$product_version_id, eff$product_version_id)
  retro = !is.na(at) & eff$retroactive[at] %in% "TRUE"
  same = retro & eff$class_hash[at] == c_hash
  rows = NULL
  if (any(same)) {
    rows = cbind(
      classification_id = vapply(which(same), \(i) {
        fp_vs_content_hash(c(obs$product_version_id[i], c_hash[i],
          "confirmed"))
      }, ""),
      product_version_id = obs$product_version_id[same],
      sku = obs$sku[same], product_key = obs$product_key[same],
      live[same, cols], class_hash = c_hash[same],
      valid_from_date = mf$date, provenance = "live_collector",
      retroactive = "FALSE", change_type = "confirmed_by_live_run",
      recorded_in_run = basename(dirname(run_dir)),
      stringsAsFactors = F
    )
    rownames(rows) = NULL
  }
  list(rows = rows, confirmed = sum(same), retroactive_versions = sum(retro),
    mismatched = obs$sku[retro & !same])
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 6) PRODUCT CHANGE LOG WITH REVIEW ASSESSMENTS
# <-----------------------------------------------------------------------------

# The stored change log is immutable. This view joins the later assessments
# so that a technical artifact is not read as an actual product change.
fp_an_product_changes = \(root) {
  ch = fp_vs_table(root, "product_changes.csv.gz")
  asm = fp_an_effective(
    fp_an_table(root, "version_assessments.csv.gz"), "product_version_id"
  )
  ch$assessment = NA_character_
  ch$technical_artifact = "FALSE"
  ch$counts_as_product_change = ifelse(
    ch$change_type == "initial_product", "FALSE", "TRUE"
  )
  if (!is.null(asm)) {
    at = match(ch$new_version_id, asm$product_version_id)
    ch$assessment = asm$assessment[at]
    art = !is.na(at) & asm$assessment[at] == "artifact_derived_volume"
    ch$technical_artifact[art] = "TRUE"
    ch$counts_as_product_change[art] = "FALSE"
  }
  ch
}
