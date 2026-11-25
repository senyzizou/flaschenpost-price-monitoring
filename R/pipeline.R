# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261030

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) SINGLE-WAREHOUSE COLLECT AND STORE
# <-----------------------------------------------------------------------------

fp_pl_collect_and_store = \(root, ctx,
  warehouse_id, date, purpose = "production",
  evidence = NULL, classifier = NULL,
  output_base = tempfile("fp_run_")) {
  if (!dir.exists(root)) {
    stop("Store does not exist: ", root, call. = F)
  }
  result = fp_collect(
    ctx, warehouse_id, purpose,
    with_details = T,
    evidence = if (!is.null(evidence)) {
      evidence
    } else fp_load_abv_evidence(),
    classifier = classifier
  )
  if (length(result$checks$blocking)) {
    return(list(
      status = "blocked",
      blocking = result$checks$blocking,
      collector_result = result
    ))
  }
  run_name = format(
    Sys.time(), "run_%Y%m%d_%H%M%S"
  )
  run_dir = file.path(output_base, run_name)
  stored = fp_write_run(
    result, run_dir, ctx$runner,
    run_date = as.Date(date)
  )
  ingest = fp_mg_daily_adapter(
    root, stored, date,
    result$calls$at[1],
    warehouse_id
  )
  list(
    status = ingest$status,
    run_dir = stored,
    variants = nrow(result$variants),
    new_versions = NULL,
    warehouse_id = warehouse_id,
    date = date
  )
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) INGEST FROM ARCHIVED COLLECTOR PROOF
# <-----------------------------------------------------------------------------

fp_pl_from_proof = \(root, proof_dir,
  date, warehouse_id,
  work_dir = tempfile("fp_proof_ingest_")) {
  variants = file.path(proof_dir, "variants.csv")
  meta = file.path(proof_dir, "meta.json")
  if (!file.exists(variants)) {
    stop("No variants.csv in proof: ", proof_dir,
      call. = F)
  }
  if (!file.exists(meta)) {
    stop("No meta.json in proof: ", proof_dir,
      call. = F)
  }
  fp_mg_daily_adapter(
    root, proof_dir, date,
    paste0("proof_import_", basename(proof_dir)),
    warehouse_id,
    work_dir = work_dir
  )
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2b) INGEST OF A RUN BLOCKED ONLY BY A CATEGORY HIERARCHY CHANGE
# <-----------------------------------------------------------------------------

# Replays an archived run without new HTTP calls. The commit only exists
# when the coverage assessment passes; otherwise the store stays untouched.
fp_pl_from_assessed_run = \(root, run_dir, prior_dir = NULL, date,
  warehouse_id, prior_texts_csv = NULL,
  work_dir = tempfile("fp_assessed_"), prior = NULL,
  reference = NULL, require_classification = F) {
  assessment = fp_cv_assess(
    run_dir, prior_dir, prior_texts_csv,
    prior = prior, reference = reference,
    require_classification = require_classification
  )
  if (!identical(assessment$date, date)) {
    stop("Run date ", assessment$date, " differs from ", date,
      call. = F)
  }
  if (!identical(assessment$warehouse_id,
    as.character(warehouse_id))) {
    stop("Run warehouse differs from ", warehouse_id, call. = F)
  }
  if (!assessment$pass) {
    return(list(
      status = "blocked_coverage",
      failed = assessment$failed,
      assessment = assessment
    ))
  }
  tables = list(
    "quality_warnings.csv.gz" = assessment$quality_warnings,
    "assortment_changes.csv.gz" = assessment$assortment_changes,
    "category_changes.csv.gz" = assessment$category_changes
  )
  if (!is.null(assessment$regional_differences)) {
    tables[["regional_differences.csv.gz"]] =
      assessment$regional_differences
  }
  tables = tables[vapply(tables, nrow, 1L) > 0L]
  prior_label = if (!is.null(prior)) {
    paste0("store:", prior$commit)
  } else if (!is.null(prior_dir)) {
    basename(dirname(prior_dir))
  } else "baseline"
  extra = list(
    coverage_assessment = list(
      passed = T,
      version = "fp-coverage-1.0",
      collector_blocker = fp_cv_category_blocker,
      checks_passed = nrow(assessment$checks),
      baseline = assessment$baseline,
      prior_reference = prior_label
    )
  )
  ingest = fp_mg_daily_adapter(
    root, run_dir, date,
    basename(dirname(run_dir)),
    warehouse_id,
    work_dir = work_dir,
    receipt_extra = extra,
    extra_tables = tables
  )
  list(
    status = ingest$status,
    assessment = assessment,
    work_dir = ingest$work_dir
  )
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) MULTI-WAREHOUSE DAILY ORCHESTRATION
# <-----------------------------------------------------------------------------

fp_pl_daily_run = \(root, proof_dirs,
  date, warehouse_ids,
  required_warehouses = warehouse_ids) {
  stopifnot(
    length(proof_dirs) == length(warehouse_ids),
    !anyDuplicated(warehouse_ids)
  )
  results = vector("list", length(warehouse_ids))
  names(results) = paste0("w", warehouse_ids)
  for (i in seq_along(warehouse_ids)) {
    results[[i]] = tryCatch(
      fp_pl_from_proof(
        root, proof_dirs[i],
        date, warehouse_ids[i]
      ),
      error = \(e) list(
        status = "failed",
        error = conditionMessage(e)
      )
    )
  }
  completeness = fp_ar_day_completeness(
    root, date, required_warehouses
  )
  committed = vapply(results, \(r) {
    identical(r$status, "committed") ||
      identical(r$status, "already_committed")
  }, logical(1))
  list(
    results = results,
    all_committed = all(committed),
    completeness = completeness,
    date = date
  )
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) MULTI-WAREHOUSE SHARED-SKU VERIFICATION
# <-----------------------------------------------------------------------------

fp_pl_shared_sku_check = \(root, date,
  warehouse_ids) {
  history = fp_vs_history(root)
  cid_dates = vapply(history, \(m) m$date, "")
  day_idx = which(cid_dates == date)
  if (!length(day_idx)) {
    return(list(
      shared_skus = 0L,
      total_skus = 0L,
      warehouses = length(warehouse_ids)
    ))
  }
  day_cids = names(history)[day_idx]
  obs = fp_vs_table(
    root, "observations.csv.gz", day_cids
  )
  sku_wh = split(obs$warehouse_id, obs$sku)
  shared = sku_wh[
    lengths(sku_wh) > 1L
  ]
  versions = fp_vs_table(
    root, "product_versions.csv.gz", day_cids
  )
  shared_skus = names(shared)
  divergent = character()
  if (length(shared_skus) && !is.null(versions)) {
    for (sku in shared_skus) {
      sv = versions[versions$sku == sku, , drop = F]
      if (nrow(sv) > 1L) {
        hashes = unique(sv$content_sha256)
        if (length(hashes) > 1L) {
          divergent = c(divergent, sku)
        }
      }
    }
  }
  list(
    shared_skus = length(shared_skus),
    total_skus = length(unique(obs$sku)),
    warehouses = length(unique(obs$warehouse_id)),
    divergent_skus = divergent,
    version_consistent = length(divergent) == 0L
  )
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 5) PIPELINE STATUS SUMMARY
# <-----------------------------------------------------------------------------

fp_pl_store_summary = \(root) {
  history = fp_vs_history(root)
  if (!length(history)) {
    return(list(
      commits = 0L,
      dates = character(),
      warehouses = integer()
    ))
  }
  dates = vapply(history, \(m) m$date, "")
  wids = vapply(
    history,
    \(m) as.integer(m$warehouse_id),
    integer(1)
  )
  versions = fp_vs_table(
    root, "product_versions.csv.gz"
  )
  obs = fp_vs_table(root, "observations.csv.gz")
  list(
    commits = length(history),
    dates = sort(unique(dates)),
    warehouses = sort(unique(wids)),
    total_observations = nrow(obs),
    total_products = length(unique(obs$sku)),
    total_versions = if (!is.null(versions)) {
      nrow(versions)
    } else 0L,
    head_commit = tail(names(history), 1L)
  )
}
