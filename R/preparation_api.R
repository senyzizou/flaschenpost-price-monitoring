# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261102

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) SELF-CONTAINED PREPARATION LOADER
# <-----------------------------------------------------------------------------

# Replaces the external load_project() from preparation_20261023/.
# Loads the verbatim-copied preparation modules from preparation/
# within this project. The loaded environment is identical to what
# load_project() returns: same functions, same configs, same checks.
# Source: preparation/PROVENANCE.json documents the copy.

fp_load_preparation = \(root) {
  root = normalizePath(root, mustWork = T)
  prep = file.path(root, "preparation")
  if (!dir.exists(prep)) stop(
    "preparation/ directory not found in project root.",
    call. = F)

  imports = list(
    xml2 = c("read_html", "read_xml", "xml_find_all",
      "xml_find_first", "xml_attr", "xml_text",
      "xml_name", "xml_parent"),
    jsonlite = c("read_json", "write_json", "toJSON",
      "fromJSON"),
    digest = "digest",
    httr2 = c("request", "req_url_query", "req_user_agent",
      "req_timeout", "req_error", "req_perform",
      "resp_status", "resp_body_string", "req_auth_basic",
      "req_method", "req_options", "req_headers",
      "req_body_file"))

  dependencies = c(names(imports), "dplyr", "tibble")
  missing = dependencies[!vapply(dependencies, requireNamespace,
    logical(1), quietly = T)]
  if (length(missing)) stop(
    "Dependency missing: ",
    paste(missing, collapse = ", "),
    ". No installation attempted.",
    call. = F)
  stopifnot(!anyDuplicated(unlist(imports)))

  env = new.env(parent = globalenv())
  for (pkg in names(imports)) {
    for (name in imports[[pkg]]) {
      assign(name, getExportedValue(pkg, name), envir = env)
    }
  }

  env$config = env$read_json(
    file.path(prep, "config", "offline_design.json"))
  stopifnot(
    identical(env$config$postal_code, "10997"),
    identical(env$config$fulfillment_mode, "delivery"),
    identical(env$config$statistics_min_abv_pct_inclusive,
      0.5),
    identical(env$config$live_enabled, F))
  for (pkg in dependencies) {
    stopifnot(identical(
      as.character(packageVersion(pkg)),
      env$config$dependency_versions[[pkg]]))
  }

  env$runner = env$read_json(
    file.path(prep, "config", "runner_design.json"),
    simplifyVector = T)
  stopifnot(
    identical(env$runner$live_authorized, F),
    identical(env$runner$host, "www.flaschenpost.de"))

  env$collector = env$read_json(
    file.path(prep, "config", "collector_design.json"),
    simplifyVector = T)
  stopifnot(
    env$collector$warehouse_verification$status %in%
      c("unverified", "verified"),
    identical(env$collector$api_base,
      "https://www.flaschenpost.de/php-product-api/v1"))

  env$preparation_root = prep

  for (module in c("evidence.R", "discovery.R", "daily.R",
    "runner.R", "inventory.R", "categories.R",
    "collector.R", "panel.R", "analysis.R", "webdav.R",
    "dav_transport.R", "preflight.R")) {
    sys.source(file.path(prep, "R", module), envir = env)
  }

  env
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) PROVENANCE INTEGRITY CHECK
# <-----------------------------------------------------------------------------

fp_preparation_verify = \(root) {
  prep = file.path(root, "preparation")
  prov_file = file.path(prep, "PROVENANCE.json")
  if (!file.exists(prov_file)) stop(
    "PROVENANCE.json missing.", call. = F)
  prov = jsonlite::fromJSON(prov_file, simplifyVector = F)
  results = list()
  for (rel_path in names(prov$files)) {
    full = file.path(prep, rel_path)
    expected = prov$files[[rel_path]]
    exists = file.exists(full)
    actual = if (exists) {
      digest::digest(file = full, algo = "sha256")
    } else NA_character_
    ok = exists && identical(actual, expected)
    results[[rel_path]] = list(
      exists = exists, expected = expected,
      actual = actual, ok = ok)
  }
  all_ok = all(vapply(results, \(r) r$ok, logical(1)))
  list(ok = all_ok, files = results,
    source = prov$source_project,
    date = prov$integration_date)
}
