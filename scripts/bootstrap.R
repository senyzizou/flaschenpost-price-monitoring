# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261029

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 0) ESSENTIALS
# <-----------------------------------------------------------------------------

load_regional = \(root = ".") {
  root = normalizePath(root, mustWork = T)
  cfg_file = file.path(root, "config", "regional_design.json")
  if (!file.exists(cfg_file)) stop(
    "Missing config/regional_design.json", call. = F)
  cfg = jsonlite::fromJSON(cfg_file, simplifyVector = F)

  cfg$preparation_dir = file.path(root, "preparation")
  assign("config", cfg, envir = .GlobalEnv)

  for (mod in c("preparation_api", "warehouse", "snapshot",
    "compare", "sample", "validate", "multi",
    "versioned", "annotations", "archive", "migrate",
    "coverage", "pipeline", "liverun", "verify",
    "regional_price", "remote", "recovery", "daily", "analysis_view")) {
    source(file.path(root, "R", paste0(mod, ".R")), local = F)
  }
  invisible(config)
}
