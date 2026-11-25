# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  ALCOHOL PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261029

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) WAREHOUSE REGISTRY AND LOCATION MAPPING
# <-----------------------------------------------------------------------------

rp_warehouse_entry = \(warehouse_id, label, verification_status,
  verification_date = NA_character_,
  verification_method = NA_character_,
  independent_sessions = 0L, note = NA_character_) {
  stopifnot(is.numeric(warehouse_id), length(warehouse_id) == 1,
    !is.na(warehouse_id), warehouse_id > 0,
    is.character(label), nchar(label) >= 1)
  data.frame(warehouse_id = as.integer(warehouse_id), label = label,
    verification_status = verification_status,
    verification_date = as.character(verification_date),
    verification_method = as.character(verification_method),
    independent_sessions = as.integer(independent_sessions),
    note = as.character(note), stringsAsFactors = F)
}

rp_warehouse_registry = \(entries) {
  if (!is.list(entries) || !length(entries)) {
    stop("At least one warehouse entry required.", call. = F)
  }
  reg = do.call(rbind, entries)
  if (anyDuplicated(reg$warehouse_id)) {
    stop("Duplicate warehouse IDs in registry.", call. = F)
  }
  if (anyDuplicated(reg$label)) {
    stop("Duplicate labels in registry.", call. = F)
  }
  reg
}

rp_load_registry = \(cfg = config) {
  entries = lapply(names(cfg$warehouses), \(key) {
    w = cfg$warehouses[[key]]
    rp_warehouse_entry(
      warehouse_id = w$warehouse_id, label = w$label,
      verification_status = w$verification_status,
      verification_date = w$verification_date,
      verification_method = w$verification_method,
      independent_sessions = w$independent_sessions,
      note = w$note)
  })
  rp_warehouse_registry(entries)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 2) LOCATION MAPPING: PSEUDONYMISED KEYS
# <-----------------------------------------------------------------------------

rp_location_key = \(postal_code, city_label = NA_character_) {
  stopifnot(is.character(postal_code), nchar(postal_code) == 5,
    grepl("^[0-9]{5}$", postal_code))
  paste0("plz_", postal_code)
}

rp_location_entry = \(location_key, warehouse_id,
  mapping_date, mapping_source,
  mapping_confidence = c("verified", "single_session", "inferred"),
  note = NA_character_) {
  mapping_confidence = match.arg(mapping_confidence)
  data.frame(location_key = location_key,
    warehouse_id = as.integer(warehouse_id),
    mapping_date = as.character(mapping_date),
    mapping_source = as.character(mapping_source),
    mapping_confidence = mapping_confidence,
    note = as.character(note), stringsAsFactors = F)
}

rp_location_registry = \(entries) {
  reg = do.call(rbind, entries)
  if (anyDuplicated(reg$location_key)) {
    dups = reg$location_key[duplicated(reg$location_key)]
    same_wh = all(vapply(unique(dups), \(k) {
      length(unique(reg$warehouse_id[reg$location_key == k])) == 1
    }, logical(1)))
    if (!same_wh) {
      stop("Same location mapped to different warehouses.", call. = F)
    }
    reg = reg[!duplicated(reg$location_key), ]
  }
  reg
}

rp_locations_per_warehouse = \(loc_reg) {
  split(loc_reg, loc_reg$warehouse_id)
}

rp_warehouse_coverage = \(loc_reg) {
  tbl = table(loc_reg$warehouse_id)
  data.frame(warehouse_id = as.integer(names(tbl)),
    location_count = as.integer(tbl),
    stringsAsFactors = F)
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 3) API PROBE: WAREHOUSE EXISTENCE CHECK
# <-----------------------------------------------------------------------------

rp_probe_warehouse = \(warehouse_id, transport = NULL,
  cfg = config) {
  if (!isTRUE(cfg$live_authorized)) {
    stop("Live access is not authorized.", call. = F)
  }
  if (is.null(transport)) {
    stop("No transport function provided.", call. = F)
  }
  url = paste0(cfg$api_base, "/category/warehouse/",
    as.integer(warehouse_id))
  res = transport(url, list(), list(
    user_agent = cfg$user_agent,
    timeout_seconds = cfg$timeout_seconds))
  list(warehouse_id = as.integer(warehouse_id),
    status = as.integer(res$status),
    has_categories = !is.null(tryCatch(
      jsonlite::fromJSON(res$body, simplifyVector = F),
      error = \(e) NULL)),
    probed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"))
}

rp_probe_warehouses = \(ids, transport = NULL,
  cfg = config, sleep = Sys.sleep) {
  results = list()
  for (i in seq_along(ids)) {
    if (i > 1) sleep(cfg$min_request_interval_seconds)
    results[[i]] = tryCatch(
      rp_probe_warehouse(ids[i], transport, cfg),
      error = \(e) list(warehouse_id = as.integer(ids[i]),
        status = NA_integer_, has_categories = F,
        probed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ",
          tz = "UTC"),
        error = conditionMessage(e)))
  }
  do.call(rbind, lapply(results, as.data.frame,
    stringsAsFactors = F))
}

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 4) ADDRESS-TO-WAREHOUSE MAPPING
# <-----------------------------------------------------------------------------

rp_address_entry = \(address_id, postal_code, city,
  address_type = c("public_institution", "test", "other"),
  warehouse_id = NA_integer_,
  mapping_date = NA_character_,
  mapping_method = NA_character_) {
  address_type = match.arg(address_type)
  stopifnot(is.character(address_id), nchar(address_id) >= 1,
    is.character(postal_code), grepl("^[0-9]{5}$", postal_code),
    is.character(city), nchar(city) >= 1)
  data.frame(
    address_id = address_id,
    location_key = rp_location_key(postal_code),
    postal_code = postal_code,
    city = city,
    address_type = address_type,
    warehouse_id = as.integer(warehouse_id),
    mapping_date = as.character(mapping_date),
    mapping_method = as.character(mapping_method),
    stringsAsFactors = F)
}

rp_address_registry = \(entries) {
  reg = do.call(rbind, entries)
  if (anyDuplicated(reg$address_id)) {
    stop("Duplicate address IDs.", call. = F)
  }
  reg
}

rp_private_address_table = \(path) {
  if (!file.exists(path)) return(NULL)
  tbl = utils::read.csv(path, stringsAsFactors = F,
    colClasses = "character")
  required = c("address_id", "street", "house_number",
    "postal_code", "city")
  missing = setdiff(required, names(tbl))
  if (length(missing)) {
    stop(paste("Private table missing columns:",
      paste(missing, collapse = ", ")), call. = F)
  }
  tbl
}

rp_save_private_address_table = \(tbl, path) {
  dir.create(dirname(path), showWarnings = F, recursive = T)
  utils::write.csv(tbl, path, row.names = F)
  invisible(path)
}

rp_scrub_address = \(address_entry) {
  out = address_entry[, c("address_id", "location_key",
    "postal_code", "city", "address_type",
    "warehouse_id", "mapping_date", "mapping_method"),
    drop = F]
  out
}

rp_address_contains_private = \(x) {
  if (is.data.frame(x)) x = unlist(x, use.names = F)
  if (is.character(x)) {
    any(grepl("(str|stra(ss|ß)e|weg|allee|platz|gasse)",
      tolower(x)))
  } else F
}

rp_resolve_warehouse = \(address_id, private_table,
  transport = NULL, cfg = config) {
  if (!isTRUE(cfg$live_authorized)) {
    stop("Live access is not authorized.", call. = F)
  }
  if (is.null(transport)) {
    stop("No transport function provided.", call. = F)
  }
  if (is.null(private_table)) {
    stop("No private address table.", call. = F)
  }
  row = private_table[private_table$address_id == address_id, ]
  if (nrow(row) != 1) {
    stop("Address not found in private table.", call. = F)
  }
  addr = list(
    street = row$street[1],
    houseNumber = row$house_number[1],
    zipCode = row$postal_code[1],
    city = row$city[1])
  url = paste0(cfg$api_base, "/../address/check")
  res = transport(url, addr, list(
    user_agent = cfg$user_agent,
    timeout_seconds = cfg$timeout_seconds,
    method = "POST",
    content_type = "application/json"))
  wh_id = tryCatch({
    body = jsonlite::fromJSON(res$body, simplifyVector = F)
    as.integer(body$warehouseId %||% body$warehouse_id)
  }, error = \(e) NA_integer_)
  list(
    address_id = address_id,
    location_key = rp_location_key(row$postal_code[1]),
    warehouse_id = wh_id,
    status = as.integer(res$status),
    resolved_at = format(Sys.time(),
      "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
}
