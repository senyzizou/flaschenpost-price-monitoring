# <=============================================================================
# <=============================================================================
# <=============================================================================

# PROJECT TITLE:  FLASCHENPOST PRICE MONITORING
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261125

# <=============================================================================
# <=============================================================================
# <=============================================================================

# 1) VERSIONED ANNOTATIONS FOR THE FUTURE ANALYTICAL VIEW
# <-----------------------------------------------------------------------------

fp_apply_annotations = \(panel, root = ".") {
  file = file.path(root, "config", "analysis_annotations_20261125.csv")
  changes = read.csv(file, colClasses = "character")
  panel$analysis_annotation_version = rep(NA_character_, nrow(panel))
  for (i in seq_len(nrow(changes))) {
    a = changes[i, ]
    hit = which(panel$sku == a$sku & panel$product_key == a$product_key &
      panel$product_version_id == a$product_version_id &
      panel$daily_date >= a$effective_from &
      as.character(panel[[a$field]]) == a$before)
    if (!length(hit)) next
    panel$analysis_annotation_version[hit] = a$version
    if (a$field == "alcohol_content" && is.numeric(panel[[a$field]])) {
      panel[[a$field]][hit] = as.numeric(a$after)
    } else if (a$after == "separate_wine_mixed_drink") {
      panel$price_category[hit] = NA_character_
      panel$category_state[hit] = a$after
      panel$analysis_ready[hit] = F
      panel$analysis_ready_gt_0_5[hit] = F
    } else {
      panel[[a$field]][hit] = a$after
    }
  }
  panel
}
