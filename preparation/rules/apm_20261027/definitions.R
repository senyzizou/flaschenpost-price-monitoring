# <=============================================================================
# <=============================================================================
# <=============================================================================
# PROJECT TITLE:  ALCOHOL PRICE MONITORING - FLASCHENPOST PREPARATION
# CODE AUTHOR:    SENADIN
# DATE STARTED:   261027
# <=============================================================================
# <=============================================================================
# <=============================================================================
# 1) ISOLATED USE OF THE PROJECT CLASSIFIER (READ ONLY)
# <-----------------------------------------------------------------------------
# 1) FROZEN EXTRACTS; SEE provenance.json FOR ORIGINAL DATES
# <-----------------------------------------------------------------------------
# Exact source expressions; do not edit or style-normalize these extracts.

# Origin: utils.R definition: clean_text
clean_text = \(x) {
  x = gsub("[  ]", " ", x)
  trimws(gsub("\\s+", " ", x, perl = T))
}

# Origin: quality.R definition: no_lo_levels
no_lo_levels = c('alcohol_free_label', 'reduced_alcohol_label',
  'zero_numeric', 'low_numeric_threshold_1.2')

# Origin: retailers/conalco.R definition: match_groups
match_groups = \(text, pattern) {
  if (is.na(text))
    return(character())
  matched_groups = regexec(pattern, text, perl = T, ignore.case = T)
  regmatches(text, matched_groups)[[1]]
}

# Origin: classification.R definition: CLASSIFICATION_VERSION
CLASSIFICATION_VERSION = "beverage-0.7.1"

# Origin: classification.R definition: family_patterns
family_patterns = list(
  designation_spirits = paste0(
    "^(alkoholfreie[rns]? )?(getreidespirituose|spirituose|",
    "destillierter gin|london dry gin|gin|whisk\\w*|", "rum|vodka|wodka|",
    "grappa|tequila|mezcal|cognac|brandy|likör|", "liqueur|aquavit|",
    "weinbrand|obstbrand|obstgeist|obstdestillat|", "tresterbrand|",
    "eau.de.vie|calvados|armagnac|branntwein|brand ", "aus|korn|ouzo|",
    "pastis|raki|absinth|pisco|cachaça|aperitif|aperitiv|",
    "kräutertrunk|ethyl-alkohol)\\b"
  ),
  designation_beer = "^(alkoholfreie[rns]? )?bier(\\b|misch)",
  designation_cider = paste0(
    "^(alkoholfreie[rns]? )?(cider|cidre|apfelwein|",
    "most|aromatisierter apfelmost)(\\b|haltig)"),
  retailer_category_spirits = paste0(
    "(^|[|>])\\s*(korn|calvados|obstbrand|obstgeist|", "tresterbrand|",
    "grappa|cognac|armagnac|weinbrand)\\s*($|[|>])"
  ),
  product_name_spirits = paste0(
    "\\b(calvados|lagerkorn|doppelkorn|kornbrand|", "grappa|cognac|",
    "armagnac|weinbrand|obstbrand|obstgeist)\\b"
  ),
  product_name_cider = "\\b(cider|cidre|apfelwein)\\b",
  designation_semi_sparkling_wine = "perlwein|frizzante",
  designation_sparkling_wine = paste0(
    "schäumende[s]? getränke? aus (entalkoholisiertem|",
    "alkoholfreiem) wein|schaumwein|^sekt\\b"
  ),
  designation_fortified_wine_aperitif = paste0(
    "likörwein|portwein|sherry|madeira|wermut|", "vermouth|wein.?aperitif"),
  designation_rice_wine = "reiswein|sake",
  designation_ready_to_drink_other = paste0(
    "alkoholhaltige[s]? mischgetränk|", "alkoholische[s]? mischgetränk|",
    "spirituosenmischgetränk|^bitter mit soda|", "alkoholhaltiges heißgetränk|",
    "^(alkoholfreie[rns]? )?cocktail"
  ),
  designation_other_wine = paste0(
    "glühwein|fruchtwein|honigwein|heidelbeerwein|",
    "erdbeerwein|kirschwein|weinhaltig|weinmisch|",
    "aromatisiertes getränk aus entalkoholisiertem wein"
  ),
  designation_still_wine = paste0(
    "^entalkoholisierte[rnms]* wein|", "^(alkoholfreie[rns]? )?(rotwein|",
    "weißwein|weisswein|roséwein|wein)\\b"
  ),
  product_name_semi_sparkling_wine = "perlwein|frizzante",
  product_name_sparkling_wine = paste0(
    "schaumwein|sparkling|\\b\\w*sekt\\b|cava|", "champagn|prosecco|",
    "spumante|cr[eé]mant"
  ),
  product_name_fortified_wine_aperitif = paste0(
    "portwein|sherry|madeira|marsala|likörwein|", "vermouth|wermut|",
    "lillet|martini (vibrante|floreale)"
  ),
  product_name_rice_wine = "\\bsake\\b|reiswein",
  product_name_other_wine = paste0(
    "glühwein|glögg|fruchtwein|honigwein|\\bmet\\b|", "weinhaltig|",
    "weinmisch|aromatisiertes getränk aus ", "entalkoholisiertem wein"
  ),
  product_name_still_wine = paste0(
    "pinot grigio|rotwein|weißwein|weisswein|", "ros[eé]wein|dessertwein|",
    "süßwein|\\bwein\\b|\\bwine\\b|chardonnay|", "sauvignon|riesling|merlot|",
    "cabernet"
  ),
  product_name_beer = paste0(
    "\\bbier\\b|biermisch|\\bale\\b|\\blager\\b|", "stout|\\bpils\\b|pilsener|",
    "lambic|radler|witbier|weizen.mix"
  ),
  product_name_spirits_n10 = paste0(
    "\\bgin\\b|whisk|\\brum\\b|vodka|wodka|tequila|", "mezcal|brandy|likör|",
    "liqueur|aquavit|schnaps|brand\\b|ouzo|pastis|", "raki|absinth|",
    "anisspirituose|sambuca|genever|pisco|cacha|trinkalkohol|",
    "primasprit|neutralalkohol|weingeist|\\bspirit\\b"
  ),
  product_name_ready_to_drink_other = paste0(
    "cocktail|ready.to.drink|ready.to.serve|spritz|", "punch|punsch|",
    "hard.?seltzer|mojito|bellini|negroni|daiquiri|", "margarita|colada|",
    "cuba libre"
  ),
  product_name_spirits_n12 = "aperitif|aperitiv|bitter|kräutertrunk|amaretto",
  retailer_category_still_wine = paste0(
    "(^|[|>])\\s*(rotwein|weißwein|weisswein|", "weißwein & roséwein|",
    "weisswein-rosewein|roséwein)\\s*($|[|>])"
  ),
  retailer_category_beer = "(^|[|>])\\s*bier\\s*($|[|>])",
  retailer_category_sparkling_wine =
  "(^|[|>])\\s*(sekt|schaumwein|champagner)\\s*($|[|>])",
  retailer_category_spirits_c05 = paste0(
    "(^|[|>])\\s*(spirituosen|likör|whisky|vodka|gin|", "rum|anis|",
    "obst)\\s*($|[|>])"
  ),
  description_sparkling_wine = paste0(
    "perlend.*entalkoholisiertem wein|",
    "schäumende[s]? getränke? aus (entalkoholisiertem|",
    "alkoholfreiem) wein"
  ),
  description_still_wine = "entalkoholisiert.*(weißwein|weisswein|rotwein|wein)"
)

# Origin: classification.R definition: classification_rules
classification_rules = tribble(
  ~rule_id, ~source, ~pattern, ~beverage_group,
  # Explicit finished mixtures and fortified fruit must override generic shop
  # labels.
  "X01", "product_name",
  "ready.to.drink|pisco.cocktail|\\bcoctel\\b", "ready_to_drink_other",
  "X02", "product_name",
  "\\bpommeau\\b", "fortified_fruit_must",
  "D01", "designation_raw",
  family_patterns$designation_spirits, "spirits",
  "D02", "designation_raw",
  family_patterns$designation_beer, "beer",
  "D03", "designation_raw",
  family_patterns$designation_cider, "cider",
  # Specific category and spirit-name evidence.
  "C01", "retailer_category",
  family_patterns$retailer_category_spirits, "spirits",
  "N01", "product_name",
  family_patterns$product_name_spirits, "spirits",
  "N02", "product_name",
  family_patterns$product_name_cider, "cider",
  # Remaining legal designations.
  "D04", "designation_raw",
  family_patterns$designation_semi_sparkling_wine, "semi_sparkling_wine",
  "D05", "designation_raw",
  family_patterns$designation_sparkling_wine, "sparkling_wine",
  "D06", "designation_raw",
  family_patterns$designation_fortified_wine_aperitif,
  "fortified_wine_aperitif",
  "D07", "designation_raw",
  family_patterns$designation_rice_wine, "rice_wine",
  "D08", "designation_raw",
  family_patterns$designation_ready_to_drink_other, "ready_to_drink_other",
  "D09", "designation_raw",
  family_patterns$designation_other_wine, "other_wine",
  "D10", "designation_raw",
  family_patterns$designation_still_wine, "still_wine",
  # Explicit product descriptions precede generic name hints, not legal
  # designations.
  "X03", "description_raw",
  "^aromatisierte[rs]?,?\\s+weinhaltiger cocktail\\b", "ready_to_drink_other",
  "X04", "description_raw",
  "^italienischer aperitif auf basis von wein\\b", "fortified_wine_aperitif",
  # Product-name evidence.
  "N03", "product_name",
  family_patterns$product_name_semi_sparkling_wine, "semi_sparkling_wine",
  "N04", "product_name",
  family_patterns$product_name_sparkling_wine, "sparkling_wine",
  "N05", "product_name",
  family_patterns$product_name_fortified_wine_aperitif,
  "fortified_wine_aperitif",
  "N06", "product_name",
  family_patterns$product_name_rice_wine, "rice_wine",
  "N07", "product_name",
  family_patterns$product_name_other_wine, "other_wine",
  "N08", "product_name",
  family_patterns$product_name_still_wine, "still_wine",
  "N09", "product_name",
  family_patterns$product_name_beer, "beer",
  "N10", "product_name",
  family_patterns$product_name_spirits_n10, "spirits",
  "N11", "product_name",
  family_patterns$product_name_ready_to_drink_other, "ready_to_drink_other",
  "N12", "product_name",
  family_patterns$product_name_spirits_n12, "spirits",
  # Remaining retailer-category evidence.
  "C02", "retailer_category",
  family_patterns$retailer_category_still_wine, "still_wine",
  "C03", "retailer_category",
  family_patterns$retailer_category_beer, "beer",
  "C04", "retailer_category",
  family_patterns$retailer_category_sparkling_wine, "sparkling_wine",
  "C05", "retailer_category",
  family_patterns$retailer_category_spirits_c05, "spirits",
  # Description evidence is evaluated last.
  "P01", "description_raw",
  family_patterns$description_sparkling_wine, "sparkling_wine",
  "P02", "description_raw",
  family_patterns$description_still_wine, "still_wine"
)
classification_rules$priority = seq_len(nrow(classification_rules))

# Origin: classification.R definition: scope_patterns
scope_patterns = list(
  accessory = paste0(
    "tragetasche|dosierpumpe|geschenkverpackung|geschenktüte|",
    "sonstiges zubehör|eiswürfelbehälter|eiskübel|", "kartenhalter|",
    "\\w*kelch|\\w*pfännchen|\\w*verschluss|\\w*schach\\b|",
    "\\w*portionierer|\\w*sieb|wand\\w*schild|", "ausfuhrbescheinigung|",
    "\\b(glas|shotglas|longdrinkglas|trinkglas|whiskyglas|",
    "gläsern?|shotgläser|tumbler|untersetzer|", "hornträger|hornglas|",
    "likörschale|strohhalme?|flaschenständer|streichhölzer|",
    "pinnchen|zeckenklatsche|zitronenpresse|", "leerflasche|rührstab|",
    "ausgießer|außgießer|barlöffel|trinkhalm|shaker|", "korkenzieher|",
    "gutschein|t.?shirt|tasse|becher|poster|stössel|stößel|",
    "jigger|serviertablett|stamper|krug|karaffe|strainer|",
    "trinkhorn|auslaufhahn|flaschenöffner|", "schlüsselanhänger)\\b|",
    "ice ball maker"
  ),
  ingredient_designation = paste0(
    "^(sirup|püree|pur[eé]e|zubereitung (auf fruchtbasis|",
    "mit kokosnuss)|olivenöl|kokosnussmilch|barbecue|",
    "karamell.*soße|schokoladen.*soße|dessertsauce|senf|",
    "haferdrink|hafermilch|entsteinte.*kirschen|", "gewürzmischung|",
    "eingelegte früchte|getrocknete früchte|", "cocktail.kirschen|",
    "frucht.püree|cocktail[ -]?aroma|aromatic bitter|",
    "getränkekonzentrat|konzentrat|",
    "verjus.saft|\\w*zucker)"
  ),
  ingredient_name = paste0(
    "sirup|püree|pur[eé]e|olivenöl|kokosmilch|cream ", "of coconut|",
    "crema de coco|eiweißpulver|fee foam|cocktailkirschen|",
    "maraschino kirschen|frapp[eé]|dessertsauce|\\bsauce\\b|",
    "\\bsalz\\b|rohrzucker|zuckerhut"
  )
)

# Origin: classification.R definition: classify_products
classify_products = \(products, rules = classification_rules,
  evidence_retailers = 'conalco') {
  ## Classification evidence <--------------------------------------------

  for (column in c('product_name', 'designation_raw', 'retailer_category',
      'description_raw')) {
    if (!column %in% names(products)) products[[column]] = NA_character_
  }
  if (!'alcohol_content' %in% names(products)) products$alcohol_content =
    products$abv
  classified_products = products %>% mutate(
    beverage_group = 'unclassified', classification_rule = 'U00',
    classification_source = NA_character_, classification_evidence =
    NA_character_,
    classification_conflict = F
  )
  for (i in seq_len(nrow(rules))) {
    rule = rules[i, ]
    values = tolower(coalesce(classified_products[[rule$source]], ''))
    matched = grepl(rule$pattern, values, perl = T)
    # Conflicting legal designations remain visible for human review.
    conflict = matched & startsWith(classified_products$classification_rule,
      'D') &
      rule$source == 'designation_raw' & classified_products$beverage_group !=
      rule$beverage_group &
      !paste(classified_products$classification_rule, rule$rule_id) %in% c(
      'D03 D09', 'D06 D10', 'D05 D10')
    classified_products$classification_conflict =
      classified_products$classification_conflict | conflict
    use = matched & classified_products$classification_rule == 'U00'
    classified_products$beverage_group[use] = rule$beverage_group
    classified_products$classification_rule[use] = rule$rule_id
    classified_products$classification_source[use] = rule$source
    classified_products$classification_evidence[use] = classified_products[[
        rule$source]][use]
  }

  ## Scope evidence <-----------------------------------------------------

  product_name_lower = tolower(coalesce(classified_products$product_name, ''))
  designation_lower = tolower(coalesce(classified_products$designation_raw, ''))
  category_lower = tolower(coalesce(classified_products$retailer_category, ''))
  scope_text = paste(product_name_lower, designation_lower)
  primary_evidence = paste(product_name_lower, designation_lower,
    tolower(coalesce(classified_products$description_raw, '')))
  alcohol_free_label = grepl(
    paste0("alkoholfrei|ohne alkohol|non.?alcoholic|",
      "alcohol.?free|entalkoholisiert|\\b0[,.]0\\s*%"),
    primary_evidence, perl = T
  )
  reduced_alcohol_label = grepl(
    paste0("alkoholreduziert|alkoholarm|low.?alcohol|",
      "lower.?alcohol|reduced.?alcohol|reduzierter ", "alkoholgehalt"),
    primary_evidence
  )
  accessory = grepl(scope_patterns$accessory, scope_text, perl = T)
  # Explicit beverage/glass bundles and alcoholic miniatures remain reviewable.
  accessory = accessory & !(grepl('in geschenkverpackung', product_name_lower) &
      startsWith(classified_products$classification_rule, 'D'))
  bundle = accessory & grepl('\\bmit\\b|inkl[.]?|\\+|geschenkset|\\bset\\b',
    product_name_lower, perl = T) &
    (startsWith(classified_products$classification_rule, 'D') | !is.na(
      classified_products$alcohol_content))
  miniature = grepl('glas miniatur|mini glas', product_name_lower) &
    !is.na(classified_products$alcohol_content) &
    classified_products$alcohol_content > 0
  accessory = accessory & !miniature
  ingredient_designation = grepl(scope_patterns$ingredient_designation,
    designation_lower, perl = T)
  ingredient_name = grepl(scope_patterns$ingredient_name, product_name_lower,
    perl = T)
  ingredient_bundle = ingredient_name & grepl('bundle|\\+', product_name_lower,
    perl = T) &
    !is.na(classified_products$alcohol_content)
  ingredient = ingredient_designation |
    (ingredient_name & !startsWith(classified_products$classification_rule,
      'D'))
  alternative_basis = grepl(
    paste0("basis.*alkoholfreie.*(cocktails|longdrinks)|",
      "alkoholfreie basis.*cocktails"), designation_lower
  )
  soft = grepl(
    paste0("tonic|limonade|\\bsoda\\b|mineralwasser|saft|",
      "säfte|ginger beer|ginger ale|erfrischungsgetränk"),
    scope_text, perl = T
  )
  verjus_candidate = grepl('verjus', product_name_lower) &
    grepl('alkoholfreie weine', category_lower) &
    !grepl('saft|erfrischungsgetränk', designation_lower)
  basis_unspecified = alternative_basis &
    !classified_products$beverage_group %in% c('spirits', 'still_wine',
    'sparkling_wine',
    'fortified_wine_aperitif')

  ## Family adjustment and inclusion <-------------------------------------

  # Evaluate the mixer predicate after the same family adjustment as before.
  classified_products = classified_products %>% mutate(
    beverage_group = if_else(basis_unspecified, 'alternative_unspecified',
      beverage_group)
  )
  labelled_family = alcohol_free_label &
    classified_products$beverage_group %in% c(
    'spirits', 'beer', 'cider', 'still_wine', 'other_wine', 'rice_wine',
    'sparkling_wine',
    'semi_sparkling_wine', 'fortified_wine_aperitif', 'ready_to_drink_other'
  ) & !grepl('tonic|soda', product_name_lower)
  designated_alcoholic = coalesce(
    classified_products$alcohol_content > 0 & startsWith(
      classified_products$classification_rule, 'D'), F
  )
  ordinary_mixer = soft & !alternative_basis & !labelled_family &
    !designated_alcoholic
  classified_products = classified_products %>% mutate(
    scope_rule = case_when(
      bundle | ingredient_bundle ~ 'S01_bundle_review',
      accessory ~ 'S02_accessory',
      ingredient ~ 'S03_ingredient_food',
      verjus_candidate ~ 'S04_verjus_review',
      .env$ordinary_mixer ~ 'S05_softdrink_mixer',
      classification_conflict ~ 'S06_conflicting_designation',
      basis_unspecified ~ 'S07_unspecified_alternative_base',
      beverage_group != 'unclassified' ~ 'S08_drink_family',
      T ~ 'S09_insufficient_evidence'
    ),
    scope_status = case_when(
      scope_rule %in% c('S02_accessory', 'S03_ingredient_food',
        'S05_softdrink_mixer') ~
        'excluded_nonfamily_product',
      scope_rule == 'S08_drink_family' ~ 'included',
      T ~ 'unclear_review'
    ),
    beverage_group = case_when(
      scope_rule == 'S02_accessory' ~ 'non_beverage',
      scope_rule == 'S03_ingredient_food' ~ 'ingredient_food',
      scope_rule == 'S05_softdrink_mixer' ~ 'softdrink_mixer',
      beverage_group == 'unclassified' & alcohol_free_label ~
        'alternative_unspecified',
      beverage_group == 'unclassified' & alcohol_content > 0 ~
        'other_alcoholic',
      T ~ beverage_group
    )
  )

  ## No/Lo evidence and provenance <---------------------------------------

  classified_products %>% mutate(
    no_lo_status = case_when(
      scope_status == 'excluded_nonfamily_product' ~ 'not_applicable',
      alcohol_free_label ~ 'alcohol_free_label',
      reduced_alcohol_label ~ 'reduced_alcohol_label',
      alcohol_content == 0 ~ 'zero_numeric',
      alcohol_content > 0 & alcohol_content <= 1.2 ~
        'low_numeric_threshold_1.2',
      alcohol_content > 1.2 ~ 'alcoholic',
      grepl('alkoholfrei|alkoholreduziert|alkoholarm', category_lower) ~
        'no_lo_candidate_category',
      T ~ 'unknown'
    ),
    no_lo_evidence = case_when(
      alcohol_free_label | reduced_alcohol_label ~ primary_evidence,
      !is.na(alcohol_content) ~ paste('explicit ABV', alcohol_content),
      no_lo_status == 'no_lo_candidate_category' ~ retailer_category,
      T ~ NA_character_
    ),
    scope_evidence = paste(product_name, designation_raw, sep = ' | '),
    classification_version = if ('retailer' %in% names(classified_products))
      ifelse(is.na(retailer) | retailer %in% evidence_retailers,
        CLASSIFICATION_VERSION, 'beverage-0.5.0')
    else CLASSIFICATION_VERSION,
    beverage_family = beverage_group,
    beverage_family_source = classification_source,
    scope_reason = scope_rule
  ) %>% classify_alkopops(evidence_retailers)
}

# Origin: classification.R definition: retailer_rtd_patterns
retailer_rtd_patterns = c(
  conalco = 'ready to drink|ready to serve',
  mytime = 'cocktails?[^|]*mixgetränke|cocktails?[^|]*mixgetraenke',
  netto_online = 'mixgetränke[^|]*longdrinks|mixgetraenke[^|]*longdrinks',
  edeka24 = 'mixgetränke in dosen|mixgetraenke.in.dosen',
  aldi_nord = paste0("spirituosen[^|]*liköre|spirituosen[^|]*likoere|",
    "mischgetränke|mischgetraenke")
)

# Origin: classification.R definition: evidence_rtd_patterns
evidence_rtd_patterns = c(
  conalco = 'ready to drink|ready to serve',
  aldi_nord = 'mischgetränke|mischgetraenke|cocktails|longdrinks'
)

# Origin: classification.R definition: retailer_rtd_candidates
retailer_rtd_candidates = \(products, patterns = retailer_rtd_patterns) {
  text = paste(coalesce(products$retailer_category, ''),
    coalesce(products$retailer_category_labels, ''))
  if (!'retailer' %in% names(products)) products$retailer = NA_character_
  vapply(seq_len(nrow(products)), \(i) {
      pattern = patterns[products$retailer[i]]
      length(pattern) == 1 && !is.na(pattern) && grepl(pattern, tolower(text[
            i]), perl = T)
    }, logical(1))
}

# Origin: classification.R definition: classify_alkopops
classify_alkopops = \(products, evidence_retailers = 'conalco') {
  x = products
  text_fields = c('product_name', 'designation_raw', 'retailer_category',
    'retailer_category_labels',
    'description_raw', 'archived_description_raw', 'beverage_group',
    'scope_status', 'no_lo_status',
    'packaging_type', 'ingredients_raw', 'alcohol_tax_status',
    'alcohol_tax_evidence', 'cn_code',
    'ready_to_drink_evidence', 'sealed_container_evidence',
    'joint_components_evidence',
    'alcohol_basis_evidence', 'alcohol_content_basis')
  for (field in text_fields) if (!field %in% names(x)) x[[field]] =
      NA_character_
  if (!'alcohol_content' %in% names(x)) x$alcohol_content = NA_real_
  for (field in c('ready_to_drink', 'sealed_container', 'joint_components')) {
    if (!field %in% names(x)) x[[field]] = NA
    x[[field]] = as.logical(x[[field]])
  }
  name = tolower(coalesce(x$product_name, ''))
  designation = tolower(coalesce(x$designation_raw, ''))
  description = coalesce(x$archived_description_raw, x$description_raw, '')
  # Section headings, rather than mentions of a classic cocktail, delimit
  # recipes.
  production = sub(paste0("(?i)(rezept(vorschlag|idee)\\b|rezept\\s*:|",
      "servier(vorschlag|empfehlung)).*"),
    '', description, perl = T)
  extracted_ingredients = vapply(production, \(text) {
      m = match_groups(text, paste0(
          "Zutaten:\\s*(.*?)(?:Allergene:|Zusatzstoffe:|",
          "Ursprungsland:|Lebensmittelunternehmer:|$)"))
      if (length(m) < 2 || grepl('ein zutatenverzeichnis', m[2], ignore.case =
            T)) '' else m[2]
    }, character(1))
  ingredients = tolower(coalesce(x$ingredients_raw, extracted_ingredients))
  spirit_pattern = paste0("\\b(rum|gin|vodka|wodka|tequila|whisk\\w*|",
    "branntwein|ethylalkohol|neutralalkohol)\\b")
  ingredient_spirit = grepl(spirit_pattern, ingredients, perl = T)
  named_spirit_mix = grepl(paste0(spirit_pattern,
      '.*(cola|tonic|limonade|saft)|spirituosenmischgetränk'),
    paste(name, designation), perl = T)
  documented_spirit = !is.na(x$alcohol_basis_evidence) &
    grepl(spirit_pattern, tolower(x$alcohol_basis_evidence), perl = T)
  spirit = ingredient_spirit | named_spirit_mix | documented_spirit
  nonspirit_component = grepl(
    'wasser|saft|limonade|cola|tonic|wein|bier|cider', ingredients) |
    named_spirit_mix | (documented_spirit & grepl(
      'wasser|saft|limonade|cola|tonic|wein|bier',
      tolower(coalesce(x$alcohol_basis_evidence, ''))))
  fermented = grepl(paste0("hergestellt (auf basis von|aus) (vergorenem ",
      ")?fruchtwein|produziert auf basis von ",
      "vergorenem fruchtwein|basis bilden vergorene ", "fruchtweine"),
    tolower(production), perl = T) | grepl(paste0(
      "^weinmischgetränk|^weinschorle|",
      "^biermischgetränk|^cider|^apfelwein"), designation)
  fermented = fermented | (grepl('wein|bier|cider|fruchtwein', ingredients) &
      !ingredient_spirit &
      !grepl('alkohol|destillat|branntwein', ingredients))
  base_conflict = fermented & spirit
  conalco = if ('retailer' %in% names(x)) is.na(x$retailer) | x$retailer %in%
      evidence_retailers else
    rep(T, nrow(x))
  category_candidate = retailer_rtd_candidates(x)
  category_candidate[conalco] = retailer_rtd_candidates(x,
    evidence_rtd_patterns)[conalco]
  labelled_rtd = grepl(paste0("ready.to.(drink|serve)|trinkfertig|",
      "servierfertig|fertig gemischt"),
    paste(name, designation, tolower(production)), perl = T)
  mixing = grepl(paste0("mischgetränk|cocktail|longdrink|spritz|",
      "ready.to.(drink|serve)|rum.*cola|gin.*tonic"),
    paste(name, designation), perl = T) | x$beverage_group ==
    'ready_to_drink_other'
  jointly_prepared = x$joint_components %in% T &
    !is.na(x$joint_components_evidence) & nzchar(coalesce(
      x$joint_components_evidence, ''))
  assessed_abv = x$alcohol_content
  assessed_abv[jointly_prepared & !x$alcohol_content_basis %in%
      'prepared_final_mixture'] = NA_real_
  candidate = category_candidate | mixing | jointly_prepared
  excluded_scope = x$scope_status %in% 'excluded_nonfamily_product'
  evidence_present = \(value) !is.na(value) & nzchar(trimws(coalesce(value,
        '')))
  ready = (x$ready_to_drink %in% T & evidence_present(
      x$ready_to_drink_evidence)) | labelled_rtd
  # Bottle/can mentions in recipes cannot establish a sealed retail container.
  sealed = x$sealed_container %in% T & evidence_present(
    x$sealed_container_evidence)
  cn = gsub('[^0-9]', '', coalesce(x$cn_code, ''))
  taxable = (x$alcohol_tax_status == 'subject_to_alcohol_tax' &
      !is.na(x$alcohol_tax_evidence) & nzchar(coalesce(x$alcohol_tax_evidence,
        ''))) |
    (substr(cn, 1, 4) %in% c('2207', '2208') & evidence_present(
      x$alcohol_tax_evidence))
  taxable = coalesce(taxable, F)
  outside_abv = !is.na(assessed_abv) & (assessed_abv <= 1.2 | assessed_abv >=
      10)
  known_non_taxable = x$alcohol_tax_status %in% 'not_subject_to_alcohol_tax' &
    evidence_present(x$alcohol_tax_evidence)
  tax_conflict = taxable & known_non_taxable
  evidence_input = x
  evidence_input$rtd_category_candidate = category_candidate
  evidence = conalco_rtd_evidence(evidence_input)
  spirit[conalco] = evidence$spirit_component_documented[conalco]
  nonspirit_component[conalco] = evidence$other_component_documented[conalco]
  fermented[conalco] = evidence$fermented_only_documented[conalco]
  base_conflict[conalco] = evidence$alcohol_basis_conflict[conalco]
  candidate[conalco] = evidence$rtd_candidate_documented[conalco]
  ready[conalco] = evidence$ready_mixed_documented[conalco]
  sealed[conalco] = sealed[conalco] | nzchar(
    evidence$sealed_retail_text_evidence[conalco])
  taxable[conalco] = taxable[conalco] | nzchar(
    evidence$finished_product_tax_text_evidence[conalco])
  tax_conflict = taxable & known_non_taxable
  not_ready = conalco & evidence$not_ready_documented & candidate &
    !jointly_prepared
  nolo_label = x$no_lo_status %in% c('alcohol_free_label', 'zero_numeric') &
    (is.na(x$alcohol_content) | x$alcohol_content <= 0.5)
  confirmed = candidate & spirit & nonspirit_component & (ready & sealed |
      jointly_prepared) &
    taxable & !is.na(assessed_abv) & assessed_abv > 1.2 & assessed_abv < 10 &
    !base_conflict & !excluded_scope
  x$alkopop_candidate = candidate
  x$alkopop_status = case_when(
    not_ready ~ 'ruled_out',
    excluded_scope ~ 'not_applicable',
    outside_abv ~ 'ruled_out',
    !candidate ~ 'not_candidate',
    nolo_label ~ 'not_applicable_nolo_label',
    base_conflict | tax_conflict ~ 'unclear',
    confirmed ~ 'confirmed',
    known_non_taxable | (fermented & !spirit) ~ 'ruled_out',
    T ~ 'unclear'
  )
  x$alkopop_rule = case_when(
    not_ready ~ 'A00b_documented_nonready_product',
    excluded_scope ~ 'A00_excluded_scope',
    outside_abv ~ 'A01_open_abv_interval_1.2_10',
    !candidate ~ 'A02_no_rtd_candidate',
    nolo_label ~ 'A03_nolo_label_separate',
    base_conflict | tax_conflict ~
      'A04_conflicting_alcohol_basis_or_tax_evidence',
    confirmed & jointly_prepared ~ 'A05_joint_industrial_components_section1_3',
    confirmed ~ 'A06_all_four_statutory_features_evidenced',
    known_non_taxable ~ 'A07_documented_non_taxable',
    fermented & !spirit ~ 'A08_documented_fermented_base',
    T ~ 'A09_missing_statutory_evidence'
  )
  x$alkopop_missing_features = vapply(seq_len(nrow(x)), \(i) {
      if (x$alkopop_status[i] != 'unclear') return('')
      missing = character()
      if (is.na(assessed_abv[i])) missing = c(missing, 'exact_abv')
      if (jointly_prepared[i] && is.na(assessed_abv[i])) missing = c(missing,
        'final_mixture_abv_not_component_abv')
      if (!spirit[i]) missing = c(missing,
        'spirit_or_alcohol_tax_product_component')
      if (!nonspirit_component[i]) missing = c(missing,
        'nonalcoholic_or_fermented_component')
      if (!ready[i] && !jointly_prepared[i]) missing = c(missing,
        'ready_mixed_or_industrial_joint_components')
      if (!sealed[i] && !jointly_prepared[i]) missing = c(missing,
        'sealed_retail_container_or_common_packaging')
      if (!taxable[i]) missing = c(missing,
        'finished_product_alcohol_tax_liability')
      if (base_conflict[i]) missing = c(missing,
        'resolve_conflicting_spirit_and_fermented_basis')
      if (tax_conflict[i]) missing = c(missing,
        'resolve_conflicting_tax_evidence')
      paste(missing, collapse = ';')
    }, character(1))
  x$alkopop_reason = paste(x$alkopop_rule,
    ifelse(nzchar(x$alkopop_missing_features), x$alkopop_missing_features,
      'recorded_evidence_applied'), sep = ': ')
  x$alkopop_evidence = paste(x$product_name, x$designation_raw, ingredients,
    coalesce(x$alcohol_basis_evidence, ''), coalesce(x$alcohol_tax_evidence,
      ''), sep = ' | ')
  x$rtd_product_type = case_when(
    jointly_prepared ~ 'joint_mixture_components',
    candidate & spirit ~ 'spirit_based_mixture',
    candidate & fermented & !spirit ~ 'fermented_based_mixture',
    candidate ~ 'mixture_basis_unknown',
    T ~ 'not_rtd'
  )
  x$rtd_category_candidate = category_candidate
  x$alkopop_assessed_abv = assessed_abv
  x$alkopop_legal_status = recode(x$alkopop_status,
    not_applicable = 'not_candidate', not_applicable_nolo_label = 'ruled_out')
  for (field in names(evidence)) x[[field]] = evidence[[field]]
  # One evidence assessment feeds both definitions; no legal criterion is
  # relaxed.
  x$alkopop_research_status = case_when(
    !conalco ~ 'not_evaluated_outside_conalco',
    candidate & (outside_abv | not_ready | (known_non_taxable &
          !tax_conflict)) ~ 'ruled_out',
    excluded_scope | nolo_label ~ 'not_candidate',
    candidate & fermented & !spirit ~ 'ruled_out',
    !candidate ~ 'not_candidate',
    x$alkopop_legal_status == 'confirmed' ~ 'confirmed_legal_alkopop',
    (base_conflict | tax_conflict) &
      x$suitable_alcohol_component_positive_hint ~ 'possible_alkopop',
    spirit & nonspirit_component & (ready | jointly_prepared) & !is.na(
      assessed_abv) ~ 'documented_spirit_rtd',
    x$suitable_alcohol_component_positive_hint ~ 'possible_alkopop',
    T ~ 'general_rtd_unknown_basis'
  )
  x$alkopop_research_rule = case_when(
    x$alkopop_research_status == 'confirmed_legal_alkopop' ~
      'R01_legal_confirmation',
    candidate & outside_abv ~ 'R03_outside_open_abv_interval',
    candidate & fermented & !spirit ~ 'R02_exclusively_fermented_components',
    not_ready ~ 'R04_nonready_product',
    x$alkopop_research_status == 'documented_spirit_rtd' ~
      'R05_documented_spirit_mixture_and_rtd',
    base_conflict | tax_conflict ~ 'R06_conflicting_product_evidence',
    x$alkopop_research_status == 'possible_alkopop' ~
      'R07_positive_component_hint_missing_operational_evidence',
    x$alkopop_research_status == 'general_rtd_unknown_basis' ~
      'R09_general_rtd_without_positive_suitable_alcohol_hint',
    T ~ 'R08_not_relevant_or_outside_conalco'
  )
  x$alkopop_research_missing_features = vapply(seq_len(nrow(x)), \(i) {
      if (!x$alkopop_research_status[i] %in% c('possible_alkopop',
          'general_rtd_unknown_basis')) return('')
      missing = c(if (is.na(assessed_abv[i])) 'exact_final_mixture_abv',
        if (!spirit[i]) 'documented_alcohol_tax_product_component',
        if (!nonspirit_component[i]) 'documented_other_mixture_component',
        if (!ready[i] && !jointly_prepared[
            i]) 'documented_ready_mixed_sale_form',
        if (base_conflict[i] | tax_conflict[
            i]) 'resolve_conflicting_product_evidence')
      paste(missing, collapse = ';')
    }, character(1))
  x$alkopop_research_reason = paste(x$alkopop_research_rule,
    x$composition_evidence,
    x$alkopop_research_missing_features, sep = ': ')
  x$alkopop_evidence[conalco] = paste(x$composition_evidence[conalco],
    coalesce(x$alcohol_tax_evidence[conalco], ''), sep = ' | ')
  x$rtd_product_type[conalco] = case_when(
    jointly_prepared[conalco] ~ 'joint_mixture_components',
    !candidate[conalco] ~ 'not_rtd',
    spirit[conalco] ~ 'spirit_based_mixture',
    fermented[conalco] ~ 'fermented_based_mixture',
    T ~ 'mixture_basis_unknown')
  restored_aperitif = conalco & x$aperitif_base_documented &
    x$beverage_group == 'ready_to_drink_other'
  x$beverage_group[restored_aperitif] = 'spirits'
  x$classification_rule[restored_aperitif] = 'R00_documented_aperitif_base'
  x$classification_source[restored_aperitif] = 'product_description_evidence'
  x$classification_evidence[restored_aperitif] =
    x$product_description_evidence[restored_aperitif]
  if ('beverage_family_source' %in% names(x))
  x$beverage_family_source[restored_aperitif] = x$classification_source[
    restored_aperitif]
  if ('beverage_family' %in% names(x)) x$beverage_family = x$beverage_group
  x
}

# Origin: classification.R definition: conalco_evidence_sections
conalco_evidence_sections = \(description) {
  description = if (is.na(description)) '' else description
  fact = gregexpr('Verkehrsbezeichnung\\s*:', description, ignore.case = T,
    perl = T)[[1]]
  facts = if (fact[1] > 0) substring(description, tail(fact, 1)) else ''
  narrative = if (fact[1] > 0) substring(description, 1, tail(fact, 1) -
        1) else description
  narrative = sub('Name:\\s*[^:]*$', '', narrative, perl = T)
  heading = paste0("(?i)(?:rezept(?:vorschlag|idee)?\\s*:|",
    "servier(?:vorschl[aä]g[ee]?|empfehlungen?|", "tipps?|varianten)\\b|",
    "cocktail[- ](?:inspiration|rezepte?)\\b|",
    "zubereitung\\s*:|verwendung(?:sempfehlungen|", "smöglichkeiten)?\\s*:|",
    "verwendungsempfehlungen\\b|empfehlungen (?:zur|",
    "für die) verwendung\\b|perfekt serviert\\s*:|", "DIY[- ]Tipp\\s*:)")
  start = regexpr(heading, narrative, perl = T)
  serving = if (start[1] > 0) substring(narrative, start[1]) else ''
  production = if (start[1] > 0) substring(narrative, 1, start[1] -
        1) else narrative
  # Facts after a recipe remain available. Dot-all is required for HTML block
  # breaks.
  ingredients = match_groups(if (nzchar(facts)) facts else production,
    paste0("(?s)Zutaten\\s*:\\s*(.*?)(?:Allergene\\s*:|",
      "Zusatzstoffe\\s*:|Ursprungsland\\s*:|",
      "Lebensmittelunternehmer\\s*:|Hinweise zur|$)"))
  ingredients = if (length(ingredients) >= 2) clean_text(ingredients[
        2]) else NA_character_
  if (!is.na(ingredients) && grepl(
      'zutatenverzeichnis.*nicht erforderlich|keine angaben',
      ingredients, ignore.case = T)) ingredients = NA_character_
  list(production = coalesce(clean_text(production), ''), ingredients =
    ingredients,
    facts = facts, recipe_removed = start[1] > 0,
    serving_excluded = coalesce(clean_text(serving), ''))
}

# Origin: classification.R definition: conalco_component_patterns
conalco_component_patterns = \() {
  word = \(pattern) paste0('(?<![\\p{L}\\p{N}_])(?:', pattern,
    ')(?![\\p{L}\\p{N}_])')
  list(
    spirit = word(paste0("wodka|vodka|rum|gin|whisky|whiskey|tequila|",
        "mezcal|branntwein|",
        "ethylalkohol|neutralalkohol|agraralkohol|",
        "weingeist|getreidedestillat|weindestillat|",
        "cognac|brandy|korn|calvados|armagnac|pisco|",
        "grappa|cacha[çc]a|limoncello|limoncino|triple[ ", "-]sec|",
        '[\\p{L}-]*(?:likör|likoer|liqueur)')),
    beer = word(paste0("(?:voll|weizen|weiß|weiss|lager|stark|",
        "schwarz)?bier(?:e|en|es|s|haltig(?:e|er|es|en)?|",
        "mischgetränk(?:e|es|en)?)?")),
    wine = word(paste0("(?:wei[ßs]wein|weisswein|rotwein|ros[eé]wein|",
        "traubenwein|wein|perlwein|schaumwein|prosecco|",
        "sekt|spumante|secco)(?:basis|basiert|e|en|es|s|",
        "haltig(?:e|er|es|en)?|mischgetränk(?:e|es|en)?)?")),
    fruit_wine = word(paste0("(?:fruchtwein|fruchtperlwein|beerenwein|",
        "honigwein|frucht[ -]*secco)(?:basis|basiert|e|",
        "en|es|s|haltig(?:e|er|es|en)?|mischgetränk(?:e|", "es|en)?)?")),
    cider = word(paste0("(?:cider|apfelwein|apfeltischwein|",
        "birnenwein)(?:basis|basiert|e|en|es|s|",
        "haltig(?:e|er|es|en)?|mischgetränk(?:e|es|",
        "en)?)?|(?:fermentiert|vergoren)[\\p{L}]*\\s+(?:a",
        "pfelsaft|apfelmost)")),
    soft = word(paste0("wasser|mineralwasser|sodawasser|[\\p{L}-]*saft|",
        "[\\p{L}-]*limonade|cola|tonic|eistee|aromen?|",
        "fruchtmark|fruchtfleisch|joghurt|kohlensäure|",
        "cranberry|orange[n]?|pfirsich|ananas|",
        "limette[n]?|zitrone[n]?|kokos")))
}

# Origin: classification.R definition: conalco_rtd_evidence
conalco_rtd_evidence = \(products) {
  x = products
  n = nrow(x)
  fields = c('product_name', 'designation_raw', 'archived_description_raw',
    'description_raw',
    'ingredients_raw', 'alcohol_basis_evidence', 'ready_to_drink_evidence')
  for (field in fields) if (!field %in% names(x)) x[[field]] = rep(
      NA_character_, n)
  present = \(v) !is.na(v) & nzchar(trimws(coalesce(v, '')))
  for (field in c('ready_to_drink', 'joint_components',
      'rtd_category_candidate'))
    if (!field %in% names(x)) x[[field]] = rep(NA, n)
  patterns = conalco_component_patterns()
  types_in = \(text) {
    value = tolower(text)
    for (type in c('beer','wine','fruit_wine','cider')) value = gsub(paste0(
        '(?:ohne|kein[enrms]*)\\s+', patterns[[type]]), '<negated>', value,
      perl = T)
    for (type in c('beer','wine','fruit_wine','cider')) {
      value = gsub(paste0(patterns[[type]],
          '[ -]?(?:aroma|aromen|geschmack|noten?)'), '<flavour>', value,
        perl = T)
      value = gsub(paste0('(?:aroma|aromen|geschmack|noten?) (?:von|nach) ',
          patterns[[type]]), '<flavour>', value,
        perl = T)
    }
    fruit = grepl(patterns$fruit_wine, value, perl = T)
    value = gsub(patterns$fruit_wine, '<fruit>', value, perl = T)
    cider = grepl(patterns$cider, value, perl = T)
    value = gsub(patterns$cider, '<cider>', value, perl = T)
    found = c(if (grepl(patterns$beer, value, perl = T)) 'beer',
      if (grepl(patterns$wine, value, perl = T)) 'wine', if (
        fruit) 'fruit_wine',
      if (cider) 'cider')
    # A named apple-wine subtype of generic fruit wine is not proof of two
    # bases.
    if (all(c('fruit_wine', 'cider') %in% found) && grepl(
        'darunter|zum beispiel|z\\.b\\.', value))
    found = setdiff(found, 'fruit_wine')
    found
  }
  component_assertion = paste0(
    "hergestellt|produziert|verfeinert|angereichert|",
    "versetzt|enthält|enthalten|besteht aus|",
    "mischung aus|kombination aus|vereint|vereinen|",
    "verbindet|kombiniert|kombinier[te]+|basiert|auf ", "basis|",
    "basis bild[etn]+|grundlage bild[etn]+|stammt|",
    "ausschließlich vergoren|ausschliesslich ",
    "vergoren|genau das steckt auch hier drin|",
    "überträgt genau diese (?:kombination|rezeptur)|",
    "setzt auf die kombination")
  serving_statement = paste0(
    "(?:mischen|mixen|kombinieren|fügen|geben|gießen|",
    "füllen|verleihen) sie|",
    "zur zubereitung|zubereitung (?:eines|von|des)|",
    "zum mixen|als basis für|basis für (?:cocktails|", "longdrinks)|",
    "(?:lässt|lassen) sich[^.?!]{0,100}(?:kombinier|", "gemisch|mixen)|",
    "von klassischen drinks|möglichkeiten für ",
    "experimente|\\b(?:serviervarianten|foodpairing|", "cocktailideen)\\b|",
    "(?<![\\p{L}])(?:kann|können|",
    "könnte)(?![\\p{L}])[^.?!]{0,100}(?:gemischt|",
    "mischen|mixen|kombiniert)|",
    "(?:spritz|cocktail)[^.?!]{0,40}(?:benötigt man|",
    "benötigen sie)|[0-9]+ (?:teile|ml) prosecco")
  rows = lapply(seq_len(n), \(i) {
      sections = conalco_evidence_sections(coalesce(x$archived_description_raw[
            i], x$description_raw[i], ''))
      production = sections$production
      ing = coalesce(sections$ingredients, x$ingredients_raw[i], '')
      if (grepl('zutatenverzeichnis.*nicht erforderlich|keine angaben', ing,
          ignore.case = T)) ing = ''
      p = tolower(production)
      designation = tolower(coalesce(x$designation_raw[i], ''))
      basis = coalesce(x$alcohol_basis_evidence[i], '')
      name = tolower(coalesce(x$product_name[i], ''))
      parts = trimws(strsplit(production,
          '(?<=[.!?])\\s+|(?<=[.!?])(?=[[:upper:]])', perl = T)[[1]])
      recipe = grepl(serving_statement, parts, ignore.case = T, perl = T)
      traditional = grepl(paste0("während|anders als|im gegensatz|",
          "traditionelle[nr]? (?:cocktail|rezept)|der ",
          "klassische|ursprünglich|ein klassiker|bekannt ",
          "für (?:seine|ihre) (?:erstklassigen|",
          "hochwertigen|traditionellen)|hat sich[^.?!]{0,",
          "60}symbol|beschreibt|leitet sich|inbegriff|seit ", "den [0-9]+er"),
        parts, ignore.case = T, perl = T)
      explicit_product_bridge = grepl(paste0(
          "genau das steckt auch hier drin|überträgt genau ",
          "diese (?:kombination|rezeptur)"),
        parts, ignore.case = T, perl = T)
      traditional = traditional & !explicit_product_bridge
      generic_context = F
      product_label = sub('\\s+-\\s+.*', '', name)
      for (j in seq_along(parts)) {
        if (explicit_product_bridge[j] || (nzchar(product_label) &&
              grepl(product_label, tolower(parts[j]), fixed =
                T))) generic_context = F
        if (traditional[j]) generic_context = T
        if (generic_context && grepl(paste0(
              "^(?:dieser|dieses|der|das|er)[^.?!]{0,",
              "35}(?:cocktail|spritz|drink)"),
            parts[j], ignore.case = T, perl = T)) traditional[j] = T
      }
      taste_only = grepl(paste0("geschmacklich|geschmacksprofil|aroma:|",
          "geschmack:|organoleptisch"), parts, ignore.case = T)
      raw_distillation_material = grepl(
        'destillier|destillation|branntwein|grappa|brandy|cognac', parts,
        ignore.case = T) &
        grepl('aus[^.?!]{0,60}(?:wein|trauben|schalen)', parts, ignore.case = T,
        perl = T) &
        !grepl(paste0("mischung|kombination|vereint|kombiniert|",
          "gemischt|versetzt mit|verfeinert mit"), parts, ignore.case = T)
      claims = parts[!recipe & !traditional & !taste_only &
          !raw_distillation_material &
          grepl(component_assertion, parts, ignore.case = T, perl = T)]
      claim_text = tolower(paste(claims, collapse = ' '))
      # Remove the product label so Tequila Sunrise does not become an
      # ingredient.
      product_label = sub('\\s+-\\s+.*', '', name)
      checked = tolower(claims)
      if (nzchar(product_label)) checked = gsub(product_label, '<product>',
        checked, fixed = T)
      checked = gsub(paste0(
          '(?:inspiriert von|hommage an|erinnert an)[^.?!]{0,70}',
          patterns$spirit),
        '<inspiration>', checked, perl = T)
      flavour = paste0(patterns$spirit,
        "[ -]?(?:aroma|aromen|geschmack|noten?)|(?:aroma|",
        "aromen|geschmack|noten?)[^.?!]{0,25}", patterns$spirit)
      spirit_claims = claims[grepl(patterns$spirit, checked, perl = T) & !grepl(
          flavour, checked, perl = T) &
          !grepl(paste0('(?:ohne|kein[enrms]*|nicht mit)\\s+', patterns$spirit),
          checked, perl = T)]
      ingredient_components = gsub(flavour, '<flavour>', tolower(ing), perl = T)
      spirit_ing = nzchar(ing) && grepl(paste0(patterns$spirit,
          '|(?<![\\p{L}])alkohol(?![\\p{L}])'),
        ingredient_components, perl = T)
      spirit_basis = nzchar(basis) && grepl(patterns$spirit, tolower(basis),
        perl = T)
      spirit_designation = grepl(paste0(
          "^spirituosenmischgetränk|^alkoholhaltiges ",
          "mischgetränk mit (wodka|vodka|rum|gin|whisk|", "tequila)"),
        designation) ||
        (grepl('(?<![\\p{L}])(?:aus|mit)(?![\\p{L}])', designation, perl = T) &&
          grepl(patterns$spirit, designation, perl = T))
      spirit = spirit_ing || spirit_basis || spirit_designation || length(
        spirit_claims) > 0
      ing_types = types_in(ing)
      description_types = types_in(paste(claims, collapse = ' '))
      designation_types = types_in(designation)
      # An actual ingredient list outranks a generic Fruchtwein-Cocktail
      # designation.
      quantified_designation = length(designation_types) > 0 && grepl(
        '[0-9]+(?:[,.][0-9]+)?\\s*%', designation,
        perl = T)
      types = if (length(ing_types)) ing_types else if (
          quantified_designation) designation_types else
          if (length(
              description_types)) description_types else designation_types
      fermented_component = length(types) > 0
      fermented_proof = if (length(ing_types)) ing else if (
          quantified_designation) x$designation_raw[i] else if (length(
              description_types))
            paste(claims[vapply(claims, \(s) length(types_in(s)) > 0, logical(
                    1))], collapse = ' | ') else
            if (length(designation_types)) x$designation_raw[i] else ''
      fermented_source = if (length(ing_types)) 'ingredients_list' else if (
          quantified_designation) 'designation_quantified_composition' else if (
            length(description_types))
            'product_retailer_description' else if (length(
                designation_types)) 'designation_limited_to_component' else
              'none'
      complete = nzchar(ing) && !grepl(
        'unter anderem|u\\.a\\.|auszug|zum beispiel|unvollständig', ing,
        ignore.case = T)
      unresolved_added_claims = claims[grepl(paste0(
            '(?:weinmischgetränk\\s+aus|basis bildet)\\s+',
            "[^,.!?]{0,50}(?<![\\p{L}])(?:apéritif|aperitif|",
            "aperitivo)(?![\\p{L}-])"), tolower(claims), perl = T)]
      ambiguous = (grepl(
          'destillat|aperitif|apéritif|aperitivo|alkohol|spirituose', tolower(
            ing), perl = T) &&
          !spirit_ing) || (!spirit && length(unresolved_added_claims) > 0)
      exclusive_claims = claims[grepl(paste0(
            "alkohol[^.?!]{0,70}(?:stammt|entsteht|", "besteht)[^.?!]{0,70}",
            "(?:ausschließlich|ausschliesslich|nur)|",
            "ausschließlich vergoren|ausschliesslich ", "vergoren|",
            "keine (?:weiteren|zusätzlichen) ",
            "(?:alkoholkomponenten|spirituosen)"), tolower(claims), perl = T)]
      exclusive_claims = exclusive_claims[vapply(exclusive_claims, \(s) length(
            types_in(s)) > 0, logical(1))]
      percentages = regmatches(designation, gregexpr(
          '[0-9]+(?:[,.][0-9]+)?\\s*%', designation, perl = T))[[1]]
      percentages = suppressWarnings(as.numeric(gsub(',', '.', gsub('[ %]', '',
              percentages), fixed = T)))
      complete_formula = grepl('(?<![\\p{L}])aus(?![\\p{L}])', designation,
        perl =
          T) &&
        length(percentages) >= 2 && all(is.finite(percentages)) && abs(sum(
          percentages)-100) < 1e-8
      conflict = (complete && fermented_component && !spirit_ing &&
          !ambiguous &&
          (spirit_basis || spirit_designation || length(spirit_claims) > 0)) ||
        (complete && fermented_component && length(unresolved_added_claims) >
          0 &&
          !spirit) ||
        (length(exclusive_claims) > 0 && spirit)
      exclusive = fermented_component && (complete || complete_formula ||
          length(
          exclusive_claims) > 0) &&
        !spirit && !ambiguous && !conflict
      exclusive_proof = if (!exclusive) '' else if (complete) paste0(
            'Declared ingredients list: ', ing) else
          if (complete_formula) paste0('Declared 100-percent composition: ',
              x$designation_raw[i]) else
            paste(exclusive_claims, collapse = ' | ')
      spirit_proof = if (spirit_ing) ing else if (spirit_basis) basis else if (
            spirit_designation)
            x$designation_raw[i] else paste(spirit_claims, collapse = ' | ')
      spirit_source = if (spirit_ing) 'ingredients_list' else if (
          spirit_basis) 'structured_alcohol_basis_evidence' else
          if (spirit_designation) 'designation' else if (length(
                spirit_claims)) 'product_retailer_description' else 'none'
      uncertain_added = claims[grepl(paste0(
            "(?:mit|aus|und|enthält|verfeinert)[^.?!]{0,",
            "70}[\\p{L}-]*destillat"), tolower(claims), perl = T)]
      positive_hint = spirit || length(uncertain_added) > 0
      hint_proof = if (spirit) spirit_proof else paste(uncertain_added,
          collapse =
            ' | ')
      other = fermented_component || grepl(patterns$soft, tolower(paste(ing,
            basis)), perl = T) ||
        (length(spirit_claims) > 0 && grepl(patterns$soft, claim_text, perl =
            T))
      ready_pattern = paste0("trinkfertig|servierfertig|fertig[ -]*(gemischt|",
        "gemixt|abgemischt)|ready[ -]to[ -](drink|serve|",
        "pour)|fertigcocktail|ohne mix(?:en|aufwand)|", "kein mixen")
      ready_parts = parts[!recipe & grepl(ready_pattern, parts, ignore.case = T,
          perl = T)]
      supplied_ready = x$ready_to_drink[i] %in% T && present(
        x$ready_to_drink_evidence[i])
      ready = supplied_ready || length(ready_parts) > 0 || grepl(ready_pattern,
        designation, perl = T)
      ready_proof = if (supplied_ready) x$ready_to_drink_evidence[i] else paste(
          ready_parts, collapse = ' | ')
      ordinary = grepl(paste0("^(?:[\\p{L}]*(?:likör|likoer)|liqueur|",
          "spirituose|gin|rum|wodka|vodka|whisk|",
          "getreidespirituose)(?![\\p{L}])"), designation, perl = T) &&
        !grepl('ready[ -]to[ -]drink|fertigcocktail|premix', name, perl = T) &&
        !spirit_designation
      cocktail_ingredient = grepl(paste0("cocktail[ -]?aroma|basis für|",
          "(?<![\\p{L}])(?:konzentrat|sirup)(?![\\p{L}])"), designation,
        perl = T) ||
        grepl('(?:nur )?wenige[nr]? tropfen|tropfenweise', p, perl = T)
      aperitif_base = grepl('aperitivo|aperitif', name, perl = T) && !ready &&
        !grepl('alkoholfrei|non.alcoholic|not (?:gin|rum|whisk|italian)', paste(
          name,designation), perl = T) &&
        (grepl(serving_statement, production, ignore.case = T, perl = T) ||
          nzchar(
          sections$serving_excluded))
      nonready = (ordinary && !ready) || cocktail_ingredient || aperitif_base ||
        grepl(paste0("(?<![\\p{L}])(?:konzentrat|cocktail[ -]?basis|",
          "sirup|zum (?:mixen|mischen|", "verdünnen))(?![\\p{L}])"), paste(name,
          designation), perl = T)
      mixture_label = grepl(paste0("(?<![\\p{L}])(?:cocktails?|longdrinks?|",
          "mischgetränke?|weinmischgetränke?|",
          "biermischgetränke?|radler|spritz|sprizz|",
          "fertigcocktail|premix)(?![\\p{L}])|ready[ -]to[ ",
          "-](drink|serve)"),
        paste(name, designation), perl = T)
      candidate = (mixture_label || x$rtd_category_candidate[i] %in% T || (
          ready &&
            spirit && other) ||
          x$joint_components[i] %in% T) && (!ordinary || ready)
      extract_text = \(pattern) {
        match = regexpr(pattern, production, perl = T, ignore.case = T)
        if (match[1] < 0) '' else clean_text(regmatches(production, match))
      }
      sealed = if (grepl('nicht[^.?!]{0,30}verschlossen', p)) '' else
        extract_text(paste0("in[^.?!]{0,70}verschlossenen[^.?!]{0,",
            "70}(?:abgefüllt|behältnissen|flaschen|dosen)"))
      tax = if (grepl(paste0("nicht[^.?!]{0,30}(?:alkoholsteuerpflichtig|der ",
            "alkoholsteuer)|unterliegt[^.?!]{0,30}nicht"), p)) '' else
        extract_text(paste0("(?:fertiggetränk|fertigprodukt|dieses getränk|",
            "das getränk|das produkt)[^.?!]{0,",
            "100}(?:unterliegt der alkoholsteuer|", "alkoholsteuerpflichtig)"))
      list(product_description_evidence = production,
        ingredients_product_evidence = if (nzchar(ing)) ing else NA_character_,
        ingredient_list_complete_declared = complete,
        ingredient_list_source = if (present(
            sections$ingredients)) 'labelled_archived_facts' else if (nzchar(
              ing)) 'structured_ingredients_raw' else 'none',
        actual_composition_statements = paste(claims, collapse = ' | '),
        taste_aroma_text = paste(parts[taste_only | grepl(
              'aroma|aromen|geschmack|note', parts, ignore.case = T)],
          collapse = ' | '),
        serving_recipe_text_excluded = paste(sections$serving_excluded, paste(
            parts[recipe | traditional],
            collapse = ' | ')),
        spirit_component_documented = spirit, spirit_component_evidence =
        spirit_proof,
        spirit_evidence_source = spirit_source,
        other_component_documented = other, fermented_component_documented =
        fermented_component,
        fermented_component_evidence = fermented_proof,
        fermented_component_source = fermented_source,
        fermented_components = if (length(types)) paste(types, collapse =
              ';') else 'unknown',
        fermented_only_documented = exclusive, exclusive_fermentation_evidence =
        exclusive_proof,
        exclusive_fermentation_rule = if (exclusive &&
            complete) 'E01_complete_declared_ingredients' else if (exclusive &&
              complete_formula)
            'E03_declared_exhaustive_100_percent_composition' else
            if (
              exclusive) 'E02_explicit_exclusive_final_alcohol_origin' else
              'E04_exclusivity_not_evidenced',
        alcohol_basis_status = if (fermented_component &&
            spirit) 'fermented_plus_suitable_alcohol_component' else if (
            exclusive)
            'exclusively_fermented_documented' else if (
              fermented_component)
              'fermented_component_exclusivity_unknown' else if (spirit)
                'suitable_alcohol_component_documented' else 'unknown',
        fermented_base_type = if (length(types) > 1) 'mixed_fermented' else if (
            length(types)) types[1] else 'unknown',
        alcohol_basis_conflict = conflict, ready_mixed_documented = ready,
        ready_mixed_evidence = ready_proof,
        sealed_retail_text_evidence = sealed,
        finished_product_tax_text_evidence =
        tax,
        ordinary_spirit_designation = ordinary, not_ready_documented = nonready,
        aperitif_base_documented = aperitif_base, rtd_candidate_documented =
        candidate,
        suitable_alcohol_component_positive_hint = positive_hint,
        suitable_alcohol_component_hint_evidence = hint_proof,
        composition_evidence = paste(unique(c(if (nzchar(
                  spirit_proof)) spirit_proof,
              if (nzchar(fermented_proof)) fermented_proof,
              if (exclusive) exclusive_proof)), collapse = ' | '),
        composition_evidence_fields = paste(unique(c(spirit_source,
              fermented_source)[c(spirit_source,
                fermented_source) != 'none']), collapse = ';'))
  })
  logical_fields = c('ingredient_list_complete_declared',
    'spirit_component_documented', 'other_component_documented',
    'fermented_component_documented', 'fermented_only_documented',
    'alcohol_basis_conflict', 'ready_mixed_documented',
    'ordinary_spirit_designation', 'not_ready_documented',
    'aperitif_base_documented', 'rtd_candidate_documented',
    'suitable_alcohol_component_positive_hint')
  names = c('product_description_evidence','ingredients_product_evidence',
    'ingredient_list_complete_declared',
    'ingredient_list_source','actual_composition_statements',
    'taste_aroma_text','serving_recipe_text_excluded',
    'spirit_component_documented','spirit_component_evidence',
    'spirit_evidence_source','other_component_documented',
    'fermented_component_documented',
    'fermented_component_evidence',
    'fermented_component_source','fermented_components',
    'fermented_only_documented','exclusive_fermentation_evidence',
    'exclusive_fermentation_rule','alcohol_basis_status','fermented_base_type',
    'alcohol_basis_conflict',
    'ready_mixed_documented','ready_mixed_evidence',
    'sealed_retail_text_evidence','finished_product_tax_text_evidence',
    'ordinary_spirit_designation','not_ready_documented',
    'aperitif_base_documented','rtd_candidate_documented',
    'suitable_alcohol_component_positive_hint',
    'suitable_alcohol_component_hint_evidence','composition_evidence',
    'composition_evidence_fields')
  as_tibble(setNames(lapply(names, \(field) vapply(rows, `[[`,
          if (field %in% logical_fields) logical(1) else character(1), field)),
      names))
}

# Origin: price_analysis.R definition: PRICE_ANALYSIS_VERSION
PRICE_ANALYSIS_VERSION = 'price-analysis-0.4.1'

# Origin: price_analysis.R definition: ANALYSIS_CATEGORIES
ANALYSIS_CATEGORIES = c('Bier', 'Wein', 'Schaumwein', 'Spirituosen',
  'Zwischenerzeugnisse ≤15 % vol', 'Zwischenerzeugnisse >15 % vol', 'Alkopops')

# Origin: price_analysis.R definition: ETHANOL_DENSITY_G_ML
ETHANOL_DENSITY_G_ML = 0.789

# Origin: price_analysis.R definition: assign_price_categories
assign_price_categories = \(products, evidence_retailers = 'conalco') {
  x = classify_alkopops(products, evidence_retailers)
  if (!'archived_description_raw' %in% names(x)) x$archived_description_raw =
    NA_character_
  name = tolower(coalesce(x$product_name, ''))
  designation = tolower(coalesce(x$designation_raw, ''))
  description = coalesce(x$archived_description_raw, '')
  if (length(description) == 0) description = rep('', nrow(x))
  # Serving suggestions must not establish the composition of the sold product.
  production = sub(paste0("(?i)(rezept(vorschlag|idee)\\b|rezept\\s*:|",
      "servier(vorschlag|empfehlung)|", "cocktail-inspiration).*"),
    '', description, perl = T)
  fortified_pattern = paste0(
    '(?i)[^.?!]*(aufgesprit|fortifiz|gespritet|',
    'weine werden mit[^.?!]*fruchtlikören|',
    'zugabe von[^.?!]*(branntwein|weingeist)|',
    "mit[^.?!]*(alkohol|weingeist)[^.?!]*(angereicher", "t|versetzt))[^.?!]*"
  )
  # Preserve row positions when no fortification evidence is present.
  fortification = vapply(production, \(text) {
      hint = paste0("aufgesprit|fortifiz|gespritet|fruchtlikören|",
        "branntwein|weingeist|angereichert|versetzt")
      if (!grepl(hint, text, ignore.case = T)) return('')
      # The original pattern cannot cross sentence punctuation; test only those
      # spans.
      spans = regmatches(text, gregexpr('[^.?!]+', text, perl = T))[[1]]
      spans = spans[grepl(hint, spans, ignore.case = T)]
      for (span in spans) {
        m = regexpr(fortified_pattern, span, perl = T)
        if (m[1] >= 0) return(clean_text(regmatches(span, m)))
      }
      ''
    }, character(1))
  designated_fortified = grepl('^likörwein\\b|^sherry\\b', designation,
    perl = T)
  wine_aperitif = x$beverage_group == 'fortified_wine_aperitif' |
    grepl(paste0("^aromatisierte[rs] wein|^aromatisiertes ",
      "weinhaltiges getränk"), designation)
  intermediate = (designated_fortified | (wine_aperitif & nzchar(
        fortification))) &
    !is.na(x$alcohol_content) & x$alcohol_content > 1.2 &
    x$alcohol_content <= 22

  alkopop = x$alkopop_status == 'confirmed'
  nolo = x$no_lo_status %in% no_lo_levels | (!is.na(x$alcohol_content) &
      x$alcohol_content <= 1.2)
  x$analysis_category = case_when(
    x$scope_status != 'included' | nolo ~ NA_character_,
    alkopop ~ 'Alkopops',
    intermediate & x$alcohol_content <= 15 ~ ANALYSIS_CATEGORIES[5],
    intermediate & x$alcohol_content > 15 ~ ANALYSIS_CATEGORIES[6],
    x$beverage_group == 'beer' ~ 'Bier',
    x$beverage_group == 'still_wine' ~ 'Wein',
    x$beverage_group == 'sparkling_wine' ~ 'Schaumwein',
    x$beverage_group == 'spirits' ~ 'Spirituosen',
    T ~ NA_character_
  )
  x$analysis_category_rule = case_when(
    x$scope_status != 'included' ~ x$scope_rule,
    nolo ~ 'T00_nolo_separate',
    alkopop ~ 'T01_spirit_soft_mixture_packaged_abv_gt1.2_lt10',
    intermediate & designated_fortified ~
      'T02_designated_fortified_wine_abv_gt1.2_le22',
    intermediate ~ paste0("T03_wine_aperitif_documented_fortification_abv_g",
      "t1.2_le22"),
    x$beverage_group %in% c('beer', 'still_wine', 'sparkling_wine', 'spirits') ~
      'T04_descriptive_beverage_family',
    x$beverage_group %in% c('cider', 'rice_wine', 'other_wine') ~
      'T05_other_fermented_separate',
    T ~ 'T06_tax_classification_unresolved'
  )
  x$analysis_category_evidence = case_when(
    alkopop ~ x$alkopop_evidence,
    intermediate & designated_fortified ~ x$designation_raw,
    intermediate ~ fortification,
    T ~ x$classification_evidence
  )
  x$tax_assessment_status = case_when(
    intermediate | alkopop ~ paste0(
      "product_evidence_supported_not_binding_customs_d", "etermination"),
    x$analysis_category_rule == 'T04_descriptive_beverage_family' ~
      'descriptive_family_not_automatic_tax_determination',
    T ~ 'not_assigned'
  )
  # Preserve the historical tax_group placeholder: no customs classification is
  # invented.
  x$tax_review_reason = case_when(
    x$beverage_group == 'fortified_fruit_must' ~
      paste0("Pommeau: fruit must plus distillate; CN 2206 ",
      "versus 2208 and fermentation evidence unresolved"),
    wine_aperitif & !intermediate & !nolo ~
      paste0("Wine aperitif/vermouth alone does not establish ",
      "added alcohol, CN code or exclusion from wine"),
    x$beverage_group == 'semi_sparkling_wine' & !nolo ~
      paste0("Perlwein: pressure/closure and fermentation ",
      "must be checked before a tax assignment"),
    x$beverage_group == 'ready_to_drink_other' & !alkopop & !nolo ~
      paste0("Mixture: spirit origin/composition or statutory ",
      "ABV >1.2 and <10 not established"),
    x$beverage_group == 'spirits' & !is.na(x$alcohol_content) &
      x$alcohol_content < 15 & !nolo &
      !grepl('eierlikör', designation) ~
      paste0("Low-strength descriptive spirits family: legal ",
      "spirit-drink status/tax basis unverified"),
    x$beverage_group == 'still_wine' & x$alcohol_content > 15 ~
      paste0("Wine above 15: exclusive fermentation and ",
      "non-enrichment require confirmation"),
    T ~ NA_character_
  )
  # Ambiguous low-strength generic aperitifs must not enter the spirits result.
  unclear_spirit = !is.na(x$tax_review_reason) & x$beverage_group == 'spirits' &
    !grepl('^(likör|spirituose)', designation)
  x$analysis_category[unclear_spirit] = NA_character_
  x$analysis_category_rule[unclear_spirit] = 'T06_tax_classification_unresolved'
  explicit_rtd = grepl(paste0(
      "mischgetränk|cocktail|longdrink|ready.to.(drink|", "serve)|spritz"),
    paste(name, designation), perl = T) | x$beverage_group ==
    'ready_to_drink_other' |
    x$joint_components %in% T
  unconfirmed_rtd = (explicit_rtd | x$alkopop_status == 'unclear') & !alkopop &
    !x$scope_status %in% 'excluded_nonfamily_product'
  conalco = if ('retailer' %in% names(x)) is.na(x$retailer) | x$retailer %in%
      evidence_retailers else
    rep(T, nrow(x))
  fermented_regular = x$fermented_only_documented & x$beverage_group %in% c(
    'beer', 'cider')
  regular_spirit_rtd = conalco & x$ordinary_spirit_designation &
    x$ready_mixed_documented &
    !is.na(x$alcohol_content) & x$alcohol_content >= 15
  regular_family = fermented_regular | regular_spirit_rtd
  unconfirmed_rtd[conalco] = (x$rtd_candidate_documented[conalco] &
      !x$not_ready_documented[conalco] & !regular_family[conalco]) |
    x$alkopop_research_status[conalco] %in% c('documented_spirit_rtd',
    'possible_alkopop', 'general_rtd_unknown_basis')
  unconfirmed_rtd = unconfirmed_rtd & !alkopop & !x$scope_status %in%
    'excluded_nonfamily_product'
  x$analysis_category[unconfirmed_rtd] = NA_character_
  x$analysis_category_rule[unconfirmed_rtd] = 'T07_rtd_status_separate'
  family_main = recode(x$beverage_group, beer = 'Bier', still_wine = 'Wein',
    sparkling_wine = 'Schaumwein', spirits = 'Spirituosen', .default =
      x$beverage_group)
  x$main_category = case_when(
    x$scope_status == 'excluded_nonfamily_product' ~ family_main,
    alkopop ~ 'Alkopops',
    x$alkopop_status == 'unclear' ~ 'Unklare RTD',
    unconfirmed_rtd ~ 'Sonstige RTD',
    intermediate ~ 'Zwischenerzeugnisse',
    T ~ family_main
  )
  x$main_category[conalco & x$alkopop_research_status ==
      'documented_spirit_rtd'] = 'Spirituosenhaltige RTD'
  x$main_category[conalco & x$alkopop_research_status == 'possible_alkopop'] =
    'Unklare RTD'
  x$main_category[conalco & x$alkopop_research_status ==
      'general_rtd_unknown_basis'] = 'RTD mit unbekannter Alkoholbasis'
  restore_family = conalco & (!unconfirmed_rtd | regular_family) & !alkopop &
    !intermediate
  x$main_category[restore_family] = family_main[restore_family]
  unresolved_aperitif = conalco & x$aperitif_base_documented &
    x$beverage_group == 'spirits' &
    !intermediate & !x$spirit_component_documented &
    !x$ordinary_spirit_designation
  x$analysis_category[unresolved_aperitif] = NA_character_
  x$analysis_category_rule[unresolved_aperitif] =
    'T06_aperitif_base_alcohol_origin_unresolved'
  x$main_category[unresolved_aperitif] =
    'Aperitif-/Cocktailbasis (Alkoholart ungeklärt)'
  x$analysis_category_evidence[unresolved_aperitif] =
    x$product_description_evidence[unresolved_aperitif]
  x$tax_assessment_status[unresolved_aperitif] = 'not_assigned'
  x$tax_review_reason[unresolved_aperitif] =
    paste0("Aperitif/cocktail base identified; suitable ",
    "spirit origin is not documented for this product")
  x$main_category_rule = if_else(unconfirmed_rtd | alkopop,
    x$alkopop_rule, x$analysis_category_rule)
  x$main_category_reason = if_else(unconfirmed_rtd | alkopop,
    x$alkopop_reason, coalesce(x$tax_review_reason, x$classification_evidence))
  x$analysis_version = ifelse(conalco, PRICE_ANALYSIS_VERSION,
    'price-analysis-0.2.0')
  x
}

# Origin: snapshot_analysis.R definition: SNAPSHOT_CLASSIFICATION_VERSION
SNAPSHOT_CLASSIFICATION_VERSION = 'beverage-0.9.1'

# Origin: snapshot_analysis.R definition: SNAPSHOT_ANALYSIS_VERSION
SNAPSHOT_ANALYSIS_VERSION = 'price-analysis-0.5.0'

# Origin: snapshot_analysis.R definition: snapshot_product_style
snapshot_product_style = \(x) {
  label = tolower(paste(coalesce(x$product_name, ''),
      coalesce(x$designation_raw, '')))
  production = if ('product_description_evidence' %in% names(x))
    tolower(coalesce(x$product_description_evidence, '')) else rep('', nrow(x))
  introduction = substr(production, 1, 600)
  cocktail_pattern = paste0(
    "cocktail|longdrink|mojito|cuba[ -]?libre|sex on ",
    "the beach|blue lagoon|",
    "pi[nñ]a colada|caipirinha|margarita|negroni|", "daiquiri|cosmopolitan|",
    "old fashioned|whisk[e]?y sour|moscow mule|mai ", "tai|gin.{0,8}tonic|",
    "rum.{0,8}cola|paloma|bellini|spritz|sprizz|hugo|",
    "caipiroska|espresso martini|",
    "pornstar martini|long island|campari soda|",
    "tequila sunrise|appletini|apple martini|",
    "mimosa|amaretto sour|gin sour|lemon drop|",
    "\\bcosmo\\b|lynchburg lemonade|limao fizz|", "skinny bitch|",
    "lillet.{0,20}berry|limon.{0,8}lemonade|apple ",
    "soda|wild berry|\\bbatida\\b")
  named_cocktail = grepl(cocktail_pattern, label, perl = T)
  longdrink_mix = grepl(paste0(
      '(?:whisk[e]?y|vodka|wodka|korn|rum|bourbon).{0,100}',
      "(?:cola|lemonade|lemon|soda|ginger|energy|mate|",
      "maracuja|ice[ -]?tea|pineapple|raspberry|",
      "grapefruit|mango|ananas|passion)"),
    paste(label, introduction), perl = T)
  marketed_cocktail = named_cocktail | longdrink_mix |
    (x$in_rtd_rts_category %in% T & grepl('cocktail|longdrink', introduction)) |
    grepl(paste0("(?:premix|fertig|trinkfertig|",
      "servierfertig)[^.?!]{0,30}(?:cocktail|", "longdrink)|",
      "(?:cocktail|longdrink)[^.?!]{0,40}(?:premix|",
      "fertig|trinkfertig|servierfertig)"),
    introduction, perl = T)
  ordinary_radler = grepl('radler', label, fixed = T)
  ordinary_schorle = grepl('weinschorle|weinschörle|wein.?schorle', label,
    perl = T)
  ordinary_cider = x$beverage_group == 'cider' & !named_cocktail
  own_ready = grepl(paste0(
      'trinkfertig|servierfertig|fertig[ -]*(gemischt|gemixt)|',
      "ready[ -]to[ -](drink|serve)|premix[ -]cocktail|",
      "vorgemischte[rn]? cocktail|fertigcocktail"),
    paste(label, introduction), perl = T)
  designation_base = grepl(
    'basis für|basis mit|cocktail[ -]?basis|konzentrat|sirup',
    tolower(coalesce(x$designation_raw, '')), perl = T)
  liqueur_base = named_cocktail & grepl('^(?:likör|liqueur)(?![\\p{L}])',
    tolower(coalesce(x$designation_raw, '')), perl = T) &
    !own_ready & grepl('basis für cocktails|basis für|zutat', production,
    perl = T)
  # Marketing style does not establish spirit composition or any tax criterion.
  ready_style = (own_ready | x$ready_mixed_documented |
      x$in_rtd_rts_category %in% T) & !liqueur_base &
    (!x$not_ready_documented | (own_ready & !designation_base))
  x$product_style = case_when(
    x$scope_status == 'excluded_nonfamily_product' ~
      'nonbeverage_or_ingredient',
    ordinary_radler ~ 'beer_mixture',
    ordinary_schorle ~ 'wine_spritzer',
    ordinary_cider ~ 'ordinary_cider',
    own_ready & designation_base & (x$in_rtd_rts_category %in% T |
        named_cocktail) ~ 'unclear_ready_form',
    liqueur_base ~ 'cocktail_flavoured_liqueur_or_base',
    marketed_cocktail & ready_style ~ 'cocktail_rtd',
    x$rtd_candidate_documented & ready_style ~ 'other_rtd',
    x$in_rtd_rts_category %in% T & !x$not_ready_documented ~ 'other_rtd',
    x$in_rtd_rts_category %in% T & x$not_ready_documented ~
      'nonready_cocktail_base_or_spirit',
    T ~ 'other_beverage'
  )
  x$product_style_rule = paste0(
    "S01_product_label_own_introduction_and_ready_for",
    "m_independent_of_alcohol_origin")
  x$product_style_ready_evidence = ifelse(own_ready, introduction,
    ifelse(x$in_rtd_rts_category %in% T,
      'Explicit RTD/RTS category; no documented required mixing', ''))
  x$product_style_evidence = paste(x$product_name, x$designation_raw,
    ifelse(x$in_rtd_rts_category %in% T,
      'Explicit RTD/RTS category membership', ''),
    x$product_style_ready_evidence, sep = ' | ')
  x$rtd_abv_band = case_when(
    is.na(x$alcohol_content) ~ 'unknown_exact_abv',
    x$alcohol_content <= 1.2 ~ 'le_1.2',
    x$alcohol_content < 10 ~ 'gt_1.2_lt_10',
    x$alcohol_content == 10 ~ 'exactly_10',
    T ~ 'gt_10'
  )
  x
}

# Origin: snapshot_analysis.R definition: snapshot_intermediate_evidence
snapshot_intermediate_evidence = \(x) {
  designation = tolower(coalesce(x$designation_raw, ''))
  aromatised_wine_designation = grepl(
    paste0("^(wermut|vermouth|wein-aperitif|aperitif auf ",
      "weinbasis|aromatisierte[rs]? wein)\\b"),
    designation, perl = T)
  candidate = x$beverage_group == 'fortified_wine_aperitif' &
    aromatised_wine_designation &
    is.na(x$analysis_category) & x$analysis_category_rule ==
    'T06_tax_classification_unresolved' &
    x$scope_status == 'included' & !is.na(x$alcohol_content) &
    x$alcohol_content > 10 & x$alcohol_content <= 22
  # A separate alcohol component in the declared ingredient list of the same
  # offer.
  ingredient_alcohol_pattern = paste0("^(ethyl|wein|neutral)?alkohol( ",
    "landwirtschaftlichen ursprungs)?$|",
    '^(weindestillat|branntwein|weinbrand|brandy|grappa)$')
  ingredient_alcohol = vapply(coalesce(x$ingredients_raw, ''), \(text) {
      components = trimws(strsplit(tolower(text), ',', fixed = T)[[1]])
      hits = components[grepl(ingredient_alcohol_pattern, components, perl = T)]
      if (length(hits)) paste('Zutaten:', text) else ''
    }, character(1), USE.NAMES = F)
  # Own production statements before any recipe or serving suggestion.
  description = coalesce(x$archived_description_raw, '')
  production = sub(paste0("(?i)(rezept(vorschlag|idee)\\b|rezept\\s*:|",
      "servier(vorschlag|empfehlung)).*"),
    '', description, perl = T)
  spirit_dash = paste0("schuss [^.!?]{0,40}(obstbrand|weinbrand|brandy|",
    "grappa|gin)\\b")
  wine_spirit_blend = paste0("komposition aus [^.!?]{0,30}wein[^.!?]{0,",
    "20}\\b(gin|weinbrand|brandy|grappa|destillat)")
  # The verb must take the base as its direct object, not a historical aside.
  liqueur_wine_base = 'verwendet\\s+(\\p{L}+\\s+){0,2}(mistelle|likörwein)'
  added_alcohol_pattern = paste0('(?i)[^.!?]*(', spirit_dash, '|',
    wine_spirit_blend, '|',
    liqueur_wine_base, ')[^.!?]*')
  statement = regmatches(production, regexpr(added_alcohol_pattern, production,
      perl = T))
  description_alcohol = rep('', nrow(x))
  description_alcohol[grepl(added_alcohol_pattern, production, perl = T)] =
    clean_text(statement)
  documented = candidate & (nzchar(ingredient_alcohol) | nzchar(
      description_alcohol))
  # Above 18 % vol a CN 2204/2205 product cannot be still wine; 22 % vol is
  # excluded by 251/2014.
  above_still_wine = candidate & !documented & x$alcohol_content > 18 &
    x$alcohol_content < 22
  assigned = documented | above_still_wine
  x$analysis_category[assigned] = ifelse(x$alcohol_content[assigned] <= 15,
    ANALYSIS_CATEGORIES[5], ANALYSIS_CATEGORIES[6])
  x$analysis_category_rule[documented] = paste0(
    "T08_aromatised_wine_documented_added_alcohol_abv", "_gt10_le22")
  x$analysis_category_rule[above_still_wine] =
    'T09_aromatised_wine_abv_gt18_lt22_not_still_wine'
  x$analysis_category_evidence[documented] = ifelse(nzchar(ingredient_alcohol[
        documented]),
    ingredient_alcohol[documented], description_alcohol[documented])
  x$analysis_category_evidence[above_still_wine] = paste0(
    'Verkehrsbezeichnung: ',
    x$designation_raw[above_still_wine], '; ', x$alcohol_content[
      above_still_wine], ' % vol > 18 % vol')
  x$main_category[assigned] = 'Zwischenerzeugnisse'
  x$main_category_rule[assigned] = x$analysis_category_rule[assigned]
  x$tax_assessment_status[assigned] = paste0(
    "product_evidence_supported_not_binding_customs_d", "etermination")
  x$tax_review_reason[assigned] = NA_character_
  x
}
