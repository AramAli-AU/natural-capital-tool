# ============================================================
# GREEN SHEEP PROJECT - Natural-capital assessment and reporting
# Full Shiny application with natural-capital scoring guides
# PART 1 of 2
# 
# Copyright © September 2026. All rights reserved.
# Owner & Creator: Dr Aram Ali, Green Sheep Project Manager
# Affiliation: Wheatbelt Natural Resource Management Inc.
# Contact: aali@wheatbeltnrm.org.au | 0470 488 185
#
# LICENCE: Proprietary - All rights reserved
# This software is protected by Australian copyright law.
# Unauthorised copying, reproduction, modification, distribution,
# or commercial use is strictly prohibited without prior written
# permission from Dr Aram Ali.
# ============================================================

required <- c("shiny", "bslib", "DT", "plotly", "writexl", "base64enc")

missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Install missing packages with: install.packages(c(",
       paste(sprintf("'%s'", missing), collapse = ", "), "))")
}

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(plotly)
  library(writexl)
  
  
})

brand <- list(dark = "#2F5D3A", green = "#5E8C57", cream = "#F7F8F4", red = "#A33A45", text = "#26352B")

contact <- list(
  name = "Dr Aram Ali",
  role = "Green Sheep Project Manager",
  email = paste(
    "aali@wheatbeltnrm.org.au,",
    "a.aramali@outlook.com",
    sep = "\n"
  ),
  mobile = "0470 488 185",
  address = "269 Fitzgerald St E, Northam, WA"
)

disclaimer_text <- paste(
  "This tool has been developed ",
  "as a producer-friendly resource to support natural capital, farm management",
  "and sustainability discussions. It has been informed by published journal",
  "articles, industry reports, expert opinion and other trusted reference sources included within",
  "the tool, and has been developed to the best of knowledge at the",
  "time of preparation. The tool provides indicative screening information only",
  "based on user-entered information, simplified assumptions, and proxy indicators",
  "and it does not replace specific professional advice, field verification, GIS analysis,",
  "emissions accounting, financial assessment, regulatory requirements, or formal",
  "natural capital accounting. We make no guarantee that the outputs",
  "are complete, error-free, or suitable for every farming circumstance, and",
  "accepts no liability for decisions made or actions taken based on use of the tool."
)

safe <- function(x) {
  if (is.null(x) || !length(x) || is.na(x) || !nzchar(trimws(as.character(x)))) return("Farm self-assessment")
  trimws(as.character(x))
}

clamp <- function(x) {
  x <- as.numeric(x)
  x[!is.finite(x)] <- NA_real_
  pmax(0, pmin(1, x))
}

band <- function(x) {
  x <- as.numeric(x)
  out <- rep(NA_character_, length(x))
  out[is.finite(x) & x < 20] <- "Very low"
  out[is.finite(x) & x >= 20 & x < 40] <- "Low"
  out[is.finite(x) & x >= 40 & x < 60] <- "Moderate"
  out[is.finite(x) & x >= 60 & x < 80] <- "High"
  out[is.finite(x) & x >= 80] <- "Very high"
  out
}

safe_mean <- function(x) {
  x <- as.numeric(x)
  if (!length(x) || all(!is.finite(x))) return(0)
  mean(x[is.finite(x)])
}

# 27 References
refs <- data.frame(
  Title = c(
    "SEEA Ecosystem Accounting",
    "Natural Capital Methods Paper (2024)",
    "Natural capital enhances farm production, profitability and financial resilience",
    "Natural capital and sustainable agriculture - NRM Regions Australia",
    "Farm-scale Natural Capital Accounting: An Overview",
    "Metrics for Nature Positive Farming - Australian Wool Innovation",
    "Nature Positive Farming Framework | Woolmark+",
    "Nature Positive Farming - Beyond the Bale",
    "Understanding the value of natural capital on farm | Sheep Sustainability",
    "Farm-scale Natural Capital Accounting: Unlocking the potential of natural capital",
    "Natural Capital Data Collection Methods (2024)",
    "Lamb Planner | Department of Primary Industries and Regional Development",
    "Sheep health | Department of Primary Industries and Regional Development",
    "Sheep Health & Welfare | Sheep Producers Australia",
    "Digitised greenhouse gas calculator launched for producers | Meat & Livestock Australia",
    "Community programs | Meat & Livestock Australia",
    "Farm Business Resilience Program | Department of Agriculture, Fisheries and Forestry",
    "Farm safety - sheep and shearing | Better Health Channel",
    "Beyond The Shed | WA Shearing Industry Association",
    "Biosecurity for sheep owners | Agriculture Victoria",
    "The general biosecurity duty and sheep producers | NSW Department of Primary Industries",
    "Animal biosecurity | Department of Primary Industries and Regional Development",
    "Drought management | Meat & Livestock Australia",
    "Sheep dry season management | Department of Primary Industries and Regional Development",
    "Saltland management | Department of Primary Industries and Regional Development",
    "Productive saltland pastures for southern WA | Meat & Livestock Australia",
    "Saltbush for saline land | Agriculture Victoria",
    "Natural capital metrics as predictors of farm-scale richness of birds and plants"
  ),
  URL = c(
    "https://seea.un.org/en/methodology/ecosystem-accounting",
    "https://farmingforthefuture.org.au/wp-content/uploads/2024/03/Natural-Capital-Methods-Paper-May-2024.pdf",
    "https://doi.org/10.1016/j.agsy.2025.104553",
    "https://nrmregionsaustralia.com.au/collection/natural-capital-and-sustainable-agriculture/",
    "https://www.latrobe.edu.au/research/centres/environment/future-landscapes/research/interventions/natural-capital-accounting/FACT-SHEET-1-Farm-scale-NCA-An-Overview.pdf",
    "https://www.wool.com/news-events/news/nature-positive-farming/",
    "https://www.woolmark.com/industry/sustainability/woolmarkplus/nature-positive-farming-framework/",
    "https://www.wool.com/news-events/beyond-the-bale/issue-104-spring-2025/nature-positive-farming-104/",
    "https://www.sheepsustainabilityframework.com.au/resources/news/understanding-the-value-of-natural-capital-on-farm/",
    "https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.70245",
    "https://farmingforthefuture.org.au/wp-content/uploads/2024/03/Natural-Capital-Data-Collection-Methods-Master-February-2024.pdf",
    "https://www.dpird.wa.gov.au/online-tools/livestock-calculators/lambing-planner/",
    "https://www.dpird.wa.gov.au/businesses/livestock-farming/sheep-management/sheep-health/",
    "https://www.sheepproducers.com.au/sheep-health-welfare",
    "https://www.mla.com.au/news-and-events/industry-news/digitised-greenhouse-gas-calculator-launched-for-producers/",
    "https://www.mla.com.au/marketing-beef-and-lamb/community-programs/",
    "https://www.agriculture.gov.au/agriculture-land/farm-food-drought/drought/future-drought-fund/farm-business-resilience-program",
    "https://www.betterhealth.vic.gov.au/health/healthyliving/farm-safety-sheep-and-shearing",
    "https://wasia.com.au/beyond-the-shed/",
    "https://agriculture.vic.gov.au/livestock-and-animals/sheep/biosecurity-for-sheep/biosecurity-for-sheep-owners",
    "https://www.dpird.nsw.gov.au/animals-and-livestock/sheep/health/other/the-general-biosecurity-duty-and-sheep-producers",
    "https://www.dpird.wa.gov.au/businesses/biosecurity/animal-biosecurity/",
    "https://www.mla.com.au/research-and-development/dealing-with-natural-disasters/drought-management/",
    "https://www.dpird.wa.gov.au/businesses/livestock-farming/sheep-management/sheep-dry-season-management/",
    "https://www.dpird.wa.gov.au/environment-and-sustainability/soils/managing-soils/establishing-saltbush-and-understorey/",
    "https://www.mla.com.au/news-and-events/industry-news/saltbush-delivers-pasture-transformation/",
    "https://agriculture.vic.gov.au/farm-management/water/saltbush-for-saline-land",
    "https://www.sciencedirect.com/science/article/pii/S0167880925002786"
  ),
  stringsAsFactors = FALSE
)

# 26 Metrics including Salt Affected Land
metric_keys <- c(
  "Ecological_Condition", "Woody_Vegetation_Cover", "Aggregation", "Proximity",
  "Soil_Condition", "Riparian_Buffer_Condition", "Forage_Condition",
  "Native_Vegetation_Extent", "Habitat_Features", "Landscape_Connectivity",
  "Shade_and_Shelter", "Waterpoint_Condition", "GHG_Climate_Plan",
  "Drought_Preparedness", "Nutrient_Efficiency", "Feed_Efficiency", "Animal_Welfare",
  "Pasture_Planning", "Animal_Health", "Biosecurity", "Lambing_Management",
  "People_and_Safety", "Business_Resilience", "Community_Engagement", "Stocking_Fit",
  "Salt_Affected_Land"
)

metric_labels <- c(
  "Ecological Condition", "Woody Vegetation Cover", "Aggregation", "Proximity",
  "Soil Condition", "Riparian Buffer Condition", "Forage Condition",
  "Native Vegetation Extent", "Habitat Features", "Landscape Connectivity",
  "Shade and Shelter", "Waterpoint Condition", "GHG / Climate Plan",
  "Drought Preparedness", "Nutrient Efficiency", "Feed Efficiency", "Animal Welfare",
  "Pasture Planning", "Animal Health", "Biosecurity", "Lambing Management",
  "People and Safety", "Business Resilience", "Community Engagement", "Stocking Fit",
  "Salt Affected Land Management"
)

metric_defs <- data.frame(
  Category = c(rep("Major Natural Capital Metric", 7), rep("Farming Management & Sustainability Metric", 19)),
  Metric = metric_labels,
  Definition = c(
    "Area-weighted ecological condition relative to reference or best-on-offer condition. Ecological Condition measures the degree to which the farm's ecosystems have been modified from their reference or best-on-offer ecological condition. Higher condition generally indicates greater potential to support habitat, native species, soil regulation, pollination, pest control and other ecosystem services.",
    "Proportion of farm area covered by woody vegetation. Woody Vegetation Cover includes trees and shrubs, remnant native vegetation, planted vegetation, shelterbelts and other woody vegetation. It measures extent, not necessarily ecological condition or habitat quality.",
    "Concentration of woody vegetation in contiguous patches rather than many small patches. More aggregated vegetation generally provides larger areas of interior habitat, reduces fragmentation and can improve structural connectivity and shelter.",
    "Mean distance from production areas to woody vegetation. Shorter distances can improve access to shade, shelter, pollination, pest suppression, beneficial invertebrate habitat and biodiversity movement pathways.",
    "Ground-cover proxy for erosion protection, infiltration, moisture, fertility and biological activity. Soil Condition is represented through median cover, minimum cover and soil stability inputs.",
    "Proportion of mapped riparian area with woody vegetation cover. Riparian vegetation can stabilise banks, reduce erosion, filter sediment and nutrients, regulate water flow, improve water quality and provide habitat.",
    "Area-weighted grazed-pasture quality based on palatability, productivity, perenniality and persistence. High forage condition generally indicates a more productive and resilient feed base.",
    "Extent of native vegetation. Native Vegetation Extent measures how much of the farm retains native vegetation or native ecosystem cover, expressed as a percentage of total farm area.",
    "Logs, litter, shrubs, old trees, hollows, rocks and structural habitat. Habitat Features measures the presence, abundance and structural diversity of features that provide shelter, nesting, feeding or refuge opportunities for fauna.",
    "Linkage of habitat patches across the farm and surrounding landscape. Connectivity considers continuity, proximity and functional linkage of native vegetation, shelterbelts, riparian vegetation and stepping-stone patches.",
    "Availability of tree shade and wind protection. Shade and Shelter measures the availability, distribution, accessibility and functional adequacy of vegetation or structures that protect livestock and crops from heat, solar radiation, cold and wind.",
    "Reliability, condition and distribution of livestock water sources. It includes dams, tanks, troughs, bores, pumps, pipelines, storage capacity, water quality, reserve capacity and livestock access.",
    "Documented greenhouse-gas sources, climate risks, actions and monitoring. This application uses a management-readiness score rather than calculating actual emissions intensity.",
    "Readiness to act before drought affects feed, water, ground cover or animal condition. It includes early-warning indicators, feed budgeting, water planning, livestock monitoring and drought trigger points.",
    "Nutrient testing, input matching and reduction of losses. Nutrient Efficiency measures how effectively nutrients are matched to pasture and crop requirements without excessive runoff, leaching or volatilisation.",
    "Efficiency of converting feed to production while maintaining welfare. It is influenced by pasture quality, grazing allocation, genetics, animal health, feed wastage and supplementary-feeding decisions.",
    "Systems protecting sheep welfare, health, condition and survival. It includes feed, water, disease prevention, timely treatment, humane handling, shelter, predator management and suitable stocking pressure.",
    "Feed budgeting, grazing allocation, rest and grazing pressure. Pasture Planning links animal demand to pasture supply and helps prevent repeated overgrazing, pasture decline and avoidable feed shortages.",
    "Preventive health planning, monitoring, vaccination and parasite control. Animal Health measures systems that maintain sheep health, productivity and survival.",
    "Quarantine, traceability and prevention of disease, pest and weed spread. Biosecurity measures the farm's capacity to prevent biological threats from entering or spreading.",
    "Lambing paddocks, nutrition, shelter, supervision and survival monitoring. Lambing Management measures how effectively the farm protects ewe health, lamb survival and timely intervention.",
    "Training, safety, emergency procedures and workforce capability. People and Safety measures the extent to which the farm protects workers, contractors, visitors and family members.",
    "Records, financial awareness, risk management and market readiness. Business Resilience measures the capacity of the farm business to continue operating and recover from climate, market, production and financial disruption.",
    "Producer, Landcare, NRM, adviser and community networks. Community Engagement measures the strength and usefulness of relationships that support coordinated landscape action and knowledge exchange.",
    "Actual stocking pressure compared with entered sustainable capacity. Stocking Fit is a simplified screening indicator and should not be interpreted as a certified carrying-capacity estimate.",
    "Management of salt-affected land through salt-tolerant pastures, fencing, grazing control and rehabilitation. Salt Affected Land Management measures the extent to which saline areas are fenced, vegetated with salt-tolerant species (saltbush, puccinellia, tall wheat grass), and managed to prevent further degradation while maintaining productive use. About 2.5 million hectares of Australian agricultural land are affected by dryland salinity, including about 1.8 million hectares in Western Australia—over 70% of the national affected area, with saltbush grazing shown to reduce autumn supplementary feed costs by up to 30 percent and quadruple sheep vitamin E levels."
  ),
  stringsAsFactors = FALSE
)

metric_defs$Priority <- ifelse(metric_defs$Category == "Major Natural Capital Metric", "High (Rainsford et al. 2025)", "Standard")

formulas <- data.frame(
  Component = c("Ecological condition", "Woody vegetation cover", "Aggregation", "Proximity", "Soil condition",
                "Riparian buffer condition", "Forage condition", "Native vegetation extent", "Metric score",
                "Domain score", "Overall score", "Stocking fit", "Salt affected land"),
  Formula = c("EC = sum(Ai x Wi) / Afarm", "Woody cover = Awood / Afarm", "Aggregation = farm-entered proxy (0-1)",
              "Proximity = farm-entered proximity score (0-1)", "Soil = mean(P50, P10, stability) / 100", "Riparian = riparian % / 100",
              "FC = farm-entered proxy (0-1)", "Native extent = native % / 100", "Score = 100 x clamp(raw, 0, 1)",
              "Mean of component metric scores", "Mean of seven domain scores",
              "Stocking fit = clamp(1 - abs(actual - capacity) / capacity)", "Salt = management score (0-5) / 5"),
  stringsAsFactors = FALSE
)

calculation_methods <- data.frame(
  Metric = metric_keys,
  Equation_or_calculation = c("Area-weighted ecosystem-condition states relative to reference condition.",
                              "Woody vegetation area divided by total farm area.",
                              "Farm-entered patch-consolidation proxy between 0 and 1.",
                              "Farm-entered proximity score between 0 and 1.",
                              "Mean of median cover, minimum cover and stability, divided by 100.",
                              "Riparian woody vegetation percentage divided by 100.",
                              "Farm-entered forage condition proxy between 0 and 1.",
                              "Native vegetation percentage divided by 100.",
                              "Entered structural habitat score divided by 5.",
                              "Entered connectivity score divided by 5.",
                              "Entered shade-and-shelter score divided by 5.",
                              "Entered waterpoint score divided by 5.",
                              "Entered GHG/climate-plan score divided by 5.",
                              "Entered drought-preparedness score divided by 5.",
                              "Entered nutrient-efficiency score divided by 5.",
                              "Entered feed-efficiency score divided by 5.",
                              "Entered animal-welfare score divided by 5.",
                              "Entered pasture-planning score divided by 5.",
                              "Entered animal-health score divided by 5.",
                              "Entered biosecurity score divided by 5.",
                              "Entered lambing-management score divided by 5.",
                              "Entered people-and-safety score divided by 5.",
                              "Entered business-resilience score divided by 5.",
                              "Entered community-engagement score divided by 5.",
                              "Stocking pressure relative to entered sustainable capacity.",
                              "Salt-affected land management score (0-5) divided by 5."),
  App_implementation = c("Farm-entered ecological condition proxy.", "Woody vegetation cover score (0-1).",
                         "Farm-entered aggregation proxy.", "Farm-entered proximity score (0-1).",
                         "Mean of three cover inputs / 100.", "Riparian percentage / 100.",
                         "Farm-entered forage proxy.", "Native percentage / 100.",
                         "Habitat score / 5.", "Connectivity score / 5.", "Shade score / 5.",
                         "Waterpoint score / 5.", "GHG score / 5.", "Drought score / 5.",
                         "Nutrient score / 5.", "Feed score / 5.", "Welfare score / 5.",
                         "Pasture score / 5.", "Health score / 5.", "Biosecurity score / 5.",
                         "Lambing score / 5.", "People score / 5.", "Business score / 5.",
                         "Community score / 5.", "Clamped stocking-fit value.",
                         "Saltland management score / 5."),
  stringsAsFactors = FALSE
)

score_methods <- data.frame(
  Item = c("Raw values", "Metric scores", "Score bands", "Domain scores", "Overall score",
           "Strength threshold", "Improvement threshold", "Interpretation", "Scoring scales"),
  Calculation = c("Each metric is converted to a 0-1 value.",
                  "Score = 100 x raw value.",
                  "Very low <20; Low 20-<40; Moderate 40-<60; High 60-<80; Very high >=80.",
                  "Mean of the component metric scores.",
                  "Mean of the seven domain scores.",
                  "Scores >=60 appear under What works well.",
                  "Scores <60 appear under What needs improvement.",
                  "The app is a screening tool and does not replace GIS, field validation, emissions accounting or professional farm planning.",
                  "Different metrics use different input scales based on how they are measured in practice: proportions (0-1) for ecological condition, percentages (0-100%) for vegetation cover, and management scores (0-5) for practices like animal health, biosecurity and saltland management. All scores are converted to a common 0-100 scale for reporting, interpretation and domain aggregation. This approach reflects best-practice natural capital accounting (SEEA EA) and industry guidance (DPIRD, MLA, AWI)."),
  stringsAsFactors = FALSE
)

calc_metrics <- function(x) {
  prox <- clamp(as.numeric(x$distance))
  soil <- safe_mean(c(x$soil_median, x$soil_minimum, x$soil_stability)) / 100
  fit <- if (is.finite(x$capacity) && x$capacity > 0) {
    clamp(1 - abs(x$stocking - x$capacity) / x$capacity)
  } else {
    NA_real_
  }
  
  z <- c(
    Ecological_Condition = x$ecological,
    Woody_Vegetation_Cover = x$woody,
    Aggregation = x$aggregation,
    Proximity = prox,
    Soil_Condition = soil,
    Riparian_Buffer_Condition = x$riparian / 100,
    Forage_Condition = x$forage,
    Native_Vegetation_Extent = x$native / 100,
    Habitat_Features = x$habitat / 5,
    Landscape_Connectivity = x$connectivity / 5,
    Shade_and_Shelter = x$shade / 5,
    Waterpoint_Condition = x$water / 5,
    GHG_Climate_Plan = x$ghg / 5,
    Drought_Preparedness = x$drought / 5,
    Nutrient_Efficiency = x$nutrient / 5,
    Feed_Efficiency = x$feed / 5,
    Animal_Welfare = x$welfare / 5,
    Pasture_Planning = x$pasture / 5,
    Animal_Health = x$health / 5,
    Biosecurity = x$biosecurity / 5,
    Lambing_Management = x$lambing / 5,
    People_and_Safety = x$people / 5,
    Business_Resilience = x$business / 5,
    Community_Engagement = x$community / 5,
    Stocking_Fit = fit,
    Salt_Affected_Land = x$salt / 5
  )
  
  raw <- clamp(z)
  score <- round(100 * raw, 1)
  
  data.frame(
    Metric = names(z),
    Raw = round(raw, 4),
    Score = score,
    Interpretation = band(score),
    stringsAsFactors = FALSE
  )
}

calc_domains <- function(m) {
  get_scores <- function(names) safe_mean(m$Score[match(names, m$Metric)])
  
  d <- data.frame(
    Domain = c(
      "Ecological condition and habitat",
      "Woody vegetation and connectivity",
      "Soil, forage and stocking",
      "Water and riparian systems",
      "Climate and emissions readiness",
      "Sheep welfare and grazing",
      "People and business resilience"
    ),
    Score = c(
      get_scores(c("Ecological_Condition", "Native_Vegetation_Extent", "Habitat_Features")),
      get_scores(c("Woody_Vegetation_Cover", "Aggregation", "Proximity", "Landscape_Connectivity", "Shade_and_Shelter")),
      get_scores(c("Soil_Condition", "Forage_Condition", "Pasture_Planning", "Nutrient_Efficiency", "Stocking_Fit", "Salt_Affected_Land")),
      get_scores(c("Riparian_Buffer_Condition", "Waterpoint_Condition")),
      get_scores(c("GHG_Climate_Plan", "Drought_Preparedness", "Feed_Efficiency")),
      get_scores(c("Animal_Welfare", "Animal_Health", "Biosecurity", "Lambing_Management")),
      get_scores(c("People_and_Safety", "Business_Resilience", "Community_Engagement"))
    ),
    stringsAsFactors = FALSE
  )
  
  d$Score <- round(d$Score, 1)
  d$Interpretation <- band(d$Score)
  d
}

recommendation_map <- data.frame(
  Metric = metric_keys,
  Action = c(
    "Protect high-condition ecosystem units, control priority weeds, use appropriate grazing exclusions and restore degraded patches.",
    "Protect existing woody vegetation and establish strategic tree or shrub plantings in priority areas.",
    "Protect large contiguous woody patches and improve links between fragmented patches where feasible.",
    "Establish shelterbelts, scattered trees or small vegetation clusters closer to production areas.",
    "Increase ground cover through adaptive stocking, tactical spelling and protection of vulnerable soil surfaces.",
    "Fence or manage riparian areas to prevent prolonged grazing pressure and revegetate gaps in the buffer.",
    "Improve pasture persistence, palatability and perenniality while matching grazing pressure to available forage.",
    "Retain and map native vegetation, avoid clearing and prioritise connections between remnant areas.",
    "Retain logs, litter, shrubs, old trees, hollows, rocks and other structural habitat features.",
    "Protect stepping-stone habitats and establish corridor plantings where they improve landscape linkage.",
    "Maintain accessible shade and shelter near stock concentrations and add protection in exposed areas.",
    "Audit trough reliability, leakage, pressure, distribution and livestock access; upgrade weak points.",
    "Document Scope 1, Scope 2 and relevant Scope 3 emissions, sequestration, climate risks and planned actions.",
    "Define drought trigger points for feed, water, ground cover, animal condition and destocking.",
    "Use soil testing and nutrient budgets to match inputs to crop and pasture demand and reduce losses.",
    "Improve feed conversion through pasture quality, animal condition, genetics and low-stress management.",
    "Review heat, nutrition, handling, shelter, predator and disease risks affecting sheep welfare.",
    "Use feed budgets, grazing allocation, rest periods and monitoring to control grazing pressure.",
    "Maintain vaccination, parasite, condition-score and preventive animal-health programs.",
    "Strengthen quarantine, traceability, visitor controls, weed hygiene and disease-prevention procedures.",
    "Improve lambing paddock selection, nutrition, shelter, supervision and lamb-survival monitoring.",
    "Update inductions, emergency procedures, chemical handling, machinery safety and workforce training.",
    "Strengthen financial records, scenario planning, cash-flow monitoring, risk buffers and market readiness.",
    "Maintain active links with NRM organisations, advisers, Landcare groups, neighbours and industry networks.",
    "Adjust stocking pressure to sustainable capacity, or increase the feed base before increasing stock numbers.",
    "Fence salt-affected areas, establish salt-tolerant pastures (saltbush, puccinellia, tall wheat grass), control grazing pressure and prevent further expansion of bare saline patches. Saltbush-based systems may provide feed and animal-health benefits; outcomes depend on site conditions and management."
  ),
  Mechanism = c(
    "Represents departure from reference condition and ecosystem-service capacity.",
    "Provides habitat, shade, shelter, carbon storage, connectivity and microclimate regulation.",
    "Larger connected patches reduce fragmentation effects and support habitat quality.",
    "Shorter distances improve delivery of shade, shelter, pollination and pest-suppression services.",
    "Ground cover reduces erosion, slows runoff, improves infiltration and supports soil function.",
    "Riparian vegetation supports bank stability, water quality, flow regulation, habitat and connectivity.",
    "Forage condition influences livestock production, pasture persistence, drought response and soil protection.",
    "Native extent measures retained native ecosystem assets and biodiversity opportunity.",
    "Structural habitat increases habitat complexity and supports more organisms.",
    "Connectivity supports movement of species and continuity of ecosystem services.",
    "Shade and shelter reduce heat and wind exposure and support welfare and productivity.",
    "Reliable water supports welfare and better distribution of grazing pressure.",
    "Emissions and climate-risk accounting supports transparent performance reporting.",
    "Preparedness protects production, animals, soil cover and natural-capital assets during drought.",
    "Efficient nutrient use reduces losses, pollution risk and unnecessary input costs.",
    "Feed efficiency links pasture quality and animal performance with production and emissions intensity.",
    "Welfare management protects health, survival, performance and social licence.",
    "Pasture planning prevents repeated overgrazing and protects forage and ground cover.",
    "Preventive health management reduces production losses and welfare risks.",
    "Biosecurity reduces disease, pest and weed risks.",
    "Lambing management affects lamb survival, welfare and enterprise productivity.",
    "Safety systems reduce injury and business disruption.",
    "Business resilience improves capacity to manage climate, market and production shocks.",
    "Engagement supports knowledge exchange, benchmarking and coordinated action.",
    "Stocking fit directly controls grazing pressure relative to the sustainable feed and land base.",
    "Salt-tolerant vegetation and controlled grazing stabilise saline areas, prevent erosion, maintain productivity and reduce further salinity spread. About 2.5 million hectares of Australian agricultural land are affected by dryland salinity, including about 1.8 million hectares in Western Australia—over 70% of the national affected area."
  ),
  Priority = c(
    "High", "High", "High", "High", "High", "High", "High", "High", "Moderate", "Moderate",
    "High", "High", "High", "High", "Moderate", "Moderate", "High", "Moderate", "Moderate",
    "Moderate", "High", "Moderate", "Moderate", "Low", "High", "High"
  ),
  Timeframe = c(
    "This season", "1-5 years", "1-5 years", "1-3 years", "Immediate and seasonal",
    "Before the next high-flow period", "Seasonal", "Ongoing", "Ongoing", "1-5 years",
    "Before next summer", "Immediate", "This year", "Before the dry season", "This season",
    "Ongoing", "Immediate", "Each grazing cycle", "Seasonal", "Ongoing", "Before next lambing",
    "This year", "Quarterly", "Ongoing", "Immediate and seasonal", "1-3 years"
  ),
  ReferenceIndex = c(1, 7, 10, 3, 10, 1, 3, 1, 10, 1, 7, 6, 15, 23, 7, 3, 14, 3, 13, 20, 12, 18, 17, 16, 3, 25),
  stringsAsFactors = FALSE
)

recommendation_map$ReferenceIndex[recommendation_map$Metric == "Lambing_Management"] <- 12
recommendation_map$ReferenceIndex[recommendation_map$Metric == "Animal_Health"] <- 13
recommendation_map$ReferenceIndex[recommendation_map$Metric == "Animal_Welfare"] <- 14
recommendation_map$ReferenceIndex[recommendation_map$Metric == "GHG_Climate_Plan"] <- 15
recommendation_map$ReferenceIndex[recommendation_map$Metric == "Community_Engagement"] <- 16
recommendation_map$ReferenceIndex[recommendation_map$Metric == "Business_Resilience"] <- 17
recommendation_map$ReferenceIndex[recommendation_map$Metric == "People_and_Safety"] <- 18
recommendation_map$ReferenceIndex[recommendation_map$Metric == "Biosecurity"] <- 20
recommendation_map$ReferenceIndex[recommendation_map$Metric == "Drought_Preparedness"] <- 23

rec_engine <- function(m) {
  out <- merge(
    m[, c("Metric", "Score", "Interpretation")],
    recommendation_map,
    by = "Metric",
    all.x = TRUE,
    sort = FALSE
  )
  out$Reference <- refs$Title[out$ReferenceIndex]
  out$ReferenceURL <- refs$URL[out$ReferenceIndex]
  out$ReferenceIndex <- NULL
  out
}

svg <- function(x) {
  paste0("data:image/svg+xml;charset=utf-8,", utils::URLencode(x, reserved = TRUE))
}

fallback <- svg(
  "<svg xmlns='http://www.w3.org/2000/svg' width='760' height='240'>
   <rect width='760' height='240' rx='18' fill='#DCEBDD'/>
   <path d='M0 190Q190 135 380 185T760 170V240H0Z' fill='#78A85E'/>
   <g fill='white' stroke='#193B2A' stroke-width='5'>
   <ellipse cx='370' cy='150' rx='95' ry='52'/>
   <circle cx='470' cy='135' r='28'/>
   <ellipse cx='290' cy='142' rx='25' ry='32'/>
   </g>
   <text x='25' y='38' font-family='Arial' font-size='25' font-weight='bold' fill='#193B2A'>Green Sheep Project</text>
   </svg>"
)

# Portable static assets. Shiny serves the contents of www directly.
# Prefer WebP images; retain filename aliases to support the existing asset names.
asset <- function(filename) {
  aliases <- list(
    "Merino.webp" = "SheepMerin.webp",
    "SheepMerin.webp" = "Merino.webp",
    "Aggregation.webp" = "Aggregatio.webp",
    "Aggregatio.webp" = "Aggregation.webp"
  )

  alias_candidates <- aliases[[filename]]
  if (is.null(alias_candidates)) alias_candidates <- character(0)

  # For any legacy PNG/JPG reference, try the matching WebP filename first.
  webp_candidate <- sub("\\.(png|jpe?g)$", ".webp", filename, ignore.case = TRUE)
  candidates <- unique(c(webp_candidate, filename, alias_candidates))

  for (candidate in candidates) {
    if (!is.na(candidate) && nzchar(candidate) &&
        file.exists(file.path("www", candidate))) {
      return(utils::URLencode(candidate, reserved = TRUE))
    }
  }
  ""
}
metric_asset <- asset

project_img <- function(path, alt = "Project image") {
  tags$img(
    src = if (nzchar(path)) path else fallback,
    alt = alt,
    class = "project-image"
  )
}

project_video <- function(path, alt = "Sheep video") {
  mp4 <- asset("SheepVideo.mp4")
  video_path <- if (nzchar(mp4)) mp4 else path
  if (!nzchar(video_path)) return(div(class = "image-missing",
                                      "Video not found: put SheepVideo.mp4 in www."))
  tagList(
    tags$video(src = video_path, controls = NA, autoplay = NA,
               loop = NA, muted = NA, playsinline = NA, preload = "metadata",
               class = "project-image", `aria-label` = alt,
               onerror = "this.nextElementSibling.style.display='block';",
               "Your browser does not support this video."),
    div(style = "display:none;padding:12px;color:#A33A45;",
        "The video could not be loaded or decoded. Convert it to H.264 MP4, save as www/SheepVideo.mp4, and export again.")
  )
}

metric_score_image <- function(filename, alt_text) {
  image_path <- metric_asset(filename)
  
  if (!nzchar(image_path)) {
    return(
      div(
        class = "image-missing",
        paste("Image not found:", filename)
      )
    )
  }
  
  tags$img(
    src = image_path,
    alt = alt_text,
    class = "metric-score-image"
  )
}

natural_capital_figure <- asset("Nat Capital.webp")
natural_capital_img <- tags$img(
  src = if (nzchar(natural_capital_figure)) natural_capital_figure else fallback,
  alt = "Seven farm-scale natural-capital metrics",
  class = "natural-capital-figure"
)

css <- paste0(
  ".hero{background:linear-gradient(120deg,#193B2A,#4E8C57);color:white;padding:10px 20px;border-radius:18px;margin-bottom:10px;}",
  ".card{background:white;border-radius:16px;padding:20px;box-shadow:0 3px 18px rgba(25,59,42,.10);margin-bottom:18px;}",
  ".logos{display:flex;gap:12px;justify-content:flex-start;align-items:center;flex-wrap:wrap;}",
  ".logos img{max-height:65px;max-width:35%;width:auto;border-radius:11px;padding:1px;background:rgba(255,255,255,0.25);box-shadow:0 2px 10px rgba(0,0,0,0.08);}",
  ".project-image{width:100%;max-height:320px;object-fit:contain;background:#F2F5EF;border-radius:14px;margin:8px 0;}",
  ".definition{display:flex;gap:15px;border-left:5px solid #4E8C57;background:#F4F8F2;padding:14px;margin:9px 0;border-radius:10px;}",
  ".definition h4{margin-top:0;color:#193B2A;}",
  ".index{font-size:1.3rem;font-weight:700;color:#C49A3A;}",
  ".good{border-left:5px solid #4E8C57;background:#F0F8F0;padding:14px;margin:9px 0;border-radius:10px;}",
  ".need{border-left:5px solid #A33A45;background:#FFF3F3;padding:14px;margin:9px 0;border-radius:10px;}",
  ".natural-capital-figure{width:100%;max-width:1200px;height:auto;display:block;margin:10px auto 18px auto;border-radius:12px;background:#FFFFFF;border:1px solid #D7E4D8;padding:8px;}",
  ".metric-score-image{width:100%;max-width:950px;height:auto;display:block;margin:12px auto 20px auto;border-radius:12px;border:1px solid #D7E4D8;background:#FFFFFF;padding:6px;}",
  ".image-missing{padding:12px;margin:10px 0;background:#FFF3F3;border-left:5px solid #A33A45;border-radius:8px;color:#7A202A;}",
  ".disclaimer-box{margin-top:18px;padding:14px 16px;background:#FFF8E8;border-left:5px solid #C49A3A;border-radius:10px;color:#26352B;font-size:0.95rem;line-height:1.55;}",
  ".disclaimer-box strong{color:#193B2A;}",
  ".app-footer{margin-top:40px;padding:20px;background:linear-gradient(to right,#f9f9f9,#f0f0f0);border-top:3px solid #2F5D3A;text-align:center;color:#555;font-size:0.8rem;}",
  ".app-footer a{color:#2F5D3A;text-decoration:none;font-weight:600;}",
  ".app-footer a:hover{color:#4E8C57;text-decoration:underline;}",
  ".copyright-notice{margin:0;padding:0;}"
)

guide_slider_specs <- list(
  go_ecological_input = list(list(id = "ecological", label = "Ecological Condition (0-1)", min = 0, max = 1, step = 0.01, value = 0.50)),
  go_woody_input = list(list(id = "woody", label = "Woody Vegetation Cover (0-1)", min = 0, max = 1, step = 0.01, value = 0.70)),
  go_aggregation_input = list(list(id = "aggregation", label = "Aggregation (0-1)", min = 0, max = 1, step = 0.01, value = 0.35)),
  go_proximity_input = list(list(id = "distance", label = "Proximity (0-1)", min = 0, max = 1, step = 0.01, value = 0.80)),
  go_soil_input = list(
    list(id = "soil_median", label = "Soil median cover (%)", min = 0, max = 100, step = 1, value = 70),
    list(id = "soil_minimum", label = "Soil minimum cover (%)", min = 0, max = 100, step = 1, value = 85),
    list(id = "soil_stability", label = "Soil stability (%)", min = 0, max = 100, step = 1, value = 70)
  ),
  go_riparian_input = list(list(id = "riparian", label = "Riparian Buffer Condition (%)", min = 0, max = 100, step = 1, value = 75)),
  go_forage_input = list(list(id = "forage", label = "Forage Condition (0-1)", min = 0, max = 1, step = 0.01, value = 0.60)),
  go_habitat_input = list(list(id = "habitat", label = "Habitat Features (0-5)", min = 0, max = 5, step = 1, value = 2)),
  go_connectivity_input = list(list(id = "connectivity", label = "Landscape Connectivity (0-5)", min = 0, max = 5, step = 1, value = 2)),
  go_salt_input = list(list(id = "salt", label = "Salt Affected Land Management (0-5)", min = 0, max = 5, step = 1, value = 3))
)

guide_panel <- function(title, metric_title, description, image_name, image_alt, observations, score_text, link_id, link_text) {
  sliders <- guide_slider_specs[[link_id]]
  slider_ui <- if (!is.null(sliders)) {
    tagList(
      h4("Quick score entry"),
      lapply(sliders, function(s) {
        sliderInput(inputId = s$id, label = s$label, min = s$min, max = s$max,
                    value = s$value, step = s$step, width = "100%")
      })
    )
  } else NULL
  
  bslib::accordion_panel(
    title,
    h3(metric_title),
    p(description),
    metric_score_image(image_name, image_alt),
    slider_ui,
    h4("What to look for"),
    tags$ul(lapply(observations, tags$li)),
    h4("Score to enter"),
    p(score_text),
    actionLink(link_id, link_text)
  )
}

definitions_ui <- tagList(
  lapply(seq_len(nrow(metric_defs)), function(i) {
    div(
      class = "definition",
      div(class = "index", sprintf("%02d", i)),
      div(h4(metric_defs$Metric[i]), p(metric_defs$Definition[i]))
    )
  })
)

# END OF PART 1 - CONTINUE WITH PART 2
# Copyright © 2026 All rights reserved.
# ============================================================

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bg = brand$cream,
    fg = brand$text,
    primary = brand$dark,
    secondary = brand$green
  ),
  tags$head(tags$style(HTML(css)), tags$script(HTML("(function () {\n  function registerDownload() {\n    if (!window.Shiny || !Shiny.addCustomMessageHandler) {\n      setTimeout(registerDownload, 100); return;\n    }\n    Shiny.addCustomMessageHandler('natcap_download', function (message) {\n      try {\n        const binary = atob(message.data);\n        const bytes = new Uint8Array(binary.length);\n        for (let i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);\n        const url = URL.createObjectURL(new Blob([bytes], {type: message.mime}));\n        const link = document.createElement('a');\n        link.href = url; link.download = message.filename;\n        document.body.appendChild(link); link.click(); link.remove();\n        setTimeout(function () { URL.revokeObjectURL(url); }, 60000);\n      } catch (error) { window.alert('PDF download failed: ' + error.message); }\n    });\n  }\n  registerDownload();\n})();"))),
  
  div(
    class = "hero",
    fluidRow(
      column(
        8,
        # SusSheepSys.webp header image — adjustable size and rounded edges
        tags$img(
          src = asset("SusSheepSys.webp"),
          
          style = paste0(
            "width: 450px;",          # Adjust width
            "height: 130px;",          # Adjust height
            "object-fit: contain;",   # Preserve image proportions
            "border-radius: 80px;",   # Rounded corners
            "overflow: hidden;",
            "display: block;"
          )
        ),
        p("Natural-capital metrics assessment.")
      ),
      column(
        4,
        div(
          class = "logos",
          # SheepLogo2.webp — top-right header position
          tags$div(
            style = paste0(
              "display:flex;",
              "justify-content:flex-end;",
              "align-items:flex-start;",
              "width:100%;",
              "height:100%;",
              "padding:0px;"
            ),
            
            tags$img(
              src = if (nzchar(asset("SheepLogo2.webp"))) {
                asset("SheepLogo2.webp")
              } else {
                fallback
              },
              alt = "Sheep project logo",
              
              style = paste0(
                "max-height: 170px;",
                "max-width: 100%;",
                "width: auto;",
                "height: auto;",
                "object-fit: contain;",
                "background: transparent;",
                "border: none;",
                "box-shadow: none;",
                "display: block;"
              )
            )
          )
        )
      )
    )
  ),
  
  navbarPage(
    title = NULL,
    id = "tabs",
    
    tabPanel(
      "Overview",
      fluidRow(
        column(
          7,
          div(
            class = "card",
            h3("Purpose"),
            p("This simplified, producer-friendly tool supports sheep producers to understand and assess their farm’s natural capital and management practices. It brings together information on soil, pasture and forage, biodiversity, water, climate readiness, animal welfare and business resilience to inform practical on-farm decisions.")
          ),
          div(
            class = "card",
            h3("Score scale"),
            p("Very low <20; Low 20-<40; Moderate 40-<60; High 60-<80; Very high >=80."),
            tags$br(),
            tags$strong("Scoring scales:"),
            tags$p(
              "Different metrics use different input scales based on how they are measured and reported in industry practice:",
              tags$ul(
                tags$li("Proportions (0-1) for ecological condition and forage proxies (aligned with SEEA Ecosystem Accounting)."),
                tags$li("Percentages (0-100%) for vegetation cover metrics (how producers naturally think about land cover)."),
                tags$li("Management scores (0-5) for practices like animal health, biosecurity and saltland management (ordinal Likert-style scales).")
              ),
              "All scores are converted to a common 0-100 scale for reporting, interpretation and domain aggregation. This approach reflects best-practice natural capital accounting (SEEA EA) and industry guidance (DPIRD, MLA, AWI)."
            )
          )
        ),
        column(5, div(class = "card", h3(""), project_video(asset("SheepVideo.mp4"))))
      ),
      div(
        class = "card",
        h2("Farm-scale natural-capital metrics"),
        p("The figure illustrates how the seven natural-capital metrics are distributed across an integrated sheep-grazing landscape."),
        natural_capital_img,
        h2("Complete metric definitions"),
        definitions_ui
      )
    ),
    
    tabPanel(
      "Scoring guides",
      div(
        class = "card",
        h2("Natural Capital Scoring Guides"),
        p("Use these visual guides before completing the Farm inputs tab. They are intended to help producers make consistent, practical farm-scale estimates of natural-capital condition."),
        div(
          class = "disclaimer-box",
          tags$strong("Important: "),
          "These guides support farm self-assessment and screening. Scores are indicative only and should be interpreted alongside paddock history, seasonal conditions, local soil types, vegetation communities and professional advice where required."
        ),
        br(),
        
        bslib::accordion(
          id = "metric_guides",
          multiple = TRUE,
          
          guide_panel(
            "1. Ecological Condition",
            "Ecological Condition",
            "Ecological condition describes how closely an area resembles a healthy native ecosystem. Consider vegetation cover, plant diversity, trees and shrubs, habitat features, soil condition, weeds and signs of disturbance.",
            "Ecological Condition.webp",
            "Ecological condition scoring guide",
            c("Amount and continuity of native vegetation cover", "Presence of trees, shrubs, native grasses and groundcover", "Evidence of erosion, bare soil, compaction or disturbance", "Weed dominance or loss of native species", "Habitat complexity, including logs, litter, hollows and mature trees"),
            "Enter a score from 0 to 1 in Farm inputs. A score near 0 represents severely modified or degraded land; a score near 1 represents a healthy, minimally modified native ecosystem.",
            "go_ecological_input",
            "Go to Farm inputs to enter Ecological Condition"
          ),
          
          guide_panel(
            "2. Woody Vegetation Cover",
            "Woody Vegetation Cover",
            "Woody vegetation cover is the percentage of the farm occupied by trees, shrubs, native bushland, shelterbelts, revegetation and other woody vegetation.",
            "Woody Vegetation Cover.webp",
            "Woody vegetation cover scoring guide",
            c("Remnant native woodland, shrubland or forest", "Shelterbelts and planted vegetation", "Scattered paddock trees and shrub patches", "Woody vegetation along drainage lines, dams and waterways", "Total woody vegetation area relative to total farm area"),
            "Enter a score from 0 to 1 for woody vegetation cover. For example, 0.10 means approximately 10% of the farm has tree or shrub cover; 0.70 means approximately 70% of the farm has woody cover.",
            "go_woody_input",
            "Go to Farm inputs to enter Woody Vegetation Cover"
          ),
          
          guide_panel(
            "3. Aggregation",
            "Aggregation of Woody Vegetation",
            "Aggregation describes whether woody vegetation is retained in large, connected patches or divided into many small and isolated fragments. The score represents the concentration and consolidation of woody vegetation across the farm, rather than the total amount of woody cover.",
            "Aggregation.webp",
            "Aggregation scoring guide",
            c("Whether woody vegetation occurs in one or several large patches", "The presence of isolated small remnants versus consolidated vegetation", "The amount of interior habitat away from patch edges", "Fragmentation caused by clearing, tracks, cultivation or infrastructure", "Opportunities to protect large remnants or reconnect smaller patches"),
            "Enter a score from 0 to 1. A score near 0 represents strongly fragmented or dispersed woody vegetation; a score near 1 represents woody vegetation concentrated in large, contiguous patches.",
            "go_aggregation_input",
            "Go to Farm inputs to enter Aggregation"
          ),
          
          guide_panel(
            "4. Proximity to Woody Vegetation",
            "Proximity",
            "Proximity measures how close production areas are to trees and shrubs. Shorter distances can improve livestock access to shade and shelter and increase the reach of ecosystem services such as pollination and pest control.",
            "Proximity.webp",
            "Proximity scoring guide",
            c("Distance from grazed paddocks to trees, shrubs or shelterbelts", "Accessibility of shade and shelter for sheep", "Whether vegetation is distributed across the farm or confined to one area", "Distance from lambing paddocks and stock congregation areas to shelter"),
            "Enter a proximity score from 0 to 1. A score near 0 indicates that production areas are far from woody vegetation; a score near 1 indicates that shade, shelter and woody habitat are close and accessible to production areas.",
            "go_proximity_input",
            "Go to Farm inputs to enter Proximity"
          ),
          
          guide_panel(
            "5. Soil Condition",
            "Soil Condition",
            "Soil condition is represented in this tool using ground cover and soil stability indicators. Good cover helps reduce erosion, improve infiltration, protect soil organic matter and support pasture production.",
            "Soil Condition.webp",
            "Soil condition scoring guide",
            c("Median or typical ground cover across seasons", "Lowest ground cover during the most vulnerable period", "Bare soil, erosion, scalding or runoff pathways", "Litter, stubble, living pasture and other protective cover", "Signs that soil remains stable under grazing and rainfall"),
            "Enter estimated percentages for median ground cover, minimum ground cover and soil stability. The tool averages these three values to generate the Soil Condition score.",
            "go_soil_input",
            "Go to Farm inputs to enter Soil Condition"
          ),
          
          guide_panel(
            "6. Riparian Condition",
            "Riparian Buffer Condition",
            "Riparian condition refers to vegetation along waterways, drainage lines, wetlands, dams and other water-associated areas. Healthy buffers help protect water quality, stabilise banks and provide habitat.",
            "Riparian Condition.webp",
            "Riparian condition scoring guide",
            c("Woody vegetation beside streams, drainage lines and wetlands", "Vegetation continuity along riparian areas", "Bare banks, erosion, pugging or heavy stock access", "Gaps requiring fencing, revegetation or grazing management", "Evidence that the buffer filters sediment and slows runoff"),
            "Enter the estimated percentage of mapped riparian area with woody vegetation cover. If the farm has no riparian areas, record this limitation in the assessment notes.",
            "go_riparian_input",
            "Go to Farm inputs to enter Riparian Condition"
          ),
          
          guide_panel(
            "7. Forage Condition",
            "Forage Condition",
            "Forage condition reflects the quality, persistence, palatability and productivity of pasture available to livestock. It is distinct from short-term feed quantity because it considers the condition of the pasture resource.",
            "Forage Condition.webp",
            "Forage condition scoring guide",
            c("Pasture cover, productivity and persistence", "Perennial species and desirable pasture composition", "Palatability and feed quality for sheep", "Evidence of repeated overgrazing or pasture decline", "Capacity to maintain feed and ground cover during dry periods"),
            "Enter a score from 0 to 1. A score near 0 indicates poor or degraded forage resources; a score near 1 indicates productive, persistent and resilient pasture.",
            "go_forage_input",
            "Go to Farm inputs to enter Forage Condition"
          ),
          
          guide_panel(
            "8. Habitat Condition",
            "Habitat Features",
            "Habitat condition captures the diversity of structural features used by wildlife, including mature trees, hollows, shrubs, litter, rocks, logs, native grasses and dense ground cover.",
            "Habitat Condition.webp",
            "Habitat condition scoring guide",
            c("Old trees, tree hollows and standing dead trees where safe to retain", "Fallen timber, logs, leaf litter and rocks", "Native shrubs, grasses and flowering plants", "Multiple vegetation layers from groundcover to canopy", "Habitat features distributed across the farm rather than isolated"),
            "Enter a score from 0 to 5 for Habitat Features. A score of 0 represents very limited habitat structure, while 5 represents diverse and well-retained habitat features.",
            "go_habitat_input",
            "Go to Farm inputs to enter Habitat Features"
          ),
          
          guide_panel(
            "9. Landscape Connectivity",
            "Landscape Connectivity",
            "Landscape connectivity describes how well vegetation and habitat patches are linked across the farm and into the wider landscape. Connected areas can support wildlife movement, dispersal, gene flow, pollination, pest suppression and ecological resilience.",
            "Landscape Connectivity.webp",
            "Landscape connectivity scoring guide",
            c("Continuous native vegetation, shelterbelts or riparian corridors", "Stepping-stone patches that link larger remnants", "Gaps between habitat patches and barriers to movement", "Connections between on-farm vegetation and neighbouring bushland", "Opportunities for corridor plantings, buffer restoration or protection of linkages"),
            "Enter a score from 0 to 5. A score of 0 represents isolated habitat with little functional linkage; a score of 5 represents a well-connected network of habitat patches across the farm and surrounding landscape.",
            "go_connectivity_input",
            "Go to Farm inputs to enter Landscape Connectivity"
          ),
          
          guide_panel(
            "10. Salt Affected Land Management",
            "Salt Affected Land Management",
            "This metric considers how salt-affected areas are managed to maintain ground cover and productive use while limiting further degradation. It focuses on practical management, not simply the area of saline land.",
            "Saltlands.webp",
            "Salt affected land management scoring guide",
            c("Salt-affected areas are identified and monitored", "Fencing or controlled grazing protects vulnerable saline land", "Salt-tolerant species or suitable saltland pastures are established where appropriate", "Bare saline patches, erosion and grazing pressure are actively managed", "Management is reviewed and adapted to local soil, water and seasonal conditions"),
            "Enter a management score from 0 to 5. A low score indicates little planning or active management; a high score indicates an established, monitored and adaptive approach. If no salt-affected land is known, use the score that best reflects whether risk has been assessed and the absence of affected areas verified.",
            "go_salt_input",
            "Go to Farm inputs to enter Salt Affected Land Management"
          )
        ),
        
        br(),
        div(
          style = "text-align:center;",
          actionButton("go_to_farm_inputs", "Continue to Farm Inputs", class = "btn-primary btn-lg")
        )
      )
    ),
    
    tabPanel(
      "Farm inputs",
      div(
        class = "card",
        h3("Farm information"),
        fluidRow(
          column(2, textInput("farm_name", "Farm name", "")),
          column(2, selectInput("system", "System type", c("Self-Replacing Merino", "Prime Lamb", "Mixed Crop-Sheep Enterprise", "Other"))),
          column(2, numericInput("area", "Farm area (ha)", 5000, min = 1)),
          column(2, numericInput("stocking", "Stocking pressure (DSE/ha)", 20, min = 0, step = 0.1)),
          column(2, numericInput("capacity", "Sustainable capacity (DSE/ha)", 5, min = 0.1, step = 0.1))
        ),
        div(
          class = "disclaimer-box",
          tags$strong("Scoring assistance: "),
          "Use the Scoring guides tab before entering values. The guides provide visual examples for Ecological Condition, Woody Vegetation Cover, Aggregation, Proximity, Soil Condition, Riparian Condition, Forage Condition, Habitat Features, Landscape Connectivity and Salt Affected Land Management."
        ),
        uiOutput("inputs"),
        br(),
        actionButton("run", "Run assessment", class = "btn-primary btn-lg"),
        br(),
        br(),
        textOutput("status")
      )
    ),
    
    tabPanel(
      "Results",
      div(class = "card",
          h2(textOutput("overall")),
          p(textOutput("overall_band")),
          p("Results display version: HTML-26", style = "font-size:0.85rem;color:#667267;"),
          h3("All 26 individual metric results"),
          p(textOutput("metric_row_count_html")),
          uiOutput("metric_results_html"),
          tags$hr(),
          h3("Seven domain summaries"),
          p("Domains group related metrics; they are not the individual metric list."),
          uiOutput("domain_results_html"),
          tags$hr(),
          plotlyOutput("plot", height = "450px")
      )
    ),
    
    tabPanel(
      "Recommendations",
      div(
        class = "card",
        h2("What works well"),
        uiOutput("works"),
        h2("What needs improvement"),
        uiOutput("needs")
      )
    ),
    
    tabPanel(
      "Improvement plan",
      div(class = "card", h2("Detailed improvement plan"), DTOutput("improvement"))
    ),
    
    tabPanel(
      "Methods & references",
      div(
        class = "card",
        h2("Methods, equations and calculations"),
        p("The app combines farm-entered proxies with farm-scale natural-capital accounting concepts. Results are screening indicators and do not replace GIS analysis, field validation, emissions accounting or professional farm planning."),
        h3("Metric definitions"),
        DTOutput("method_metrics"),
        h3("References"),
        uiOutput("reference_links")
      )
    ),
    
    tabPanel(
      "Report",
      div(
        class = "card",
        h2("Full PDF report"),
        p("The PDF report includes an executive abstract, farm profile, domain and metric figures, results tables, strengths, improvement plan, references, interpretation and contact details."),
        actionButton("pdf_browser", "Download full PDF report", icon = icon("download")),
        br(),
        br(),
        actionButton(   "xlsx_browser",   "Download Excel workbook",   icon = icon("download") ),
        div(class = "disclaimer-box", tags$strong("Disclaimer: "), disclaimer_text)
      )
    ),
    
    ##     tabPanel(
    ##       "About & Licence",
    ##       div(
    ##         class = "card",
    ##         h2("About Green Sheep Project"),
    ##         p("The Natural Capital Assessment Tool was developed by Dr Aram Ali, Green Sheep Project Manager, to support producer-friendly farm-scale natural capital assessment and sustainability reporting."),
    ##
    ##         h3("Creator & Owner"),
    ##         p(tags$strong("Dr Aram Ali"), ", Green Sheep Project Manager"),
    ##         p("Email: aali@wheatbeltnrm.org.au, a.aramali@outlook.com"),
    ##         p("Mobile: 0470 488 185"),
    ##
    ##         h3("Affiliation"),
    ##         p("This Tool is hosted on the Wheatbelt Natural Resource Management Inc. website with permission for non-commercial, producer-friendly use."),
    ##
    ##         h3("Permitted Use"),
    ##         tags$ul(
    ##           tags$li("Free use for personal, non-commercial farm assessment"),
    ##           tags$li("Download and print reports for your own farm records"),
    ##           tags$li("Share the Tool's web link with other producers")
    ##         ),
    ##
    ##         h3("Prohibited Use"),
    ##         tags$ul(
    ##           tags$li("Copying or reproducing source code"),
    ##           tags$li("Creating derivative works based on the methodology"),
    ##           tags$li("Commercial use without written permission"),
    ##           tags$li("Removing copyright notices from reports")
    ##         ),
    ##
    ##         h3("Attribution"),
    ##         p("Reference as: 'Green Sheep Project Natural Capital Assessment Tool, Dr Aram Ali, 2026'"),
    ##
    ##         h3("Licensing Enquiries"),
    ##         p("For commercial use, partnerships, or licensing: aali@wheatbeltnrm.org.au"),
    ##
    ##         div(class = "disclaimer-box",
    ##              tags$strong("Disclaimer: "),
    ##              "The Tool provides indicative screening information only and does not replace professional advice, field validation, or formal natural capital accounting.",
    ##              tags$br(),
    ##              tags$strong("Copyright: "),
    ##              "(c) Copyright: All rights reserved. Free use for personal, non-commercial farm assessment. Contact aali@wheatbeltnrm.org.au.")
    ##       )
    ##     ),
    
    tabPanel(
      "Contact",
      div(
        class = "card",
        fluidRow(
          column(
            7,
            h2(contact$name),
            p(contact$role),
            p(contact$email),
            p(contact$mobile),
            p(contact$address)
          ),
          column(
            5,
            div(
              style = "display:flex; justify-content:flex-end; align-items:center; gap:14px; flex-wrap:wrap; padding:10px;",
              tags$img(
                src = if (nzchar(asset("GSP_Logo.webp"))) asset("GSP_Logo.webp") else fallback,
                alt = "Green Sheep Project logo",
                # MANUAL SIZE SETTINGS: edit the width and height values for the GSP logo.
                style = paste0(
                  "width: 180px;",   # Adjust GSP logo width here.
                  "height: 140px;",  # Adjust GSP logo height here.
                  "max-width: 100%;",
                  "object-fit: contain;",
                  "display: inline-block;"
                )
              ),
              tags$img(
                src = if (nzchar(asset("WNRM2.webp"))) asset("WNRM2.webp") else fallback,
                alt = "Wheatbelt Natural Resource Management logo",
                # MANUAL SIZE SETTINGS: edit the width and height values for the WNRM logo.
                style = paste0(
                  "width: 220px;",   # Adjust WNRM logo width here.
                  "height: 160px;",  # Adjust WNRM logo height here.
                  "max-width: 100%;",
                  "object-fit: contain;",
                  "display: inline-block;"
                )
              )
            )
          )
        ),
        div(
          style = "text-align: center; margin-top: 20px;",
          tags$img(
            src = if (nzchar(asset("SheepMerin.webp"))) asset("SheepMerin.webp") else fallback,
            alt = "Lamb image",
            style = "width: 900px; max-width: 100%; height: auto; border-radius: 30px; display: inline-block; object-fit: cover;"
          )
        )
      )
    ),
    
    
    # Copyright footer on every page
    footer = tags$div(
      class = "app-footer",
      tags$p(
        class = "copyright-notice",
        "(c) Copyright: All rights reserved. Free use for personal, non-commercial farm assessment. Contact aali@wheatbeltnrm.org.au | ",
        tags$a(href = "#", onclick = "Shiny.setInputValue('show_terms', Math.random()); return false;", "Terms of Use", style = "color: #2F5D3A;"),
        " | Green Sheep Project Manager"
      )
    )
  )
)

# Complete Server function
server <- function(input, output, session) {
  # Terms modal
  observeEvent(input$show_terms, {
    showModal(modalDialog(
      title = "Terms of Use & Copyright",
      easyClose = TRUE,
      size = "l",
      tags$div(
        style = "text-align: left; font-size: 0.9rem; line-height: 1.6;",
        tags$h4("© 2026. All rights reserved."),
        tags$p("The Natural Capital Assessment Tool is owned by Dr Aram Ali with permission for non-commercial, producer-friendly use."),
        tags$h5("Permitted Use:"),
        tags$ul(
          tags$li("Free use for personal, non-commercial farm assessment"),
          tags$li("Download and print reports for your own farm records"),
          tags$li("Share the Tool's web link with other producers")
        ),
        tags$h5("Prohibited Use:"),
        tags$ul(
          tags$li("Copying or reproducing source code"),
          tags$li("Creating derivative works"),
          tags$li("Commercial use without permission"),
          tags$li("Removing copyright notices")
        ),
        tags$h5("Contact: Dr Aram Ali | aali@wheatbeltnrm.org.au")
      )
    ))
  })
  
  
  observeEvent(input$go_to_farm_inputs, { updateNavbarPage(session, "tabs", selected = "Farm inputs") })
  observeEvent(input$go_ecological_input, { updateNavbarPage(session, "tabs", selected = "Farm inputs") })
  observeEvent(input$go_woody_input, { updateNavbarPage(session, "tabs", selected = "Farm inputs") })
  observeEvent(input$go_aggregation_input, { updateNavbarPage(session, "tabs", selected = "Farm inputs") })
  observeEvent(input$go_proximity_input, { updateNavbarPage(session, "tabs", selected = "Farm inputs") })
  observeEvent(input$go_soil_input, { updateNavbarPage(session, "tabs", selected = "Farm inputs") })
  observeEvent(input$go_riparian_input, { updateNavbarPage(session, "tabs", selected = "Farm inputs") })
  observeEvent(input$go_forage_input, { updateNavbarPage(session, "tabs", selected = "Farm inputs") })
  observeEvent(input$go_habitat_input, { updateNavbarPage(session, "tabs", selected = "Farm inputs") })
  observeEvent(input$go_connectivity_input, { updateNavbarPage(session, "tabs", selected = "Farm inputs") })
  observeEvent(input$go_salt_input, { updateNavbarPage(session, "tabs", selected = "Farm inputs") })
  
  # Farm-input metrics remain on the Farm inputs page. Metrics with scoring-guide
  # sliders are read from those sliders and combined in one assessment object.
  farm_ids <- c("native", "shade", "water", "ghg", "drought", "nutrient", "feed", "welfare", "pasture", "health", "biosecurity", "lambing", "people", "business", "community")
  farm_labels <- c("Native vegetation (%)", "Shade and Shelter (0-5)", "Waterpoint Condition (0-5)", "GHG Climate Plan (0-5)", "Drought Preparedness (0-5)", "Nutrient Efficiency (0-5)", "Feed Efficiency (0-5)", "Animal Welfare (0-5)", "Pasture Planning (0-5)", "Animal Health (0-5)", "Biosecurity (0-5)", "Lambing Management (0-5)", "People and Safety (0-5)", "Business Resilience (0-5)", "Community Engagement (0-5)")
  farm_defaults <- c(15, 5, 5, 1, 3, 3, 3, 4, 3, 4, 3, 3, 4, 3, 3)
  farm_mins <- c(0, rep(0, 14))
  farm_maxs <- c(100, rep(5, 14))
  farm_steps <- c(1, rep(1, 14))
  
  output$inputs <- renderUI({
    tagList(lapply(seq_along(farm_ids), function(i) {
      numericInput(
        inputId = farm_ids[i], label = farm_labels[i], value = farm_defaults[i],
        min = farm_mins[i], max = farm_maxs[i], step = farm_steps[i]
      )
    }))
  })
  
  guide_defaults <- list(
    ecological = 0.50, woody = 0.70, aggregation = 0.35, distance = 0.80,
    soil_median = 70, soil_minimum = 85, soil_stability = 70, riparian = 75,
    forage = 0.60, habitat = 2, connectivity = 2, salt = 3
  )
  
  input_or_default <- function(id, default) {
    value <- input[[id]]
    if (is.null(value) || length(value) != 1L || is.na(value)) return(default)
    if (is.numeric(default)) {
      number <- suppressWarnings(as.numeric(value))
      if (!is.finite(number)) return(default)
      return(number)
    }
    value <- trimws(as.character(value))
    if (!nzchar(value)) default else value
  }
  
  current <- reactive({
    guide_values <- lapply(names(guide_defaults), function(id) input_or_default(id, guide_defaults[[id]]))
    names(guide_values) <- names(guide_defaults)
    farm_values <- lapply(seq_along(farm_ids), function(i) input_or_default(farm_ids[i], farm_defaults[i]))
    names(farm_values) <- farm_ids
    
    c(list(
      farm_name = input_or_default("farm_name", "Farm self-assessment"),
      system = input_or_default("system", "Self-Replacing Merino"),
      area = input_or_default("area", 5000),
      stocking = input_or_default("stocking", 20),
      capacity = input_or_default("capacity", 5)
    ), guide_values, farm_values)
  })
  
  assessment <- eventReactive(input$run, {
    x <- current()
    m <- calc_metrics(x)
    list(metrics = m, domains = calc_domains(m), rec = rec_engine(m), x = x, time = Sys.time())
  }, ignoreNULL = TRUE)
  
  output$status <- renderText({
    if (input$run == 0) "Edit inputs and click Run assessment."
    else paste("Assessment created:", format(assessment()$time, "%d %B %Y %H:%M:%S"))
  })
  
  output$overall <- renderText({ req(assessment()); paste0("Overall score: ", round(safe_mean(assessment()$domains$Score), 1), "/100") })
  output$overall_band <- renderText({ req(assessment()); paste0("Interpretation: ", band(safe_mean(assessment()$domains$Score))) })
  html_results_table <- function(d, caption) {
    shown <- lapply(d, function(column) {
      if (is.numeric(column)) {
        vapply(column, function(value) {
          if (!is.finite(value)) "Not available" else format(value, trim = TRUE, scientific = FALSE)
        }, character(1))
      } else {
        column <- as.character(column)
        column[is.na(column)] <- "Not available"
        column
      }
    })
    div(style = "width:100%;overflow-x:auto;",
        tags$table(class = "table table-striped table-hover table-bordered",
                   style = "width:100%;margin-bottom:20px;",
                   tags$caption(caption),
                   tags$thead(tags$tr(lapply(names(d), function(label) tags$th(scope = "col", label)))),
                   tags$tbody(lapply(seq_len(nrow(d)), function(i) {
                     tags$tr(lapply(shown, function(column) tags$td(column[[i]])))
                   }))
        )
    )
  }
  
  output$metric_row_count_html <- renderText({
    req(assessment())
    d <- assessment()$metrics
    validate(need(is.data.frame(d), "Metric results are not available."))
    paste0("Individual metrics available: ", nrow(d), " of ", length(metric_keys), ".")
  })
  
  output$metric_results_html <- renderUI({
    req(assessment())
    d <- assessment()$metrics
    validate(
      need(is.data.frame(d) && nrow(d) > 0, "Run an assessment to view metric results."),
      need(all(c("Metric", "Raw", "Score", "Interpretation") %in% names(d)),
           "Metric results do not contain the expected columns."),
      need(nrow(d) == length(metric_keys) && setequal(d$Metric, metric_keys),
           "The assessment did not return all 26 metrics. Please rerun the assessment.")
    )
    d <- d[match(metric_keys, d$Metric), , drop = FALSE]
    display <- data.frame(
      Metric = metric_labels[match(d$Metric, metric_keys)],
      `Raw value (0-1)` = d$Raw, `Score (/100)` = d$Score,
      Interpretation = d$Interpretation,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    html_results_table(display, "All individual metrics; no pagination or hidden rows.")
  })
  
  output$domain_results_html <- renderUI({
    req(assessment())
    d <- assessment()$domains
    validate(need(is.data.frame(d) && nrow(d) > 0, "No domain results are available."))
    display <- data.frame(
      Domain = d$Domain, `Score (/100)` = d$Score,
      Interpretation = d$Interpretation,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    html_results_table(display, "Seven domain summaries calculated from the individual metrics.")
  })
  
  output$plot <- renderPlotly({
    req(assessment())
    d <- assessment()$domains
    theta_closed <- c(d$Domain, d$Domain[1])
    r_closed <- c(d$Score, d$Score[1])
    plot_ly(type = "scatterpolar", mode = "lines+markers", r = r_closed, theta = theta_closed, fill = "toself",
            line = list(width = 3), marker = list(size = 8),
            text = paste0(theta_closed, "<br>Score: ", r_closed, "/100"), hoverinfo = "text") %>%
      layout(title = list(text = "Natural Capital Domain Scores"),
             polar = list(radialaxis = list(visible = TRUE, range = c(0, 100), tickvals = c(0, 20, 40, 60, 80, 100)),
                          angularaxis = list(direction = "clockwise")),
             showlegend = FALSE, margin = list(l = 60, r = 60, t = 70, b = 60))
  })
  
  render_cards <- function(df, cls) {
    if (!nrow(df)) return(div(class = cls, p("No metrics currently fall within this category.")))
    tagList(lapply(seq_len(nrow(df)), function(i) {
      div(class = cls,
          h4(gsub("_", " ", df$Metric[i])),
          p(strong("Score: "), paste0(df$Score[i], "/100 - ", df$Interpretation[i])),
          p(strong("Action: "), df$Action[i]),
          p(strong("Mechanism: "), df$Mechanism[i]),
          p(strong("Priority: "), df$Priority[i]),
          p(strong("Timeframe: "), df$Timeframe[i]),
          p(strong("Reference: "), tags$a(href = as.character(df$ReferenceURL[i]), target = "_blank", rel = "noopener noreferrer", df$Reference[i])))
    }))
  }
  
  output$works <- renderUI({ req(assessment()); render_cards(assessment()$rec[assessment()$rec$Score >= 60, , drop = FALSE], "good") })
  output$needs <- renderUI({ req(assessment()); render_cards(assessment()$rec[assessment()$rec$Score < 60, , drop = FALSE], "need") })
  output$improvement <- DT::renderDT({
    req(assessment())
    d <- assessment()$rec
    d$Metric <- gsub("_", " ", d$Metric)
    DT::datatable(d, rownames = FALSE,
                  options = list(pageLength = 10, scrollX = TRUE, order = list()))
  }, server = FALSE)
  
  output$method_metrics <- DT::renderDT({
    DT::datatable(metric_defs, rownames = FALSE,
                  options = list(pageLength = 10, scrollX = TRUE, order = list()))
  }, server = FALSE)
  
  output$reference_links <- renderUI({ tags$ol(lapply(seq_len(nrow(refs)), function(i) {
    tags$li(tags$a(href = refs$URL[i], target = "_blank", rel = "noopener noreferrer", refs$Title[i])) })) })
  
  observeEvent(input$xlsx_browser, {
    
    if (is.null(input$run) || input$run == 0) {
      showNotification(
        "Please click Run assessment before downloading Excel.",
        type = "warning",
        duration = 8
      )
      return(invisible(NULL))
    }
    
    xlsx_file <- tempfile(fileext = ".xlsx")
    on.exit(unlink(xlsx_file), add = TRUE)
    
    notification <- showNotification(
      "Preparing Excel workbook...",
      duration = NULL
    )
    on.exit(removeNotification(notification), add = TRUE)
    
    tryCatch({
      
      a <- assessment()
      
      if (is.null(a) || !is.data.frame(a$metrics)) {
        stop("No assessment results are available.")
      }
      
      input_values <- vapply(
        a$x,
        function(value) {
          if (is.null(value) || !length(value)) {
            return("")
          }
          
          paste(as.character(value), collapse = "; ")
        },
        character(1)
      )
      
      workbook <- list(
        Inputs = data.frame(
          Variable = names(input_values),
          Value = unname(input_values),
          stringsAsFactors = FALSE
        ),
        Metrics = a$metrics,
        Domains = a$domains,
        Recommendations = a$rec,
        Metric_definitions = metric_defs,
        Calculation_methods = calculation_methods,
        Score_methods = score_methods,
        Formulas = formulas,
        References = refs
      )
      
      writexl::write_xlsx(
        x = workbook,
        path = xlsx_file
      )
      
      if (
        !file.exists(xlsx_file) ||
        !is.finite(file.info(xlsx_file)$size) ||
        file.info(xlsx_file)$size < 4
      ) {
        stop("The workbook generator did not create a file.")
      }
      
      # XLSX is a ZIP-based format. Check its initial ZIP signature.
      signature <- local({
        con <- file(xlsx_file, open = "rb")
        on.exit(close(con))
        readBin(con, what = "raw", n = 4L)
      })
      
      if (!identical(
        signature,
        as.raw(c(0x50, 0x4B, 0x03, 0x04))
      )) {
        stop("The generated file does not have the expected XLSX ZIP signature.")
      }
      
      filename <- paste0(
        gsub(
          "[^A-Za-z0-9_]+",
          "_",
          safe(a$x$farm_name)
        ),
        "_assessment_",
        Sys.Date(),
        ".xlsx"
      )
      
      session$sendCustomMessage(
        "natcap_download",
        list(
          filename = filename,
          mime = paste0(
            "application/",
            "vnd.openxmlformats-officedocument.",
            "spreadsheetml.sheet"
          ),
          data = base64enc::base64encode(xlsx_file)
        )
      )
      
    }, error = function(e) {
      
      showNotification(
        paste(
          "Excel export failed:",
          conditionMessage(e)
        ),
        type = "error",
        duration = NULL
      )
      
    })
    
  }, ignoreInit = TRUE)
  
  generate_pdf <- function(file) {
    # Always remove any open PDF device when the download function exits.
    initial_devices <- grDevices::dev.list()
    close_report_devices <- function() {
      for (device in setdiff(grDevices::dev.list(), initial_devices))
        try(grDevices::dev.off(device), silent = TRUE)
    }
    on.exit(close_report_devices(), add = TRUE)
    
    # Build the assessment even if the user has not clicked "Run assessment".
    a <- tryCatch({
      if (is.null(input$run) || input$run == 0) {
        x <- current()
        m <- calc_metrics(x)
        list(metrics = m, domains = calc_domains(m), rec = rec_engine(m),
             x = x, time = Sys.time())
      } else {
        assessment()
      }
    }, error = function(e) NULL)
    
    band_colour <- function(score) {
      if (!is.finite(score)) return("#9E9E9E")
      if (score < 20) return("#A33A45")
      if (score < 40) return("#C96A4A")
      if (score < 60) return("#C49A3A")
      if (score < 80) return("#6E9B60")
      return("#2F5D3A")
    }
    
    clean_metric <- function(x) gsub("_", " ", x)
    
    new_page <- function(header_title = NULL, header_sub = NULL) {
      plot.new()
      par(mar = c(0.35, 0.35, 0.35, 0.35), xaxs = "i", yaxs = "i", usr = c(0, 1, 0, 1))
      if (!is.null(header_title)) {
        rect(0, 0.925, 1, 1, col = brand$dark, border = NA)
        text(0.055, 0.962, header_title, adj = c(0, 0.5), col = "white", cex = 1.12, font = 2)
        if (!is.null(header_sub)) text(0.945, 0.962, header_sub, adj = c(1, 0.5), col = "white", cex = 0.68)
      }
      rect(0, 0, 1, 0.028, col = brand$cream, border = NA)
      text(0.055, 0.014, "Green Sheep Project | Natural Capital Assessment", adj = c(0, 0.5), cex = 0.48, col = "#555555")
      text(0.945, 0.014, format(Sys.Date(), "%d %B %Y"), adj = c(1, 0.5), cex = 0.48, col = "#555555")
    }
    
    wrapped_lines <- function(x, width) strwrap(as.character(x), width = width, simplify = TRUE)
    
    write_paragraph <- function(x, y, width = 108, cex = 0.72, leading = 0.026, colour = brand$text, font = 1) {
      lines <- wrapped_lines(x, width)
      for (line in lines) {
        if (y < 0.09) { new_page(); y <- 0.88 }
        text(0.065, y, line, adj = c(0, 1), cex = cex, col = colour, font = font)
        y <- y - leading
      }
      y - 0.012
    }
    
    write_heading <- function(x, y, cex = 0.98, colour = brand$dark) {
      if (y < 0.13) { new_page(); y <- 0.88 }
      text(0.065, y, x, adj = c(0, 1), cex = cex, font = 2, col = colour)
      lines(x = c(0.065, 0.935), y = c(y - 0.018, y - 0.018), col = brand$green, lwd = 1.4)
      y - 0.042
    }
    
    draw_table <- function(dat, y, widths = NULL, title = NULL, caption = NULL,
                           cex = 0.52, min_row_height = 0.026, max_start_y = 0.86) {
      if (!is.null(title)) y <- write_heading(title, y)
      
      dat <- as.data.frame(dat, stringsAsFactors = FALSE)
      dat[] <- lapply(dat, function(z) gsub("[\r\n]+", " ", as.character(z)))
      
      n <- ncol(dat)
      if (is.null(widths)) widths <- rep(1 / n, n)
      widths <- widths / sum(widths)
      
      char_widths <- pmax(8, round(widths * 150))
      cell_lines <- lapply(seq_len(n), function(j) {
        lapply(dat[[j]], function(z) wrapped_lines(z, char_widths[j]))
      })
      
      row_lines <- sapply(seq_len(nrow(dat)), function(i) {
        max(vapply(seq_len(n), function(j) length(cell_lines[[j]][[i]]), integer(1)))
      })
      row_heights <- pmax(min_row_height, row_lines * 0.021 + 0.012)
      
      draw_header_row <- function(y) {
        rect(0.065, y - 0.032, 0.935, y, col = brand$dark, border = NA)
        x <- 0.065
        for (j in seq_len(n)) {
          text(x + 0.006, y - 0.016, names(dat)[j], adj = c(0, 0.5), cex = cex, font = 2, col = "white")
          x <- x + widths[j] * 0.87
        }
        y - 0.032
      }
      
      if (y > max_start_y) y <- max_start_y
      y <- draw_header_row(y)
      
      for (i in seq_len(nrow(dat))) {
        if (y - row_heights[i] < 0.09) {
          new_page()
          y <- draw_header_row(0.86)
        }
        
        fill <- if (i %% 2 == 0) "#F2F6F0" else "#FFFFFF"
        rect(0.065, y - row_heights[i], 0.935, y, col = fill, border = "#D7E4D8")
        
        x <- 0.065
        for (j in seq_len(n)) {
          lines <- cell_lines[[j]][[i]]
          yy <- y - 0.014
          for (line in lines) {
            text(x + 0.006, yy, line, adj = c(0, 1), cex = cex, col = brand$text)
            yy <- yy - 0.021
          }
          x <- x + widths[j] * 0.87
        }
        y <- y - row_heights[i]
      }
      
      if (!is.null(caption)) {
        y <- y - 0.014
        y <- write_paragraph(caption, y, width = 115, cex = 0.55, leading = 0.020, colour = "#555555")
      }
      y - 0.020
    }
    
    score_figure <- function(dat, label_col, score_col, title) {
      d <- dat[order(dat[[score_col]]), , drop = FALSE]
      labels <- clean_metric(d[[label_col]])
      scores <- as.numeric(d[[score_col]])
      cols <- vapply(scores, band_colour, character(1))
      
      n <- length(scores)
      needed <- 0.055 * n + 0.20
      pages <- max(1, ceiling(needed / 0.72))
      per_page <- ceiling(n / pages)
      
      for (p in seq_len(pages)) {
        idx <- ((p - 1) * per_page + 1):min(n, p * per_page)
        new_page(title, if (pages > 1) paste("Page", p, "of", pages) else NULL)
        par(fig = c(0.05, 0.95, 0.08, 0.86), new = TRUE)
        op <- par(mar = c(4.2, 11.5, 2.5, 2.2))
        barplot(scores[idx], names.arg = labels[idx], horiz = TRUE, las = 1,
                col = cols[idx], border = NA, xlim = c(0, 100),
                xlab = "Score (0-100)", cex.names = 0.62, cex.axis = 0.68, cex.lab = 0.75)
        abline(v = c(20, 40, 60, 80), col = "#B9C8BB", lty = 2, lwd = 0.8)
        par(op)
        text(0.5, 0.055, "Score bands: Very low <20 | Low 20-<40 | Moderate 40-<60 | High 60-<80 | Very high >=80",
             adj = c(0.5, 0.5), cex = 0.52, col = "#555555")
      }
    }
    
    report_ok <- tryCatch({
      grDevices::pdf(file = file, width = 8.27, height = 11.69,
                     paper = "a4", onefile = TRUE, family = "Helvetica",
                     useDingbats = FALSE)
      
      # ---------- Cover ----------
      new_page()
      rect(0, 0.72, 1, 1, col = brand$dark, border = NA)
      rect(0, 0.70, 1, 0.72, col = brand$green, border = NA)
      text(0.5, 0.905, "SUSTAINABLE SHEEP FARMING SYSTEMS", adj = c(0.5, 0.5), cex = 1.55, font = 2, col = "white")
      text(0.5, 0.845, "Farm-Scale Natural Capital Assessment Report", adj = c(0.5, 0.5), cex = 1.05, col = "white")
      text(0.5, 0.805, "", adj = c(0.5, 0.5), cex = 0.70, col = "#EAF2E7")
      
      text(0.5, 0.60, safe(a$x$farm_name), adj = c(0.5, 0.5), cex = 1.65, font = 2, col = brand$dark)
      text(0.5, 0.545, paste(a$x$system, "|", a$x$area, "hectares"), adj = c(0.5, 0.5), cex = 0.85, col = brand$text)
      text(0.5, 0.505, format(a$time, "%d %B %Y"), adj = c(0.5, 0.5), cex = 0.75, col = "#555555")
      
      overall_score <- round(safe_mean(a$domains$Score), 1)
      rect(0.30, 0.375, 0.70, 0.455, col = band_colour(overall_score), border = NA)
      text(0.5, 0.432, "Overall natural capital score", adj = c(0.5, 0.5), cex = 0.72, col = "white")
      text(0.5, 0.398, paste0(overall_score, "/100 | ", band(overall_score)), adj = c(0.5, 0.5), cex = 1.05, font = 2, col = "white")
      
      text(0.5, 0.205, "Prepared by Dr Aram Ali", adj = c(0.5, 0.5), cex = 0.78, font = 2, col = brand$dark)
      text(0.5, 0.172, "Green Sheep Project Manager | Wheatbelt Natural Resource Management Inc.", adj = c(0.5, 0.5), cex = 0.62, col = brand$text)
      text(0.5, 0.142, "aali@wheatbeltnrm.org.au | 0470 488 185", adj = c(0.5, 0.5), cex = 0.62, col = brand$text)
      text(0.5, 0.055, "(c) Copyright: All rights reserved. Free use for personal, non-commercial farm assessment.", adj = c(0.5, 0.5), cex = 0.50, col = "#555555")
      
      # ---------- Abstract ----------
      new_page("Abstract", "Natural Capital Assessment")
      y <- 0.86
      
      strengths <- a$rec[a$rec$Score >= 60, , drop = FALSE]
      priorities <- a$rec[a$rec$Score < 60, , drop = FALSE]
      strengths <- strengths[order(-strengths$Score), , drop = FALSE]
      priorities <- priorities[order(priorities$Score), , drop = FALSE]
      
      strongest <- if (nrow(strengths)) paste(clean_metric(strengths$Metric[1:min(3, nrow(strengths))]), collapse = ", ") else "no metrics currently scored at or above 60"
      priority <- if (nrow(priorities)) paste(clean_metric(priorities$Metric[1:min(3, nrow(priorities))]), collapse = ", ") else "no metrics currently scored below 60"
      
      abstract <- paste0(
        "This report presents a farm-scale natural capital assessment for ", safe(a$x$farm_name),
        ", a ", a$x$system, " enterprise operating over ", a$x$area,
        " hectares. The assessment integrates indicators of ecological condition, native vegetation, soil and forage condition, water and riparian systems, climate readiness, sheep welfare and grazing management, and people and business resilience. ",
        "The overall natural capital score was ", overall_score, " out of 100 (", band(overall_score),
        "). The strongest areas were ", strongest, ". The highest-priority areas for improvement were ", priority,
        ". Results are intended to support practical farm planning, stakeholder communication and ongoing monitoring. They are screening-level indicators and should be interpreted alongside local knowledge, seasonal conditions, paddock history and professional advice where required."
      )
      
      y <- write_paragraph(abstract, y, cex = 0.76, leading = 0.028)
      y <- y - 0.010
      y <- write_heading("Key results", y)
      
      key_results <- data.frame(
        Measure = c("Overall natural capital score", "Overall interpretation",
                    "Strongest performing metrics", "Priority improvement metrics",
                    "Number of metrics at or above 60", "Number of metrics below 60"),
        Result = c(paste0(overall_score, "/100"), band(overall_score),
                   strongest, priority,
                   as.character(sum(a$metrics$Score >= 60)),
                   as.character(sum(a$metrics$Score < 60))),
        stringsAsFactors = FALSE
      )
      y <- draw_table(key_results, y, widths = c(0.42, 0.58), cex = 0.58)
      
      # ---------- Farm profile ----------
      new_page("Farm profile", "Assessment context")
      y <- 0.86
      farm_profile <- data.frame(
        Item = c("Farm name", "System type", "Farm area", "Stocking pressure",
                 "Sustainable capacity", "Assessment date"),
        Detail = c(safe(a$x$farm_name), a$x$system, paste0(a$x$area, " ha"),
                   paste0(a$x$stocking, " DSE/ha"), paste0(a$x$capacity, " DSE/ha"),
                   format(a$time, "%d %B %Y")),
        stringsAsFactors = FALSE
      )
      y <- draw_table(farm_profile, y, widths = c(0.35, 0.65), cex = 0.60)
      
      y <- write_heading("Metric input values", y)
      input_table <- data.frame(
        Variable = clean_metric(names(a$x)),
        Value = as.character(unlist(a$x)),
        stringsAsFactors = FALSE
      )
      y <- draw_table(input_table, y, widths = c(0.55, 0.45), cex = 0.52)
      
      # ---------- Domain results ----------
      new_page("Natural capital domain results", "Overall performance")
      y <- 0.86
      y <- write_paragraph(
        "Domain scores summarise the performance of related natural capital metrics. Scores are reported from 0 to 100, with higher scores indicating stronger condition, management or resilience. The figure and table below identify where the farm is performing strongly and where targeted management may deliver the greatest improvement.",
        y, cex = 0.70, leading = 0.026
      )
      
      score_figure(a$domains, "Domain", "Score", "Figure 1. Natural capital domain scores")
      
      new_page("Natural capital domain results", "Domain scores")
      y <- 0.86
      domain_table <- a$domains
      names(domain_table)[names(domain_table) == "Score"] <- "Score (/100)"
      y <- draw_table(domain_table, y, widths = c(0.58, 0.20, 0.22),
                      title = "Table 1. Domain scores and interpretation", cex = 0.58)
      
      # ---------- Metric results ----------
      new_page("Metric results", "Detailed assessment")
      y <- 0.86
      y <- write_paragraph(
        "Metric scores convert farm-entered indicators to a common 0-100 scale. Scores at or above 60 are interpreted as strengths, while scores below 60 identify opportunities for improvement. The figure and table below present all assessed metrics.",
        y, cex = 0.70, leading = 0.026
      )
      
      score_figure(a$metrics, "Metric", "Score", "Figure 2. Metric scores")
      
      new_page("Metric results", "Metric scores")
      y <- 0.86
      metric_table <- a$metrics
      metric_table$Metric <- clean_metric(metric_table$Metric)
      names(metric_table)[names(metric_table) == "Score"] <- "Score (/100)"
      y <- draw_table(metric_table, y, widths = c(0.42, 0.16, 0.18, 0.24),
                      title = "Table 2. Metric scores and interpretation", cex = 0.52)
      
      # ---------- Strengths ----------
      new_page("What works well", "Strengths")
      y <- 0.86
      if (!nrow(strengths)) {
        y <- write_paragraph("No metrics currently score at or above 60. This suggests substantial opportunities to strengthen natural capital condition and management across the farm.", y, cex = 0.72)
      } else {
        for (i in seq_len(nrow(strengths))) {
          if (y < 0.20) { new_page("What works well", "Strengths"); y <- 0.86 }
          y <- write_heading(paste0(clean_metric(strengths$Metric[i]), " - ", strengths$Score[i], "/100 (", strengths$Interpretation[i], ")"), y, cex = 0.82, colour = brand$green)
          y <- write_paragraph(paste("Action:", strengths$Action[i]), y, cex = 0.66)
          y <- write_paragraph(paste("Why it matters:", strengths$Mechanism[i]), y, cex = 0.66)
          y <- write_paragraph(paste("Priority:", strengths$Priority[i], "| Timeframe:", strengths$Timeframe[i]), y, cex = 0.62, colour = "#555555")
          y <- y - 0.008
        }
      }
      
      # ---------- Improvement plan ----------
      new_page("Improvement plan", "Priority actions")
      y <- 0.86
      y <- write_paragraph(
        "The improvement plan identifies metrics scoring below 60. Actions are ordered from lowest to highest score, helping direct attention to the areas where improvement is likely to provide the greatest benefit.",
        y, cex = 0.70, leading = 0.026
      )
      
      if (!nrow(priorities)) {
        y <- write_paragraph("No metrics currently fall below the improvement threshold of 60.", y, cex = 0.72)
      } else {
        for (i in seq_len(nrow(priorities))) {
          if (y < 0.20) { new_page("Improvement plan", "Priority actions"); y <- 0.86 }
          y <- write_heading(paste0(clean_metric(priorities$Metric[i]), " - ", priorities$Score[i], "/100 (", priorities$Interpretation[i], ")"), y, cex = 0.82, colour = brand$red)
          y <- write_paragraph(paste("Recommended action:", priorities$Action[i]), y, cex = 0.66)
          y <- write_paragraph(paste("Why it matters:", priorities$Mechanism[i]), y, cex = 0.66)
          y <- write_paragraph(paste("Priority:", priorities$Priority[i], "| Timeframe:", priorities$Timeframe[i]), y, cex = 0.62, colour = "#555555")
          y <- write_paragraph(paste("Supporting reference:", priorities$Reference[i]), y, cex = 0.58, colour = "#555555")
          y <- y - 0.008
        }
      }
      
      # ---------- References ----------
      new_page("References", "Supporting sources")
      y <- 0.86
      for (i in seq_len(nrow(refs))) {
        if (y < 0.12) { new_page("References", "Supporting sources"); y <- 0.86 }
        y <- write_paragraph(paste0(i, ". ", refs$Title[i]), y, width = 112, cex = 0.62, leading = 0.022, colour = brand$dark, font = 2)
        y <- write_paragraph(refs$URL[i], y, width = 112, cex = 0.52, leading = 0.020, colour = "#555555")
        y <- y - 0.006
      }
      
      # ---------- Interpretation and contact ----------
      new_page("Interpretation and contact", "Important information")
      y <- 0.86
      y <- write_heading("How to interpret this report", y)
      y <- write_paragraph(
        "This report provides a screening-level assessment of farm-scale natural capital. It combines user-entered indicators with simplified assumptions and proxy measures. It does not replace field survey, remote-sensing analysis, soil testing, biodiversity assessment, emissions accounting, financial analysis, regulatory assessment or professional natural capital accounting.",
        y, cex = 0.70, leading = 0.026
      )
      y <- write_paragraph(
        "Scores should be used to identify relative strengths and priorities, support discussion, and guide monitoring and management decisions. Results will be most useful when reviewed alongside seasonal conditions, farm maps, paddock records and local expert knowledge.",
        y, cex = 0.70, leading = 0.026
      )
      
      y <- write_heading("Contact", y)
      y <- write_paragraph(paste(contact$name, "-", contact$role), y, cex = 0.72, font = 2)
      y <- write_paragraph(contact$email, y, cex = 0.66)
      y <- write_paragraph(contact$mobile, y, cex = 0.66)
      y <- write_paragraph(contact$address, y, cex = 0.66)
      
      y <- write_heading("Disclaimer", y)
      y <- write_paragraph(disclaimer_text, y, cex = 0.58, leading = 0.022)
      
      grDevices::dev.off()
      TRUE
    }, error = function(e) {
      close_report_devices()
      
      # Write a valid one-page PDF so the download is never interpreted as HTML.
      grDevices::pdf(file = file, width = 8.27, height = 11.69,
                     paper = "a4", onefile = TRUE, family = "Helvetica",
                     useDingbats = FALSE)
      plot.new()
      par(mar = c(0.5, 0.5, 0.5, 0.5), usr = c(0, 1, 0, 1))
      rect(0, 0.88, 1, 1, col = brand$dark, border = NA)
      text(0.5, 0.94, "Green Sheep Project", adj = c(0.5, 0.5), cex = 1.4, font = 2, col = "white")
      text(0.5, 0.70, "Natural Capital Assessment Report", adj = c(0.5, 0.5), cex = 1.05, font = 2, col = brand$dark)
      text(0.5, 0.55, "The full report could not be generated because of an error.", adj = c(0.5, 0.5), cex = 0.72, col = brand$text)
      text(0.5, 0.48, paste("Error:", conditionMessage(e)), adj = c(0.5, 0.5), cex = 0.58, col = brand$red)
      text(0.5, 0.30, "Please check the farm inputs and try again.", adj = c(0.5, 0.5), cex = 0.65, col = brand$text)
      text(0.5, 0.10, "aali@wheatbeltnrm.org.au", adj = c(0.5, 0.5), cex = 0.60, col = "#555555")
      grDevices::dev.off()
      
      showModal(modalDialog(
        title = "PDF report could not be generated",
        paste("The full PDF report could not be created. Error:", conditionMessage(e)),
        easyClose = TRUE
      ))
      FALSE
    })
    
    invisible(report_ok)
  }
  
  observeEvent(input$pdf_browser, {
    pdf_file <- tempfile(fileext = ".pdf")
    on.exit(unlink(pdf_file), add = TRUE)
    notification <- showNotification("Preparing PDF report...", duration = NULL)
    on.exit(removeNotification(notification), add = TRUE)
    tryCatch({
      ok <- generate_pdf(pdf_file)
      if (!isTRUE(ok)) return(invisible(NULL))
      if (!file.exists(pdf_file) || file.info(pdf_file)$size < 5)
        stop("No PDF file was generated.")
      con <- file(pdf_file, "rb")
      signature <- rawToChar(readBin(con, "raw", n = 5L))
      close(con)
      if (!identical(signature, "%PDF-")) stop("The generated file is not a PDF.")
      filename <- paste0(gsub("[^A-Za-z0-9_]+", "_", safe(input$farm_name)),
                         "Natural_Capital_Report_", Sys.Date(), ".pdf")
      session$sendCustomMessage("natcap_download", list(
        filename = filename, mime = "application/pdf",
        data = base64enc::base64encode(pdf_file)
      ))
    }, error = function(e) {
      showNotification(paste("PDF export failed:", conditionMessage(e)),
                       type = "error", duration = NULL)
    })
  }, ignoreInit = TRUE)
  
}

shinyApp(ui = ui, server = server)