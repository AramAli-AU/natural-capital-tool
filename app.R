# Startup-optimised ShinyLive edition (2026-11-07)
# Removes Plotly and unused Excel export dependency; preserves assessment,
# detailed scoring guides, DT tables and PDF reporting.
# Save as app.R alongside the existing www/ directory before export.

# ============================================================
# GREEN SHEEP PROJECT - Natural-capital assessment and reporting
# Full Shiny application with natural-capital scoring guides
# Complete single-file Shiny application
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

required <- c("shiny", "bslib", "DT", "png")

missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Install missing packages with: install.packages(c(",
       paste(sprintf("'%s'", missing), collapse = ", "), "))")
}

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(png)
})

brand <- list(dark = "#2F5D3A", green = "#5E8C57", cream = "#F7F8F4", red = "#A33A45", text = "#26352B")

contact <- list(
  name = "Dr Aram Ali",
  role = "Green Sheep Project Manager",
  email = "aali@wheatbeltnrm.org.au, a.aramali@outlook.com",
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
  if (is.null(x) || length(x) == 0L) return("Farm self-assessment")
  x <- as.character(x)[1L]
  if (is.na(x) || !nzchar(trimws(x))) return("Farm self-assessment")
  trimws(x)
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

metric_defs$Category <- ifelse(
  metric_defs$Metric == "Salt Affected Land Management",
  "Natural Asset / Condition Metric",
  metric_defs$Category
)

metric_defs$Priority <- ifelse(metric_defs$Category == "Major Natural Capital Metric", "High (Rainsford et al. 2025)", "Standard")



# ------------------------------------------------------------
# Detailed metric guidance
# ------------------------------------------------------------
# The guidance separates four elements that are useful for producer
# decision-making: what the metric means, why it matters, what to look for,
# and what management actions can influence it. This keeps the assessment
# evidence -> score -> mechanism -> action -> outcome -> monitoring pathway.
metric_guidance <- data.frame(
  Metric = metric_keys,
  Why_it_matters = c(
    "Ecological condition influences the capacity of vegetation, soils and associated habitats to provide ecosystem services. Higher condition can support biodiversity, soil protection, pollination, pest regulation, microclimate regulation and landscape resilience.",
    "Woody vegetation can provide habitat, shelter, shade, carbon storage and microclimate benefits. The useful amount and placement of woody vegetation depend on landscape context, farm production needs, salinity risk and existing native vegetation.",
    "Large, consolidated patches generally retain more interior habitat and can be less vulnerable to fragmentation than many small isolated patches. Aggregation should therefore be considered alongside total woody cover and connectivity.",
    "Shorter functional distances can improve access to shade and shelter and bring ecosystem services closer to productive areas. Distance should be interpreted with the actual distribution and accessibility of vegetation, not distance alone.",
    "Protective cover and soil stability reduce erosion and runoff and help maintain infiltration, soil organic matter and pasture establishment. The lowest-cover period is particularly important because short periods of exposure can create disproportionate soil loss risk.",
    "Riparian vegetation can reduce bank erosion and sediment movement, filter nutrients, improve water quality and provide habitat and movement corridors. The condition and continuity of the buffer matter as well as its mapped extent.",
    "Forage condition is a key link between pasture resources, sheep production, ground cover and resilience. Persistent, productive and palatable pastures can provide more reliable feed while protecting soil during variable seasons.",
    "Retained native vegetation represents an on-farm natural asset and can support biodiversity, ecosystem services and landscape resilience. Extent should be interpreted with vegetation condition, legal requirements and productive land-use context.",
    "Structural habitat features create nesting, refuge, feeding and shelter opportunities for fauna. Maintaining a range of features can increase habitat complexity even where large native vegetation patches are limited.",
    "Connectivity affects movement between habitat patches and can influence recolonisation, gene flow, pollination, pest regulation and ecological resilience. On-farm actions can be more effective when they connect with neighbouring vegetation.",
    "Accessible shade and shelter can reduce heat and cold stress and improve animal welfare. Distribution is important because a large amount of shelter that livestock cannot reach when needed has limited functional value.",
    "Reliable and well-distributed water supports animal welfare and allows grazing pressure to be managed more evenly. Failures, poor water quality or long walking distances can create production and welfare risks.",
    "A documented climate and GHG plan helps identify major emission sources, climate risks, mitigation opportunities and monitoring needs. This is a management-readiness indicator and is not a direct farm emissions inventory.",
    "Drought preparedness reduces the chance that decisions are delayed until feed, water, ground cover or animal condition have already deteriorated. Trigger points and pre-planned actions improve response timing.",
    "Efficient nutrient management can maintain pasture and crop production while reducing nutrient losses, unnecessary expenditure and environmental impacts. Soil and plant information are important for matching inputs to actual requirements.",
    "Feed efficiency links the feed resource to animal output. Better matching of feed supply, animal demand, health and grazing management can improve production efficiency without compromising welfare.",
    "Animal welfare depends on reliable feed and water, health prevention and treatment, suitable handling, shelter, stocking pressure and timely responses to risks. Strong welfare systems also support productivity and reduce avoidable losses.",
    "Pasture planning connects animal demand with feed supply, rest periods and grazing pressure. Good planning helps maintain pasture persistence, ground cover and feed availability through seasonal variation.",
    "Preventive animal-health programs reduce avoidable disease and parasite impacts and support sheep condition, survival and productivity. Monitoring is important because risk changes with season, property and enterprise.",
    "Biosecurity reduces the probability and consequences of introducing or spreading diseases, pests and weeds. Good systems rely on practical controls at livestock entry, visitor access, equipment movement and traceability points.",
    "Lambing management has a direct relationship with ewe welfare, lamb survival and enterprise output. Nutrition, paddock selection, shelter and timely observation should be matched to seasonal and flock conditions.",
    "Safe systems protect people and reduce the likelihood that injuries, poor procedures or emergencies disrupt farm operations. Training, equipment maintenance and clear emergency procedures are practical foundations.",
    "Business resilience supports the farm's ability to absorb and recover from climate, market, production and financial shocks. Good records and scenario planning improve the quality and timing of management decisions.",
    "Community and industry relationships provide access to local knowledge, technical support, coordinated landscape action and learning. Useful engagement is more important than the number of relationships alone.",
    "Stocking fit is a screening indicator of grazing pressure relative to the entered sustainable capacity. It should be interpreted with seasonal feed supply, ground cover, pasture condition and drought conditions rather than treated as a fixed carrying-capacity certification.",
    "Salt-affected land is a natural asset and land-condition issue because salinity can alter soil function, vegetation composition, productivity, erosion risk and habitat. Management determines whether affected areas are stabilised, protected and progressively improved or allowed to deteriorate."
  ),
  Context = c(
    "Use local reference condition where possible. A high score should not be interpreted as proof that every ecosystem service is functioning at its maximum.",
    "There is no universal target percentage for every farm. Consider existing remnants, rainfall, salinity, productive area, biodiversity priorities and landscape context.",
    "A high aggregation score does not compensate for very low woody cover. Consider aggregation together with cover, condition and connectivity.",
    "The calculation gives a higher score to shorter distances. Check whether livestock can actually access the shelter and whether shade is available at the times and locations of greatest need.",
    "Use the most vulnerable season when estimating minimum cover. Ground-cover measurements should ideally be supported by field observation or monitoring data.",
    "If no meaningful riparian area exists, record that limitation rather than treating absence as automatically equivalent to poor condition.",
    "Separate pasture condition from short-term feed-on-offer. Seasonal rainfall and recent grazing history can change the apparent condition of forage.",
    "Native vegetation extent alone does not describe ecological condition. Consider clearing history, vegetation type, condition and statutory requirements.",
    "Some habitat features should be retained even when they have little direct production value, provided they do not create safety or biosecurity hazards.",
    "Connectivity is partly a landscape-scale property. Consider neighbouring remnants and regional corridors, not just the farm boundary.",
    "Consider heat load, prevailing winds, stock movement, lambing locations and waterpoints when judging functional access to shelter.",
    "Water quality and reliability can vary during drought and summer. Distribution should be considered alongside paddock size and grazing patterns.",
    "The metric does not estimate tonnes CO2-e per hectare or emissions intensity. A formal inventory is needed for quantitative GHG claims.",
    "Drought preparedness should be reviewed before each high-risk season and updated when feed, water or market conditions change.",
    "Use locally relevant soil tests, pasture information and agronomic advice when making fertiliser or amendment decisions.",
    "Production efficiency should not be improved by reducing animal welfare, underfeeding or excessive stocking pressure.",
    "Welfare indicators should reflect actual practice, seasonal risks and applicable Australian animal-welfare requirements.",
    "Pasture planning is most useful when feed budgets and grazing observations are updated as seasons change.",
    "Health priorities vary with enterprise, local parasite pressure, vaccination programs and veterinary advice.",
    "Biosecurity controls should be proportionate to the actual disease, weed and livestock-movement risks of the property.",
    "Lambing practices should reflect ewe condition, litter size, weather, shelter availability and the enterprise's lambing system.",
    "Review safety risks for family members, employees, contractors and visitors rather than focusing only on formal employees.",
    "Business resilience is broader than profitability in one season; it includes liquidity, debt exposure, risk management and capacity to respond to disruption.",
    "Engagement should support practical outcomes such as coordinated weed control, habitat restoration, knowledge exchange or access to technical advice.",
    "The result is only as reliable as the sustainable capacity entered by the user. Consider feed supply, seasonal variability and land condition when setting that value.",
    "Salt responses are highly site-specific. Consider soil type, groundwater, salinity severity, rainfall, drainage, species suitability, grazing pressure and available water before changing management."
  ),
  Actions = c(
    "Protect high-condition areas; control priority weeds; limit damaging disturbance; use appropriate grazing exclusions; and restore degraded patches where feasible.",
    "Retain existing woody vegetation; protect remnants; establish strategic plantings or shelterbelts where they address a clear farm or landscape need; and avoid poorly placed plantings on productive land or in areas with other environmental constraints.",
    "Protect large patches from further fragmentation; retain internal habitat; and reconnect smaller patches where a practical corridor or stepping-stone opportunity exists.",
    "Maintain or establish shade and shelter close to production areas; improve distribution where livestock cannot access existing vegetation; and prioritise high-use paddocks and lambing areas.",
    "Maintain protective ground cover; adjust grazing pressure; use tactical spelling; protect vulnerable soils; and monitor minimum cover during high-risk periods.",
    "Fence or manage riparian areas where stock pressure is damaging condition; maintain buffer vegetation; stabilise priority erosion points; and revegetate important gaps.",
    "Maintain persistent desirable pasture species; manage grazing pressure; address pasture decline early; and use feed budgeting to align demand with pasture supply.",
    "Retain native vegetation; prevent avoidable clearing; protect remnant condition; and prioritise restoration where it improves ecological function or landscape linkage.",
    "Retain logs, litter, shrubs, mature trees, hollows and other safe structural features; and avoid unnecessary removal of habitat elements.",
    "Protect existing links; retain riparian and shelterbelt corridors; and establish stepping-stone or corridor vegetation where it provides a clear connectivity benefit.",
    "Maintain accessible shade and wind protection; improve distribution near stock concentration areas; and plan for shade and shelter before heat or cold events.",
    "Audit troughs, tanks, dams, pumps, pipelines and water quality; repair leaks and weak points; maintain reserve capacity; and improve distribution where grazing pressure is uneven.",
    "Document major emissions sources and climate risks; identify practical mitigation or adaptation actions; set responsibilities; and review progress annually.",
    "Set drought trigger points; monitor feed, water, ground cover and animal condition; prepare destocking and feeding options; and act before critical thresholds are reached.",
    "Use soil and plant information to match nutrient inputs to crop or pasture demand; keep nutrient records; and review application timing and loss risks.",
    "Match feed supply to animal demand; reduce avoidable feed wastage; maintain animal health; and use pasture and animal records to identify efficiency opportunities.",
    "Maintain reliable feed and water; monitor condition and health; provide suitable shelter; use low-stress handling; and respond promptly to welfare risks.",
    "Use feed budgets, grazing plans and rest periods; monitor pasture response; and adjust allocation when seasonal conditions change.",
    "Maintain vaccination and parasite-control programs; monitor animal condition; keep health records; and seek veterinary advice when disease risk or unexplained losses arise.",
    "Strengthen quarantine, traceability, visitor controls, livestock-entry procedures, equipment hygiene and weed-seed movement controls.",
    "Plan lambing paddocks and nutrition; provide appropriate shelter; monitor ewes and lambs; and record lamb survival to identify recurring losses.",
    "Maintain induction and training; review machinery, chemical and livestock-handling risks; keep emergency procedures current; and ensure appropriate protective equipment is available.",
    "Maintain useful financial and production records; monitor cash flow; test key scenarios; maintain appropriate risk buffers; and review exposure to major climate and market risks.",
    "Maintain useful relationships with NRM organisations, Landcare groups, neighbours, advisers and industry networks where they contribute to practical farm or landscape outcomes.",
    "Review actual stocking pressure against realistic sustainable capacity; reduce pressure when feed or ground cover is limiting; and increase the feed base before increasing stock numbers.",
    "Map and monitor saline areas; protect vulnerable land from damaging grazing; establish suitable salt-tolerant vegetation where appropriate; control erosion and bare areas; and review progress over time."
  ),
  Mechanism = c(
    "Maintaining vegetation structure and reducing disturbance preserves habitat, soil protection and ecosystem-service capacity.",
    "Woody vegetation modifies microclimate, provides structure and habitat, stores carbon and can improve shelter and connectivity when appropriately located.",
    "Consolidated patches reduce edge effects and can retain larger areas of interior habitat, while linked patches can improve movement across fragmented landscapes.",
    "Reducing functional distance increases the likelihood that livestock and beneficial organisms can use shelter and vegetation-associated services.",
    "Ground cover intercepts rainfall, reduces runoff and detachment, and supports infiltration and soil biological function.",
    "Vegetated buffers slow overland flow, trap sediment and nutrients, stabilise banks and provide habitat along water-associated areas.",
    "Persistent forage maintains feed supply and ground cover, reducing the risk that grazing pressure drives pasture decline and soil exposure.",
    "Retaining native vegetation preserves ecosystem structure and the habitat and regulating functions associated with native ecosystems.",
    "Structural complexity creates more niches and refuge opportunities for wildlife and beneficial organisms.",
    "Connected habitat provides movement pathways and reduces isolation between suitable patches.",
    "Shade and shelter reduce thermal and wind exposure, helping animals maintain body condition and welfare during stressful weather.",
    "Reliable water reduces welfare risk and allows grazing to be distributed more effectively across the farm.",
    "A documented plan turns climate and emissions risks into identifiable management actions and measurable follow-up tasks.",
    "Early decisions can prevent feed and ground-cover deficits from becoming severe enough to cause larger production, welfare and land-condition losses.",
    "Matching nutrient supply to demand improves nutrient-use efficiency while reducing the amount available for loss through runoff, leaching or volatilisation.",
    "Better matching of feed supply and animal demand can improve output per unit of feed while maintaining health and welfare.",
    "Preventive management reduces exposure to avoidable welfare hazards and allows earlier intervention when problems arise.",
    "Planned grazing reduces repeated pressure on the same pasture areas and helps maintain recovery and ground cover.",
    "Prevention and early detection reduce disease and parasite impacts before they become larger production or welfare problems.",
    "Entry controls and hygiene reduce opportunities for biological threats to enter or spread through livestock, people, vehicles and equipment.",
    "Good lambing management improves the probability that ewes and lambs have adequate nutrition, shelter and timely intervention during a high-risk production period.",
    "Clear procedures, training and maintained equipment reduce exposure to preventable hazards and limit disruption after incidents.",
    "Records and scenario planning improve the speed and quality of decisions when climate, market or production conditions change.",
    "Knowledge exchange and coordinated action can increase access to information, technical support and landscape-scale outcomes.",
    "Keeping grazing pressure within the feed and land base reduces pasture depletion and soil-cover loss; exceeding capacity increases these risks.",
    "Salinity-aware vegetation and grazing management can maintain cover, reduce erosion and stabilise productive use of affected land while limiting further degradation."
  ),
  Expected_outcome = c(
    "More stable ecological function, habitat value and protection of soil and vegetation assets.",
    "Better habitat and shelter distribution, with potential benefits for biodiversity, livestock protection and landscape resilience.",
    "Less fragmentation and stronger functional habitat structure where consolidation and reconnection are feasible.",
    "Improved access to shade and shelter and better distribution of vegetation-related ecosystem services.",
    "More persistent ground cover, lower erosion risk and stronger soil protection through variable seasons.",
    "Improved bank stability, water-quality protection and habitat continuity.",
    "More reliable pasture productivity, persistence and feed availability.",
    "Retention of native ecological assets and improved opportunities for biodiversity and ecosystem services.",
    "Greater habitat complexity and more opportunities for fauna and beneficial organisms.",
    "Improved movement pathways and stronger links between habitat patches.",
    "Reduced weather stress and better livestock access to functional shelter.",
    "More reliable water supply and more even distribution of grazing pressure.",
    "Clearer climate-risk management and a stronger basis for future quantitative emissions accounting.",
    "Earlier drought responses, reduced emergency decisions and better protection of feed, animals and ground cover.",
    "Better nutrient-use efficiency with less unnecessary input and loss risk.",
    "Improved production efficiency without compromising welfare.",
    "Reduced avoidable welfare risks and stronger animal health and productivity outcomes.",
    "Better pasture recovery, feed planning and ground-cover protection.",
    "Lower avoidable health losses and improved flock condition and productivity.",
    "Lower risk of introducing or spreading diseases, pests and weeds.",
    "Improved lamb survival, ewe welfare and identification of recurring lambing losses.",
    "Fewer preventable incidents and stronger workforce readiness.",
    "Greater capacity to absorb and respond to financial, market and climate shocks.",
    "Stronger access to knowledge, support and coordinated landscape outcomes.",
    "Grazing pressure better matched to feed and land capability, reducing degradation risk.",
    "More stable saline areas, better ground cover and improved productive or ecological function where site conditions allow."
  ),
  Monitoring = c(
    "Reassess condition after major disturbance, restoration or significant seasonal change; use repeat field observations or mapped evidence where available.",
    "Track woody cover and condition periodically using farm maps, imagery or repeat field observations.",
    "Monitor patch size, fragmentation and links between patches after clearing, restoration or revegetation projects.",
    "Review distance and accessibility after vegetation establishment and during high-heat or high-shelter-demand periods.",
    "Monitor median and minimum ground cover through vulnerable seasons and after major grazing or rainfall events.",
    "Inspect riparian vegetation, bank condition, bare areas and stock-access pressure before and after management changes.",
    "Track pasture composition, persistence, cover and feed availability across seasons.",
    "Update native vegetation mapping after clearing, restoration or major disturbance and verify condition in priority areas.",
    "Record retained habitat features and inspect their condition during farm planning or biodiversity checks.",
    "Review connectivity after revegetation, clearing or major infrastructure changes and consider landscape-scale links.",
    "Check shade availability and stock access before and during hot or cold periods.",
    "Test water reliability and quality, inspect infrastructure routinely and record failures and repairs.",
    "Review climate risks, actions and indicators at least annually and update emissions data when a quantitative inventory is available.",
    "Review trigger points and drought plans before high-risk seasons and record decisions made during drought.",
    "Retain soil-test and input records and review nutrient budgets against production outcomes.",
    "Track animal performance, feed use and pasture outcomes where reliable records are available.",
    "Review welfare risks routinely and record significant incidents, treatments and corrective actions.",
    "Update feed budgets and grazing plans as seasonal conditions change and compare planned versus observed pasture response.",
    "Review health records, vaccination status, parasite indicators and unexplained mortality or condition losses.",
    "Audit quarantine, traceability, visitor and equipment controls periodically and after relevant incidents.",
    "Record lambing outcomes, lamb survival and recurring causes of loss to guide future management.",
    "Review incidents, near misses, training records and emergency procedures regularly.",
    "Review cash flow, key financial ratios or indicators, debt exposure and scenario assumptions at appropriate intervals.",
    "Record practical outcomes from engagement such as advice adopted, coordinated works, training or restoration activities.",
    "Review stocking pressure against pasture condition, feed availability, ground cover and seasonal outlook rather than using stocking rate alone.",
    "Map saline areas and repeat observations of vegetation cover, bare soil, erosion and management effectiveness through time."
  ),
  stringsAsFactors = FALSE
)

formulas <- data.frame(
  Component = c("Ecological condition", "Woody vegetation cover", "Aggregation", "Proximity", "Soil condition",
                "Riparian buffer condition", "Forage condition", "Native vegetation extent", "Metric score",
                "Domain score", "Overall score", "Stocking fit", "Salt affected land"),
  Formula = c("EC = sum(Ai x Wi) / Afarm", "Woody cover = Awood / Afarm", "Aggregation = farm-entered proxy (0-1)",
              "Proximity = piecewise distance-to-score index: <300 m = 0.70-1.00; 300-500 m = 0.40-0.69; >500 m = 0-0.39, declining to 0 at 1,000 m and remaining 0 beyond 1,000 m.", "Soil = mean(P50, P10, stability) / 100", "Riparian = riparian % / 100",
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
                              "Piecewise proximity index based on straight-line distance: <300 m = 0.70-1.00; 300-500 m = 0.40-0.69; >500 m = 0-0.39, declining to 0 at 1,000 m and remaining 0 beyond 1,000 m.",
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
  App_implementation = c("Farm-entered ecological condition proxy.", "Woody percentage / 100.",
                         "Farm-entered aggregation proxy.", "Piecewise distance-to-score conversion using the 300 m and 500 m thresholds shown in the Proximity scoring guide.",
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

# Proximity scoring follows the producer-facing scoring guide exactly:
#   <300 m    -> 0.70-1.00 (highly desirable)
#   300-500 m -> 0.40-0.69 (desirable)
#   >500 m    -> 0.00-0.39 (undesirable)
# The score is continuous within each class. At 300 m and 500 m the score
# changes class, so the boundary values are 0.69 and 0.39 respectively.
# For distances above 500 m, the score declines linearly to 0 at 1,000 m
# and remains 0 for greater distances.
proximity_score <- function(distance_m) {
  d <- suppressWarnings(as.numeric(distance_m))
  if (!is.finite(d)) return(NA_real_)
  d <- max(0, d)
  
  if (d < 300) {
    # 0 m = 1.00; just below 300 m approaches 0.70.
    score <- 1.00 - (0.30 * d / 300)
  } else if (d <= 500) {
    # 300 m = 0.69; 500 m = 0.40.
    score <- 0.69 - (0.29 * (d - 300) / 200)
  } else if (d < 1000) {
    # >500 m = 0.39 down to 0 at 1,000 m.
    score <- 0.39 * (1000 - d) / 500
  } else {
    score <- 0
  }
  
  clamp(score)
}

calc_metrics <- function(x) {
  prox <- proximity_score(x$distance)
  soil <- safe_mean(c(x$soil_median, x$soil_minimum, x$soil_stability)) / 100
  fit <- if (is.finite(x$capacity) && x$capacity > 0) {
    clamp(1 - abs(x$stocking - x$capacity) / x$capacity)
  } else {
    NA_real_
  }
  
  z <- c(
    Ecological_Condition = x$ecological,
    Woody_Vegetation_Cover = x$woody / 100,
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
    "Salt-tolerant vegetation and controlled grazing stabilise saline areas, prevent erosion, maintain productivity and reduce further salinity spread. About 2.5 million hectares of Australian agricultural land are affected by dryland salinity, including about 1.8 million hectares in Western Australia which is over 70% of the national affected area."),
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
  
  # Add the detailed guidance by explicit metric-key matching. This avoids
  # duplicate .x/.y columns and guarantees that Mechanism, Actions, Expected
  # outcome and Monitoring are populated for every metric.
  gi <- match(out$Metric, metric_guidance$Metric)
  guidance_cols <- c(
    "Why_it_matters", "Context", "Actions", "Mechanism",
    "Expected_outcome", "Monitoring"
  )
  for (nm in guidance_cols) {
    out[[nm]] <- metric_guidance[[nm]][gi]
  }
  
  out$Reference <- refs$Title[out$ReferenceIndex]
  out$ReferenceURL <- refs$URL[out$ReferenceIndex]
  out$ReferenceIndex <- NULL
  
  # Keep the original assessment metric order.
  out <- out[match(m$Metric, out$Metric), , drop = FALSE]
  rownames(out) <- NULL
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
               loop = NA, muted = NA, playsinline = NA, preload = "none",
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
  ".logos{display:flex;gap:12px;justify-content:left;align-items:left;flex-wrap:wrap;}",
  ".logos img{max-height:65px;max-width:35%;width:auto;border-radius:11px;padding:1px;background:rgba(255,255,255,0.25);box-shadow:0 2px 10px rgba(0,0,0,0.08);}",
  ".project-image{width:100%;max-height:320px;object-fit:contain;background:#F2F5EF;border-radius:14px;margin:8px 0;}",
  ".definition{display:flex;gap:15px;border-left:5px solid #4E8C57;background:#F4F8F2;padding:14px;margin:9px 0;border-radius:10px;}",
  ".definition h4{margin-top:0;color:#193B2A;}",
  ".index{font-size:1.3rem;font-weight:700;color:#C49A3A;}",
  ".good{border-left:5px solid #4E8C57;background:#F0F8F0;padding:14px;margin:9px 0;border-radius:10px;}",
  ".need{border-left:5px solid #A33A45;background:#FFF3F3;padding:14px;margin:9px 0;border-radius:10px;}",
  ".natural-capital-figure{width:100%;max-width:1200px;height:auto;display:block;margin:10px auto 18px auto;border-radius:12px;background:#FFFFFF;border:1px solid #D7E4D8;padding:8px;}",
  ".metric-score-image{width:100%;max-width:950px;height:auto;display:block;margin:12px auto 20px auto;border-radius:12px;border:1px solid #D7E4D8;background:#FFFFFF;padding:6px;}",
  ".guide-score-control{background:#F4F8F2;border:1px solid #D7E4D8;border-radius:12px;padding:14px 18px;margin:0 auto 18px auto;max-width:950px;box-shadow:0 2px 8px rgba(25,59,42,.06);}",
  ".guide-score-control h4{margin-top:0;color:#193B2A;}",
  ".guide-slider{margin:8px 0 16px 0;}",
  ".guide-slider .form-group{margin-bottom:8px;}",
  ".image-missing{padding:12px;margin:10px 0;background:#FFF3F3;border-left:5px solid #A33A45;border-radius:8px;color:#7A202A;}",
  ".disclaimer-box{margin-top:18px;padding:14px 16px;background:#FFF8E8;border-left:5px solid #C49A3A;border-radius:10px;color:#26352B;font-size:0.95rem;line-height:1.55;}",
  ".disclaimer-box strong{color:#193B2A;}",
  ".app-footer{margin-top:40px;padding:20px;background:linear-gradient(to right,#f9f9f9,#f0f0f0);border-top:3px solid #2F5D3A;text-align:center;color:#555;font-size:0.8rem;}",
  ".app-footer a{color:#2F5D3A;text-decoration:none;font-weight:600;}",
  ".app-footer a:hover{color:#4E8C57;text-decoration:underline;}",
  ".copyright-notice{margin:0;padding:0;}"
)

guide_panel <- function(title, metric_title, description, image_name, image_alt, observations, score_text, slider_specs) {
  slider_ui <- lapply(slider_specs, function(s) {
    div(
      class = "guide-slider",
      sliderInput(
        inputId = s$id,
        label = s$label,
        min = s$min,
        max = s$max,
        value = s$value,
        step = s$step,
        width = "100%"
      )
    )
  })
  
  # metric_guidance uses the internal metric keys, while the scoring-guide
  # headings use producer-friendly metric labels. Match through metric_labels
  # so the detailed guidance is always retrieved correctly.
  metric_key <- metric_keys[match(metric_title, metric_labels)]
  if (is.na(metric_key) || !nzchar(metric_key)) metric_key <- metric_title
  g <- metric_guidance[match(metric_key, metric_guidance$Metric), , drop = FALSE]
  
  get_guidance <- function(column) {
    if (!nrow(g)) return("Guidance not available.")
    value <- as.character(g[[column]][1])
    if (is.na(value) || !nzchar(trimws(value))) "Guidance not available." else value
  }
  
  why <- get_guidance("Why_it_matters")
  context <- get_guidance("Context")
  actions <- get_guidance("Actions")
  mechanism <- get_guidance("Mechanism")
  expected <- get_guidance("Expected_outcome")
  monitoring <- get_guidance("Monitoring")
  
  bslib::accordion_panel(
    title,
    h3(metric_title),
    h4("What is this metric?"),
    p(description),
    h4("Why does it matter?"),
    p(why),
    h4("Context and interpretation"),
    p(context),
    metric_score_image(image_name, image_alt),
    div(
      class = "guide-score-control",
      h4("Set your score"),
      slider_ui
    ),
    p(score_text),
    h4("What to look for"),
    tags$ul(lapply(observations, tags$li)),
    h4("What can you do?"),
    p(actions),
    h4("Mechanism"),
    p(mechanism),
    h4("Expected outcome"),
    p(expected),
    h4("What should you monitor?"),
    p(monitoring)
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

# Application UI begins here
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
  tags$head(tags$style(HTML(css))),
  
  div(
    class = "hero",
    fluidRow(
      column(
        8,
        # SusSheepSys header image — adjustable size and rounded edges
        tags$img(
          src = asset("SusSheepSys.png"),
          
          style = paste0(
            "width: 450px;",          # Adjust width
            "height: 130px;",          # Adjust height
            "object-fit: contain;",   # Preserve image proportions
            "border-radius: 80px;",   # Rounded corners
            "overflow: hidden;",
            "display: block;"
          )
        ),
        p("Natural-capital assessment and reporting.")
      ),
      column(
        4,
        div(
          class = "logos",
          # SheepLogo2.png — top-right header position
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
              src = if (nzchar(asset("SheepLogo2.png"))) {
                asset("SheepLogo2.png")
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
        column(5, div(class = "card", h3(""), project_video(asset("SheepVideo.mov"))))
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
        p("Use these visual guides to assess the image-backed natural asset and land-condition metrics. Each guide explains what the metric means, why it matters, what to look for, how to score it, practical actions, the management mechanism and what to monitor. Metrics without a scoring-guide image are entered in Farm & Management Inputs."),
        div(class = "disclaimer-box", tags$strong("Natural asset / condition focus: "), "Ecological Condition, Woody Vegetation Cover, Aggregation, Proximity, Soil Condition, Riparian Buffer Condition, Forage Condition, Native Vegetation Extent, Habitat Features, Landscape Connectivity and Salt Affected Land Management are treated as natural asset or land-condition indicators. Salt-affected land is included here because salinity is a land-condition constraint; the score still recognises the importance of management response."),
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
            "Ecological Condition.png",
            "Ecological condition scoring guide",
            c("Amount and continuity of native vegetation cover", "Presence of trees, shrubs, native grasses and groundcover", "Evidence of erosion, bare soil, compaction or disturbance", "Weed dominance or loss of native species", "Habitat complexity, including logs, litter, hollows and mature trees"),
            "Set the Ecological Condition score using the slider directly below the image. A score near 0 represents severely modified or degraded land; a score near 1 represents a healthy, minimally modified native ecosystem.",
            list(list(id = "ecological", label = "Ecological Condition (0-1)", min = 0, max = 1, value = 0.50, step = 0.01))
          ),
          
          guide_panel(
            "2. Woody Vegetation Cover",
            "Woody Vegetation Cover",
            "Woody vegetation cover is the percentage of the farm occupied by trees, shrubs, native bushland, shelterbelts, revegetation and other woody vegetation.",
            "Woody Vegetation Cover.png",
            "Woody vegetation cover scoring guide",
            c("Remnant native woodland, shrubland or forest", "Shelterbelts and planted vegetation", "Scattered paddock trees and shrub patches", "Woody vegetation along drainage lines, dams and waterways", "Total woody vegetation area relative to total farm area"),
            "Enter the estimated percentage of total farm area covered by woody vegetation. For example, 10 means approximately 10% of the farm has tree or shrub cover.",
            list(list(id = "woody", label = "Woody Vegetation Cover (%)", min = 0, max = 100, value = 70, step = 1))
          ),
          
          guide_panel(
            "3. Aggregation",
            "Aggregation",
            "Aggregation describes whether woody vegetation is retained in large, connected patches or divided into many small and isolated fragments. The score represents the concentration and consolidation of woody vegetation across the farm, rather than the total amount of woody cover.",
            "Aggregation.png",
            "Aggregation scoring guide",
            c("Whether woody vegetation occurs in one or several large patches", "The presence of isolated small remnants versus consolidated vegetation", "The amount of interior habitat away from patch edges", "Fragmentation caused by clearing, tracks, cultivation or infrastructure", "Opportunities to protect large remnants or reconnect smaller patches"),
            "Enter a score from 0 to 1. A score near 0 represents strongly fragmented or dispersed woody vegetation; a score near 1 represents woody vegetation concentrated in large, contiguous patches.",
            list(list(id = "aggregation", label = "Aggregation (0-1)", min = 0, max = 1, value = 0.35, step = 0.01))
          ),
          
          guide_panel(
            "4. Proximity to Woody Vegetation",
            "Proximity",
            "Proximity measures how close production areas are to trees and shrubs. Shorter distances can improve livestock access to shade and shelter and increase the reach of ecosystem services such as pollination and pest control.",
            "Proximity.png",
            "Proximity scoring guide",
            c("Distance from grazed paddocks to trees, shrubs or shelterbelts", "Accessibility of shade and shelter for sheep", "Whether vegetation is distributed across the farm or confined to one area", "Distance from lambing paddocks and stock congregation areas to shelter"),
            "Enter the approximate mean straight-line distance in metres from the centre of production paddocks to the nearest tree line or woody vegetation patch. The scoring follows the guide shown above: distances below 300 m are scored 0.70-1.00, 300-500 m are scored 0.40-0.69, and distances above 500 m are scored 0-0.39, declining to 0 at 1,000 m.",
            list(list(id = "distance", label = "Mean straight-line distance from production paddocks to woody vegetation (m)", min = 0, max = 1000, value = 300, step = 1))
          ),
          
          guide_panel(
            "5. Soil Condition",
            "Soil Condition",
            "Soil condition is represented in this tool using ground cover and soil stability indicators. Good cover helps reduce erosion, improve infiltration, protect soil organic matter and support pasture production.",
            "Soil Condition.png",
            "Soil condition scoring guide",
            c("Median or typical ground cover across seasons", "Lowest ground cover during the most vulnerable period", "Bare soil, erosion, scalding or runoff pathways", "Litter, stubble, living pasture and other protective cover", "Signs that soil remains stable under grazing and rainfall"),
            "Enter estimated percentages for median ground cover, minimum ground cover and soil stability. The tool averages these three values to generate the Soil Condition score.",
            list(
              list(id = "soil_median", label = "Median / typical ground cover (%)", min = 0, max = 100, value = 70, step = 1),
              list(id = "soil_minimum", label = "Minimum ground cover (%)", min = 0, max = 100, value = 85, step = 1),
              list(id = "soil_stability", label = "Soil stability (%)", min = 0, max = 100, value = 70, step = 1)
            )
          ),
          
          guide_panel(
            "6. Riparian Condition",
            "Riparian Buffer Condition",
            "Riparian condition refers to vegetation along waterways, drainage lines, wetlands, dams and other water-associated areas. Healthy buffers help protect water quality, stabilise banks and provide habitat.",
            "Riparian Condition.png",
            "Riparian condition scoring guide",
            c("Woody vegetation beside streams, drainage lines and wetlands", "Vegetation continuity along riparian areas", "Bare banks, erosion, pugging or heavy stock access", "Gaps requiring fencing, revegetation or grazing management", "Evidence that the buffer filters sediment and slows runoff"),
            "Enter the estimated percentage of mapped riparian area with woody vegetation cover. If the farm has no riparian areas, record this limitation in the assessment notes.",
            list(list(id = "riparian", label = "Riparian woody vegetation cover (%)", min = 0, max = 100, value = 75, step = 1))
          ),
          
          guide_panel(
            "7. Forage Condition",
            "Forage Condition",
            "Forage condition reflects the quality, persistence, palatability and productivity of pasture available to livestock. It is distinct from short-term feed quantity because it considers the condition of the pasture resource.",
            "Forage Condition.png",
            "Forage condition scoring guide",
            c("Pasture cover, productivity and persistence", "Perennial species and desirable pasture composition", "Palatability and feed quality for sheep", "Evidence of repeated overgrazing or pasture decline", "Capacity to maintain feed and ground cover during dry periods"),
            "Enter a score from 0 to 1. A score near 0 indicates poor or degraded forage resources; a score near 1 indicates productive, persistent and resilient pasture.",
            list(list(id = "forage", label = "Forage Condition (0-1)", min = 0, max = 1, value = 0.60, step = 0.01))
          ),
          
          guide_panel(
            "8. Habitat Condition",
            "Habitat Features",
            "Habitat condition captures the diversity of structural features used by wildlife, including mature trees, hollows, shrubs, litter, rocks, logs, native grasses and dense ground cover.",
            "Habitat Condition.png",
            "Habitat condition scoring guide",
            c("Old trees, tree hollows and standing dead trees where safe to retain", "Fallen timber, logs, leaf litter and rocks", "Native shrubs, grasses and flowering plants", "Multiple vegetation layers from groundcover to canopy", "Habitat features distributed across the farm rather than isolated"),
            "Enter a score from 0 to 5 for Habitat Features. A score of 0 represents very limited habitat structure, while 5 represents diverse and well-retained habitat features.",
            list(list(id = "habitat", label = "Habitat Features (0-5)", min = 0, max = 5, value = 2, step = 1))
          ),
          
          guide_panel(
            "9. Landscape Connectivity",
            "Landscape Connectivity",
            "Landscape connectivity describes how well vegetation and habitat patches are linked across the farm and into the wider landscape. Connected areas can support wildlife movement, dispersal, gene flow, pollination, pest suppression and ecological resilience.",
            "Landscape Connectivity.png",
            "Landscape connectivity scoring guide",
            c("Continuous native vegetation, shelterbelts or riparian corridors", "Stepping-stone patches that link larger remnants", "Gaps between habitat patches and barriers to movement", "Connections between on-farm vegetation and neighbouring bushland", "Opportunities for corridor plantings, buffer restoration or protection of linkages"),
            "Enter a score from 0 to 5. A score of 0 represents isolated habitat with little functional linkage; a score of 5 represents a well-connected network of habitat patches across the farm and surrounding landscape.",
            list(list(id = "connectivity", label = "Landscape Connectivity (0-5)", min = 0, max = 5, value = 2, step = 1))
          ),
          
          guide_panel(
            "10. Salt Affected Land Management",
            "Salt Affected Land Management",
            "This metric considers how salt-affected areas are managed to maintain ground cover and productive use while limiting further degradation. It focuses on practical management, not simply the area of saline land.",
            "Saltlands.png",
            "Salt affected land management scoring guide",
            c("Salt-affected areas are identified and monitored", "Fencing or controlled grazing protects vulnerable saline land", "Salt-tolerant species or suitable saltland pastures are established where appropriate", "Bare saline patches, erosion and grazing pressure are actively managed", "Management is reviewed and adapted to local soil, water and seasonal conditions"),
            "Enter a management score from 0 to 5. A low score indicates little planning or active management; a high score indicates an established, monitored and adaptive approach. If no salt-affected land is known, use the score that best reflects whether risk has been assessed and the absence of affected areas verified.",
            list(list(id = "salt", label = "Salt Affected Land Management (0-5)", min = 0, max = 5, value = 3, step = 1))
          )
        ),
        
        br(),
        div(
          style = "text-align:center;",
          actionButton("go_to_farm_inputs", "Continue to Farm & Management Inputs", class = "btn-primary btn-lg")
        )
      )
    ),
    
    tabPanel(
      "Farm & Management Inputs",
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
          "The image-backed natural asset and land-condition metrics are scored directly on the Scoring guides page using sliders. Use this page for the remaining farm and management inputs that do not have a scoring-guide image."
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
      div(
        class = "card",
        h2(textOutput("overall")),
        p(textOutput("overall_band")),
        DTOutput("metrics"),
        h3("Why these scores?"),
        p("Use the explanations below to connect each score with the underlying concept, context, management mechanism and practical monitoring. The score is a screening result based on the information entered by the user."),
        uiOutput("score_explanations"),
        DTOutput("domains"),
        uiOutput("plot")
      )
    ),
    
    tabPanel(
      "Recommendations",
      div(
        class = "card",
        h2("Strengths to maintain"),
        p("Metrics at or above 60 are presented as areas to maintain or strengthen. A high score does not mean no further action is needed; continue monitoring and adapt management to seasonal and farm-specific conditions."),
        uiOutput("works"),
        h2("Where management attention may help"),
        p("Metrics below 60 are presented as potential areas for management attention. The score is a screening indicator, so use the context, mechanism and farm circumstances before deciding whether an action is appropriate."),
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
        h3("Scoring and interpretation"),
        DTOutput("score_methods"),
        h3("Metric definitions"),
        DTOutput("method_metrics"),
        h3("Detailed metric guidance"),
        DTOutput("metric_guidance_table"),
        h3("References"),
        uiOutput("reference_links")
      )
    ),
    
    tabPanel(
      "Report",
      div(
        class = "card",
        h2("Full PDF report"),
        p("The professional PDF report presents the farm profile, scoring-guide inputs, domain results, all 26 metric results and descriptions, integrated management priorities, monitoring actions, references and the assessment limitations. "),
        downloadButton("pdf", "Download full PDF report"),
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
    ##         p("Email: aali@wheatbeltnrm.org.au"),
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
    ##              "© 2026 All rights reserved. Free use for personal, non-commercial farm assessment. Contact aali@wheatbeltnrm.org.au.")
    ##       )
    ##     ),
    
    tabPanel(
      "Contact",
      div(
        class = "card",
        div(
          style = "display:flex; flex-wrap:wrap; align-items:center; justify-content:space-between; gap:24px;",
          div(
            style = "flex:1 1 300px; min-width:0;",
            h2(contact$name),
            p(contact$role),
            p(contact$email),
            p(contact$mobile),
            p(contact$address),
          ),
          div(
            style = "display:flex; flex:0 1 auto; align-items:center; justify-content:center; flex-wrap:wrap; gap:16px;",
            tags$img(
              src = if (nzchar(asset("GSP_Logo.webp"))) asset("GSP_Logo.webp") else fallback,
              alt = "Green Sheep Project logo",
              # MANUAL SIZE SETTINGS: edit GSP logo dimensions here.
              style = paste0(
                "width:180px;",  # GSP logo width
                "height:140px;", # GSP logo height
                "max-width:100%;object-fit:contain;display:inline-block;"
              )
            ),
            tags$img(
              src = if (nzchar(asset("WNRM2.webp"))) asset("WNRM2.webp") else fallback,
              alt = "Wheatbelt Natural Resource Management logo",
              # MANUAL SIZE SETTINGS: edit WNRM logo dimensions here.
              style = paste0(
                "width:220px;",  # WNRM logo width
                "height:160px;", # WNRM logo height
                "max-width:100%;object-fit:contain;display:inline-block;"
              )
            )
          )
        ),
        
        div(
          style = "text-align: center; margin-top: 20px;",
          tags$img(
            src = if (nzchar(asset("SheepMerin.png"))) asset("SheepMerin.png") else if (nzchar(asset("SheepMerin.png"))) asset("SheepMerin.png") else fallback,
            alt = "Lamb image",
            style = "width: 900px; max-width: 100%; height: auto; border-radius: 30px; display: inline-block; object-fit: cover;"
          )
        )
      )
    ),
    
    
    # Copyright footer on every page
    tags$div(
      class = "app-footer",
      tags$p(
        class = "copyright-notice",
        "© 2026 All rights reserved. Free use for personal, non-commercial farm assessment. Contact aali@wheatbeltnrm.org.au | ",
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
        tags$p("The Natural Capital Assessment Tool is for non-commercial, producer-friendly use."),
        tags$h5("Permitted Use:"),
        tags$ul(
          tags$li("Free use for personal, non-commercial farm assessment"),
          tags$li("Download and print reports for your own farm records"),
          tags$li("Share the Tool's web link with other producers")
        ),
        tags$h5("Prohibited Use:"),
        tags$ul(
          tags$li("Copying or reproducing source code"),
          tags$li("Commercial use without permission"),
          tags$li("Removing copyright notices")
        ),
        tags$h5("Contact: Dr Aram Ali | aali@wheatbeltnrm.org.au")
      )
    ))
  })
  
  
  observeEvent(input$go_to_farm_inputs, { updateNavbarPage(session, "tabs", selected = "Farm & Management Inputs") })
  
  ids <- c("ecological", "woody", "aggregation", "distance", "soil_median", "soil_minimum", "soil_stability",
           "riparian", "forage", "native", "habitat", "connectivity", "shade", "water", "ghg", "drought",
           "nutrient", "feed", "welfare", "pasture", "health", "biosecurity", "lambing", "people", "business", "community", "salt")
  
  labels <- c("Ecological Condition (0-1)", "Woody Vegetation Cover (%)", "Aggregation (0-1)",
              "Proximity (Mean straight-line distance to woody vegetation) (m)", "Soil median cover (%)", "Soil minimum cover (%)",
              "Soil stability (%)", "Riparian Buffer Condition (%)", "Forage Condition (0-1)", "Native vegetation (%)",
              "Habitat Features (0-5)", "Landscape Connectivity (0-5)", "Shade and Shelter (0-5)", "Waterpoint Condition (0-5)",
              "GHG / Climate Plan (0-5)", "Drought Preparedness (0-5)", "Nutrient Efficiency (0-5)", "Feed Efficiency (0-5)",
              "Animal Welfare (0-5)", "Pasture Planning (0-5)", "Animal Health (0-5)", "Biosecurity (0-5)",
              "Lambing Management (0-5)", "People and Safety (0-5)", "Business Resilience (0-5)",
              "Community Engagement (0-5)", "Salt Affected Land Management (0-5)")
  
  defaults <- c(0.50, 70, 0.35, 300, 70, 85, 70, 75, 0.6, 15, 2, 2, 5, 5, 1, 3, 3, 3, 4, 3, 4, 3, 3, 4, 3, 3, 3)
  mins <- c(0, 0, 0, 0, rep(0, 23))
  maxs <- c(1, 100, 1, 1000, 100, 100, 100, 100, 1, 100, rep(5, 17))
  steps <- c(0.01, 1, 0.01, 1, 1, 1, 1, 1, 0.01, 1, rep(1, 17))
  
  scoring_ids <- c("ecological", "woody", "aggregation", "distance", "soil_median", "soil_minimum", "soil_stability",
                   "riparian", "forage", "habitat", "connectivity", "salt")
  farm_ids <- setdiff(ids, scoring_ids)
  
  if (length(ids) != length(labels) || length(ids) != length(defaults) ||
      length(ids) != length(mins) || length(ids) != length(maxs) ||
      length(ids) != length(steps)) {
    stop("Farm-input configuration error: ids, labels, defaults, mins, maxs and steps must have identical lengths.")
  }
  
  output$inputs <- renderUI({
    tagList(lapply(farm_ids, function(id) {
      i <- match(id, ids)
      numericInput(inputId = ids[i], label = labels[i], value = defaults[i], min = mins[i], max = maxs[i], step = steps[i])
    }))
  })
  
  current <- reactive({
    as.list(c(list(farm_name = input$farm_name, system = input$system, area = input$area,
                   stocking = input$stocking, capacity = input$capacity),
              setNames(lapply(ids, function(z) input[[z]]), ids)))
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
  output$metrics <- renderDT({ req(assessment()); datatable(assessment()$metrics, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE)) })
  output$domains <- renderDT({ req(assessment()); datatable(assessment()$domains, rownames = FALSE) })
  
  # Dependency-free SVG radar chart. Preserves the seven domain scores and
  # their 0-100 scale without downloading plotly, ggplot2 or htmlwidgets.
  output$plot <- renderUI({
    req(assessment())
    d <- assessment()$domains
    n <- nrow(d)
    if (n < 3L) return(NULL)
    scores <- pmax(0, pmin(100, as.numeric(d$Score)))
    angles <- (seq_len(n) - 1) * 2 * pi / n - pi / 2
    cx <- 310; cy <- 255; radius <- 165
    xy <- function(r) cbind(cx + r * cos(angles), cy + r * sin(angles))
    pts <- function(m) paste(apply(round(m, 1), 1, paste, collapse = ","), collapse = " ")
    rings <- vapply(c(20, 40, 60, 80, 100), function(level) {
      sprintf('<polygon points="%s" fill="none" stroke="#d7e4d8" stroke-width="1"/>',
              pts(xy(radius * level / 100)))
    }, character(1))
    spokes <- vapply(seq_len(n), function(i) {
      pos <- xy(radius)[i, ]
      sprintf('<line x1="%s" y1="%s" x2="%.1f" y2="%.1f" stroke="#d7e4d8"/>',
              cx, cy, pos[1], pos[2])
    }, character(1))
    label_pos <- xy(radius + 30)
    labels <- vapply(seq_len(n), function(i) {
      # Domain labels are fixed strings defined by the application.
      lab <- htmltools::htmlEscape(as.character(d$Domain[i]))
      sprintf('<text x="%.1f" y="%.1f" text-anchor="middle" font-size="10" fill="#26352B">%s</text>',
              label_pos[i, 1], label_pos[i, 2], lab)
    }, character(1))
    values <- vapply(seq_len(n), function(i) {
      pos <- xy(radius * scores / 100)[i, ]
      sprintf('<circle cx="%.1f" cy="%.1f" r="5" fill="#2F5D3A"><title>%s: %.1f/100</title></circle>',
              pos[1], pos[2], htmltools::htmlEscape(as.character(d$Domain[i])), scores[i])
    }, character(1))
    poly <- sprintf('<polygon points="%s" fill="#5E8C57" fill-opacity="0.27" stroke="#2F5D3A" stroke-width="3"/>',
                    pts(xy(radius * scores / 100)))
    svg_chart <- paste0(
      '<svg viewBox="0 0 620 540" role="img" aria-label="Natural capital domain scores radar chart" ',
      'style="width:100%;max-width:760px;height:auto;display:block;margin:auto" xmlns="http://www.w3.org/2000/svg">',
      '<text x="310" y="28" text-anchor="middle" font-size="19" font-weight="bold" fill="#26352B">Natural Capital Domain Scores</text>',
      paste(rings, collapse = ""), paste(spokes, collapse = ""), poly,
      paste(values, collapse = ""), paste(labels, collapse = ""), '</svg>'
    )
    div(style = "overflow-x:auto;", HTML(svg_chart))
  })

  render_cards <- function(df, cls) {
    if (!nrow(df)) return(div(class = cls, p("No metrics currently fall within this category.")))
    tagList(lapply(seq_len(nrow(df)), function(i) {
      div(class = cls,
          h4(gsub("_", " ", df$Metric[i])),
          p(strong("Score: "), paste0(df$Score[i], "/100 - ", df$Interpretation[i])),
          p(strong("Why it matters: "), df$Why_it_matters[i]),
          p(strong("Context: "), df$Context[i]),
          p(strong("Action: "), df$Action[i]),
          p(strong("Mechanism: "), df$Mechanism[i]),
          p(strong("Expected outcome: "), df$Expected_outcome[i]),
          p(strong("Suggested management attention: "), df$Priority[i]),
          p(strong("Timeframe: "), df$Timeframe[i]),
          p(strong("Monitoring: "), df$Monitoring[i]),
          p(strong("Reference: "), tags$a(href = as.character(df$ReferenceURL[i]), target = "_blank", rel = "noopener noreferrer", df$Reference[i])))
    }))
  }
  
  output$score_explanations <- renderUI({
    req(assessment())
    r <- assessment()$rec
    panels <- lapply(seq_len(nrow(r)), function(i) {
      bslib::accordion_panel(
        paste0(gsub("_", " ", r$Metric[i]), " — ", r$Score[i], "/100 (", r$Interpretation[i], ")"),
        h4("Why it matters"), p(r$Why_it_matters[i]),
        h4("Context and interpretation"), p(r$Context[i]),
        h4("What can influence the score?"), p(r$Action[i]),
        h4("Mechanism"), p(r$Mechanism[i]),
        h4("Expected outcome"), p(r$Expected_outcome[i]),
        h4("Monitoring"), p(r$Monitoring[i])
      )
    })
    do.call(bslib::accordion, c(list(id = "score_explanation_accordion", multiple = FALSE), panels))
  })
  
  output$works <- renderUI({ req(assessment()); render_cards(assessment()$rec[assessment()$rec$Score >= 60, , drop = FALSE], "good") })
  output$needs <- renderUI({ req(assessment()); render_cards(assessment()$rec[assessment()$rec$Score < 60, , drop = FALSE], "need") })
  output$improvement <- renderDT({ req(assessment()); datatable(assessment()$rec, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) })
  output$calculation_methods <- renderDT({ datatable(calculation_methods, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) })
  output$score_methods <- renderDT({ datatable(score_methods, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) })
  output$formula_table <- renderDT({ datatable(formulas, rownames = FALSE, options = list(scrollX = TRUE)) })
  output$method_metrics <- renderDT({ datatable(metric_defs, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) })
  output$metric_guidance_table <- renderDT({ datatable(metric_guidance, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) })
  output$reference_links <- renderUI({ tags$ol(lapply(seq_len(nrow(refs)), function(i) {
    tags$li(tags$a(href = refs$URL[i], target = "_blank", rel = "noopener noreferrer", refs$Title[i])) })) })
  
  # Removed unused Excel download handler: there is no Excel download button
  # in the interface. This avoids bundling the writexl package into ShinyLive.

  output$pdf <- downloadHandler(
    filename = function() {
      paste0(
        gsub("[^A-Za-z0-9_]+", "_", safe(input$farm_name)),
        "_Green_Sheep_Natural_Capital_Assessment_",
        Sys.Date(), ".pdf"
      )
    },
    content = function(file) {
      req(assessment())
      a <- assessment()
      
      # ------------------------------------------------------------
      # Professional producer report
      # ------------------------------------------------------------
      # The PDF deliberately excludes the technical formula/calculation
      # tables. Those remain available in the Excel workbook and the
      # Methods & references tab. The PDF focuses on decisions:
      # profile -> inputs -> results -> interpretation -> action -> monitoring.
      #
      # Base text is 12 pt (cex = 1 at pointsize = 12). Headings are larger.
      # Landscape A4 is used so integrated tables can remain readable.
      grDevices::pdf(
        file = file,
        width = 11.69,
        height = 8.27,
        paper = "a4r",
        onefile = TRUE,
        family = "Times",
        pointsize = 12,
        useDingbats = FALSE
      )
      on.exit(grDevices::dev.off(), add = TRUE)
      
      page_no <- 0L
      overall_score <- round(safe_mean(a$domains$Score), 1)
      overall_band <- band(overall_score)
      
      # ----------------------------
      # Page and typography helpers
      # ----------------------------
      new_page <- function() {
        plot.new()
        par(
          mar = c(0.25, 0.25, 0.25, 0.25),
          xaxs = "i", yaxs = "i"
        )
        page_no <<- page_no + 1L
      }
      
      footer <- function() {
        text(
          0.5, 0.018,
          paste0(
            "| Natural Capital Assessment | ",
            " Sustainable Sheep Farming Systems| Page ",
            page_no
          ),
          adj = c(0.5, 0),
          cex = 0.72,
          col = "#555555"
        )
      }
      
      header <- function(title, subtitle = NULL) {
        rect(0, 0.915, 1, 1, col = brand$dark, border = NA)
        text(
          0.045, 0.957, title,
          adj = c(0, 0.5),
          col = "white",
          cex = 1.45,
          font = 2
        )
        if (!is.null(subtitle)) {
          text(
            0.955, 0.957, subtitle,
            adj = c(1, 0.5),
            col = "white",
            cex = 0.90
          )
        }
        footer()
      }
      
      wrap_lines <- function(x, width_chars = 92) {
        x <- as.character(x)
        x[is.na(x)] <- ""
        x <- gsub("[\r\n]+", " ", x)
        if (!nzchar(trimws(x))) return("")
        strwrap(x, width = width_chars, simplify = TRUE)
      }
      
      text_block <- function(
    x, y, width_chars = 92, cex = 1,
    line_height = 0.040, colour = brand$text,
    x_pos = 0.055, min_y = 0.075
      ) {
        lines2 <- wrap_lines(x, width_chars)
        if (!length(lines2)) return(y)
        for (ln in lines2) {
          if (y < min_y) {
            new_page()
            header("Green Sheep Project")
            y <- 0.875
          }
          text(
            x_pos, y, ln,
            adj = c(0, 1),
            cex = cex,
            col = colour
          )
          y <- y - line_height
        }
        y
      }
      
      section_heading <- function(title, y, colour = brand$dark) {
        if (y < 0.14) {
          new_page()
          header("Green Sheep Project")
          y <- 0.875
        }
        text(
          0.055, y, title,
          adj = c(0, 1),
          cex = 1.30,
          font = 2,
          col = colour
        )
        y - 0.060
      }
      
      # Draw a compact two-column key-value block.
      key_value <- function(label, value, x, y, width = 0.42) {
        rect(
          x, y - 0.048, x + width, y + 0.008,
          col = "#F4F8F2", border = "#D7E4D8"
        )
        text(
          x + 0.012, y - 0.020, label,
          adj = c(0, 0.5),
          cex = 0.86,
          font = 2,
          col = brand$dark
        )
        text(
          x + 0.16, y - 0.020, as.character(value),
          adj = c(0, 0.5),
          cex = 0.86,
          col = brand$text
        )
      }
      
      # Draw a compact table with fixed column widths and wrapped cells.
      # This is intentionally not a tiny spreadsheet-like table: every
      # cell remains at >= 12 pt and is wrapped across multiple lines.
      draw_table <- function(
    dat, y, widths, title = NULL,
    header_fill = brand$dark,
    row_fill = "#FFFFFF",
    line_height = 0.034,
    header_height = 0.055,
    font_size = 1.0,
    header_size = 1.0,
    left = 0.045,
    right = 0.955,
    row_gap = 0.008,
    min_y = 0.080
      ) {
        dat <- as.data.frame(dat, stringsAsFactors = FALSE)
        dat[] <- lapply(dat, function(z) {
          z <- as.character(z)
          z[is.na(z)] <- ""
          gsub("[\r\n]+", " ", z)
        })
        
        if (!is.null(title)) {
          y <- section_heading(title, y)
        }
        
        if (!nrow(dat)) {
          return(text_block("No records available.", y, cex = font_size))
        }
        
        if (length(widths) != ncol(dat)) {
          widths <- rep(1 / ncol(dat), ncol(dat))
        }
        widths <- widths / sum(widths)
        usable_width <- right - left
        x_left <- left + c(0, cumsum(widths[-length(widths)])) * usable_width
        col_width <- widths * usable_width
        
        draw_header_row <- function(y0) {
          rect(
            left, y0 - header_height,
            right, y0,
            col = header_fill, border = header_fill
          )
          for (j in seq_len(ncol(dat))) {
            text(
              x_left[j] + 0.008, y0 - header_height / 2,
              names(dat)[j],
              adj = c(0, 0.5),
              cex = max(1, header_size),
              font = 2,
              col = "white"
            )
          }
          y0 - header_height
        }
        
        y <- draw_header_row(y)
        
        for (i in seq_len(nrow(dat))) {
          cell_lines <- lapply(seq_len(ncol(dat)), function(j) {
            # Width is converted to approximate Times characters.
            chars <- max(16, floor(col_width[j] * 135))
            wrap_lines(dat[i, j], chars)
          })
          n_lines <- max(1L, vapply(cell_lines, length, integer(1)))
          row_height <- max(
            0.065,
            0.018 + max(n_lines) * line_height
          )
          
          if ((y - row_height) < min_y) {
            footer()
            new_page()
            header(if (!is.null(title)) title else "Green Sheep Project")
            y <- 0.875
            y <- draw_header_row(y)
          }
          
          rect(
            left, y - row_height,
            right, y,
            col = if (i %% 2L == 0L) "#F7FAF6" else row_fill,
            border = "#D7E4D8"
          )
          
          for (j in seq_len(ncol(dat))) {
            lines_j <- cell_lines[[j]]
            if (!length(lines_j)) lines_j <- ""
            yy <- y - 0.024
            for (ln in lines_j) {
              text(
                x_left[j] + 0.008, yy, ln,
                adj = c(0, 1),
                cex = max(1, font_size),
                col = brand$text
              )
              yy <- yy - line_height
            }
          }
          y <- y - row_height - row_gap
        }
        
        y - 0.015
      }
      
      # ----------------------------
      # Metric input / result labels
      # ----------------------------
      input_label <- function(key, x) {
        switch(
          key,
          Ecological_Condition =
            paste0("Scoring guide slider: ", formatC(x$ecological, format = "f", digits = 2), " / 1"),
          Woody_Vegetation_Cover =
            paste0("Scoring guide slider: ", formatC(x$woody, format = "f", digits = 0), "%"),
          Aggregation =
            paste0("Scoring guide slider: ", formatC(x$aggregation, format = "f", digits = 2), " / 1"),
          Proximity =
            paste0("Scoring guide slider: ", formatC(x$distance, format = "f", digits = 0), " m mean distance"),
          Soil_Condition =
            paste0(
              "Scoring guide sliders: median cover ",
              x$soil_median, "%; minimum cover ",
              x$soil_minimum, "%; stability ",
              x$soil_stability, "%"
            ),
          Riparian_Buffer_Condition =
            paste0("Scoring guide slider: ", x$riparian, "%"),
          Forage_Condition =
            paste0("Scoring guide slider: ", formatC(x$forage, format = "f", digits = 2), " / 1"),
          Habitat_Features =
            paste0("Scoring guide slider: ", x$habitat, " / 5"),
          Landscape_Connectivity =
            paste0("Scoring guide slider: ", x$connectivity, " / 5"),
          Salt_Affected_Land =
            paste0("Scoring guide slider: ", x$salt, " / 5"),
          Native_Vegetation_Extent =
            paste0("Farm input: ", x$native, "% native vegetation"),
          Shade_and_Shelter =
            paste0("Farm input: ", x$shade, " / 5"),
          Waterpoint_Condition =
            paste0("Farm input: ", x$water, " / 5"),
          GHG_Climate_Plan =
            paste0("Farm input: ", x$ghg, " / 5"),
          Drought_Preparedness =
            paste0("Farm input: ", x$drought, " / 5"),
          Nutrient_Efficiency =
            paste0("Farm input: ", x$nutrient, " / 5"),
          Feed_Efficiency =
            paste0("Farm input: ", x$feed, " / 5"),
          Animal_Welfare =
            paste0("Farm input: ", x$welfare, " / 5"),
          Pasture_Planning =
            paste0("Farm input: ", x$pasture, " / 5"),
          Animal_Health =
            paste0("Farm input: ", x$health, " / 5"),
          Biosecurity =
            paste0("Farm input: ", x$biosecurity, " / 5"),
          Lambing_Management =
            paste0("Farm input: ", x$lambing, " / 5"),
          People_and_Safety =
            paste0("Farm input: ", x$people, " / 5"),
          Business_Resilience =
            paste0("Farm input: ", x$business, " / 5"),
          Community_Engagement =
            paste0("Farm input: ", x$community, " / 5"),
          Stocking_Fit =
            paste0(
              "Calculated from stocking pressure ",
              x$stocking, " DSE/ha versus sustainable capacity ",
              x$capacity, " DSE/ha"
            ),
          ""
        )
      }
      
      # ------------------------------------------------------------
      # Cover
      # ------------------------------------------------------------
      new_page()
      rect(0, 0, 1, 1, col = brand$cream, border = NA)
      rect(0, 0.73, 1, 1, col = brand$dark, border = NA)
      
      logo_path <- file.path("www", "SheepMerin.png")
      if (!file.exists(logo_path)) {
        logo_path <- file.path("www", "Merino.png")
      }
      if (!is.null(logo_path) && nzchar(logo_path) && file.exists(logo_path)) {
        img <- tryCatch(png::readPNG(logo_path), error = function(e) NULL)
        if (!is.null(img)) {
          rasterImage(img, 0.67, 0.10, 0.94, 0.61, interpolate = TRUE)
        }
      }
      
      text(
        0.055, 0.905,
        "Sustainable Sheep Farming System",
        adj = c(0, 0.5),
        cex = 1.05,
        font = 2,
        col = "#DCEBDD"
      )
      text(
        0.055, 0.835,
        "Natural Capital & Sustainable Sheep Farming",
        adj = c(0, 0.5),
        cex = 1.85,
        font = 2,
        col = "white"
      )
      text(
        0.055, 0.785,
        "Producer Farm Assessment Report",
        adj = c(0, 0.5),
        cex = 1.20,
        col = "white"
      )
      
      text(
        0.055, 0.635,
        safe(a$x$farm_name),
        adj = c(0, 0.5),
        cex = 1.65,
        font = 2,
        col = brand$dark
      )
      text(
        0.055, 0.585,
        paste(
          a$x$system,
          "|", a$x$area, "ha",
          "|", format(a$time, "%d %B %Y")
        ),
        adj = c(0, 0.5),
        cex = 1.00,
        col = brand$text
      )
      
      rect(0.055, 0.36, 0.58, 0.51, col = "white", border = "#D7E4D8")
      text(0.075, 0.475, "OVERALL ASSESSMENT", adj = c(0, 0.5),
           cex = 1.0, font = 2, col = brand$dark)
      text(0.075, 0.415, paste0(overall_score, " / 100"),
           adj = c(0, 0.5), cex = 1.65, font = 2, col = brand$green)
      text(0.075, 0.375, overall_band,
           adj = c(0, 0.5), cex = 1.0, font = 2, col = brand$text)
      
      text(
        0.055, 0.075,
        paste0(
          contact$role, " | ", contact$name,
          " | ", contact$email, " | ", contact$mobile
        ),
        adj = c(0, 0.5),
        cex = 0.78,
        col = "#555555"
      )
      footer()
      
      # ------------------------------------------------------------
      # Executive summary and farm profile
      # ------------------------------------------------------------
      new_page()
      header("Executive summary", format(a$time, "%d %B %Y"))
      
      y <- 0.875
      y <- text_block(
        paste(
          "This report summarises a farm-scale screening assessment across",
          length(metric_keys), "natural-capital, land-condition and sustainable",
          "sheep-farming metrics. The assessment combines producer-entered",
          "scoring-guide values with farm and management inputs. Scores are",
          "standardised to a 0-100 scale so results can be compared across",
          "metrics and aggregated into seven assessment domains."
        ),
        y, width_chars = 108, cex = 1.0, line_height = 0.041
      )
      
      y <- y - 0.025
      y <- section_heading("Farm profile", y)
      
      key_value("Farm name", safe(a$x$farm_name), 0.055, y, 0.43)
      key_value("System", a$x$system, 0.515, y, 0.43)
      y <- y - 0.070
      key_value("Farm area", paste0(a$x$area, " ha"), 0.055, y, 0.43)
      key_value("Assessment date", format(a$time, "%d %B %Y"), 0.515, y, 0.43)
      y <- y - 0.070
      key_value("Stocking pressure", paste0(a$x$stocking, " DSE/ha"), 0.055, y, 0.43)
      key_value("Sustainable capacity", paste0(a$x$capacity, " DSE/ha"), 0.515, y, 0.43)
      
      y <- y - 0.085
      y <- section_heading("Domain results", y)
      
      domain_table <- data.frame(
        Domain = a$domains$Domain,
        Score = paste0(a$domains$Score, "/100"),
        Interpretation = a$domains$Interpretation,
        stringsAsFactors = FALSE
      )
      y <- draw_table(
        domain_table, y,
        widths = c(0.62, 0.16, 0.22),
        font_size = 1.0,
        header_size = 1.0
      )
      
      # ------------------------------------------------------------
      # Input record: only values that are not duplicated by sliders
      # ------------------------------------------------------------
      new_page()
      header("Assessment input record")
      
      y <- 0.875
      y <- text_block(
        "The report separates the input record into two groups. Scoring-guide metrics are recorded from the image-backed sliders. Farm and management inputs are listed separately below. This avoids duplicating the same metric in two parts of the report.",
        y, width_chars = 108
      )
      
      y <- y - 0.025
      y <- section_heading("Farm and management inputs", y)
      
      management_keys <- setdiff(
        ids,
        c(
          "ecological", "woody", "aggregation", "distance",
          "soil_median", "soil_minimum", "soil_stability",
          "riparian", "forage", "habitat", "connectivity", "salt"
        )
      )
      
      management_labels <- labels[match(management_keys, ids)]
      management_values <- vapply(
        management_keys,
        function(k) as.character(a$x[[k]]),
        character(1)
      )
      
      management_table <- data.frame(
        Input = management_labels,
        Value = management_values,
        stringsAsFactors = FALSE
      )
      
      y <- draw_table(
        management_table, y,
        widths = c(0.72, 0.28),
        font_size = 1.0,
        header_size = 1.0
      )
      
      if (y < 0.25) {
        new_page()
        header("Assessment input record")
        y <- 0.875
      }
      
      y <- section_heading("Scoring-guide metric inputs", y)
      
      scoring_table <- data.frame(
        Metric = metric_labels[match(
          c(
            "ecological", "woody", "aggregation", "distance",
            "soil_median", "soil_minimum", "soil_stability",
            "riparian", "forage", "habitat", "connectivity", "salt"
          ), ids
        )],
        Entered_value = c(
          paste0(a$x$ecological, " / 1"),
          paste0(a$x$woody, " %"),
          paste0(a$x$aggregation, " / 1"),
          paste0(a$x$distance, " m"),
          paste0(a$x$soil_median, " %"),
          paste0(a$x$soil_minimum, " %"),
          paste0(a$x$soil_stability, " %"),
          paste0(a$x$riparian, " %"),
          paste0(a$x$forage, " / 1"),
          paste0(a$x$habitat, " / 5"),
          paste0(a$x$connectivity, " / 5"),
          paste0(a$x$salt, " / 5")
        ),
        stringsAsFactors = FALSE
      )
      
      y <- draw_table(
        scoring_table, y,
        widths = c(0.72, 0.28),
        font_size = 1.0,
        header_size = 1.0
      )
      
      # ------------------------------------------------------------
      # Overall and domain interpretation
      # ------------------------------------------------------------
      new_page()
      header("Assessment overview")
      
      y <- 0.875
      rect(0.055, 0.76, 0.32, 0.86, col = "#F4F8F2", border = "#D7E4D8")
      text(0.075, 0.82, "OVERALL SCORE", adj = c(0, 0.5),
           cex = 1.0, font = 2, col = brand$dark)
      text(0.075, 0.785, paste0(overall_score, " / 100"),
           adj = c(0, 0.5), cex = 1.45, font = 2, col = brand$green)
      
      rect(0.395, 0.76, 0.945, 0.86, col = "#F4F8F2", border = "#D7E4D8")
      text(0.415, 0.82, "INTERPRETATION", adj = c(0, 0.5),
           cex = 1.0, font = 2, col = brand$dark)
      text(0.415, 0.785, overall_band,
           adj = c(0, 0.5), cex = 1.20, font = 2, col = brand$text)
      
      y <- 0.715
      y <- section_heading("Domain scores", y)
      y <- draw_table(
        domain_table, y,
        widths = c(0.62, 0.16, 0.22),
        font_size = 1.0
      )
      
      y <- y - 0.02
      y <- section_heading("Interpretation", y)
      y <- text_block(
        paste(
          "The overall result is the mean of the seven domain scores.",
          "Metric scores of 60 or above are presented as areas to maintain",
          "or strengthen; scores below 60 are presented as areas that may",
          "benefit from management attention. These categories are screening",
          "interpretations rather than a certification of farm performance."
        ),
        y, width_chars = 108
      )
      
      # ------------------------------------------------------------
      # Integrated metric results and management plan
      # ------------------------------------------------------------
      # One integrated table means the reader does not have to move between
      # separate 'results' and 'recommendations' sections to understand a metric.
      metric_rows <- vector("list", length(metric_keys))
      
      for (i in seq_along(metric_keys)) {
        key <- metric_keys[i]
        ri <- match(key, a$rec$Metric)
        di <- match(key, metric_guidance$Metric)
        score_i <- a$rec$Score[ri]
        interpretation_i <- a$rec$Interpretation[ri]
        
        result_text <- input_label(key, a$x)
        
        if (key == "Soil_Condition") {
          result_text <- paste0(
            result_text,
            "; composite score = ",
            score_i, "/100"
          )
        } else if (key == "Proximity") {
          result_text <- paste0(
            result_text,
            "; proximity score = ",
            score_i, "/100"
          )
        } else if (key == "Stocking_Fit") {
          result_text <- paste0(
            result_text,
            "; stocking-fit score = ",
            score_i, "/100"
          )
        } else {
          result_text <- paste0(
            result_text,
            "; metric score = ",
            score_i, "/100"
          )
        }
        
        metric_rows[[i]] <- data.frame(
          Metric = metric_labels[i],
          Score = paste0(score_i, "/100\n", interpretation_i),
          "Meaning and context" = paste(
            metric_defs$Definition[match(metric_labels[i], metric_defs$Metric)],
            a$rec$Why_it_matters[ri],
            a$rec$Context[ri],
            sep = " "
          ),
          Result = result_text,
          "Management plan" = paste(
            a$rec$Action[ri],
            paste0("Mechanism: ", a$rec$Mechanism[ri]),
            paste0("Expected outcome: ", a$rec$Expected_outcome[ri]),
            paste0(
              "Suggested management attention: ",
              a$rec$Priority[ri],
              " | Timeframe: ",
              a$rec$Timeframe[ri]
            ),
            sep = " "
          ),
          "Monitoring and reference" = paste(
            a$rec$Monitoring[ri],
            paste0("Reference: ", a$rec$Reference[ri]),
            sep = " "
          ),
          stringsAsFactors = FALSE
        )
      }
      
      metric_table <- do.call(rbind, metric_rows)
      
      new_page()
      header("Metric results and integrated management plan", "26 metrics")
      
      y <- 0.875
      y <- text_block(
        "Each metric below combines the definition, entered/calculated result, standardised score, management action, suggested attention level, timeframe and monitoring indicator. The scoring-guide metrics are sourced from the sliders on the Scoring guides page; they are not repeated as Farm & Management Inputs.",
        y, width_chars = 108
      )
      y <- y - 0.015
      
      # Split into manageable blocks while keeping the same integrated table structure.
      block_size <- 7L
      for (st in seq(1, nrow(metric_table), by = block_size)) {
        en <- min(st + block_size - 1L, nrow(metric_table))
        block <- metric_table[st:en, , drop = FALSE]
        
        if (st > 1L) {
          new_page()
          header("Metric results and integrated management plan", "continued")
          y <- 0.875
        }
        
        y <- draw_table(
          block, y,
          widths = c(0.13, 0.09, 0.24, 0.16, 0.24, 0.14),
          font_size = 1.0,
          header_size = 1.0,
          line_height = 0.033,
          row_gap = 0.007
        )
      }
      
      # ------------------------------------------------------------
      # Priority summary
      # ------------------------------------------------------------
      new_page()
      header("Priority summary")
      
      y <- 0.875
      y <- text_block(
        "This page provides a concise management view of the assessment. It does not replace the detailed metric table; it highlights where attention may be most useful based on the screening scores and the predefined management guidance.",
        y, width_chars = 108
      )
      y <- y - 0.015
      
      priority_table <- data.frame(
        Metric = metric_labels[match(
          a$rec$Metric[order(a$rec$Score, decreasing = FALSE)],
          metric_keys
        )],
        Score = paste0(
          a$rec$Score[order(a$rec$Score, decreasing = FALSE)],
          "/100"
        ),
        Interpretation = a$rec$Interpretation[order(a$rec$Score, decreasing = FALSE)],
        Priority = a$rec$Priority[order(a$rec$Score, decreasing = FALSE)],
        Timeframe = a$rec$Timeframe[order(a$rec$Score, decreasing = FALSE)],
        stringsAsFactors = FALSE
      )
      
      y <- section_heading("Metrics requiring the most management attention", y, colour = brand$red)
      y <- draw_table(
        priority_table[priority_table$Score != "", , drop = FALSE],
        y,
        widths = c(0.42, 0.12, 0.18, 0.13, 0.15),
        font_size = 1.0
      )
      
      # Strengths and lower-scoring metrics are both retained.
      y <- section_heading("Areas to maintain or strengthen", y, colour = brand$green)
      strong_table <- data.frame(
        Metric = metric_labels[match(
          a$rec$Metric[a$rec$Score >= 60], metric_keys
        )],
        Score = paste0(a$rec$Score[a$rec$Score >= 60], "/100"),
        Interpretation = a$rec$Interpretation[a$rec$Score >= 60],
        stringsAsFactors = FALSE
      )
      y <- draw_table(
        strong_table, y,
        widths = c(0.62, 0.16, 0.22),
        font_size = 1.0
      )
      
      # ------------------------------------------------------------
      # Natural-capital interpretation and limitations
      # ------------------------------------------------------------
      new_page()
      header("Interpretation, scope and limitations")
      
      y <- 0.875
      y <- section_heading("How to use this report", y)
      y <- text_block(
        paste(
          "The assessment is designed as a producer-friendly screening tool.",
          "It converts different input scales into a common 0-100 score and",
          "groups results into seven domains. The report is most useful when",
          "used as a repeatable baseline for identifying management priorities,",
          "tracking change and supporting discussion with advisers, NRM staff",
          "and farm decision-makers."
        ),
        y, width_chars = 108
      )
      
      y <- y - 0.015
      y <- section_heading("Important limitations", y)
      y <- text_block(disclaimer_text, y, width_chars = 108)
      
      y <- y - 0.015
      y <- section_heading("Proximity scoring interpretation", y)
      y <- text_block(
        paste(
          "The proximity metric follows the producer-facing scoring guide:",
          "less than 300 m is the highly desirable class (0.70-1.00);",
          "300-500 m is the desirable class (0.40-0.69); and greater than",
          "500 m is the undesirable class (0.00-0.39). Within these classes",
          "the score is continuous, declines to zero at 1,000 m, and remains",
          "zero beyond 1,000 m. The reported distance is the entered mean",
          "straight-line distance to woody vegetation."
        ),
        y, width_chars = 108
      )
      
      y <- y - 0.015
      y <- section_heading("Salt-affected land", y)
      y <- text_block(
        metric_defs$Definition[match("Salt Affected Land Management", metric_defs$Metric)],
        y, width_chars = 108
      )
      
      # ------------------------------------------------------------
      # References
      # ------------------------------------------------------------
      new_page()
      header("Supporting references")
      
      y <- 0.875
      y <- text_block(
        "The following references support the concepts and management guidance used in the assessment. Full technical calculation tables and the complete reference dataset are retained in the Excel workbook.",
        y, width_chars = 108
      )
      y <- y - 0.015
      
      # Keep all references, but use an integrated table so the report remains
      # readable and avoids a long URL-per-line bibliography.
      ref_table <- data.frame(
        No. = seq_len(nrow(refs)),
        Reference = refs$Title,
        URL = refs$URL,
        stringsAsFactors = FALSE
      )
      y <- draw_table(
        ref_table, y,
        widths = c(0.07, 0.48, 0.45),
        font_size = 1.0,
        line_height = 0.032,
        row_gap = 0.006
      )
      
      # ------------------------------------------------------------
      # Contact and record
      # ------------------------------------------------------------
      new_page()
      header("Report record and contact")
      
      y <- 0.84
      y <- text_block(
        "Assessment record",
        y, cex = 1.25, width_chars = 108, colour = brand$dark
      )
      y <- text_block(
        paste0("Farm: ", safe(a$x$farm_name)),
        y, width_chars = 108
      )
      y <- text_block(
        paste0("System: ", a$x$system),
        y, width_chars = 108
      )
      y <- text_block(
        paste0("Farm area: ", a$x$area, " ha"),
        y, width_chars = 108
      )
      y <- text_block(
        paste0("Assessment date and time: ", format(a$time, "%d %B %Y %H:%M")),
        y, width_chars = 108
      )
      y <- text_block(
        paste0("Overall score: ", overall_score, "/100 (", overall_band, ")"),
        y, width_chars = 108
      )
      
      y <- y - 0.04
      y <- section_heading("Project contact", y)
      y <- text_block(contact$name, y, cex = 1.20, width_chars = 108, colour = brand$dark)
      y <- text_block(contact$role, y, width_chars = 108)
      y <- text_block(contact$email, y, width_chars = 108)
      y <- text_block(contact$mobile, y, width_chars = 108)
      y <- text_block(contact$address, y, width_chars = 108)
      
      y <- y - 0.035
      y <- section_heading("Copyright and use", y)
      y <- text_block(
        "© 2026 All rights reserved. Free use for personal, non-commercial farm assessment. Commercial use, reproduction, modification or derivative use requires prior written permission.",
        y, width_chars = 108
      )
    }
  )
  
  
}

shinyApp(ui = ui, server = server)