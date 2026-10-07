#' SF object defining ROI metaleurop
#'
#' @description Simple feature collection with 1 feature and 1 field.
#' Geodetic CRS:  WGS 84.
#'
#' @usage data(roi_metaleurop)
#'
#' @examples
#' data(roi_metaleurop)
#' @keywords datasets
"roi_metaleurop"


#' SF object defining very simplified OCS-GE soil cover metaleurop
#'
#' @description Simple feature collection with 9 features and 11 fields.
#' Projected CRS: RGF93 v1 / Lambert-93.
#'
#' @usage data(ocsge_metaleurop)
#'
#' @examples
#' data(ocsge_metaleurop)
#' @keywords datasets
"ocsge_metaleurop"


#' Nomenclature of OCS-GE soil cover
#'
#' @usage data(ref_ocsge)
#'
#' @examples
#' data(ref_ocsge)
#' @keywords datasets
"ref_ocsge"

#' Nomenclature and render colors of EUNIS land cover classes
#'
#' @description
#' Code / label / render-color lookup table for the EEA "Ecosystem Type Map
#' v3.1, terrestrial part" ArcGIS service, at both EUNIS Level 1 (9 broad
#' classes) and Level 2 (46 detailed classes). It is used internally by
#' \code{\link{get_eunis_data}} to decode the service's exported image back
#' into EUNIS codes, since that service publishes no raw-value raster
#' endpoint.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{level}{Character. \code{"L1"} or \code{"L2"}.}
#'   \item{code}{Character. The EUNIS code (e.g. \code{"J1"} at L2, \code{"J"} at L1).}
#'   \item{nomenclature}{Character. The class label.}
#'   \item{couleur}{Character. The hex render color used by the source ArcGIS service.}
#'   \item{r, g, b}{Integer (0-255). The render color, split into channels for exact matching.}
#' }
#'
#' @details
#' At \code{level == "L2"}, two pairs of classes share an identical render
#' color and cannot be distinguished from the exported image alone:
#' \code{D5}/\code{F6} and \code{D6}/\code{F7}. \code{level == "L1"} has no
#' such collision. See \code{\link{get_eunis_data}} for how this is handled.
#'
#' @source European Environment Agency, "Ecosystem Type Map v3.1, terrestrial
#' part": \url{https://bio.discomap.eea.europa.eu/arcgis/rest/services/Ecosystem/EcosystemTypeMap_v3_1_Terrestrial/MapServer}
#'
#' @usage data(ref_eunis)
#'
#' @examples
#' data(ref_eunis)
#' @keywords datasets
"ref_eunis"

#' DataBase of collected MicroMammals species
#'
#' @usage data(sf_micromammals)
#'
#' @examples
#' data(sf_micromammals)
#' @keywords datasets
"sf_micromammals"

#' Valued weights and resistance between OCS-GE layers and species
#'
#' @description
#' A dataset providing detailed ecological scores linking European terrestrial
#' vertebrate species to French OCS-GE (Occupation du Sol à Grande Échelle) land cover classes.
#'
#' Unlike traditional expert-based crosswalks that assign simple presence/absence or
#' discrete suitability scores (e.g., 0, 1, 2), this dataset was generated using a Large
#' Language Model (LLM - Google Gemini). The model semantically analyzed free-text
#' descriptions of preferred and avoided habitats (derived from the EEA dataset) against
#' ecological definitions of OCS-GE classes to infer granular scores (0 to 10).
#'
#' These scores are specifically designed for spatial ecology modeling, separating general
#' suitability from movement permeability and foraging potential.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{nom_espece}{Character. The scientific name of the species.}
#'   \item{code_cs}{Character. The OCS-GE layer code (e.g., "CS 1.1.1.1").}
#'   \item{weight_global}{Numeric (0-10). The overall habitat suitability of the OCS-GE layer for the species.}
#'   \item{weight_movement}{Numeric (0-10). The permeability of the layer, representing how easily the species can disperse through it.}
#'   \item{weight_foraging}{Numeric (0-10). The suitability of the layer specifically for feeding and resource acquisition.}
#'   \item{resistance}{Numeric (0-10). The impedance or barrier effect of the layer, heavily influenced by known avoided habitats.}
#' }
#'
#' @details
#' The methodology differs significantly from global Area of Habitat (AOH) approaches.
#' While global AOH maps rely on hard-coded rules and keyword-based penalties to score
#' land use intensity, this dataset leverages AI to interpret complex ecological nuances
#' (e.g., thermophily, physical barriers, seasonal cover) described in the OCS-GE standard.
#'
#' @references
#' The underlying conceptual framework for linking terrestrial vertebrate species to
#' European land systems, as well as the baseline habitat descriptions, draws inspiration from:
#'
#' O'Connor, L. M. J., Renaud, J., Dou, Y., Karger, D. N., Maiorano, L., Verburg, P. H., &
#' Thuiller, W. (2024). Habitat Suitability of European Land Systems for Terrestrial Vertebrates.
#' \emph{Global Ecology and Biogeography}, 33(11), e13903.
#'
#' @usage data(ocsge_species_dict)
#'
#' @examples
#' data(ocsge_species_dict)
#' head(ocsge_species_dict)
#'
#' @keywords datasets
"ocsge_species_dict"

#' Species habitat dictionary for EUNIS Level 2 land cover classes
#'
#' @description
#' Habitat suitability, breeding, foraging and movement scores of 935 European
#' terrestrial vertebrates (amphibians, reptiles, birds, mammals) for each of
#' the 45 EUNIS Level 2 classes of the EEA Ecosystem Type Map v3.1 (terrestrial
#' part), as returned by \code{\link{get_eunis_data}} with \code{level = "L2"}.
#' It is the EUNIS counterpart of \code{\link{ocsge_species_dict}}, with an
#' improved schema: scores follow an anchored rubric, breeding is separated
#' from foraging, and resistance is derived from permeability on the 1-100 log
#' scale expected by connectivity models such as Omniscape.
#'
#' @format A data frame with 42,075 rows (935 species x 45 EUNIS classes) and 13 columns:
#' \describe{
#'   \item{code_eunis}{Character. EUNIS Level 2 code (e.g. \code{"G1"}); matches \code{ref_eunis$code} for \code{level == "L2"}.}
#'   \item{nom_espece}{Character. Scientific name, genus and species separated by \code{"_"}.}
#'   \item{sppID}{Character. Species identifier from the EEA source table; the first letter gives the group (A, R, B, M).}
#'   \item{taxon_group}{Character. \code{"amphibian"}, \code{"reptile"}, \code{"bird"} or \code{"mammal"}.}
#'   \item{suitability}{Integer (0-10). Overall habitat suitability: 0 unused or avoided, 2 marginal, 5 secondary, 8 main, 10 core habitat.}
#'   \item{weight_breeding}{Integer (0-10). Value of the class as a breeding, nesting, denning, roosting or larval site.}
#'   \item{weight_foraging}{Integer (0-10). Value of the class for feeding.}
#'   \item{permeability}{Integer (0-10). Ease of movement through the class: 10 moves freely, 7 readily, 4 reluctantly, 1 strong barrier, 0 impassable. Accounts for flight, swimming or fossorial habits.}
#'   \item{resistance}{Numeric (1-100). Derived from permeability as \code{10^(2 * (1 - permeability / 10))}: 10 gives 1, 5 gives 10, 0 gives 100.}
#'   \item{habitat_breadth}{Integer. Species-level: number of EUNIS classes with \code{suitability >= 5}.}
#'   \item{specialization}{Numeric (0-1). Species-level: 1 minus the normalised Shannon evenness of the suitability profile (0 generalist, 1 specialist).}
#'   \item{confidence}{Integer (1-3). Species-level: 1 vague or missing source description, 2 medium, 3 detailed.}
#'   \item{in_france}{Integer (0/1). Species-level: 1 if the species occurs natively and regularly in metropolitan France (expert knowledge).}
#' }
#'
#' @details
#' Scores were assigned by a large language model (Claude Opus 5.5) reading the
#' EEA habitat and avoided-habitat descriptions of each species
#' (\code{raw_data/spp_EEA_habitats_cleaned.csv}) against short ecological
#' descriptions of each EUNIS class. They are expert-judgement scores and have
#' not been validated against occurrence data. Four species had an empty
#' description and were scored from general knowledge (\code{confidence = 1}).
#' The scoring files, the comparison with \code{ocsge_species_dict} and the
#' build scripts are in \code{raw_data/opus_scoring/} of the source repository.
#'
#' @seealso \code{\link{get_eunis_data}}, \code{\link{ref_eunis}}, \code{\link{ocsge_species_dict}}
#'
#' @usage data(eunis_species_dict)
#'
#' @examples
#' data(eunis_species_dict)
#' subset(eunis_species_dict, nom_espece == "Apodemus_sylvaticus")
#'
#' @keywords datasets
"eunis_species_dict"
#' Data concentration Cd Soil - Veetation extracted from BAPPET
#'
#' @usage data(bappet_cd)
#'
#' @examples
#' data(bappet_cd)
#' @keywords datasets
"bappet_cd"

#' Data concentration Cd Soil - Earthworm
#'
#' @usage data(earthworm_cd)
#'
#' @examples
#' data(earthworm_cd)
#' @keywords datasets
"earthworm_cd"


#' Field Metabolic Rates Database (FmrBT)
#'
#' A comprehensive database containing Field Metabolic Rates (FMR), body mass,
#' and ambient temperature across more than 700 species.
#'
#' @format A data frame with variables detailing energetic properties and taxonomy:
#' \describe{
#'   \item{Kingdom}{Taxonomic kingdom.}
#'   \item{Phylum}{Taxonomic phylum.}
#'   \item{Class}{Taxonomic class.}
#'   \item{Order}{Taxonomic order.}
#'   \item{Family}{Taxonomic family.}
#'   \item{Genus}{Taxonomic genus.}
#'   \item{SpeciesVerbatim}{Original species name as recorded in the source.}
#'   \item{SpeciesAcceptedName}{Standardized accepted species name.}
#'   \item{FMR_kJ_d}{Field metabolic rate per individual in kJ/day.}
#'   \item{Mass_g}{Body mass of the individual in grams.}
#'   \item{Temp_C}{The ambient temperature recorded during the study in Celsius.}
#'   \item{Endotherm}{Indicator of thermal status (1/TRUE for endotherm, 0/FALSE for ectotherm).}
#'   \item{Reference}{Source literature reference.}
#'   \item{Comment}{Additional notes from the dataset creators.}
#'   \item{Outlier}{Flag indicating if the record is considered a statistical outlier.}
#' }
#' @source De Castro et al. (2025) FmrBT database.
"FmrBT"

#' Mammal Functional Traits Dataset (EltonTraits 1.0)
#'
#' Dietary category percentages, foraging strategies, and functional traits for mammals.
#'
#' @format A data frame containing species dietary breakdowns (summing to 100%) and trait data:
#' \describe{
#'   \item{MSW3_ID}{Mammal Species of the World (3rd ed.) identifier.}
#'   \item{Scientific}{Scientific name of the mammal species.}
#'   \item{MSWFamilyLatin}{Mammal Species of the World (3rd ed.) family name.}
#'   \item{Diet.Inv}{Percentage of invertebrates in the diet.}
#'   \item{Diet.Vend}{Percentage of vertebrate endotherms consumed (birds and mammals).}
#'   \item{Diet.Vect}{Percentage of vertebrate ectotherms consumed (reptiles and amphibians).}
#'   \item{Diet.Vfish}{Percentage of fish consumed.}
#'   \item{Diet.Vunk}{Percentage of unknown/unclassified vertebrates in the diet.}
#'   \item{Diet.Scav}{Percentage of scavenging activity.}
#'   \item{Diet.Fruit}{Percentage of fruits consumed.}
#'   \item{Diet.Nect}{Percentage of nectar/pollen consumed.}
#'   \item{Diet.Seed}{Percentage of seeds/nuts consumed.}
#'   \item{Diet.PlantO}{Percentage of other plant tissues consumed (leaves, stems, roots).}
#'   \item{Diet.Source}{Source of the dietary information.}
#'   \item{Diet.Certainty}{Certainty score for the diet data.}
#'   \item{ForStrat.Value}{Primary foraging stratum/habitat.}
#'   \item{ForStrat.Certainty}{Certainty score for the foraging stratum.}
#'   \item{ForStrat.Comment}{Notes on foraging strategy.}
#'   \item{Activity.Nocturnal}{Binary flag for nocturnal activity.}
#'   \item{Activity.Crepuscular}{Binary flag for crepuscular activity.}
#'   \item{Activity.Diurnal}{Binary flag for diurnal activity.}
#'   \item{Activity.Source}{Source of the activity information.}
#'   \item{Activity.Certainty}{Certainty score for the activity pattern.}
#'   \item{BodyMass.Value}{Body mass value in grams.}
#'   \item{BodyMass.Source}{Source of the body mass information.}
#'   \item{BodyMass.SpecLevel}{Specificity level of the body mass record.}
#' }
#' @source EltonTraits 1.0 database.
"DBFunc_MamFuncDat"

#' Bird Functional Traits Dataset (EltonTraits 1.0)
#'
#' Dietary category percentages, foraging strategies, and functional traits for birds.
#'
#' @format A data frame containing species dietary breakdowns (summing to 100%) and trait data:
#' \describe{
#'   \item{SpecID}{Unique species identifier.}
#'   \item{PassNonPass}{Indicates whether the bird is a Passerine or Non-Passerine.}
#'   \item{IOCOrder}{IOC taxonomic order.}
#'   \item{BLFamilyLatin}{BirdLife International family Latin name.}
#'   \item{BLFamilyEnglish}{BirdLife International family English name.}
#'   \item{BLFamSequID}{BirdLife International family sequence ID.}
#'   \item{Taxo}{Taxonomic grouping code.}
#'   \item{Scientific}{Scientific name of the bird species.}
#'   \item{English}{Common English name of the bird species.}
#'   \item{Diet.Inv}{Percentage of invertebrates in the diet.}
#'   \item{Diet.Vend}{Percentage of vertebrate endotherms consumed.}
#'   \item{Diet.Vect}{Percentage of vertebrate ectotherms consumed.}
#'   \item{Diet.Vfish}{Percentage of fish consumed.}
#'   \item{Diet.Vunk}{Percentage of unknown/unclassified vertebrates in the diet.}
#'   \item{Diet.Scav}{Percentage of scavenging activity.}
#'   \item{Diet.Fruit}{Percentage of fruits consumed.}
#'   \item{Diet.Nect}{Percentage of nectar/pollen consumed.}
#'   \item{Diet.Seed}{Percentage of seeds/nuts consumed.}
#'   \item{Diet.PlantO}{Percentage of other plant tissues consumed.}
#'   \item{Diet.5Cat}{Simplified 5-category diet classification.}
#'   \item{Diet.Source}{Source of the dietary information.}
#'   \item{Diet.Certainty}{Certainty score for the diet data.}
#'   \item{Diet.EnteredBy}{Identifier of the person who entered the diet data.}
#'   \item{ForStrat.watbelowsurf}{Percentage of foraging time spent underwater.}
#'   \item{ForStrat.wataroundsurf}{Percentage of foraging time spent at the water surface.}
#'   \item{ForStrat.ground}{Percentage of foraging time spent on the ground.}
#'   \item{ForStrat.understory}{Percentage of foraging time spent in the understory.}
#'   \item{ForStrat.midhigh}{Percentage of foraging time spent in the mid-high canopy.}
#'   \item{ForStrat.canopy}{Percentage of foraging time spent in the upper canopy.}
#'   \item{ForStrat.aerial}{Percentage of foraging time spent in aerial foraging.}
#'   \item{PelagicSpecialist}{Indicator of pelagic specialization.}
#'   \item{ForStrat.Source}{Source of the foraging strategy data.}
#'   \item{ForStrat.SpecLevel}{Specificity level of the foraging strategy record.}
#'   \item{ForStrat.EnteredBy}{Identifier of the person who entered the foraging data.}
#'   \item{Nocturnal}{Indicator of nocturnal activity.}
#'   \item{BodyMass.Value}{Body mass value in grams.}
#'   \item{BodyMass.Source}{Source of the body mass information.}
#'   \item{BodyMass.SpecLevel}{Specificity level of the body mass record.}
#'   \item{BodyMass.Comment}{Notes on body mass data.}
#'   \item{Record.Comment}{General comments about the record.}
#' }
#' @source EltonTraits 1.0 database.
"DBFunc_BirdFuncDat"


#' Eco-SSL Toxicity Data for Multiple Taxonomic Groups
#'
#' A comprehensive dataset combining toxicity values used for the derivation of
#' Ecological Soil Screening Levels (Eco-SSL). It includes ecotoxicological data
#' (NOAEL and LOAEL) for mammals, birds, invertebrates, and plants exposed to
#' various chemical compounds.
#'
#' @format A tibble (data frame) with 9 variables:
#' \describe{
#'   \item{ERE}{Character. Abbreviation for the Ecological Receptor Endpoint or effect category (e.g., "BIO" for Biochemical, "BEH" for Behavior).}
#'   \item{order_tox_value}{Integer. The sequential order or index of the toxicity value within its specific group.}
#'   \item{tox_value_NOAEL}{Numeric. The No Observed Adverse Effect Level (NOAEL), typically expressed in mg/kg/day or mg/kg soil.}
#'   \item{tox_value_LOAEL}{Numeric. The Lowest Observed Adverse Effect Level (LOAEL), typically expressed in mg/kg/day or mg/kg soil.}
#'   \item{compound}{Character. The chemical compound or trace element evaluated (e.g., "Antimony", "Cadmium").}
#'   \item{species_group}{Character. The broad taxonomic or functional group of the tested species (e.g., "Mammalian Wildlife").}
#'   \item{test_organism}{Character. The common and/or scientific name of the specific organism tested (e.g., "Rat (Rattus norvegicus)").}
#'   \item{tox_value}{Numeric. The primary toxicity value retained for the assessment (usually corresponds to the NOAEL or a derived threshold).}
#'   \item{ERE_full}{Character. The full, unabbreviated name of the endpoint or effect category (e.g., "Biochemical", "Behavior").}
#' }
#'
#' @source Derived from the United States Environmental Protection Agency
#' (US EPA) Ecological Soil Screening Levels (Eco-SSL) database and documentation.
#'
#' @examples
#' data(SSD_ecoSSL_all)
#' head(SSD_ecoSSL_all)
#' table(SSD_ecoSSL_all$species_group)
"SSD_ecoSSL_all"
