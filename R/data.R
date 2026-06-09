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

#' DataBase of collected MicroMammals species
#'
#' @usage data(sf_micromammals)
#'
#' @examples
#' data(sf_micromammals)
#' @keywords datasets
"sf_micromammals"

#' Valued weight between OCSGE layer and species
#'
#' @usage data(ocsge_species_dict)
#'
#' @examples
#' data(ocsge_species_dict)
#' @keywords datasets
"ocsge_species_dict"

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
