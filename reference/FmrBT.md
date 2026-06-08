# Field Metabolic Rates Database (FmrBT)

A comprehensive database containing Field Metabolic Rates (FMR), body
mass, and ambient temperature across more than 700 species.

## Usage

``` r
FmrBT
```

## Format

A data frame with variables detailing energetic properties and taxonomy:

- Kingdom:

  Taxonomic kingdom.

- Phylum:

  Taxonomic phylum.

- Class:

  Taxonomic class.

- Order:

  Taxonomic order.

- Family:

  Taxonomic family.

- Genus:

  Taxonomic genus.

- SpeciesVerbatim:

  Original species name as recorded in the source.

- SpeciesAcceptedName:

  Standardized accepted species name.

- FMR_kJ_d:

  Field metabolic rate per individual in kJ/day.

- Mass_g:

  Body mass of the individual in grams.

- Temp_C:

  The ambient temperature recorded during the study in Celsius.

- Endotherm:

  Indicator of thermal status (1/TRUE for endotherm, 0/FALSE for
  ectotherm).

- Reference:

  Source literature reference.

- Comment:

  Additional notes from the dataset creators.

- Outlier:

  Flag indicating if the record is considered a statistical outlier.

## Source

De Castro et al. (2025) FmrBT database.
