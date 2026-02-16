# Create a trophic table

Creates a data.frame of class `trophic_tbl` designed to store trophic
links. Can be initialized empty or from an existing data.frame.

## Usage

``` r
trophic(data = NULL, from = NULL, to = NULL, weight = NULL)
```

## Arguments

- data:

  (Optional) A data.frame containing link information.

- from:

  (Optional) Character string. Name of the column in `data` containing
  source nodes.

- to:

  (Optional) Character string. Name of the column in `data` containing
  target nodes.

- weight:

  (Optional) Character string. Name of the column in `data` containing
  weights. Default is 1 if not specified.

## Value

An object of class `trophic_tbl`.

## Examples

``` r
# 1. Empty initialization (pipe style)
net <- trophic() |>
  add_link("sol", "sp1")

# 2. Initialization from data.frame
df_raw <- data.frame(src = c("A", "A"), target = c("B", "C"), w = c(2, 5))
net_from_df <- trophic(df_raw, from = "src", to = "target", weight = "w")
```
