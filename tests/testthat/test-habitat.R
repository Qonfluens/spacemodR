# tests/testthat/test-habitat.R

library(testthat)
library(sf)
# library(ton_package) # Décommente et remplace par le nom de ton package

context("Habitat constructor and basic behavior")

test_that("Habitat object is created correctly", {

  # --- geometry de test
  geom <- st_sfc(
    st_point(c(0, 0)),
    st_point(c(1, 1)),
    crs = 4326
  )

  # --- création d'un objet Habitat
  hab <- Habitat(
    geometry = geom,
    habitat = c(TRUE, FALSE),
    nohabitat = c(FALSE, TRUE),
    weight = c(0.8, 0.2)
  )

  # --- classes
  expect_s3_class(hab, "Habitat")
  expect_s3_class(hab, "sf")
  expect_s3_class(hab, "data.frame")

  # --- dimensions
  expect_equal(nrow(hab), 2)
  expect_equal(ncol(hab), 4) # habitat, nohabitat, weight, geometry

  # --- types de colonnes
  expect_type(hab$habitat, "logical")
  expect_type(hab$nohabitat, "logical")
  expect_type(hab$weight, "double")
  expect_s3_class(st_geometry(hab), "sfc")

  # --- valeurs exactes
  expect_equal(hab$habitat, c(TRUE, FALSE))
  expect_equal(hab$nohabitat, c(FALSE, TRUE))
  expect_equal(hab$weight, c(0.8, 0.2))
})

test_that("Habitat constructor fails with wrong input", {

  geom <- st_sfc(
    st_point(c(0, 0)),
    crs = 4326
  )

  # taille mismatch
  expect_error(Habitat(
    geometry = geom,
    habitat = c(TRUE, FALSE),
    nohabitat = FALSE,
    weight = 1
  ), "All columns must have the same length as geometry")

  # mauvais type habitat
  expect_error(Habitat(
    geometry = geom,
    habitat = 1,
    nohabitat = FALSE,
    weight = 1
  ), "`habitat` must be logical")

  # mauvais type nohabitat
  expect_error(Habitat(
    geometry = geom,
    habitat = TRUE,
    nohabitat = 1,
    weight = 1
  ), "`nohabitat` must be logical")

  # mauvais type weight
  expect_error(Habitat(
    geometry = geom,
    habitat = TRUE,
    nohabitat = FALSE,
    weight = "high"
  ), "`weight` must be numeric")

  # mauvais type geometry
  expect_error(Habitat(
    geometry = list(c(0,0)),
    habitat = TRUE,
    nohabitat = FALSE,
    weight = 1
  ), "`geometry` must be an object of class 'sfc'")
})
