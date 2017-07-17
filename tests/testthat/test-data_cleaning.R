library(testthat)
library(EQVisualisation)

context("Clean Data")

test_that("eq_clean_data works", {
    path = system.file("extdata", "Earthquakes.tsv", package = "EQVisualisation")
    df = read.delim(path)
    expect_gt(nrow(df),0)
    df = eq_clean_data(df)
    expect_is(df$DATE, "Date")
    expect_is(df$LONGITUDE, "numeric")
    expect_is(df$LATITUDE, "numeric")
})

test_that("eq_location_clean works", {
    path = system.file("extdata", "Earthquakes.tsv", package = "EQVisualisation")
    df = read.delim(path)
    expect_gt(nrow(df),0)
    df = eq_clean_data(df) %>% eq_location_clean()
    expect_is(df$LOCATION_NAME, "character")
    expect_equal(df$LOCATION_NAME[1], " BAB-A-DARAA,AL-KARAK")
})
