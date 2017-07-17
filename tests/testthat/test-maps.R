library(testthat)
library(EQVisualisation)

context("Maps")

test_that("eq_map works", {
    path = system.file("extdata", "Earthquakes.tsv", package = "EQVisualisation")
    df = read.delim(path)
    expect_gt(nrow(df),0)
    df = eq_clean_data(df)
    testmap = eq_map(df, annot_col = "DATE")
    expect_is(testmap, "htmlwidget")
})

test_that("eq_create_label works", {
    path = system.file("extdata", "Earthquakes.tsv", package = "EQVisualisation")
    df = read.delim(path)
    expect_gt(nrow(df),0)
    df = eq_clean_data(df)
    labels = eq_create_label(df)
    expect_identical(labels[1], "<b>Location:</b> BAB-A-DARAA,AL-KARAK<br><b>Magnitude:</b>7.3<br>")
})

