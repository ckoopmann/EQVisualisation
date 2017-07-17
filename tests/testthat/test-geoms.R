library(testthat)
library(EQVisualisation)

context("New Geoms")

test_that("geom_timeline works", {
    path = system.file("extdata", "Earthquakes.tsv", package = "EQVisualisation")
    df = read.delim(path)
    expect_gt(nrow(df),0)
    df = eq_clean_data(df)
    testplot = ggplot2::ggplot(data = df,
                      ggplot2::aes(x = DATE, y = COUNTRY, color = DEATHS, size = EQ_PRIMARY)) + geom_timeline(alpha = 0.2)
    expect_is(testplot,"ggplot")

})

test_that("geom_timeline_label works", {
    path = system.file("extdata", "Earthquakes.tsv", package = "EQVisualisation")
    df = read.delim(path)
    expect_gt(nrow(df),0)
    df = eq_clean_data(df)
    testplot = ggplot2::ggplot(data = df,
                               ggplot2::aes(x = DATE, y = COUNTRY, color = DEATHS, size = EQ_PRIMARY)) + geom_timeline(alpha = 0.2) +
        geom_timeline_label(ggplot2::aes(magnitude = INTENSITY, label = LOCATION_NAME), nudge_y = 0.2)
    expect_is(testplot,"ggplot")

})
