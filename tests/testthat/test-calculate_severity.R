test_that("calculate_severity works", {
  expect_s3_class(
    {
      pars <- parameters()
      pars$n_years = 1
      sev <- pars |>
        simulate() |>
        calculate_severity()
      sev
    }, "data.frame"
  )
})

test_that("first three columns in calculate_severity are time variables",{
  expect_equal(
    {
    pars <- parameters()
    pars$n_years = 1
    sev <- pars |>
      simulate() |>
      calculate_severity()
    names(sev)[1:3]
    }, c("Year","DD","time")
  )
})
