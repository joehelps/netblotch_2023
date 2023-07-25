test_that("calculate_effective_life works", {
  expect_equal({
    pars = parameters()
    pars$n_years = 1
    HAD = pars |> simulate() |> calculate_effective_life(max_HAD = 10000)
    class(HAD)
  }, "numeric")
})
