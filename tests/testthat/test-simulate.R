test_that("simulate function returns a data.frame using default parameters", {
  expect_equal({
    pars = parameters()
    pars$n_years = 1
    sim = pars |> simulate()
    class(sim)
  }, "data.frame")
})
