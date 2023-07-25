test_that("Calculating HAD works", {
  expect_equal({
    pars <- parameters()
    pars$n_years = 1
    abc = pars |> simulate() |> calculate_HAD()
    abc$field.1[1]
  }, 407.96148)
})
