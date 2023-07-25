test_that("Adding a spray works", {

  expect_equal({
    par <- parameters()
    par$management = data.frame(Fungicide = 1,Year = 1,Dose = 1,Field = 1,Time = 2)
    y = c(1,1,1,0,0,0)
    names(y) = c("H","L","I","R","D1.F1","D1.F2")
    y2 = add_spray(.y = y,.time = 2,.year = 1,par)
    y2["D1.F1"]
  },c(D1.F1 = 1))
})
