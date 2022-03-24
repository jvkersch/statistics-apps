test_that("plot grid is created correctly", {
  grid <- create_plotgrid(
    seq(1, 3, length.out=50), 
    seq(0.15, 0.40, length.out=40)
  )
  expect_equal(dim(grid$X), c(40, 50))
  expect_equal(dim(grid$Y), c(40, 50))
})