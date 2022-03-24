expect_does_not_contain <- function(w, s) {
  expect_false(grepl(w, s))
}
expect_contains <- function(w, s) {
  expect_true(grepl(w, s))
}

test_that("disabling all variables creates an empty model", {
  testServer(expr = {
    session$setInputs(variables = c())
    expect_does_not_contain("nitrogen", output$summary)
    expect_does_not_contain("phosphor", output$summary)
  })
})

test_that("toggling nitrogen adds it to the model", {
  testServer(expr = {
    session$setInputs(variables = c("nitrogen"))
    expect_contains("nitrogen", output$summary)
    expect_does_not_contain("phosphor", output$summary)
  })
})

test_that("toggling nitrogen and phosphor add them to the model", {
  testServer(expr = {
    session$setInputs(variables = c("nitrogen", "phosphor"))
    expect_contains("nitrogen", output$summary)
    expect_contains("phosphor", output$summary)
  })
})