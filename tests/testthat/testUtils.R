context("Tests on utils.Rt")

# defines limit for double values comparison
limit <- 0.00001

test_that("computeoddsc function", {
  n1 <- 4
  n2 <- 4
  x1 <- c(1,0,1,0)
  x2 <- c(0,0,0,0)
  odds <- computeoddsc(n1, n2, x1, x2, 2)

  expect_true(abs(odds[1] == 1.300317) < limit)
  expect_true(abs(odds[2] == 1.300317) < limit)
  expect_true(abs(odds[3] == 1.300317) < limit)
  expect_true(abs(odds[4] == 1.300317) < limit)
})
