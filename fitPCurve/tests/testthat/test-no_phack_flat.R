test_that("no p-hacking of H0 leads to a flat p-curve", {
  ps <- sim.multDVhack(nvar=1, r=0, d=0, het = 0, iter = 100000, alpha = 0.05)
  pcurve <- compute_pcurve(ps[, 1])

  # Fuzzy expectation: less than 1 percentage point deviation from a flat distribution
  expect_lt((pcurve - 20) |> abs() |> max(), 1)
})
