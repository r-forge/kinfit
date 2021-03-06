context("Time step normalisation")

test_that("Simple temperature and moisture normalisation works", {
  expect_equal(round(f_time_norm_focus(25, 20, 25), 2), 1.37)
})

test_that("Time step normalisation for a dataset works", {
  expect_output(f_time_norm_focus(D24_2014, study_moisture_ref_source = "focus", f_na = 1),
    "was set to")
  expect_equal(round(D24_2014$f_time_norm, 3), c(1.606, 0.712, 0.716, 0.716, 0.898))
  expect_output(f_time_norm_focus(dimethenamid_2018), "was set to")

  # Reference values from Dimethenamid RAR 2018 Vol3 B.8
  expect_equal(round(dimethenamid_2018$f_time_norm, 3), 
    c(1,
      rep(0.971, 2),                   # p. 56
      rep(round(1.329 * 0.924, 3), 2), # p. 51
      0.623, 0.768, 0.673)             # p. 45
  )
})
