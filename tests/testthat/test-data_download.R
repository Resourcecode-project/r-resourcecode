test_that("downloading parameters data works",{
  skip_if_offline()
  dat = get_parameters(parameters = c('hs','tp'),node=42,start="1994-01-01 00:00:00 UTC",end = 760057200)
  expect_s3_class(dat,"data.frame")
  expect_equal(names(dat),c('time','hs','tp'))
  expect_equal(NROW(dat),24*31)
  expect_equal(get_parameters(parameters = c('hs','tp'),node=42,start=757378800,end = "1994-01-31 23:00:00 UTC"),dat)
})

