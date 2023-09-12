test_that("downloading parameters data works",{
  skip_if_offline()
  dat = get_parameters(parameters = c('hs','tp'),node=42,start="1994-01-01 00:00:00 UTC",end = 760057200)
  expect_s3_class(dat,"data.frame")
  expect_equal(names(dat),c('time','hs','tp'))
  expect_equal(NROW(dat),24*31)
  expect_equal(get_parameters(parameters = c('hs','tp'),node=42,start=757382400,end = "1994-01-31 23:00:00 UTC"),dat)
})

test_that("downloading 1D spectral data works",{
  skip_if_offline()
  spec = get_1Dspectrum(1L,start = "1994-01-01 00:00:00 UTC",end="1994-01-01 00:00:00 UTC")
  expect_type(spec,"list")
  expect_equal(names(spec),c("longitude","latitude","frequency1","frequency2","ef","th1m","th2m","sth1m","sth2m","freq","forcings","station"))
  expect_equal(NROW(spec$forcings),744)
  expect_equal(spec$station,"E001500N52000")
})


test_that("downloading 2D spectral data works",{
  skip_if_offline()
  spec = get_2Dspectrum(1L,start = "1994-01-01 00:00:00 UTC",end="1994-01-01 00:00:00 UTC")
  expect_type(spec,"list")
  expect_equal(names(spec),c("longitude","latitude","frequency1","frequency2","efth","freq","dir","forcings","station"))
  expect_equal(NROW(spec$forcings),744)
  expect_equal(spec$station,"E001500N52000")
})
