test_that("meridional conversion works", {
  u = c(1,0,-1,0)
  v = c(0,1,0,-1)
  m = cbind(u,v)
  res = cbind(1,c(270,180,90,0))
  dimnames(res)=list(NULL,c("V",'D'))

  expect_equal(zmcomp2metconv(m),res)
  expect_equal(zmcomp2metconv(u,v),res)
})
