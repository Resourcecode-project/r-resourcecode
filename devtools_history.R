usethis::use_build_ignore("devtools_history.R")
usethis::use_gpl3_license()
usethis::use_readme_md()
usethis::use_news_md()


rscd_field = arrow::read_feather("\\\\datawork\\datawork-resourcecode\\EFTP\\RESULTS\\stats\\grid_FIELD.arrow")
rscd_spectral = arrow::read_feather("\\\\datawork\\datawork-resourcecode\\EFTP\\RESULTS\\stats\\grid_SPEC.arrow")
rscd_coastline = arrow::read_feather("\\\\datawork\\datawork-resourcecode\\EFTP\\RESULTS\\stats\\coastline.arrow")
rscd_islands = arrow::read_feather("\\\\datawork\\datawork-resourcecode\\EFTP\\RESULTS\\stats\\islands.arrow")
rscd_triangles = t(arrow::read_feather("\\\\datawork\\datawork-resourcecode\\EFTP\\RESULTS\\stats\\triangles.arrow"))
rscd_variables = arrow::read_feather("\\\\datawork\\datawork-resourcecode\\EFTP\\RESULTS\\stats\\variables.arrow")
rscd_freq = array(0.0339*1.1^(0:35))
rscd_dir = array(seq(from=0,to=350,by=10))
usethis::use_data(rscd_field,rscd_spectral,rscd_coastline,rscd_islands,rscd_triangles,rscd_variables,rscd_freq,rscd_dir,version=3,overwrite = TRUE)

usethis::use_description()

usethis::use_build_ignore("dev/")
usethis::use_git()
usethis::use_github()

rcd_cassandra_url = "https://resourcecode-datacharts.ifremer.fr/"
rscd_hindcast_start_date=as.POSIXct("1994-01-01 00:00:00 UTC",tz='UTC')
rscd_hindcast_end_date=as.POSIXct("2022-12-31 23:00:00 UTC",tz='UTC')

rscd_casandra_start_date=as.POSIXct("1994-01-01 00:00:00 UTC",tz='UTC')
rscd_casandra_end_date=as.POSIXct("2020-12-31 23:00:00 UTC",tz='UTC')

usethis::use_data(rcd_cassandra_url,rscd_hindcast_start_date,rscd_hindcast_end_date,rscd_casandra_start_date,rscd_casandra_end_date,internal = TRUE,version=3,overwrite = TRUE)

#set up automatic "check" on several plateforms
usethis::use_github_action()

#Now set up pkgdown to have a nice page
usethis::use_pkgdown()
pkgdown::build_site()
usethis::use_pkgdown_github_pages()

#Hex Logo
library(showtext)
font_add_google("Exo 2", "Exo 2")
hexSticker::sticker("rscd.png", package="resourceCODE", p_size=20, s_x=1, s_y=.75, s_width=.55,
                    p_family="Exo 2",
                    p_color = '#756662',
                    h_fill = "white",
                    h_color = "#00AAE1",
                    url = "https://resourcecode.ifremer.fr",
                    u_family="Exo 2",
                    u_size = 5,
        filename="resourcecode_logo.png")
usethis::use_logo("resourcecode_logo.png")
usethis::use_vignette("resourcecode")

#tests
devtools::test()
usethis::use_test("test_utils")

#code ceverage
usethis::use_coverage(type = "codecov")
devtools::build_readme()
usethis::use_github_action("test-coverage")

#Rcpp Armadillo for fast multivariate trapz
usethis::use_rcpp_armadillo("fast_trapz.cpp")
