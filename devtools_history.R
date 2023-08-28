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

usethis::use_data(rscd_field,rscd_spectral,rscd_coastline,rscd_islands,rscd_triangles,rscd_variables,version=3,overwrite = TRUE)

usethis::use_description()

usethis::use_build_ignore("dev/")
usethis::use_git()
usethis::use_github()

rcd_cassandra_url = "https://resourcecode-datacharts.ifremer.fr/"
rscd_hindcast_start_date=as.POSIXct("1994-01-01Z00:00:00")
rscd_hindcast_end_date=as.POSIXct("2022-12-31Z23:00:00")

rscd_casandra_start_date=as.POSIXct("1994-01-01Z00:00:00")
rscd_casandra_end_date=as.POSIXct("2020-12-31Z23:00:00")

usethis::use_data(rcd_cassandra_url,rscd_hindcast_start_date,rscd_hindcast_end_date,rscd_casandra_start_date,rscd_casandra_end_date,internal = TRUE,overwrite = TRUE)
