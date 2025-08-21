usethis::use_build_ignore("devtools_history.R")
usethis::use_gpl3_license()
usethis::use_readme_md()
usethis::use_news_md()

# This data now lives in the {resourcecodedata} package
#
# rscd_field = as.data.frame(arrow::read_feather("\\\\datawork\\datawork-resourcecode\\EFTP\\RESULTS\\stats\\grid_FIELD.arrow"))
# rscd_spectral = as.data.frame(arrow::read_feather("\\\\datawork\\datawork-resourcecode\\EFTP\\RESULTS\\stats\\grid_SPEC.arrow"))
# rscd_coastline = as.data.frame(arrow::read_feather("\\\\datawork\\datawork-resourcecode\\EFTP\\RESULTS\\stats\\coastline.arrow"))
# rscd_islands = as.data.frame(arrow::read_feather("\\\\datawork\\datawork-resourcecode\\EFTP\\RESULTS\\stats\\islands.arrow"))
# rscd_triangles = t(arrow::read_feather("\\\\datawork\\datawork-resourcecode\\EFTP\\RESULTS\\stats\\triangles.arrow"))
# rscd_variables = as.data.frame(arrow::read_feather("\\\\datawork\\datawork-resourcecode\\EFTP\\RESULTS\\stats\\variables.arrow"))
# usethis::use_data(rscd_field,rscd_spectral,rscd_coastline,rscd_islands,rscd_triangles,rscd_variables,rscd_freq,rscd_dir,version=3,overwrite = TRUE)
#
# tools::resaveRdaFiles("data/")
# tools::checkRdaFiles("data/")

usethis::use_description()

usethis::use_build_ignore("dev/")
usethis::use_git()
usethis::use_github()

rscd_freq = array(0.0339 * 1.1^(0:35))
rscd_dir = array(seq(from = 0, to = 350, by = 10))
usethis::use_data(rscd_freq, rscd_dir, version = 3, overwrite = TRUE)

rcd_cassandra_url = "https://resourcecode-datacharts.ifremer.fr/"
rscd_hindcast_start_date = as.POSIXct("1994-01-01 00:00:00", tz = 'UTC')
rscd_hindcast_end_date = as.POSIXct("2022-12-31 23:00:00", tz = 'UTC')

rscd_casandra_start_date = as.POSIXct("1994-01-01 00:00:00", tz = 'UTC')
rscd_casandra_end_date = as.POSIXct("2020-12-31 23:00:00", tz = 'UTC')

usethis::use_data(
  rcd_cassandra_url,
  rscd_hindcast_start_date,
  rscd_hindcast_end_date,
  rscd_casandra_start_date,
  rscd_casandra_end_date,
  rscd_freq,
  rscd_dir,
  internal = TRUE,
  version = 3,
  overwrite = TRUE
)


#set up automatic "check" on several plateforms
usethis::use_github_action()

#Now set up pkgdown to have a nice page
usethis::use_pkgdown()
pkgdown::build_site()
usethis::use_pkgdown_github_pages()

#Hex Logo
library(showtext)
sysfonts::font_add_google("Exo 2", "Exo 2")
hexSticker::sticker(
  "rscd.png",
  package = "resourceCODE",
  p_size = 20,
  s_x = 1,
  s_y = .75,
  s_width = .55,
  p_family = "Exo 2",
  p_color = '#756662',
  h_fill = "white",
  h_color = "#00AAE1",
  url = "https://resourcecode.ifremer.fr",
  u_family = "Exo 2",
  u_size = 5,
  filename = "resourcecode_logo.png"
)
usethis::use_logo("resourcecode_logo.png")
usethis::use_vignette("resourcecode")

#tests
devtools::test()
usethis::use_test("test_utils")

usethis::use_build_ignore("[.]svg$", escape = FALSE)

#code ceverage
usethis::use_coverage(type = "codecov")
devtools::build_readme()
usethis::use_github_action("test-coverage")

#Rcpp Armadillo for fast multivariate trapz
usethis::use_rcpp_armadillo("fast_trapz.cpp")

# Lintr checks and GHA
install.packages("lintr")
install.packages("styler")
lintr::use_lintr()
lintr::lint_package()
usethis::use_github_action("lint")
styler::style_pkg()

devtools::load_all()
devtools::spell_check()
attachment::att_amend_desc(
  extra.suggests = "resourcecodedata",
  pkg_ignore = "resourcecodedata",
  check_if_suggests_is_installed = TRUE,
  use.config = FALSE
)
devtools::document()
devtools::run_examples()
urlchecker::url_check()
devtools::build_readme()
devtools::install()
testthat::test_dir("tests/testthat/", package = "resourcecode")

# Check content
# install.packages('checkhelper', repos = 'https://thinkr-open.r-universe.dev')
# All functions must have either `@noRd` or an `@export`.
checkhelper::find_missing_tags()

# Check that you let the house clean after the check, examples and tests
# If you used parallel testing, you may need to avoid it for the next check with `Config/testthat/parallel: false` in DESCRIPTION
all_files_remaining <- checkhelper::check_clean_userspace()
# If needed, set back parallel testing with `Config/testthat/parallel: true` in DESCRIPTION

# check on other distributions
# _rhub v2
rhub::rhub_setup() # Commit, push, merge
rhub::rhub_doctor()
rhub::rhub_platforms()
rhub::rhub_check() # launch manually

# _win devel CRAN
devtools::check_win_release()
devtools::check_win_oldrelease()
devtools::check_mac_release()


# Upgrade version number
usethis::use_version(which = c("patch", "minor", "major", "dev")[1])

usethis::use_release_issue()

devtools::release()
