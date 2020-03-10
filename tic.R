# installs dependencies, runs R CMD check, runs covr::codecov()
do_package_checks()

if (ci_on_travis()) {
  # creates pkgdown site and pushes to gh-pages branch
  get_stage("install") %>%
    add_step(step_install_github("mlr-org/mlr3pkgdowntemplate"))
  do_pkgdown()
}

get_stage("after_success") %>%
  add_code_step(system("curl -s https://raw.githubusercontent.com/mlr-org/mlr3orga/master/trigger-mlr3book.sh | bash"))
