# installs dependencies, runs R CMD check, runs covr::codecov()
do_package_checks()

if (ci_on_ghactions() && ci_has_env("BUILD_PKGDOWN")) {
  
  get_stage("before_deploy") %>%
    add_step(step_setup_push_deploy(branch = "main", orphan = FALSE))
  
  # creates pkgdown site and pushes to gh-pages branch
  # only for the runner with the "BUILD_PKGDOWN" env var set
  get_stage("install") %>%
    add_step(step_install_github("mlr-org/mlr3pkgdowntemplate"))
  
  do_pkgdown()
  
  get_stage("deploy") %>%
    add_code_step(rmarkdown::render("README.Rmd")) %>%
    add_step(step_do_push_deploy(commit_paths = c("README.md")))
}
