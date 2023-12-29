all: README.md

README.md: README.Rmd
	R -e "devtools::build_readme()"

deploy:
	@R -e "try(rsconnect::terminateApp(appName = 'lifetableprojection', account = 'unpop'), silent = TRUE); \
	unlink(renv::paths[['cache']](), recursive = TRUE); \
  setwd('/home/jorge/repositories/lifetableprojection/'); \
	rsconnect::forgetDeployment('/home/jorge/repositories/lifetableprojection/'); \
	rsconnect::deployApp(appName = 'lifetableprojection', account = 'unpop', forceUpdate = TRUE)"
