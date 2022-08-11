
library(devtools)
test()

document()
# use_readme_rmd() # only need to do this once
devtools::build_readme() # builds README.md from README.Rmd
document()

check()
install()

#usethis::use_github_actions()
#usethis::use_github_actions_badge()
#usethis::use_logo("stickers/colouRmaps.png")

#usethis::use_vignette("colouRmaps")
