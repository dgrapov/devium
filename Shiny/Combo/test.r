
library(shiny)
runApp("C:\\Users\\D\\Dropbox\\Devium\\devium\\Shiny\\Combo")

#radyant fork for new devium framework
# install.packages('shiny', repos = "http://cran.rstudio.com")
shiny::runGitHub('radyant','dgrapov', subdir = 'inst/marketing')

# from local
runApp("C:/Users/D/Dropbox/Devium/radyant/inst/marketing")
# shiny::runGitHub('radyant','dgrapov', subdir = 'inst/marketing') # from repo

#gist
runGist('6147592')
runGist('6148173')
