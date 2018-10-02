
library("miniCRAN")

# use Revolution Analytics CRAN mirror
revolution <- c(CRAN="http://cran.microsoft.com")
rstudio <- c(CRAN="http://cran.rstudio.com")

# Specify list of packages to download
pkgs <- c("tidyverse")
pkgList <- pkgDep(pkgs, suggests = FALSE)
pkgList

# Create temporary folder for miniCRAN
dir.create(pth <- file.path("R:/epidemia/2017 R workshop/miniCRAN"))

# Make repo for source and win.binary
makeRepo(pkgList, path=pth, type=c("source", "win.binary"))

# install packages from your local repository
# install.packages(pkgs, 
#                  repos = paste0("file:///", pth),
#                  type = "source")
