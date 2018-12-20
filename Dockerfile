# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:3.5.1

# required
MAINTAINER Ben Marwick <benmarwick@gmail.com>

COPY . /datingfourthaisites

# go into the repo directory
RUN . /etc/environment \

  # Install linux and other depedendencies here
  # e.g. need this for ggforce::geom_sina
  && sudo apt-get update \
  && sudo apt-get install libudunits2-dev -y \
  && R -e "devtools::install_github('rstudio/gt', dep=TRUE)" \

  # build this compendium package
  && R -e "devtools::install('/datingfourthaisites', dep=TRUE)" \

 # render the manuscript into a docx, you'll need to edit this if you've
 # customised the location and name of your main Rmd file
  && R -e "rmarkdown::render('/datingfourthaisites/analysis/paper/paper.Rmd')"
