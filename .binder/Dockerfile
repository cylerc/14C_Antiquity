FROM rocker/binder:4.0.2
LABEL maintainer='Ben Marwick'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}

# run our compendium
RUN wget https://github.com/cylerc/14C_Antiquity/raw/master/DESCRIPTION && R --vanilla -s -e "options(repos = list(CRAN = 'http://cran.rstudio.com'));  install.packages('renv'); renv::restore()"

RUN rm DESCRIPTION.1; exit 0




