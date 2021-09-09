FROM rocker/binder:4.0.2
LABEL maintainer='Ben Marwick'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}



RUN wget https://github.com/cylerc/14C_Antiquity/raw/master/DESCRIPTION && R -e "renv::restore(repos = c(CRAN='https://packagemanager.rstudio.com/all/__linux__/bionic/298'))"

RUN rm DESCRIPTION.1; exit 0
