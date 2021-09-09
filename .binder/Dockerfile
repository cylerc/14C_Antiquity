FROM rocker/binder:4.0.2
LABEL maintainer='Ben Marwick'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}

# install renv
ENV RENV_VERSION 0.14.0
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

# run our compendium
RUN wget https://github.com/cylerc/14C_Antiquity/raw/master/DESCRIPTION && R -e "renv::restore(repos = c(CRAN='https://packagemanager.rstudio.com/all/__linux__/bionic/298'))"

RUN rm DESCRIPTION.1; exit 0
