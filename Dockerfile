FROM rocker/shiny:4.1.0
# https://github.com/rocker-org/rocker-versioned2
# https://hub.docker.com/r/rocker/shiny

LABEL \
    maintainer="Shixiang Wang" \
    email="w_shixiang@163.com" \
    description="Docker Image for UCSCXenaShinyV1" \
    org.label-schema.license="GPLv3 (c) Xena Shiny Team" \
    org.label-schema.vcs-url="https://github.com/openbiox/UCSCXenaShinyV1"

# Install UCSCXenaShinyV1
RUN install2.r remotes UCSCXenaShinyV1 &&\
    R -e 'remotes::install_github("openbiox/UCSCXenaShinyV1", dependencies = TRUE)'
  
# Install extra dependencies
RUN R -e 'writeLines(readLines(system.file("shinyapp", "App.R", package = "UCSCXenaShinyV1"))[25:98], "/opt/ext-deps.R")' &&\
    Rscript /opt/ext-deps.R &&\
    rm /opt/ext-deps.R
    
# Deploy Shiny shinyapp
COPY deploy.R deploy.html /opt/
RUN chmod u+x /opt/deploy.R &&\
    rm -rf /srv/shiny-server/* &&\
    mkdir /srv/shiny-server/UCSCXenaShinyV1 &&\
    mv /opt/deploy.R /srv/shiny-server/UCSCXenaShinyV1/app.R &&\
    mv /opt/deploy.html /srv/shiny-server/index.html

# allow permission
RUN mkdir -p /xena/datasets && chown -R shiny:shiny /xena 
# preload datasets
RUN cat -n /srv/shiny-server/UCSCXenaShinyV1/app.R &&\
    R -e 'writeLines(readLines("/srv/shiny-server/UCSCXenaShinyV1/app.R")[10:25], "/opt/preload.R")' &&\
    Rscript /opt/preload.R &&\
    rm /opt/preload.R
WORKDIR /xena
EXPOSE 3838
