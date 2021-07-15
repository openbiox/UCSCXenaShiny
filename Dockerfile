FROM rocker/shiny:4.1.0
# https://github.com/rocker-org/rocker-versioned2
# https://hub.docker.com/r/rocker/shiny

LABEL \
    maintainer="Shixiang Wang" \
    email="w_shixiang@163.com" \
    description="Docker Image for UCSCXenaShiny" \
    org.label-schema.license="GPLv3 (c) Xena Shiny Team" \
    org.label-schema.vcs-url="https://github.com/openbiox/UCSCXenaShiny"

# Install UCSCXenaShiny
RUN install2.r remotes UCSCXenaShiny &&\
    R -e 'remotes::install_github("openbiox/UCSCXenaShiny", dependencies = TRUE)'
  
# Install extra dependencies
RUN R -e 'writeLines(readLines(system.file("shinyapp", "App.R", package = "UCSCXenaShiny"))[25:98], "/opt/ext-deps.R")' &&\
    Rscript /opt/ext-deps.R &&\
    rm /opt/ext-deps.R
    
# Deploy Shiny shinyapp
COPY deploy.R deploy.html /opt/
RUN chmod u+x /opt/deploy.R &&\
    rm -rf /srv/shiny-server/* &&\
    mkdir /srv/shiny-server/ucscxenashiny &&\
    mv /opt/deploy.R /srv/shiny-server/ucscxenashiny/app.R &&\
    mv /opt/deploy.html /srv/shiny-server/index.html

# allow permission
RUN mkdir -p /xena/datasets && chown -R shiny:shiny /xena 
# preload datasets
RUN cat -n /srv/shiny-server/ucscxenashiny/app.R &&\
    R -e 'writeLines(readLines("/srv/shiny-server/ucscxenashiny/app.R")[10:25], "/opt/preload.R")' &&\
    Rscript /opt/preload.R &&\
    rm /opt/preload.R
WORKDIR /xena
EXPOSE 3838
