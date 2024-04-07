FROM rocker/shiny:4.3.3
# https://github.com/rocker-org/rocker-versioned2
# https://hub.docker.com/r/rocker/shiny

LABEL \
    maintainer="Shixiang Wang" \
    email="w_shixiang@163.com" \
    description="Docker Image for UCSCXenaShiny" \
    org.label-schema.license="GPLv3 (c) Xena Shiny Team" \
    org.label-schema.vcs-url="https://github.com/openbiox/UCSCXenaShiny"

# Install UCSCXenaShiny
# Install extra dependencies
RUN apt update -y && apt install -y libcurl4-openssl-dev libssl-dev libxml2-dev libgmp3-dev libmpfr-dev &&\
    install2.r remotes UCSCXenaShiny &&\
    R -e 'remotes::install_github("openbiox/UCSCXenaShiny", dependencies = TRUE)' &&\
    R -e 'source(system.file("shinyapp/utils_pkgs.R", package = "UCSCXenaShiny"))'
    
# Deploy Shiny shinyapp
# & allow permission to make sure app could also install required package(s)
COPY deploy.R deploy.html /opt/
RUN chmod u+x /opt/deploy.R &&\
    rm -rf /srv/shiny-server/* &&\
    mkdir /srv/shiny-server/ucscxenashiny &&\
    mv /opt/deploy.R /srv/shiny-server/ucscxenashiny/app.R &&\
    mv /opt/deploy.html /srv/shiny-server/index.html &&\
    mkdir -p /xena/datasets && chown -R shiny:shiny /xena &&\
    chown -R shiny:shiny /usr/local/lib/R/site-library
    
# preload datasets
RUN cat -n /srv/shiny-server/ucscxenashiny/app.R &&\
    R -e 'library(UCSCXenaShiny); source(system.file("shinyapp/utils_appdata.R", package = "UCSCXenaShiny"))'
WORKDIR /xena
EXPOSE 3838
