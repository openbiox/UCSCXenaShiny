FROM rocker/r-ver:4.1.0

LABEL \
    maintainer="Shixiang Wang" \
    email="w_shixiang@163.com" \
    description="Docker Image for UCSCXenaShiny" \
    org.label-schema.license="MIT (c) Xena Shiny Team" \
    org.label-schema.vcs-url="https://github.com/openbiox/UCSCXenaShiny"

COPY deploy.R /opt/
RUN chmod u+x /opt/deploy.R &&\
    mkdir -p /opt/xena &&\
    install2.r remotes UCSCXenaShiny &&\
    R -e 'remotes::install_github("openbiox/UCSCXenaShiny@container", dependencies = TRUE)' &&\
    R -e 'writeLines(readLines(system.file("shinyapp", "App.R", package = "UCSCXenaShiny"))[25:94], "/opt/ext-deps.R")' &&\
    Rscript /opt/ext-deps.R

WORKDIR /opt/xena
EXPOSE 3838
CMD ["/opt/deploy.R"]