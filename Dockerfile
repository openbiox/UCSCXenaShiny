FROM rocker/shiny:4.1.0
# https://github.com/rocker-org/rocker-versioned2
# https://hub.docker.com/r/rocker/shiny

LABEL \
    maintainer="Shixiang Wang" \
    email="w_shixiang@163.com" \
    description="Docker Image for UCSCXenaShiny" \
    org.label-schema.license="MIT (c) Xena Shiny Team" \
    org.label-schema.vcs-url="https://github.com/openbiox/UCSCXenaShiny"

# Install UCSCXenaShiny
RUN mkdir -p /opt/xena &&\
    install2.r remotes UCSCXenaShiny &&\
    R -e 'remotes::install_github("openbiox/UCSCXenaShiny@container", dependencies = TRUE)'
  
# Install extra dependencies
RUN R -e 'writeLines(readLines(system.file("shinyapp", "App.R", package = "UCSCXenaShiny"))[25:95], "/opt/ext-deps.R")' &&\
    Rscript /opt/ext-deps.R &&\
    rm /opt/ext-deps.R
    
# Deploy Shiny shinyapp
COPY deploy.R /opt/
RUN chmod u+x /opt/deploy.R &&\
    rm -rf /srv/shiny-server/* &&\
    mkdir /srv/shiny-server/ucscxenashiny &&\
    mv /opt/deploy.R /srv/shiny-server/ucscxenashiny/app.R &&\
    cat <<EOF >>/srv/shiny-server/index.html
<!DOCTYPE HTML>
<html lang="en-US">
  <head>
    <meta charset="UTF-8">
    <meta http-equiv="refresh" content="0; url=./ucscxenashiny">
    <script type="text/javascript">
        window.location.href = "./ucscxenashiny"
    </script>
    <title>Page Redirection</title>
  </head>
  <body>
    If you are not redirected automatically, follow <a href='./ucscxenashiny'>this link</a>.
  </body>
</html>
EOF

WORKDIR /opt/xena
EXPOSE 3838
