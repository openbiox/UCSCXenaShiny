FROM rocker/r-ver:4.1.0

LABEL \
    maintainer="Shixiang Wang" \
    email="w_shixiang@163.com" \
    description="Docker Image for UCSCXenaShiny" \
    org.label-schema.license="MIT" \
    org.label-schema.vcs-url="https://github.com/openbiox/UCSCXenaShiny"

RUN installGithub.r -d openbiox/UCSCXenaShiny@container

WORKDIR /payload/
EXPOSE 3838
CMD ["R"]