FROM rocker/r-ver:4.1.0
LABEL maintainer="Shixiang Wang"
RUN ["install2.r", "ggplot2", "ggpubr", "ggstatsplot", "shiny", "survminer", "UCSCXenaShiny"]
WORKDIR /payload/
CMD ["R"]