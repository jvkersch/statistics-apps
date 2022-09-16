# From the instructions in https://blog.sellorm.com/2021/04/25/shiny-app-in-docker/

FROM rocker/shiny:4.0.5

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev

RUN R -e 'install.packages(c(\
              "shiny", \
              "shinydashboard", \
              "ggplot2" \
            ), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2021-04-23"\
          )'

# Add apps
RUN rm -rf /srv/shiny-server/*
ADD ./apps /srv/shiny-server/

# Set up entry point and config
ADD ./conf /srv/conf
COPY ./shiny-runwrapper.sh /srv

CMD ["/srv/shiny-runwrapper.sh"]
