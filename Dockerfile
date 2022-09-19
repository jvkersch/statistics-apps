# From the instructions in https://blog.sellorm.com/2021/04/25/shiny-app-in-docker/

FROM rocker/shiny:4.0.5

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev

# Install dependencies
COPY ./install-requirements.R /srv
RUN Rscript /srv/install-requirements.R

# Add apps
ADD ./apps /srv/apps

# Set up entry point and config
ADD ./conf /srv/conf
COPY ./shiny-runwrapper.sh /srv

CMD ["/srv/shiny-runwrapper.sh"]
