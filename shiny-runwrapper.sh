#!/bin/bash

# Grab PORT from the environment and fill it in to the configuration file. This
# is necessary because Heroku dynamically configures the port that the server
# listens on, but shiny-server apparently gives us no option to configure the
# port except through the config file.

PORT="${PORT:-3838}"
USER=$(whoami)
sed "s/PORT/$PORT/g; s/USER/$USER/g" < /srv/conf/shiny-server-template.conf > /etc/shiny-server/shiny-server.conf

# Run shiny
exec /usr/bin/shiny-server
