#!/bin/bash

# Grab PORT from the environment and fill it in to the configuration file. This
# is necessary because Heroku dynamically configures the port that the server
# listens on, but shiny-server apparently gives us no option to configure the
# port except through the config file. Likewise, Heroku runs the container as
# an arbitrary USER, so that needs to go into the config file as well. For some
# reason shiny-server refuses to run if run_as is set to root, so in the case
# we're running as root we use the default shiny user instead.
PORT="${PORT:-3838}"
USER=$(whoami)
if [ "$USER" == "root" ]; then
    USER="shiny"
fi

sed "s/PORT/$PORT/g; s/USER/$USER/g" < /srv/conf/shiny-server-template.conf > /etc/shiny-server/shiny-server.conf

# Run shiny
exec /usr/bin/shiny-server
