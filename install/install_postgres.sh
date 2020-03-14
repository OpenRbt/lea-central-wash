#!/bin/bash

apt install postgresql libpq-dev postgresql-client postgresql-client-common -y
sudo -u postgres createuser -s pi
sudo -u pi createdb pi
