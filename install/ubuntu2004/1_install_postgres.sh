#!/bin/bash

sudo apt install postgresql libpq-dev postgresql-client postgresql-client-common -y
sudo -u postgres createuser -s $USER
sudo -u $USER createdb $USER
