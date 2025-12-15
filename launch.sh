#!/bin/bash


#---- linux system dependencies ----#
apt-get update -y

apt-get install -y pip python3.12-venv

# dependencies of R package sf
apt-get install -y curl libssl-dev libudunits2-dev unixodbc-dev libpq-dev libfontconfig1-dev libcurl4-openssl-dev libgdal-dev

# dependencies of R package tidyverse
apt-get install -y libharfbuzz-dev libfribidi-dev

# dependencies of R package git2r
apt-get install -y libgit2-dev


#---- python dependencies (OS independent) ----#
pip install -r requirements.txt
pip install git+https://github.com/GISRedeDev/PyNetworkFriction


#---- R dependencies (OS independent) ----#
Rscript requirements.R
