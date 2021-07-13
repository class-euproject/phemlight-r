#!/bin/bash
docker run --rm --name phemlight_cont --group-add users --user "$(id -u)" -w="/home/esabate/pollutionMap" --mount source=pollVolume,destination=/home/esabate/pollutionMap jupyter/r-poll_dash Rscript /home/esabate/pollutionMap/phemlight-r/PHEMLight_advance.R $1 $2 $3
