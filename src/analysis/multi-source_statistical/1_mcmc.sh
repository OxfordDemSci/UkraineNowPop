#!/bin/bash

# Define the path to your R script
R_SCRIPT="1_mcmc.R"

# execute in the background with logging
nohup Rscript "$R_SCRIPT" > mcmc.log 2>&1 &

# Get the Process ID (PID) of the process just started
PID=$!

echo "R script '$R_SCRIPT' is running in the background with PID: $PID"
echo "You can check progress by running: tail -f mcmc.log"