#!/bin/bash

# Script to run Study1 and Study2 simulations in sequence
# Author: Generated script
# Date: $(date)

echo "Starting simulation scripts..."
echo "================================"

# Change to the data generation directory
cd "functions/data_generation"

echo "Running Study1_simulation.R..."
echo "------------------------------"
"/c/Program Files/R/R-4.2.3/bin/x64/Rscript.exe" Study1_simulation.R

# Check if Study1 completed successfully
if [ $? -eq 0 ]; then
    echo "Study1_simulation.R completed successfully!"
    echo ""
    
    echo "Running Study2_simulation.R..."
    echo "------------------------------"
    "/c/Program Files/R/R-4.2.3/bin/x64/Rscript.exe" Study2_simulation.R
    
    # Check if Study2 completed successfully
    if [ $? -eq 0 ]; then
        echo "Study2_simulation.R completed successfully!"
        echo ""
        echo "All simulations completed successfully!"
        echo "================================"
    else
        echo "Error: Study2_simulation.R failed with exit code $?"
        exit 1
    fi
else
    echo "Error: Study1_simulation.R failed with exit code $?"
    echo "Skipping Study2_simulation.R due to Study1 failure."
    exit 1
fi

echo "Script execution finished."