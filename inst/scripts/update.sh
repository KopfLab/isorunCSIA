#! /bin/bash
# This is the script to update isorunCSIA on unix systems (linux/OS X)

# make sure we're running from the directory where the script is located
SOURCE="${BASH_SOURCE[0]}"
DIR="$( dirname "$SOURCE" )"
cd "$DIR"

Rscript -e "isorunCSIA::update_isorunCSIA(); Sys.sleep(5)"
