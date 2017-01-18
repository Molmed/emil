#!/bin/sh

bash build.sh

ls -t emil_*.tar.gz | head -1 | xargs R CMD check

