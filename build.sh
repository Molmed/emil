#!/bin/sh

R -f compile_package.R
R CMD build .

