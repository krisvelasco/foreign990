## Project: Nonprofit Foreign Expenditures

## Overview: 
# This file uses Schedule I from Form 990
# to compile a list of anti-LGBTQ+ non profit organizations.

#Last updated: April 21 by Sebastian Rojas Cabal
# NOTE: On Feb 16, I changed one of the last filters such that
# there is no "bottom" year filter. Filters can then be done with plots, etc

#--------------------------------------------------------
# Preliminaries
#   Loading packages
#--------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readxl)
#--------------------------------------------------------

