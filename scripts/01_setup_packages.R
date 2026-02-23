# Install and load required packages

packages <- c(
  "tidyverse", "janitor", "naniar", "tableone",
  "broom", "binom", "ggplot2", "lubridate", "scales"
)

to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

library(tidyverse)
library(janitor)
library(naniar)
library(tableone)
library(broom)
library(binom)
library(ggplot2)
library(lubridate)
library(scales)