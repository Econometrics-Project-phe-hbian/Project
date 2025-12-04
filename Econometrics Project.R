"upload packages"
install.packages("data.table")
library(data.table)
install.packages("fixest")
library(fixest)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("haven")
library(haven)
install.packages("patchwork")
library(patchwork)
"check and read dataset"
reg <- read.csv("~/Project/GMFD_TINV_clim_regsort.csv")
brk <- read.csv("~/Project/break_data_TINV_clim.csv")

