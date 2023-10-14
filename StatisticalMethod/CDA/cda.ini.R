# output format
options(width = 72, "scipen"= 999, "digits"= 3)
# list of packages
cda.pkgs <- c("knitr", "markdown",
              "kableExtra", "skimr", "grid", "scales", "MASS", 
              "tidyverse", "dplyr", "magrittr", "readr", "readxl", "lubridate", 
              "stringr", "forcats", "reshape2", "tidytext", "ggplot2", 
              "rlang", "ggpubr", "GGally", "ggrepel", "scico", "patchwork", 
              "lvplot", "vcd", "vcdExtra", "aplpack", "HH", 
              "nortest", "car", "gridExtra"
              )
lp <- function(pkg){
 new.pkg <- pkg[!(pkg %in% utils::installed.packages()[, "Package"])]
 if (length(new.pkg))
  utils::install.packages(new.pkg, repos = "https://cran.r-project.org", dependencies = TRUE)
 sapply(pkg, require, character.only = TRUE)
}
#
lp(cda.pkgs)
#-------------------------------------------------------------
showtext::showtext_auto() #한글보여주는 패키지
#--------------------------------------------------------------
# data_path <-"./data/" #경로 설정
data_path <- paste0(getwd(), "/data/")
#--------------------------------------------------------------