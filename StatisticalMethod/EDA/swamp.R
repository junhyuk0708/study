#new session
rm(list = ls())
#-------------------------------------------------------------
eda.pkgs <- c("knitr","kableExtra","remotes", "devtools", "skimr",
              "showtext", "rmdformats","rlang",   
              "tidyverse", "tidyr", "magrittr", "reshape2", "forcats",
              "ggplot2", "ggExtra","ggrepel",  "GGally", "ggmosaic", 
              "viridis", "scico", "patchwork",
              "lvplot", "scales", "MASS", "vcd", "vcdExtra",
              "rgdal", "plotly", "sf", "aplpack",
		  "gridExtra" #그림 여러 개 표현
)

lp <- function(pkg){
 new.pkg <- pkg[!(pkg %in% utils::installed.packages()[, "Package"])]
 if (length(new.pkg))
  utils::install.packages(new.pkg, repos = "https://cran.r-project.org", dependencies = TRUE)
 sapply(pkg, require, character.only = TRUE)
}
#
lp(eda.pkgs)
#-------------------------------------------------------------
showtext::showtext_auto()
#--------------------------------------------------------------
#data 경로 설정
data_path <- paste0(getwd(),"/data/")
#-------------------------------------------------------------
lre <- "#A50034" #lightRed
 lgr <- "#6b6b6b" #lightGreen
  lgo <- "#FAB23D" #lighGolden
   ldg <- "#383838" #DarkGray
    sbl <- "#4682B4" #SteelBlue
     sgr <- "#00704A" #Sbucks green
      ngr <- "#2DB400" #naver green

 
#---------------------------------------------------------------
