install.packages(c("available", "pak"))
library(available,pak)
available("DNAtranslate")
pak::pkg_name_check("DNAtranslate")

install.packages(c("devtools", "checkmate", "tidyverse", "remotes"))
library(devtoools, checkmate, tidyverse, remotes)

usethis::create_package("/cloud/project")
getwd()

usethis::use_r("readFASTQ")
readFASTQ <- function(x)
{
  
}

