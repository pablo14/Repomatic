#####################################################
#### LIBRARIES INSTALLATION
#####################################################
if(require("Rcpp")){
  sprintf("%s is loaded correctly", "Rcpp")
} else {
  sprintf("trying to install %s", "Rcpp")
  install.packages("Rcpp")
  if(require("Rcpp")){
    sprintf("%s installed and loaded", "Rcpp")
  } else {
    stop(sprintf("could not install %s", "Rcpp"))
  }
}

#####################################################
if(require("plyr")){
  sprintf("%s is loaded correctly", "plyr")
} else {
  sprintf("trying to install %s", "plyr")
  install.packages("plyr")
  if(require("plyr")){
    sprintf("%s installed and loaded", "plyr")
  } else {
    stop(sprintf("could not install %s", "plyr"))
  }
}

#####################################################

if(require("ggplot2")){
  sprintf("%s is loaded correctly", "ggplot2")
} else {
  sprintf("trying to install %s", "ggplot2")
  install.packages("ggplot2")
  if(require("ggplot2")){
    sprintf("%s installed and loaded", "ggplot2")
  } else {
    stop(sprintf("could not install %s", "ggplot2"))
  }
}

#####################################################

if(require("knitr")){
  sprintf("%s is loaded correctly", "knitr")
} else {
  sprintf("trying to install %s", "knitr")
  install.packages("knitr")
  if(require("knitr")){
    sprintf("%s installed and loaded", "knitr")
  } else {
    stop(sprintf("could not install %s", "knitr"))
  }
}

#####################################################
if(require("pander")){
  sprintf("%s is loaded correctly", "pander")
} else {
  sprintf("trying to install %s", "pander")
  install.packages("pander")
  if(require("pander")){
    sprintf("%s installed and loaded", "pander")
  } else {
    stop(sprintf("could not install %s", "pander"))
  }
}

#####################################################
if(require("markdown")){
  sprintf("%s is loaded correctly", "markdown")
} else {
  sprintf("trying to install %s", "markdown")
  install.packages("markdown")
  if(require("markdown")){
    sprintf("%s installed and loaded", "markdown")
  } else {
    stop(sprintf("could not install %s", "markdown"))
  }
}

#####################################################
if(require("reshape2")){
  sprintf("%s is loaded correctly", "reshape2")
} else {
  sprintf("trying to install %s", "reshape2")
  install.packages("reshape2")
  if(require("reshape2")){
    sprintf("%s installed and loaded", "reshape2")
  } else {
    stop(sprintf("could not install %s", "reshape2"))
  }
}

#####################################################
if(require("Hmisc")){
  sprintf("%s is loaded correctly", "Hmisc")
} else {
  sprintf("trying to install %s", "Hmisc")
  install.packages("Hmisc")
  if(require("Hmisc")){
    sprintf("%s installed and loaded", "Hmisc")
  } else {
    stop(sprintf("could not install %s", "Hmisc"))
  }
}

#####################################################
if(require("xlsx")){
  sprintf("%s is loaded correctly", "xlsx")
} else {
  sprintf("trying to install %s", "xlsx")
  install.packages("xlsx")
  if(require("xlsx")){
    sprintf("%s installed and loaded", "xlsx")
  } else {
    stop(sprintf("could not install %s", "xlsx"))
  }
}