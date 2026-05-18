####################
# Helper Functions #
####################

# Facilidad de iLa tarenstalacion
install_these_packages <- function(listPackages) {
    new.packages <- listPackages[!(listPackages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
}
# Data wrangling, manipulation and processing
# Tidyverse instala los siguientes paquetes:
# ggplot2, para visualización
# dplyr, para manipulacion
# tidyr, para formtatear la data a tidy
# readr, leer o importar
# purrr, para programacion funcional
# tibble, para tablas avanzadas
# stringr, para caracteres
# forcats, para factores
datawrang_libs <- function(loadLibrary = TRUE) {
    packages <- c("readxl", "magrittr", "tidyverse")
    install_these_packages(packages)
    if (loadLibrary) invisible(lapply(packages, function(t) suppressMessages(library(t, character.only = TRUE))))
}

requirement_libs <- function(loadLibrary = TRUE) {
    packages <- c("statsr", "gapminder", "openintro", "zip", "car", "ggthemes",
                  'daewr')
    install_these_packages(packages)
    if (loadLibrary) invisible(lapply(packages, function(t) suppressMessages(library(t, character.only = TRUE))))
}

statsr_lib <- function() {
    new.packages <- 'statsr' %in% installed.packages()[,"Package"]
    if(!new.packages) devtools::install_github("statswithr/statsr")
    invisible(library('statsr', character.only = TRUE))
}
# Intalando librerias basicas y de trabajo
datawrang_libs()
requirement_libs()
statsr_lib()

#if (!requireNamespace("BiocManager", quietly = TRUE))
#install.packages("BiocManager")
#BiocManager::install("ropls")
#install.packages("devtools")
library(ropls)
library(devtools)
install.packages("FNN")
library(FNN)
install.packages("rpart")
library(rpart)
install.packages("biotools")
library(biotools)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("factoextra")
library(factoextra)
install.packages("ChemoSpecUtils")
library(ChemoSpecUtils)
install.packages("ChemoSpec")
library(ChemoSpec)
install.packages("R.utils")
library(R.utils)
install.packages("baseline")
library(baseline)
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
install.packages("dbscan")
library(dbscan)
install.packages("mclust")
library(mclust)
install.packages("rattle")
library(rattle)
install.packages("cluster")
library(cluster)
install.packages("ISLR")
library(ISLR)
install.packages("corpcor")
library(corpcor)
install.packages("GPArotation")
library(GPArotation)
install.packages("psych")
library(psych)
install.packages("nFactors")
library (nFactors)
install.packages("Hmisc")
library(Hmisc)
install.packages("corrplot")
library(corrplot)
install.packages("MVA")
library(MVA)
install.packages("FactoMineR")
library(FactoMineR)
install.packages("e1071")
library(e1071)
install.packages("DMwR2")
library(DMwR2)
install.packages("randomForest")
library(randomForest)
install.packages("adabag")
library(adabag)
install.packages("kernlab")
library(kernlab)
install.packages("extrafont")
library(extrafont)
install.packages("neuralnet")
library(neuralnet) 
install.packages("groupdata2")
library(groupdata2)
install.packages("kableExtra")
library(kableExtra)
install.packages("ROCR")
library(ROCR)
install.packages("mdatools")
library(mdatools)
install.packages("pls")
library(pls)
install.packages("DiscriMiner")
library(DiscriMiner)
install.packages("performanceEstimation")
library(performanceEstimation)
install.packages("agricolae")
library(agricolae)
install.packages("nortest")
library(nortest)
install.packages("pca3d")
library(pca3d)
install.packages("h2o")
library(h2o)
install.packages("tidymodels")
library(tidymodels)
install.packages("skimr")
library(skimr)
install.packages("DataExplorer")
library(DataExplorer)
install.packages("mosaicData")
library(mosaicData)
rm(list=ls ()) 
