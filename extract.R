# TRABAJO PRÁCTICO DATA MINING: Churn Prediction Challenge (Castle Crush)
# Nombre del equipo: CPSquad
# Integrantes: Constanza Gaset, Lucía Lopez Wallace y Juan Augusto Tissone

#Limpiamos y seteamos el escritorio de trabajo
rm(list=ls())
setwd("C:/Users/A308831/Desktop/Otros/Ditella/Data Mining/TP Final/DATA")

#Listamos todas las librerías que vamos a necesitar para desarrollar el trabajo
install.packages("data.table")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("caret")
install.packages("pROC")
install.packages("lattice")
install.packages("Matrixe")
install.packages("xgboost")
install.packages("corrplot")
install.packages("ggpubr")

library(data.table)
library(ggplot2)
library(dplyr)
library(caret)
library(pROC)
library(lattice)
library(Matrix)
library(xgboost)
library(corrplot)
library(tidyr)
library(ggpubr)


# Generamos una funcion para la carga de los datos de entrenamiento
# con la librería data table

load_csv_data <- function(csv_file, sample_ratio = 1, drop_cols = NULL,
                          sel_cols = NULL) {
  
  dt <- fread(csv_file, header = TRUE, sep = ",", stringsAsFactors = TRUE,
              na.strings = "", drop = drop_cols, select = sel_cols,
              showProgress = TRUE)
  
  if (sample_ratio < 1) {
    sample_size <- as.integer(sample_ratio * nrow(dt))
    dt <- dt[sample(.N, sample_size)]
  }
  
  return(dt)
}

load_train_data <- function(data_dir, train_file="train.csv", sample_ratio=1,
                            drop_cols=NULL, sel_cols=NULL) {
  train_days <- seq(1, 5, by=1) 
  dfs <- list()
  for (i in train_days) {
    file_to_load = paste("train_", toString(i), ".csv", sep="")
    print(file_to_load)
    dt <- load_csv_data(file_to_load, sample_ratio = sample_ratio,
                        drop_cols = drop_cols, sel_cols = sel_cols)
    dfs <- c(list(dt),dfs)
    }
  data_set <- (rbindlist(dfs, fill=TRUE))
  return(data_set)
}


# Cargamos el 70% de las observaciones del train set
data_set<- load_train_data(getwd(), sample_ratio = 0.7)

str(data_set)
head(data_set)

#Asignamos etiqueta de train set sumando una columna "train_sample=TRUE"
data_set[, train_sample := TRUE]

# Cargamos los datos de test
# En este caso, cargamos el totalidad de las observaciones
eval_set <- load_csv_data("evaluation.csv", sample_ratio = 1)

#Asignamos etiqueta de eval set sumando una columna "train_sample=FALSE"
eval_set[, train_sample := FALSE]



