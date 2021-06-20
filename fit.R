# En este módulo desarrollamos los modelos y predicciones

# Una vez analizado el data set y decidido las variables 
# con las que vamos a prededir en el modelo (modulo Transform)

# Hacemos tuning de los hiperparámetros con la funcion random_grid
random_grid <- function(size,
                        min_nrounds, max_nrounds,
                        min_max_depth, max_max_depth,
                        min_eta, max_eta,
                        min_gamma, max_gamma,
                        min_colsample_bytree, max_colsample_bytree,
                        min_min_child_weight, max_min_child_weight,
                        min_subsample, max_subsample) {
  
  rgrid <- data.frame(nrounds = if (min_nrounds == max_nrounds) {
    rep(min_nrounds, size)
  } else {
    sample(c(min_nrounds:max_nrounds),
           size = size, replace = TRUE)
  },
  max_depth = if (min_max_depth == max_max_depth) {
    rep(min_max_depth, size)
  } else {
    sample(c(min_max_depth:max_max_depth),
           size = size, replace = TRUE)
  },
  eta = if (min_eta == max_eta) {
    rep(min_eta, size)
  } else {
    round(runif(size, min_eta, max_eta), 5)
  },
  gamma = if (min_gamma == max_gamma) {
    rep(min_gamma, size)
  } else {
    round(runif(size, min_gamma, max_gamma), 5)
  },
  colsample_bytree = if (min_colsample_bytree == max_colsample_bytree) {
    rep(min_colsample_bytree, size)
  } else {
    round(runif(size, min_colsample_bytree, max_colsample_bytree), 5)
  },
  min_child_weight = if (min_min_child_weight == max_min_child_weight) {
    rep(min_min_child_weight, size)
  } else {
    round(runif(size, min_min_child_weight, max_min_child_weight), 5)
  },
  subsample = if (min_subsample == max_subsample) {
    rep(min_subsample, size)
  } else {
    round(runif(size, min_subsample, max_subsample), 5)
  })
  
  return(rgrid)
}

# Definimos la función para entrenar al modelo Xgboost
train_xgboost <- function(data_train, data_val, rgrid) {
  
  watchlist <- list(train = data_train, valid = data_val)
  
  predicted_models <- list()
  
  for (i in seq_len(nrow(rgrid))) {
    print(i)
    print(rgrid[i,])
    
    trained_model <- xgb.train(data = data_train,
                               params=as.list(rgrid[i, c("max_depth",
                                                         "eta",
                                                         "gamma",
                                                         "colsample_bytree",
                                                         "subsample",
                                                         "min_child_weight")]),
                               nrounds = rgrid[i, "nrounds"],
                               watchlist = watchlist,
                               objective = "binary:logistic",
                               eval.metric = "auc",
                               print_every_n = 10)
    
    perf_tr <- tail(trained_model$evaluation_log, 1)$train_auc
    perf_vd <- tail(trained_model$evaluation_log, 1)$valid_auc
    print(c(perf_tr, perf_vd))
    
    predicted_models[[i]] <- list(results = data.frame(rgrid[i,],
                                                       perf_tr = perf_tr,
                                                       perf_vd = perf_vd),
                                  model = trained_model)
    
    rm(trained_model)
    
    gc()
  }
  
  return(predicted_models)
}

# Definimos la función result_table para analizar los resultados
result_table <- function(pred_models) {
  
  res_table <- data.frame()
  i <- 1
  
  for (m in pred_models) {
    res_table <- rbind(res_table, data.frame(i = i, m$results))
    i <- i + 1
  }
  
  res_table <- res_table[order(-res_table$perf_vd),]
  
  return(res_table)
}


# Identificamos las filas de entrenamiento y validacion con
# indices random para crear dtrain y dvalid 
#set.seed(1234)
#val_index <- sample(c(1:nrow(train_set)), 0.1*(nrow(train_set)))
#train_index <- setdiff(c(1:nrow(train_set)), val_index)


dtrain <- xgb.DMatrix(data = train_set[,
                                       colnames(train_set) != "Label"],
                      label = train_set[,
                                        colnames(train_set) == "Label"])

dvalid <- xgb.DMatrix(data = valid_set[, colnames(valid_set) != "Label"],
                      label = valid_set[, colnames(valid_set) == "Label"])

head(train_set)

# Definimos los minimos y maximos valores que pueden tomar los parametros
rgrid <- random_grid(size = 10,
                     min_nrounds = 200, max_nrounds = 400,
                     min_max_depth = 6, max_max_depth = 12,
                     min_eta = 0.0005, max_eta = 0.15,
                     min_gamma = 0, max_gamma = 1,
                     min_colsample_bytree = 0.6, max_colsample_bytree = 0.9,
                     min_min_child_weight = 1, max_min_child_weight = 6,
                     min_subsample = 0.5, max_subsample = 0.9)


# Entrenamos el modelo
predicted_models <- train_xgboost(dtrain, dvalid, rgrid)
gc()

res_table <- result_table(predicted_models)
print(res_table)


## Analizamos la importancia de las variables en el modelo
var_imp <- as.data.frame(xgb.importance(model = predicted_models[[res_table[1, "i"]]]$model))
print(head(var_imp, 20))


# Generamos las predicciones con el conjunto de evaluación
eval_preds <- data.frame(id = eval_set[, "id"],
                         Label = predict(predicted_models[[res_table[1, "i"]]]$model,
                                         newdata = eval_set[, setdiff(colnames(eval_set), c("Label"))]))

#Armamos el archivo para la plataforma de Kaggle
options(scipen = 999)  # Para evitar que se guarden valores en formato científico
write.table(eval_preds, "modelo_con_random_search_.csv",
            sep = ",", row.names = FALSE, quote = FALSE)
options(scipen=0, digits=7) # Para volver al comportamiento tradicional