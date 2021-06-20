# En este modulo nos encargamos del analisis y procesamiento de los datos,
# para entender como se comportan las variables del data set, cuales son 
# relevantes en la explicacion de la variable a predecir y poder realizar
# algo de ingeneria de atributos para mejorar la prediccion

# Tenemos 3.883.193 observaciones de una porcion de gente que juega al  
# con 102 columnas que indican diferentes caracteristicas
str(data_set)

# Creamos la variable target Label
data_set[, Label := as.numeric(Label_max_played_dsi == 3)]
data_set[, Label_max_played_dsi := NULL]



# Filtramos las observaciones con Instarll_Data < 383 para no realizar data leackeage
# dado que no tenemos la info de churn de esa data (es data futura, parte contenida en eval set)
# Obtenemos 3.714.473 observaciones luego del filtrado

data_set<- data_set[data_set$install_date <383, ]
gc()

# Nos fijamos si nuestra muestra esta desbalanceada
prop.table(table(data_set$Label))
#0          #1 
#0.803796   #0.196404 

ggplot(as.data.frame(table(data_set$Label), ), aes(x=Var1, y = Freq)) +
  geom_bar(stat="identity") + labs(title="Desbalance en la clase: Label" , x="Label", y="Frecuencia") 

# Eliminamos duplicados
# Funcion que elimina Duplicdos
drop_duplicate_rows <- function(dt) {
  dup_rows <- duplicated(dt)
  nr_duplicates <- nrow(dt[dup_rows])
  if (nr_duplicates > 0) {
    dt <- dt[!dup_rows]
  }
  return(dt)
}
# Se eliminan duplicados por la columna "id"
drop_duplicate_rows(data_set)
gc()

# Eliminamos columnas con porcentaje de nulos mayor al 75%
# Encontramos que site y age presentan una proporcion alta de NA, decidimos descartarlas para la predicciÛn
na_prop <- sapply(data_set, function(x) sum(is.na(x)) / length(x))
data_set_na <- (data.frame(na_prop))
data_set_na <- data_set_na[data_set_na$na_prop > 0.7, , drop = FALSE]
data_set_na
rm(data_set_na,na_prop)
data_set[, site := NULL]
data_set[, age := NULL]
gc()

eval_set[, site := NULL]
eval_set[, age := NULL]

#######################################
# Veo la varianza que variables tienen varianza < 1
# Eliminamos las variables con var < 1: traffic_type es la ˙nica
val_count <- lapply(data_set, function(x) length(unique(na.omit(x))))
zero_vars_cols <- which(!val_count > 1)
data_set[, traffic_type := NULL]
eval_set[, traffic_type := NULL]

# Separo el set de validaciÛn
#creamos copias auxiliares en caso de cometer un error no terner que volver a cargar los datos

data_set<-data_set_aux
eval_set<-eval_set_aux

data_set_aux<-data_set
eval_set_aux<-eval_set
data_set2<-data_set_aux

idx <- sample(seq(1, 2), size = nrow(data_set2), replace = TRUE, prob = c(.9, .1))

data_set <- data_set2[idx == 1,]
valid_set <- data_set2[idx == 2,]
summary(valid_set)
rm(idx)

# Tenemos 3342997 observaciones y 99 variables
head(data_set)
dim(data_set)
dim(valid_set)
dim(eval_set)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
## Analizamos las variables 
## Empezamos con las variables sum_dsi_x_features (18 categorias)

# PiggyBankModified
# Toma siempre valores positivos

# PiggyBankModifiedPoints_sum_dsi0
summary(data_set$PiggyBankModifiedPoints_sum_dsi0)
str(data_set$PiggyBankModifiedPoints_sum_dsi0)
# No tiene NA, distribucion asimetrica positiva

# PiggyBankModified_sum_dsi1
summary(data_set$PiggyBankModifiedPoints_sum_dsi1)
str(data_set$PiggyBankModifiedPoints_sum_dsi1)
# No tiene NA, distribucion asimetrica positiva

# PiggyBankModified_sum_dsi2
summary(data_set$PiggyBankModifiedPoints_sum_dsi2)
str(data_set$PiggyBankModifiedPoints_sum_dsi2)
# No tiene NA, distribucion asimentrica positiva

# PiggyBankModified_sum_dsi3
summary(data_set$PiggyBankModifiedPoints_sum_dsi3)
str(data_set$PiggyBankModifiedPoints_sum_dsi3)
# No tiene NA, distribucion asimentrica positiva

# Unifico el total de PiggyBankModified
data_set[, sum_PiggyBankModifiedPoints := (PiggyBankModifiedPoints_sum_dsi0 +PiggyBankModifiedPoints_sum_dsi1 
                                           + PiggyBankModifiedPoints_sum_dsi2 + PiggyBankModifiedPoints_sum_dsi3)]


m0 <- lm(Label ~ PiggyBankModifiedPoints_sum_dsi0, data = data_set)
m1 <- lm(Label ~ PiggyBankModifiedPoints_sum_dsi1, data = data_set)
m2 <- lm(Label ~ PiggyBankModifiedPoints_sum_dsi2, data = data_set)
m3 <- lm(Label ~ PiggyBankModifiedPoints_sum_dsi3, data = data_set)
m4 <- lm(Label ~ sum_PiggyBankModifiedPoints, data = data_set)

summary(m0) # Adjusted R-squared:  -5.95e-08 
summary(m1) # Adjusted R-squared:  0.01941
summary(m2) # Adjusted R-squared:  0.008469
summary(m3) # Adjusted R-squared:  0.01017 
summary(m4) # Adjusted R-squared:  -6.213e-08
# Sumadas las variables no tienen una mayor explicacion de la variable a predecir

data_set[, sum_PiggyBankModifiedPoints := NULL]
data_set[, PiggyBankModifiedPoints_sum_dsi0 := NULL] # Esta variable explica poco Label
valid_set[, PiggyBankModifiedPoints_sum_dsi0 := NULL]
eval_set[, PiggyBankModifiedPoints_sum_dsi0 := NULL]

# Tampoco tiene poder predictivo PiggyBankModifiedPoints_sum_dsi0

rm(m0)
rm(m1)
rm(m2)
rm(m3)
rm(m4)
gc()

# de las variables que consideramos explcicativas nos evaluamos la correlaciÛn de mismas, ya que para el an·lisis se utilizar·n
#aquellas que no tengan sustancial correlaciÛn entre si

#armamos el df para estudiar la correlaciÛn
df<-cbind(data_set$PiggyBankModifiedPoints_sum_dsi1,data_set$PiggyBankModifiedPoints_sum_dsi2,data_set$PiggyBankModifiedPoints_sum_dsi3)
#renombro las variables
colnames(df)<-c('data_set$PiggyBankModifiedPoints_sum_dsi1','data_set$PiggyBankModifiedPoints_sum_dsi2','data_set$PiggyBankModifiedPoints_sum_dsi3')
#carculos las correlatividades
corrplot(cor(df), method='number')
#como se puede ver , las variables no est·n correlacionadas
rm(df)

# Viendo que las variables tienen una distribuciÛn asimentrica positiva
# Consideramos la posibilidad que el modelo trabaje con la escala logaritmica (no hay valores negativos)

data_set$PiggyBankModifiedPoints_sum_dsi1_log <- log(data_set$PiggyBankModifiedPoints_sum_dsi1 +1 - min(data_set$PiggyBankModifiedPoints_sum_dsi1))
data_set$PiggyBankModifiedPoints_sum_dsi2_log <- log(data_set$PiggyBankModifiedPoints_sum_dsi2 +1 - min(data_set$PiggyBankModifiedPoints_sum_dsi2))
data_set$PiggyBankModifiedPoints_sum_dsi3_log <- log(data_set$PiggyBankModifiedPoints_sum_dsi3 +1 - min(data_set$PiggyBankModifiedPoints_sum_dsi3))
data_set[, PiggyBankModifiedPoints_sum_dsi1 := NULL]
data_set[, PiggyBankModifiedPoints_sum_dsi2 := NULL]
data_set[, PiggyBankModifiedPoints_sum_dsi3 := NULL]
gc()

valid_set$PiggyBankModifiedPoints_sum_dsi1_log <- log(valid_set$PiggyBankModifiedPoints_sum_dsi1 +1 - min(valid_set$PiggyBankModifiedPoints_sum_dsi1))
valid_set$PiggyBankModifiedPoints_sum_dsi2_log <- log(valid_set$PiggyBankModifiedPoints_sum_dsi2 +1 - min(valid_set$PiggyBankModifiedPoints_sum_dsi2))
valid_set$PiggyBankModifiedPoints_sum_dsi3_log <- log(valid_set$PiggyBankModifiedPoints_sum_dsi3 +1 - min(valid_set$PiggyBankModifiedPoints_sum_dsi3))
valid_set[, PiggyBankModifiedPoints_sum_dsi1 := NULL]
valid_set[, PiggyBankModifiedPoints_sum_dsi2 := NULL]
valid_set[, PiggyBankModifiedPoints_sum_dsi3 := NULL]
gc()

eval_set$PiggyBankModifiedPoints_sum_dsi1_log <- log(eval_set$PiggyBankModifiedPoints_sum_dsi1 +1 - min(eval_set$PiggyBankModifiedPoints_sum_dsi1))
eval_set$PiggyBankModifiedPoints_sum_dsi2_log <- log(eval_set$PiggyBankModifiedPoints_sum_dsi2 +1 - min(eval_set$PiggyBankModifiedPoints_sum_dsi2))
eval_set$PiggyBankModifiedPoints_sum_dsi3_log <- log(eval_set$PiggyBankModifiedPoints_sum_dsi3 +1 - min(eval_set$PiggyBankModifiedPoints_sum_dsi3))
eval_set[, PiggyBankModifiedPoints_sum_dsi1 := NULL]
eval_set[, PiggyBankModifiedPoints_sum_dsi2 := NULL]
eval_set[, PiggyBankModifiedPoints_sum_dsi3 := NULL]
gc()

head(valid_set)

summary(valid_set$PiggyBankModifiedPoints_sum_dsi1_log)

dim(data_set) #98
dim(valid_set)
dim(eval_set)
gc()
# ---


# Win Battle
# Siempre toma valores positivos

# WinBattle_sum_dsi0
summary(data_set$WinBattle_sum_dsi0)
str(data_set$WinBattle_sum_dsi0)
# No tiene NA, distribucion asimentrica positiva

# WinnBattle_sum_dsi1
summary(data_set$WinBattle_sum_dsi1)
str(data_set$WinBattle_sum_dsi1)
# No tiene NA, distribucion asimentrica positiva

# WinnBattle_sum_dsi2
summary(data_set$WinBattle_sum_dsi2)
str(data_set$WinBattle_sum_dsi2)
# No tiene NA, distribucion asimentrica positiva

# WinnBattle_sum_dsi3
summary(data_set$WinBattle_sum_dsi3)
str(data_set$WinBattle_sum_dsi3)
# No tiene NA, distribucion asimentrica positiva

# Unifico el total de batallas ganadas
data_set[, sum_WinBattle := (WinBattle_sum_dsi0 +WinBattle_sum_dsi1 
                             + WinBattle_sum_dsi2 + WinBattle_sum_dsi3)]

valid_set[, sum_WinBattle := (WinBattle_sum_dsi0 +WinBattle_sum_dsi1 
                             + WinBattle_sum_dsi2 + WinBattle_sum_dsi3)]

eval_set[, sum_WinBattle := (WinBattle_sum_dsi0 +WinBattle_sum_dsi1 
                             + WinBattle_sum_dsi2 + WinBattle_sum_dsi3)]




m0 <- lm(Label ~ WinBattle_sum_dsi0, data = data_set)
m1 <- lm(Label ~ WinBattle_sum_dsi1, data = data_set)
m2 <- lm(Label ~ WinBattle_sum_dsi2, data = data_set)
m3 <- lm(Label ~ WinBattle_sum_dsi3, data = data_set)
m4 <- lm(Label ~ sum_WinBattle, data = data_set)

summary(m0) # Adjusted R-squared:  0.01086
summary(m1) # Adjusted R-squared:  0.01575 
summary(m2) # Adjusted R-squared:  0.0171
summary(m3) # Adjusted R-squared:  0.03087
summary(m4) # Adjusted R-squared:  0.0308
# Sumadas las variables tienen una mayor explicacion

data_set[, WinBattle_sum_dsi1 := NULL] 
data_set[, WinBattle_sum_dsi2 := NULL]
valid_set[, WinBattle_sum_dsi1 := NULL] 
valid_set[, WinBattle_sum_dsi2 := NULL]
eval_set[, WinBattle_sum_dsi1 := NULL] 
eval_set[, WinBattle_sum_dsi2 := NULL]



rm(m0)
rm(m1)
rm(m2)
rm(m3)
rm(m4)

# de las variables que consideramos explcicativas nos evaluamos la correlaciÛn de mismas, ya que para el an·lisis se utilizar·n
#aquellas que no tengan sustancial correlaciÛn entre si

#armamos el df para estudiar la correlaciÛn
df<-cbind(data_set$WinBattle_sum_dsi0,data_set$WinBattle_sum_dsi1,data_set$WinBattle_sum_dsi2,data_set$WinBattle_sum_dsi3,data_set$sum_WinBattle)
#renombro las variables
colnames(df)<-c('WinBattle_sum_dsi0','WinBattle_sum_dsi1','WinBattle_sum_dsi2','WinBattle_sum_dsi3','sum_WinBattle')
#carculos las correlatividades
corrplot(cor(df), method='number')
#como se puede ver , las variables no est·n correlacionadas
rm(df)
#como se ve las variables no est·n lo suficientemente correlacionadas como para decidir no contar con alguna


# Consideramos la posibilidad que el modelo trabaje con la escala logaritmica, (no hay valores negativos)
data_set$sum_WinBattle_log <- log(data_set$sum_WinBattle + 1 - min(data_set$sum_WinBattle))
data_set[, sum_WinBattle := NULL]

valid_set$sum_WinBattle_log <- log(valid_set$sum_WinBattle + 1 - min(valid_set$sum_WinBattle))
valid_set[, sum_WinBattle := NULL]

eval_set$sum_WinBattle_log <- log(eval_set$sum_WinBattle + 1 - min(eval_set$sum_WinBattle))
eval_set[, sum_WinBattle := NULL]

head(valid_set$sum_WinBattle)
summary(data_set$sum_WinBattle)
min(valid_set$sum_WinBattle)
dim(data_set) # 97
dim(valid_set)
dim(eval_set)

## ---
# StartSession
# Siempre toma valores positivos. PodÌa aplicar LOG

# StartSession_sum_dsi0
summary(data_set$StartSession_sum_dsi0)
str(data_set$StartSession_sum_dsi0)
# No tiene NA, tiene outliers

# StartSession_sum_dsi1
summary(data_set$StartSession_sum_dsi1)
str(data_set$StartSession_sum_dsi1)
# No tiene NA, tiene outliers

# StartSession_sum_dsi2
summary(data_set$StartSession_sum_dsi2)
str(data_set$StartSession_sum_dsi2)
# No tiene NA, tiene outliers

# StartSession_sum_dsi3
summary(data_set$StartSession_sum_dsi3)
str(data_set$StartSession_sum_dsi3)
# No tiene NA, tiene outliers

# Unifico el total de batallas ganadas
data_set[, sum_StartSession := (StartSession_sum_dsi0 +StartSession_sum_dsi1 
                                + StartSession_sum_dsi2 + StartSession_sum_dsi3)]

valid_set[, sum_StartSession := (StartSession_sum_dsi0 +StartSession_sum_dsi1 
                                + StartSession_sum_dsi2 + StartSession_sum_dsi3)]

eval_set[, sum_StartSession := (StartSession_sum_dsi0 +StartSession_sum_dsi1 
                                + StartSession_sum_dsi2 + StartSession_sum_dsi3)]



m0 <- lm(Label ~ StartSession_sum_dsi0, data = data_set)
m1 <- lm(Label ~ StartSession_sum_dsi1, data = data_set)
m2 <- lm(Label ~ StartSession_sum_dsi2, data = data_set)
m3 <- lm(Label ~ StartSession_sum_dsi3, data = data_set)
m4 <- lm(Label ~ sum_StartSession, data = data_set)


summary(m0) # Adjusted R-squared:  0.005663
summary(m1) # Adjusted R-squared:  0.01306
summary(m2) # Adjusted R-squared:  0.01508
summary(m3) # Adjusted R-squared:  0.02387
summary(m4) # Adjusted R-squared:  0.02421
# Sumadas las variables tienen una mayor explicacion

data_set[, StartSession_sum_dsi0 := NULL]
data_set[, StartSession_sum_dsi1 := NULL]
data_set[, StartSession_sum_dsi2 := NULL]
data_set[, StartSession_sum_dsi3 := NULL]

valid_set[, StartSession_sum_dsi0 := NULL]
valid_set[, StartSession_sum_dsi1 := NULL]
valid_set[, StartSession_sum_dsi2 := NULL]
valid_set[, StartSession_sum_dsi3 := NULL]

eval_set[, StartSession_sum_dsi0 := NULL]
eval_set[, StartSession_sum_dsi1 := NULL]
eval_set[, StartSession_sum_dsi2 := NULL]
eval_set[, StartSession_sum_dsi3 := NULL]


rm(m0)
rm(m1)
rm(m2)
rm(m3)
rm(m4)
gc()

dim(data_set)
dim(valid_set)
dim(eval_set)


# Viendo que las variables tienen una distribuciÛn asimentrica positiva
# Consideramos la posibilidad que el modelo trabaje con la escala logaritmica (no hay valores negativos)
data_set$sum_StartSession_log <- log(data_set$sum_StartSession + 1 - min(data_set$sum_StartSession))
data_set[, sum_StartSession := NULL]

valid_set$sum_StartSession_log <- log(valid_set$sum_StartSession + 1 - min(valid_set$sum_StartSession))
valid_set[, sum_StartSession := NULL]

eval_set$sum_StartSession_log <- log(eval_set$sum_StartSession + 1 - min(eval_set$sum_StartSession))
eval_set[, sum_StartSession := NULL]


dim(data_set) #94
gc()

# ---

## Start Battle

# StartBattle_sum_dsi0
summary(data_set$StartBattle_sum_dsi0)
str(data_set$StartBattle_sum_dsi0)
# No tiene NA, distribucion asimetrica positiva

# StartBattle_sum_dsi1
summary(data_set$StartBattle_sum_dsi1)
summary(eval_set$StartBattle_sum_dsi1)

str(data_set$StartBattle_sum_dsi1)
# Tiene 306052 NA, los reemplazo por la mediana, distribucion asimetrica positiva
data_set$StartBattle_sum_dsi1 <- ifelse(is.na(data_set$StartBattle_sum_dsi1), median(data_set$StartBattle_sum_dsi1, na.rm = TRUE), data_set$StartBattle_sum_dsi1)
valid_set$StartBattle_sum_dsi1 <- ifelse(is.na(valid_set$StartBattle_sum_dsi1), median(valid_set$StartBattle_sum_dsi1, na.rm = TRUE), valid_set$StartBattle_sum_dsi1)
eval_set$StartBattle_sum_dsi1 <- ifelse(is.na(eval_set$StartBattle_sum_dsi1), median(eval_set$StartBattle_sum_dsi1, na.rm = TRUE), eval_set$StartBattle_sum_dsi1)

# StartBattle_sum_dsi2
summary(data_set$StartBattle_sum_dsi2)
str(data_set$StartBattle_sum_dsi2)
# No tiene NA, distribucion asimetrica positiva

# StartBattle_sum_dsi3
summary(data_set$StartBattle_sum_dsi3)
str(data_set$StartBattle_sum_dsi3)
# No tiene NA, distribucion asimetrica positiva

# Unifico el total de batallas ganadas
data_set[, sum_StartBattle := (StartBattle_sum_dsi0 +StartBattle_sum_dsi1 
                               + StartBattle_sum_dsi2 + StartBattle_sum_dsi3)]

m0 <- lm(Label ~ StartBattle_sum_dsi0, data = data_set)
m1 <- lm(Label ~ StartBattle_sum_dsi1, data = data_set)
m2 <- lm(Label ~ StartBattle_sum_dsi2, data = data_set)
m3 <- lm(Label ~ StartBattle_sum_dsi3, data = data_set)
m4 <- lm(Label ~ sum_StartBattle, data = data_set)

summary(m0) # Adjusted R-squared:  0.008392
summary(m1) # Adjusted R-squared:  0.01168
summary(m2) # Adjusted R-squared:  0.0148
summary(m3) # Adjusted R-squared:  0.02839
summary(m4) # Adjusted R-squared:  0.02506
# Sumadas las variables no tienen una mayor explicaciÛn. Tomaremos Las batallas ganadas de manera separada
data_set[, sum_StartBattle := NULL]

rm(m0)
rm(m1)
rm(m2)
rm(m3)
rm(m4)

#armamos el df para estudiar la correlaciÛn
df<-cbind(data_set$StartBattle_sum_dsi0,data_set$StartBattle_sum_dsi1,data_set$StartBattle_sum_dsi2,data_set$StartBattle_sum_dsi3)
#renombro las variables
colnames(df)<-c('StartBattle_sum_dsi0','StartBattle_sum_dsi1','StartBattle_sum_dsi2','StartBattle_sum_dsi3')
#carculos las correlatividades
corrplot(cor(df), method='number')
#como se puede ver , las variables no est·n correlacionadas
rm(df)
#no hay correlaciÛn mayor a 0,9 

# Consideramos la posibilidad que el modelo trabaje con la escala logaritmica, (no hay valores negativos)
data_set$StartBattle_sum_dsi0_log <- log(data_set$StartBattle_sum_dsi0 + 1 - min(data_set$StartBattle_sum_dsi0))
data_set$StartBattle_sum_dsi1_log <- log(data_set$StartBattle_sum_dsi1 + 1 - min(data_set$StartBattle_sum_dsi1))
data_set$StartBattle_sum_dsi2_log <- log(data_set$StartBattle_sum_dsi2 + 1 - min(data_set$StartBattle_sum_dsi2))
data_set$StartBattle_sum_dsi3_log <- log(data_set$StartBattle_sum_dsi3 + 1 - min(data_set$StartBattle_sum_dsi3))
data_set[, StartBattle_sum_dsi0 := NULL]
data_set[, StartBattle_sum_dsi1 := NULL]
data_set[, StartBattle_sum_dsi2 := NULL]
data_set[, StartBattle_sum_dsi3 := NULL]

valid_set$StartBattle_sum_dsi0_log <- log(valid_set$StartBattle_sum_dsi0 + 1 - min(valid_set$StartBattle_sum_dsi0))
valid_set$StartBattle_sum_dsi1_log <- log(valid_set$StartBattle_sum_dsi1 + 1 - min(valid_set$StartBattle_sum_dsi1))
valid_set$StartBattle_sum_dsi2_log <- log(valid_set$StartBattle_sum_dsi2 + 1 - min(valid_set$StartBattle_sum_dsi2))
valid_set$StartBattle_sum_dsi3_log <- log(valid_set$StartBattle_sum_dsi3 + 1 - min(valid_set$StartBattle_sum_dsi3))
valid_set[, StartBattle_sum_dsi0 := NULL]
valid_set[, StartBattle_sum_dsi1 := NULL]
valid_set[, StartBattle_sum_dsi2 := NULL]
valid_set[, StartBattle_sum_dsi3 := NULL]

eval_set$StartBattle_sum_dsi0_log <- log(eval_set$StartBattle_sum_dsi0 + 1 - min(eval_set$StartBattle_sum_dsi0))
eval_set$StartBattle_sum_dsi1_log <- log(eval_set$StartBattle_sum_dsi1 + 1 - min(eval_set$StartBattle_sum_dsi1))
eval_set$StartBattle_sum_dsi2_log <- log(eval_set$StartBattle_sum_dsi2 + 1 - min(eval_set$StartBattle_sum_dsi2))
eval_set$StartBattle_sum_dsi3_log <- log(eval_set$StartBattle_sum_dsi3 + 1 - min(eval_set$StartBattle_sum_dsi3))
eval_set[, StartBattle_sum_dsi0 := NULL]
eval_set[, StartBattle_sum_dsi1 := NULL]
eval_set[, StartBattle_sum_dsi2 := NULL]
eval_set[, StartBattle_sum_dsi3 := NULL]
gc()




dim(data_set)#94
dim(valid_set)
dim(eval_set)
gc()
# ---
# EnterDeck
# Toma valores positivos, puedo hacer log

# EnterDeck_sum_dsi0
summary(data_set$EnterDeck_sum_dsi0)
str(data_set$EnterDeck_sum_dsi0)
# No tiene NA, tiene una distribucion asimetrica positiva

# EnterDeck_sum_dsi1
summary(data_set$EnterDeck_sum_dsi1)
str(data_set$EnterDeck_sum_dsi1)
# No tiene NA tiene una distribucion asimetrica positiva

# EnterDeck_sum_dsi2
summary(data_set$EnterDeck_sum_dsi2)
str(data_set$EnterDeck_sum_dsi2)
# No tiene NA, tiene una distribucion asimetrica positiva

# EnterDeck_sum_dsi3
summary(data_set$EnterDeck_sum_dsi3)
str(data_set$EnterDeck_sum_dsi3)
# No tiene NA, tiene una distribucion asimetrica positiva

# Unifico el total de batallas ganadas
data_set[, sum_EnterDeck := (EnterDeck_sum_dsi0 +EnterDeck_sum_dsi1 
                             + EnterDeck_sum_dsi2 + EnterDeck_sum_dsi3)]

m0 <- lm(Label ~ EnterDeck_sum_dsi0, data = data_set)
m1 <- lm(Label ~ EnterDeck_sum_dsi1, data = data_set)
m2 <- lm(Label ~ EnterDeck_sum_dsi2, data = data_set)
m3 <- lm(Label ~ EnterDeck_sum_dsi3, data = data_set)
m4 <- lm(Label ~ sum_EnterDeck, data = data_set)

summary(m0) # Adjusted R-squared:  0.01112
summary(m1) # Adjusted R-squared:  0.01449
summary(m2) # Adjusted R-squared:  0.01693
summary(m3) # Adjusted R-squared:  0.03235
summary(m4) # Adjusted R-squared:  0.03096
# Sumadas no dan mejor explicacion, las mantengo separadas
data_set[, sum_EnterDeck := NULL]

rm(m0)
rm(m1)
rm(m2)
rm(m3)
rm(m4)

#armamos el df para estudiar la correlaciÛn
df<-cbind(data_set$EnterDeck_sum_dsi0,data_set$EnterDeck_sum_dsi1,data_set$EnterDeck_sum_dsi2,data_set$EnterDeck_sum_dsi3)
#renombro las variables
colnames(df)<-c('EnterDeck_sum_dsi0','EnterDeck_sum_dsi1','EnterDeck_sum_dsi2','EnterDeck_sum_dsi3')
#carculos las correlatividades
corrplot(cor(df), method='number')
#como se puede ver , las variables no est·n correlacionadas
rm(df)
# no se encontraron correlaciones mayores a 0,9

# Consideramos la posibilidad que el modelo trabaje con la escala logaritmica, (no hay valores negativos)
data_set$EnterDeck_sum_dsi0_log <- log(data_set$EnterDeck_sum_dsi0 + 1 - min(data_set$EnterDeck_sum_dsi0))
data_set$EnterDeck_sum_dsi1_log <- log(data_set$EnterDeck_sum_dsi1 + 1 - min(data_set$EnterDeck_sum_dsi1))
data_set$EnterDeck_sum_dsi2_log <- log(data_set$EnterDeck_sum_dsi2 + 1 - min(data_set$EnterDeck_sum_dsi2))
data_set$EnterDeck_sum_dsi3_log <- log(data_set$EnterDeck_sum_dsi3 + 1 - min(data_set$EnterDeck_sum_dsi3))
data_set[, EnterDeck_sum_dsi0 := NULL]
data_set[, EnterDeck_sum_dsi1 := NULL]
data_set[, EnterDeck_sum_dsi2 := NULL]
data_set[, EnterDeck_sum_dsi3 := NULL]
gc()

valid_set$EnterDeck_sum_dsi0_log <- log(valid_set$EnterDeck_sum_dsi0 + 1 - min(valid_set$EnterDeck_sum_dsi0))
valid_set$EnterDeck_sum_dsi1_log <- log(valid_set$EnterDeck_sum_dsi1 + 1 - min(valid_set$EnterDeck_sum_dsi1))
valid_set$EnterDeck_sum_dsi2_log <- log(valid_set$EnterDeck_sum_dsi2 + 1 - min(valid_set$EnterDeck_sum_dsi2))
valid_set$EnterDeck_sum_dsi3_log <- log(valid_set$EnterDeck_sum_dsi3 + 1 - min(valid_set$EnterDeck_sum_dsi3))
valid_set[, EnterDeck_sum_dsi0 := NULL]
valid_set[, EnterDeck_sum_dsi1 := NULL]
valid_set[, EnterDeck_sum_dsi2 := NULL]
valid_set[, EnterDeck_sum_dsi3 := NULL]

eval_set$EnterDeck_sum_dsi0_log <- log(eval_set$EnterDeck_sum_dsi0 + 1 - min(eval_set$EnterDeck_sum_dsi0))
eval_set$EnterDeck_sum_dsi1_log <- log(eval_set$EnterDeck_sum_dsi1 + 1 - min(eval_set$EnterDeck_sum_dsi1))
eval_set$EnterDeck_sum_dsi2_log <- log(eval_set$EnterDeck_sum_dsi2 + 1 - min(eval_set$EnterDeck_sum_dsi2))
eval_set$EnterDeck_sum_dsi3_log <- log(eval_set$EnterDeck_sum_dsi3 + 1 - min(eval_set$EnterDeck_sum_dsi3))
eval_set[, EnterDeck_sum_dsi0 := NULL]
eval_set[, EnterDeck_sum_dsi1 := NULL]
eval_set[, EnterDeck_sum_dsi2 := NULL]
eval_set[, EnterDeck_sum_dsi3 := NULL]


dim(data_set) #94
dim(valid_set)
dim(eval_set)
gc()

# ---
# Openchest
# En el dÌa 3, toma valores negativos

# OpenChest_sum_dsi0
summary(data_set$OpenChest_sum_dsi0)
str(data_set$OpenChest_sum_dsi0)
# No tiene NA, distribucion asimetrica positiva

# OpenChest_sum_dsi1
summary(data_set$OpenChest_sum_dsi1)
str(data_set$OpenChest_sum_dsi1)
# No tiene NA, distribucion asimetrica positiva

# OpenChest_sum_dsi2
summary(data_set$OpenChest_sum_dsi2)
summary(valid_set$OpenChest_sum_dsi2)
summary(eval_set$OpenChest_sum_dsi2)
str(data_set$OpenChest_sum_dsi2)
# Tiene NA 445915, lo reemplazamos con la mediana, distribucion asimetrica positiva
data_set$OpenChest_sum_dsi2 <- ifelse(is.na(data_set$OpenChest_sum_dsi2), median(data_set$OpenChest_sum_dsi2, na.rm = TRUE), data_set$OpenChest_sum_dsi2)
valid_set$OpenChest_sum_dsi2 <- ifelse(is.na(valid_set$OpenChest_sum_dsi2), median(valid_set$OpenChest_sum_dsi2, na.rm = TRUE), valid_set$OpenChest_sum_dsi2)
eval_set$OpenChest_sum_dsi2 <- ifelse(is.na(eval_set$OpenChest_sum_dsi2), median(eval_set$OpenChest_sum_dsi2, na.rm = TRUE), eval_set$OpenChest_sum_dsi2)

# OpenChest_sum_dsi3
summary(data_set$OpenChest_sum_dsi3)
str(data_set$OpenChest_sum_dsi3)
# No tiene NA, distribucion asimetrica positiva

# Unifico el total de Open Chest
data_set[, sum_OpenChest := (OpenChest_sum_dsi0 +OpenChest_sum_dsi1 
                             + OpenChest_sum_dsi2 + OpenChest_sum_dsi3)]

m0 <- lm(Label ~ OpenChest_sum_dsi0, data = data_set)
m1 <- lm(Label ~ OpenChest_sum_dsi1, data = data_set)
m2 <- lm(Label ~ OpenChest_sum_dsi2, data = data_set)
m3 <- lm(Label ~ OpenChest_sum_dsi3, data = data_set)
m4 <- lm(Label ~ sum_OpenChest, data = data_set)

summary(m0) # Adjusted R-squared:  0.008864
summary(m1) # Adjusted R-squared:  0.01903
summary(m2) # Adjusted R-squared:  0.02462
summary(m3) # Adjusted R-squared:  0.08952 #revisar
summary(m4) # Adjusted R-squared:  0.08929 #revisar
# Sumadas las variables no ganan explicacion
data_set[, sum_OpenChest := NULL]

rm(m0)
rm(m1)
rm(m2)
rm(m3)
rm(m4)
gc()


#armamos el df para estudiar la correlaciÛn
df<-cbind(data_set$OpenChest_sum_dsi0,data_set$OpenChest_sum_dsi1,data_set$OpenChest_sum_dsi2,data_set$OpenChest_sum_dsi3)
#renombro las variables
colnames(df)<-c('OpenChest_sum_dsi0','OpenChest_sum_dsi1','OpenChest_sum_dsi2','OpenChest_sum_dsi3')
#carculos las correlatividades
corrplot(cor(df), method='number')
#como se puede ver , las variables no est·n correlacionadas
rm(df)
#No hay correlaciones mayores a 0,9


data_set$OpenChest_sum_dsi0_log <- log(data_set$OpenChest_sum_dsi0 + 1 - min(data_set$OpenChest_sum_dsi0))
data_set$OpenChest_sum_dsi1_log <- log(data_set$OpenChest_sum_dsi1 + 1 - min(data_set$OpenChest_sum_dsi1))
data_set$OpenChest_sum_dsi2_log <- log(data_set$OpenChest_sum_dsi2 + 1 - min(data_set$OpenChest_sum_dsi2))
data_set$OpenChest_sum_dsi3_log <- log(data_set$OpenChest_sum_dsi3 + 1 - min(data_set$OpenChest_sum_dsi3))
data_set[, OpenChest_sum_dsi0 := NULL]
data_set[, OpenChest_sum_dsi1 := NULL]
data_set[, OpenChest_sum_dsi2 := NULL]
data_set[, OpenChest_sum_dsi3 := NULL]
gc()

valid_set$OpenChest_sum_dsi0_log <- log(valid_set$OpenChest_sum_dsi0 + 1 - min(valid_set$OpenChest_sum_dsi0))
valid_set$OpenChest_sum_dsi1_log <- log(valid_set$OpenChest_sum_dsi1 + 1 - min(valid_set$OpenChest_sum_dsi1))
valid_set$OpenChest_sum_dsi2_log <- log(valid_set$OpenChest_sum_dsi2 + 1 - min(valid_set$OpenChest_sum_dsi2))
valid_set$OpenChest_sum_dsi3_log <- log(valid_set$OpenChest_sum_dsi3 + 1 - min(valid_set$OpenChest_sum_dsi3))
valid_set[, OpenChest_sum_dsi0 := NULL]
valid_set[, OpenChest_sum_dsi1 := NULL]
valid_set[, OpenChest_sum_dsi2 := NULL]
valid_set[, OpenChest_sum_dsi3 := NULL]

eval_set$OpenChest_sum_dsi0_log <- log(eval_set$OpenChest_sum_dsi0 + 1 - min(eval_set$OpenChest_sum_dsi0))
eval_set$OpenChest_sum_dsi1_log <- log(eval_set$OpenChest_sum_dsi1 + 1 - min(eval_set$OpenChest_sum_dsi1))
eval_set$OpenChest_sum_dsi2_log <- log(eval_set$OpenChest_sum_dsi2 + 1 - min(eval_set$OpenChest_sum_dsi2))
eval_set$OpenChest_sum_dsi3_log <- log(eval_set$OpenChest_sum_dsi3 + 1 - min(eval_set$OpenChest_sum_dsi3))
eval_set[, OpenChest_sum_dsi0 := NULL]
eval_set[, OpenChest_sum_dsi1 := NULL]
eval_set[, OpenChest_sum_dsi2 := NULL]
eval_set[, OpenChest_sum_dsi3 := NULL]


dim(data_set) #94
dim(valid_set)
dim(eval_set)
# ---

# LoseBattle
# Toma valores positivos, se puede hacer LOG

# LoseBattle_sum_dsi0
summary(data_set$LoseBattle_sum_dsi0)
str(data_set$LoseBattle_sum_dsi0)
# No tiene NA, distribucion asimetrica positiva

# LoseBattle_sum_dsi1
summary(data_set$LoseBattle_sum_dsi1)
str(data_set$LoseBattle_sum_dsi1)
# No tiene NA, distribucion asimetrica positiva

# LoseBattle_sum_dsi2
summary(data_set$LoseBattle_sum_dsi2)
str(data_set$LoseBattle_sum_dsi2)
# No tiene NA, distribucion asimetrica positiva

# LoseBattle_sum_dsi3
summary(data_set$LoseBattle_sum_dsi3)
str(data_set$LoseBattle_sum_dsi3)
# No tiene NA, distribucion asimetrica positiva

# Unifico el total de LoseBattle
data_set[, sum_LoseBattle := (LoseBattle_sum_dsi0 +LoseBattle_sum_dsi1 
                              + LoseBattle_sum_dsi2 + LoseBattle_sum_dsi3)]

m0 <- lm(Label ~ LoseBattle_sum_dsi0, data = data_set)
m1 <- lm(Label ~ LoseBattle_sum_dsi1, data = data_set)
m2 <- lm(Label ~ LoseBattle_sum_dsi2, data = data_set)
m3 <- lm(Label ~ LoseBattle_sum_dsi3, data = data_set)
m4 <- lm(Label ~ sum_LoseBattle, data = data_set)

summary(m0) # Adjusted R-squared:  0.004405
summary(m1) # Adjusted R-squared:  0.008536
summary(m2) # Adjusted R-squared:  0.01071
summary(m3) # Adjusted R-squared:  0.02065
summary(m4) # Adjusted R-squared:  0.01686

# Pierde explicaciÛn al sumar
data_set[, sum_LoseBattle := NULL]

rm(m0)
rm(m1)
rm(m2)
rm(m3)
rm(m4)
gc()

#armamos el df para estudiar la correlaciÛn
df<-cbind(data_set$LoseBattle_sum_dsi0,data_set$LoseBattle_sum_dsi1,data_set$LoseBattle_sum_dsi2,data_set$LoseBattle_sum_dsi3)
#renombro las variables
colnames(df)<-c('LoseBattle_sum_dsi0','LoseBattle_sum_dsi1','LoseBattle_sum_dsi2','LoseBattle_sum_dsi3')
#carculos las correlatividades
corrplot(cor(df), method='number')
#como se puede ver , las variables no est·n correlacionadas
rm(df)

# Consideramos la posibilidad que el modelo trabaje con la escala logaritmica, (no hay valores negativos)
data_set$LoseBattle_sum_dsi0_log <- log(data_set$LoseBattle_sum_dsi0 + 1 - min(data_set$LoseBattle_sum_dsi0))
data_set$LoseBattle_sum_dsi1_log <- log(data_set$LoseBattle_sum_dsi1 + 1 - min(data_set$LoseBattle_sum_dsi1))
data_set$LoseBattle_sum_dsi2_log <- log(data_set$LoseBattle_sum_dsi2 + 1 - min(data_set$LoseBattle_sum_dsi2))
data_set$LoseBattle_sum_dsi3_log <- log(data_set$LoseBattle_sum_dsi3 + 1 - min(data_set$LoseBattle_sum_dsi3))
data_set[, LoseBattle_sum_dsi0 := NULL]
data_set[, LoseBattle_sum_dsi1 := NULL]
data_set[, LoseBattle_sum_dsi2 := NULL]
data_set[, LoseBattle_sum_dsi3 := NULL]
gc()

valid_set$LoseBattle_sum_dsi0_log <- log(valid_set$LoseBattle_sum_dsi0 + 1 - min(valid_set$LoseBattle_sum_dsi0))
valid_set$LoseBattle_sum_dsi1_log <- log(valid_set$LoseBattle_sum_dsi1 + 1 - min(valid_set$LoseBattle_sum_dsi1))
valid_set$LoseBattle_sum_dsi2_log <- log(valid_set$LoseBattle_sum_dsi2 + 1 - min(valid_set$LoseBattle_sum_dsi2))
valid_set$LoseBattle_sum_dsi3_log <- log(valid_set$LoseBattle_sum_dsi3 + 1 - min(valid_set$LoseBattle_sum_dsi3))
valid_set[, LoseBattle_sum_dsi0 := NULL]
valid_set[, LoseBattle_sum_dsi1 := NULL]
valid_set[, LoseBattle_sum_dsi2 := NULL]
valid_set[, LoseBattle_sum_dsi3 := NULL]

eval_set$LoseBattle_sum_dsi0_log <- log(eval_set$LoseBattle_sum_dsi0 + 1 - min(eval_set$LoseBattle_sum_dsi0))
eval_set$LoseBattle_sum_dsi1_log <- log(eval_set$LoseBattle_sum_dsi1 + 1 - min(eval_set$LoseBattle_sum_dsi1))
eval_set$LoseBattle_sum_dsi2_log <- log(eval_set$LoseBattle_sum_dsi2 + 1 - min(eval_set$LoseBattle_sum_dsi2))
eval_set$LoseBattle_sum_dsi3_log <- log(eval_set$LoseBattle_sum_dsi3 + 1 - min(eval_set$LoseBattle_sum_dsi3))
eval_set[, LoseBattle_sum_dsi0 := NULL]
eval_set[, LoseBattle_sum_dsi1 := NULL]
eval_set[, LoseBattle_sum_dsi2 := NULL]
eval_set[, LoseBattle_sum_dsi3 := NULL]

dim(data_set) #94
dim(valid_set)
dim(eval_set)
gc()
# ---

# BuyCard
# dsi_1 toma valores negativos

# BuyCard_sum_dsi0
summary(data_set$BuyCard_sum_dsi0)
str(data_set$BuyCard_sum_dsi0)
# No tiene NA, distribucion asimetrica positiva

# BuyCard_sum_dsi1
summary(data_set$BuyCard_sum_dsi1)
str(data_set$BuyCard_sum_dsi1)
# No tiene NA, distribucion asimetrica positiva, toma valores negativos

# BuyCard_sum_dsi2
summary(data_set$BuyCard_sum_dsi2)
str(data_set$BuyCard_sum_dsi2)
# No tiene NA, distribucion asimetrica positiva

# BuyCard_sum_dsi3
summary(data_set$BuyCard_sum_dsi3)
str(data_set$BuyCard_sum_dsi3)
# No tiene NA, distribucion asimetrica positiva

# Unifico el total BuyCard
data_set[, sum_BuyCard := (BuyCard_sum_dsi0 +BuyCard_sum_dsi1 
                           + BuyCard_sum_dsi2 + BuyCard_sum_dsi3)]


m0 <- lm(Label ~ BuyCard_sum_dsi0, data = data_set)
m1 <- lm(Label ~ BuyCard_sum_dsi1, data = data_set)
m2 <- lm(Label ~ BuyCard_sum_dsi2, data = data_set)
m3 <- lm(Label ~ BuyCard_sum_dsi3, data = data_set)
m4 <- lm(Label ~ sum_BuyCard, data = data_set)

summary(m0) # Adjusted R-squared:  0.0009562
summary(m1) # Adjusted R-squared:  -1.886e-07
summary(m2) # Adjusted R-squared:  0.002281 
summary(m3) # Adjusted R-squared:  0.003825
summary(m4) # Adjusted R-squared:  3.317e-06
# Sumadas no tienen una buena explicion. 
data_set[, sum_BuyCard := NULL]
data_set[, BuyCard_sum_dsi1 := NULL] # Muy baja su R cuadrado
valid_set[, BuyCard_sum_dsi1 := NULL]
eval_set[, BuyCard_sum_dsi1 := NULL]

rm(m0)
rm(m1)
rm(m2)
rm(m3)
rm(m4)
gc()

#armamos el df para estudiar la correlaciÛn
df<-cbind(data_set$BuyCard_sum_dsi0,data_set$BuyCard_sum_dsi1,data_set$BuyCard_sum_dsi2,data_set$BuyCard_sum_dsi3)
#renombro las variables
colnames(df)<-c('BuyCard_sum_dsi0','BuyCard_sum_dsi1','BuyCard_sum_dsi2','BuyCard_sum_dsi3')
#carculos las correlatividades
corrplot(cor(df), method='number')
#como se puede ver , las variables no est·n correlacionadas
rm(df)
#no hay correlaciones de mas de 0,9

#Hacemos log de las variables positivas
data_set$BuyCard_sum_dsi0_log <- log(data_set$BuyCard_sum_dsi0 + 1 - min(data_set$BuyCard_sum_dsi0))
data_set$BuyCard_sum_dsi2_log <- log(data_set$BuyCard_sum_dsi2 + 1 - min(data_set$BuyCard_sum_dsi3))
data_set$BuyCard_sum_dsi3_log <- log(data_set$BuyCard_sum_dsi2 + 1 - min(data_set$BuyCard_sum_dsi3))
data_set[, BuyCard_sum_dsi0 := NULL]
data_set[, BuyCard_sum_dsi2 := NULL]
data_set[, BuyCard_sum_dsi3 := NULL]
gc()

valid_set$BuyCard_sum_dsi0_log <- log(valid_set$BuyCard_sum_dsi0 + 1 - min(valid_set$BuyCard_sum_dsi0))
valid_set$BuyCard_sum_dsi2_log <- log(valid_set$BuyCard_sum_dsi2 + 1 - min(valid_set$BuyCard_sum_dsi3))
valid_set$BuyCard_sum_dsi3_log <- log(valid_set$BuyCard_sum_dsi2 + 1 - min(valid_set$BuyCard_sum_dsi3))
valid_set[, BuyCard_sum_dsi0 := NULL]
valid_set[, BuyCard_sum_dsi2 := NULL]
valid_set[, BuyCard_sum_dsi3 := NULL]
gc()

eval_set$BuyCard_sum_dsi0_log <- log(eval_set$BuyCard_sum_dsi0 + 1 - min(eval_set$BuyCard_sum_dsi0))
eval_set$BuyCard_sum_dsi2_log <- log(eval_set$BuyCard_sum_dsi2 + 1 - min(eval_set$BuyCard_sum_dsi3))
eval_set$BuyCard_sum_dsi3_log <- log(eval_set$BuyCard_sum_dsi2 + 1 - min(eval_set$BuyCard_sum_dsi3))
eval_set[, BuyCard_sum_dsi0 := NULL]
eval_set[, BuyCard_sum_dsi2 := NULL]
eval_set[, BuyCard_sum_dsi3 := NULL]


dim(data_set) #93
dim(valid_set)
dim(eval_set)
summary(data_set)
summary(valid_set)
summary(eval_set)
# ---
# StartGameplayModeBattle
# Siempre toma valores positivos

# StartGameplayModeBattle_sum_dsi0
summary(data_set$StartGameplayModeBattle_sum_dsi0)
str(data_set$StartGameplayModeBattle_sum_dsi0)
# No tiene NA, tiene distribucion asimetrica positiva

# StartGameplayModeBattle_sum_dsi1
summary(data_set$StartGameplayModeBattle_sum_dsi1)
str(data_set$StartGameplayModeBattle_sum_dsi1)
# No tiene NA, tiene distribucion asimetrica positiva

# StartGameplayModeBattle_sum_dsi2
summary(data_set$StartGameplayModeBattle_sum_dsi2)
str(data_set$StartGameplayModeBattle_sum_dsi2)
# No tiene NA, tiene distribucion asimetrica positiva

# StartGameplayModeBattle_sum_dsi3
summary(data_set$StartGameplayModeBattle_sum_dsi3)
str(data_set$StartGameplayModeBattle_sum_dsi3)
# No tiene NA, tiene distribucion asimetrica positiva

# Unifico el total de batallas ganadas
data_set[, sum_StartGameplayModeBattle := (StartGameplayModeBattle_sum_dsi0 +StartGameplayModeBattle_sum_dsi1 
                                           + StartGameplayModeBattle_sum_dsi2 + StartGameplayModeBattle_sum_dsi3)]


m0 <- lm(Label ~ StartGameplayModeBattle_sum_dsi0, data = data_set)
m1 <- lm(Label ~ StartGameplayModeBattle_sum_dsi1, data = data_set)
m2 <- lm(Label ~ StartGameplayModeBattle_sum_dsi2, data = data_set)
m3 <- lm(Label ~ StartGameplayModeBattle_sum_dsi3, data = data_set)
m4 <- lm(Label ~ sum_StartGameplayModeBattle, data = data_set)

summary(m0) # Adjusted R-squared:  0.0003824
summary(m1) # Adjusted R-squared:  0.0005192
summary(m2) # Adjusted R-squared:  0.0006479
summary(m3) # Adjusted R-squared:  0.001387
summary(m4) # Adjusted R-squared:  0.001132
# Sumadas no tienen una mayor explicacion
data_set[, sum_StartGameplayModeBattle := NULL]

rm(m0)
rm(m1)
rm(m2)
rm(m3)
rm(m4)
gc()

df<-cbind(data_set$StartGameplayModeBattle_sum_dsi0,data_set$StartGameplayModeBattle_sum_dsi1,data_set$StartGameplayModeBattle_sum_dsi2,data_set$StartGameplayModeBattle_sum_dsi3)
#renombro las variables
colnames(df)<-c('StartGameplayModeBattle_sum_dsi0','StartGameplayModeBattle_sum_dsi1','StartGameplayModeBattle_sum_dsi2','StartGameplayModeBattle_sum_dsi3')
#carculos las correlatividades
corrplot(cor(df), method='number')
#como se puede ver , las variables no est·n correlacionadas
rm(df)
#no hay correlaciones de mas de 0,9


# Consideramos la posibilidad que el modelo trabaje con la escala logaritmica, (no hay valores negativos)
data_set$StartGameplayModeBattle_sum_dsi0_log <- log(data_set$StartGameplayModeBattle_sum_dsi0 + 1 - min(data_set$StartGameplayModeBattle_sum_dsi0))
data_set$StartGameplayModeBattle_sum_dsi1_log <- log(data_set$StartGameplayModeBattle_sum_dsi1 + 1 - min(data_set$StartGameplayModeBattle_sum_dsi1))
data_set$StartGameplayModeBattle_sum_dsi2_log <- log(data_set$StartGameplayModeBattle_sum_dsi2 + 1 - min(data_set$StartGameplayModeBattle_sum_dsi2))
data_set$StartGameplayModeBattle_sum_dsi3_log <- log(data_set$StartGameplayModeBattle_sum_dsi3 + 1 - min(data_set$StartGameplayModeBattle_sum_dsi3))
data_set[, StartGameplayModeBattle_sum_dsi0 := NULL]
data_set[, StartGameplayModeBattle_sum_dsi1 := NULL]
data_set[, StartGameplayModeBattle_sum_dsi2 := NULL]
data_set[, StartGameplayModeBattle_sum_dsi3 := NULL]
gc()

valid_set$StartGameplayModeBattle_sum_dsi0_log <- log(valid_set$StartGameplayModeBattle_sum_dsi0 + 1 - min(valid_set$StartGameplayModeBattle_sum_dsi0))
valid_set$StartGameplayModeBattle_sum_dsi1_log <- log(valid_set$StartGameplayModeBattle_sum_dsi1 + 1 - min(valid_set$StartGameplayModeBattle_sum_dsi1))
valid_set$StartGameplayModeBattle_sum_dsi2_log <- log(valid_set$StartGameplayModeBattle_sum_dsi2 + 1 - min(valid_set$StartGameplayModeBattle_sum_dsi2))
valid_set$StartGameplayModeBattle_sum_dsi3_log <- log(valid_set$StartGameplayModeBattle_sum_dsi3 + 1 - min(valid_set$StartGameplayModeBattle_sum_dsi3))
valid_set[, StartGameplayModeBattle_sum_dsi0 := NULL]
valid_set[, StartGameplayModeBattle_sum_dsi1 := NULL]
valid_set[, StartGameplayModeBattle_sum_dsi2 := NULL]
valid_set[, StartGameplayModeBattle_sum_dsi3 := NULL]

eval_set$StartGameplayModeBattle_sum_dsi0_log <- log(eval_set$StartGameplayModeBattle_sum_dsi0 + 1 - min(eval_set$StartGameplayModeBattle_sum_dsi0))
eval_set$StartGameplayModeBattle_sum_dsi1_log <- log(eval_set$StartGameplayModeBattle_sum_dsi1 + 1 - min(eval_set$StartGameplayModeBattle_sum_dsi1))
eval_set$StartGameplayModeBattle_sum_dsi2_log <- log(eval_set$StartGameplayModeBattle_sum_dsi2 + 1 - min(eval_set$StartGameplayModeBattle_sum_dsi2))
eval_set$StartGameplayModeBattle_sum_dsi3_log <- log(eval_set$StartGameplayModeBattle_sum_dsi3 + 1 - min(eval_set$StartGameplayModeBattle_sum_dsi3))
eval_set[, StartGameplayModeBattle_sum_dsi0 := NULL]
eval_set[, StartGameplayModeBattle_sum_dsi1 := NULL]
eval_set[, StartGameplayModeBattle_sum_dsi2 := NULL]
eval_set[, StartGameplayModeBattle_sum_dsi3 := NULL]



dim(data_set) #93
dim(valid_set)
dim(eval_set)
gc()
#---

# OpenPiggyBank
# Toma siempre variables positivas

# OpenPiggyBank_sum_dsi0
summary(data_set$OpenPiggyBank_sum_dsi0)
str(data_set$OpenPiggyBank_sum_dsi0)
# No tiene NA, tiene distribucion asimetrica positiva

# OpenPiggyBank_sum_dsi1
summary(data_set$OpenPiggyBank_sum_dsi1)
str(data_set$OpenPiggyBank_sum_dsi1)
# No tiene NA, tiene distribucion asimetrica positiva

# OpenPiggyBank_sum_dsi2
summary(data_set$OpenPiggyBank_sum_dsi2)
str(data_set$OpenPiggyBank_sum_dsi2)
# No tiene NA, tiene distribucion asimetrica positiva

# OpenPiggyBank_sum_dsi3
summary(data_set$OpenPiggyBank_sum_dsi3)
str(data_set$OpenPiggyBank_sum_dsi3)
# No tiene NA, tiene distribucion asimetrica positiva

# Unifico el total de batallas ganadas
data_set[, sum_OpenPiggyBank := (OpenPiggyBank_sum_dsi0 +OpenPiggyBank_sum_dsi1 
                                 + OpenPiggyBank_sum_dsi2 + OpenPiggyBank_sum_dsi3)]

valid_set[, sum_OpenPiggyBank := (OpenPiggyBank_sum_dsi0 +OpenPiggyBank_sum_dsi1 
                                 + OpenPiggyBank_sum_dsi2 + OpenPiggyBank_sum_dsi3)]

eval_set[, sum_OpenPiggyBank := (OpenPiggyBank_sum_dsi0 +OpenPiggyBank_sum_dsi1 
                                 + OpenPiggyBank_sum_dsi2 + OpenPiggyBank_sum_dsi3)]


m0 <- lm(Label ~ OpenPiggyBank_sum_dsi0, data = data_set)
m1 <- lm(Label ~ OpenPiggyBank_sum_dsi1, data = data_set)
m2 <- lm(Label ~ OpenPiggyBank_sum_dsi2, data = data_set)
m3 <- lm(Label ~ OpenPiggyBank_sum_dsi3, data = data_set)
m4 <- lm(Label ~ sum_OpenPiggyBank, data = data_set)

summary(m0) # Adjusted R-squared:  0.002923
summary(m1) # Adjusted R-squared:  0.000963
summary(m2) # Adjusted R-squared:  0.000628
summary(m3) # Adjusted R-squared:  0.0004574
summary(m4) # Adjusted R-squared:  0.00483
# Sumadas las variables tienen una mayor explicacion

data_set[, OpenPiggyBank_sum_dsi0 := NULL]
data_set[, OpenPiggyBank_sum_dsi1 := NULL]
data_set[, OpenPiggyBank_sum_dsi2 := NULL]
data_set[, OpenPiggyBank_sum_dsi3 := NULL]

valid_set[, OpenPiggyBank_sum_dsi0 := NULL]
valid_set[, OpenPiggyBank_sum_dsi1 := NULL]
valid_set[, OpenPiggyBank_sum_dsi2 := NULL]
valid_set[, OpenPiggyBank_sum_dsi3 := NULL]

eval_set[, OpenPiggyBank_sum_dsi0 := NULL]
eval_set[, OpenPiggyBank_sum_dsi1 := NULL]
eval_set[, OpenPiggyBank_sum_dsi2 := NULL]
eval_set[, OpenPiggyBank_sum_dsi3 := NULL]

rm(m0)
rm(m1)
rm(m2)
rm(m3)
rm(m4)
gc()

# Consideramos la posibilidad que el modelo trabaje con la escala logaritmica, (no hay valores negativos)
data_set$sum_OpenPiggyBank_log <- log(data_set$sum_OpenPiggyBank + 1 - min(data_set$sum_OpenPiggyBank))
data_set[, sum_OpenPiggyBank := NULL]

valid_set$sum_OpenPiggyBank_log <- log(valid_set$sum_OpenPiggyBank + 1 - min(valid_set$sum_OpenPiggyBank))
valid_set[, sum_OpenPiggyBank := NULL]

eval_set$sum_OpenPiggyBank_log <- log(eval_set$sum_OpenPiggyBank + 1 - min(eval_set$sum_OpenPiggyBank))
eval_set[, sum_OpenPiggyBank := NULL]



dim(data_set) #90
dim(valid_set)
dim(eval_set)
gc()

# ---
# UpgradeCard
# Siempre toma valores positivos

# UpgradeCard_sum_dsi0
summary(data_set$UpgradeCard_sum_dsi0)
str(data_set$UpgradeCard_sum_dsi0)
# No tiene NA, tiene distribucion asimetrica positiva

# UpgradeCard_sum_dsi1
summary(data_set$UpgradeCard_sum_dsi1)
str(data_set$UpgradeCard_sum_dsi1)
# No tiene NA, tiene distribucion asimetrica positiva

# UpgradeCard_sum_dsi2

str(data_set$UpgradeCard_sum_dsi2)
# No tiene NA, tiene distribucion asimetrica positiva

# UpgradeCard_sum_dsi3
summary(data_set$UpgradeCard_sum_dsi3)
str(data_set$UpgradeCard_sum_dsi3)
# No tiene NA, tiene distribucion asimetrica positiva

# Unifico el UpgradeCard
data_set[, sum_UpgradeCard := (UpgradeCard_sum_dsi0 +UpgradeCard_sum_dsi1 
                               + UpgradeCard_sum_dsi2 + UpgradeCard_sum_dsi3)]

valid_set[, sum_UpgradeCard := (UpgradeCard_sum_dsi0 +UpgradeCard_sum_dsi1 
                               + UpgradeCard_sum_dsi2 + UpgradeCard_sum_dsi3)]

eval_set[, sum_UpgradeCard := (UpgradeCard_sum_dsi0 +UpgradeCard_sum_dsi1 
                               + UpgradeCard_sum_dsi2 + UpgradeCard_sum_dsi3)]


m0 <- lm(Label ~ UpgradeCard_sum_dsi0, data = data_set)
m1 <- lm(Label ~ UpgradeCard_sum_dsi1, data = data_set)
m2 <- lm(Label ~ UpgradeCard_sum_dsi2, data = data_set)
m3 <- lm(Label ~ UpgradeCard_sum_dsi3, data = data_set)
m4 <- lm(Label ~ sum_UpgradeCard, data = data_set)

summary(m0) # Adjusted R-squared:  0.007504
summary(m1) # Adjusted R-squared:  0.01082
summary(m2) # Adjusted R-squared:  0.0145
summary(m3) # Adjusted R-squared:  0.02643
summary(m4) # Adjusted R-squared:  0.02877
# Sumadas las variables tienen una mayor explicacion

data_set[, UpgradeCard_sum_dsi0 := NULL]
data_set[, UpgradeCard_sum_dsi1 := NULL]
data_set[, UpgradeCard_sum_dsi2 := NULL]
data_set[, UpgradeCard_sum_dsi3 := NULL]

valid_set[, UpgradeCard_sum_dsi0 := NULL]
valid_set[, UpgradeCard_sum_dsi1 := NULL]
valid_set[, UpgradeCard_sum_dsi2 := NULL]
valid_set[, UpgradeCard_sum_dsi3 := NULL]

eval_set[, UpgradeCard_sum_dsi0 := NULL]
eval_set[, UpgradeCard_sum_dsi1 := NULL]
eval_set[, UpgradeCard_sum_dsi2 := NULL]
eval_set[, UpgradeCard_sum_dsi3 := NULL]


rm(m0)
rm(m1)
rm(m2)
rm(m3)
rm(m4)
gc()
# Consideramos la posibilidad que el modelo trabaje con la escala logaritmica, (no hay valores negativos)
data_set$sum_UpgradeCard_log <- log(data_set$sum_UpgradeCard + 1 - min(data_set$sum_UpgradeCard))
data_set[, sum_UpgradeCard := NULL]

valid_set$sum_UpgradeCard_log <- log(valid_set$sum_UpgradeCard + 1 - min(valid_set$sum_UpgradeCard))
valid_set[, sum_UpgradeCard := NULL]

eval_set$sum_UpgradeCard_log <- log(eval_set$sum_UpgradeCard + 1 - min(eval_set$sum_UpgradeCard))
eval_set[, sum_UpgradeCard := NULL]


dim(data_set) #87
dim(valid_set)
dim(eval_set)

# ---

# Entershop
# Toma siempre valores positivos, se puede hacer LOG

# EnterShop_sum_dsi0
summary(data_set$EnterShop_sum_dsi0)
str(data_set$EnterShop_sum_dsi0)
# No tiene NA, tiene distribucion asimetrica positiva

# EnterShop_sum_dsi1
summary(data_set$EnterShop_sum_dsi1)
str(data_set$EnterShop_sum_dsi1)
# No tiene NA, tiene distribucion asimetrica positiva

# EnterShop_sum_dsi2
summary(data_set$EnterShop_sum_dsi2)
str(data_set$EnterShop_sum_dsi2)
# No tiene NA, tiene distribucion asimetrica positiva

# EnterShop_sum_dsi3
summary(data_set$EnterShop_sum_dsi3)
str(data_set$EnterShop_sum_dsi3)
# No tiene NA, tiene distribucion asimetrica positiva

# Unifico el total de batallas ganadas
data_set[, sum_EnterShop := (EnterShop_sum_dsi0 +EnterShop_sum_dsi1 
                             + EnterShop_sum_dsi2 + EnterShop_sum_dsi3)]

m0 <- lm(Label ~ EnterShop_sum_dsi0, data = data_set)
m1 <- lm(Label ~ EnterShop_sum_dsi1, data = data_set)
m2 <- lm(Label ~ EnterShop_sum_dsi2, data = data_set)
m3 <- lm(Label ~ EnterShop_sum_dsi3, data = data_set)
m4 <- lm(Label ~ sum_EnterShop, data = data_set)

summary(m0) # Adjusted R-squared:  0.006759
summary(m1) # Adjusted R-squared:  0.01102
summary(m2) # Adjusted R-squared:  0.01367
summary(m3) # Adjusted R-squared:  0.02671
summary(m4) # Adjusted R-squared:  0.02464
# Sumadas las variables no tienen una mayor expliaciÛn
data_set[, sum_EnterShop := NULL]

rm(m0)
rm(m1)
rm(m2)
rm(m3)
rm(m4)
gc()

df<-cbind(data_set$EnterShop_sum_dsi0,data_set$EnterShop_sum_dsi1,data_set$EnterShop_sum_dsi2,data_set$EnterShop_sum_dsi3)
#renombro las variables
colnames(df)<-c('EnterShop_sum_dsi0','EnterShop_sum_dsi1','EnterShop_sum_dsi2','EnterShop_sum_dsi3')
#carculos las correlatividades
corrplot(cor(df), method='number')
#como se puede ver , las variables no est·n correlacionadas
rm(df)
#no hay correlaciones de mas de 0,9


# Consideramos la posibilidad que el modelo trabaje con la escala logaritmica, (no hay valores negativos)
data_set$EnterShop_sum_dsi0_log <- log(data_set$EnterShop_sum_dsi0 + 1 - min(data_set$EnterShop_sum_dsi0))
data_set$EnterShop_sum_dsi1_log <- log(data_set$EnterShop_sum_dsi1 + 1 - min(data_set$EnterShop_sum_dsi0))
data_set$EnterShop_sum_dsi2_log <- log(data_set$EnterShop_sum_dsi2 + 1 - min(data_set$EnterShop_sum_dsi0))
data_set$EnterShop_sum_dsi3_log <- log(data_set$EnterShop_sum_dsi3 + 1 - min(data_set$EnterShop_sum_dsi0))
data_set[, EnterShop_sum_dsi0 := NULL]
data_set[, EnterShop_sum_dsi1 := NULL]
data_set[, EnterShop_sum_dsi2 := NULL]
data_set[, EnterShop_sum_dsi3 := NULL]
gc()

valid_set$EnterShop_sum_dsi0_log <- log(valid_set$EnterShop_sum_dsi0 + 1 - min(valid_set$EnterShop_sum_dsi0))
valid_set$EnterShop_sum_dsi1_log <- log(valid_set$EnterShop_sum_dsi1 + 1 - min(valid_set$EnterShop_sum_dsi0))
valid_set$EnterShop_sum_dsi2_log <- log(valid_set$EnterShop_sum_dsi2 + 1 - min(valid_set$EnterShop_sum_dsi0))
valid_set$EnterShop_sum_dsi3_log <- log(valid_set$EnterShop_sum_dsi3 + 1 - min(valid_set$EnterShop_sum_dsi0))
valid_set[, EnterShop_sum_dsi0 := NULL]
valid_set[, EnterShop_sum_dsi1 := NULL]
valid_set[, EnterShop_sum_dsi2 := NULL]
valid_set[, EnterShop_sum_dsi3 := NULL]

eval_set$EnterShop_sum_dsi0_log <- log(eval_set$EnterShop_sum_dsi0 + 1 - min(eval_set$EnterShop_sum_dsi0))
eval_set$EnterShop_sum_dsi1_log <- log(eval_set$EnterShop_sum_dsi1 + 1 - min(eval_set$EnterShop_sum_dsi0))
eval_set$EnterShop_sum_dsi2_log <- log(eval_set$EnterShop_sum_dsi2 + 1 - min(eval_set$EnterShop_sum_dsi0))
eval_set$EnterShop_sum_dsi3_log <- log(eval_set$EnterShop_sum_dsi3 + 1 - min(eval_set$EnterShop_sum_dsi0))
eval_set[, EnterShop_sum_dsi0 := NULL]
eval_set[, EnterShop_sum_dsi1 := NULL]
eval_set[, EnterShop_sum_dsi2 := NULL]
eval_set[, EnterShop_sum_dsi3 := NULL]
gc()

dim(data_set) #87
dim(valid_set)
dim(eval_set)
# ---

# ChangeArena

# ChangeArena_sum_dsi0
summary(data_set$ChangeArena_sum_dsi0)
str(data_set$ChangeArena_sum_dsi0)
# No tiene NA, tienen una distribucion asimetrica positiva

# ChangeArena_sum_dsi1
summary(data_set$ChangeArena_sum_dsi1)
str(data_set$ChangeArena_sum_dsi1)
# No tiene NA, tienen una distribucion asimetrica positiva

# ChangeArena_sum_dsi2
summary(data_set$ChangeArena_sum_dsi2)
str(data_set$ChangeArena_sum_dsi2)
# No tiene NA, tienen una distribucion asimetrica positiva

# ChangeArena_sum_dsi3
summary(data_set$ChangeArena_sum_dsi3)
summary(valid_set$ChangeArena_sum_dsi3)
summary(eval_set$ChangeArena_sum_dsi3)
str(data_set$ChangeArena_sum_dsi3)
# Tiene 652331 NA, tienen una distribucion asimetrica positiva
data_set$ChangeArena_sum_dsi3 <- ifelse(is.na(data_set$ChangeArena_sum_dsi3), median(data_set$ChangeArena_sum_dsi3, na.rm = TRUE), data_set$ChangeArena_sum_dsi3)
valid_set$ChangeArena_sum_dsi3 <- ifelse(is.na(valid_set$ChangeArena_sum_dsi3), median(valid_set$ChangeArena_sum_dsi3, na.rm = TRUE), valid_set$ChangeArena_sum_dsi3)
eval_set$ChangeArena_sum_dsi3 <- ifelse(is.na(eval_set$ChangeArena_sum_dsi3), median(eval_set$ChangeArena_sum_dsi3, na.rm = TRUE), eval_set$ChangeArena_sum_dsi3)

# Unifico el total de Change Arena
data_set[, sum_ChangeArena := (ChangeArena_sum_dsi0 +ChangeArena_sum_dsi1 
                               + ChangeArena_sum_dsi2 + ChangeArena_sum_dsi3)]

valid_set[, sum_ChangeArena := (ChangeArena_sum_dsi0 +ChangeArena_sum_dsi1 
                               + ChangeArena_sum_dsi2 + ChangeArena_sum_dsi3)]

eval_set[, sum_ChangeArena := (ChangeArena_sum_dsi0 +ChangeArena_sum_dsi1 
                               + ChangeArena_sum_dsi2 + ChangeArena_sum_dsi3)]

m0 <- lm(Label ~ ChangeArena_sum_dsi0, data = data_set)
m1 <- lm(Label ~ ChangeArena_sum_dsi1, data = data_set)
m2 <- lm(Label ~ ChangeArena_sum_dsi2, data = data_set)
m3 <- lm(Label ~ ChangeArena_sum_dsi3, data = data_set)
m4 <- lm(Label ~ sum_ChangeArena, data = data_set)

summary(m0) # Adjusted R-squared:  0.005517
summary(m1) # Adjusted R-squared:  0.006053 
summary(m2) # Adjusted R-squared:  0.006501
summary(m3) # Adjusted R-squared:  0.01006
summary(m4) # Adjusted R-squared:  0.01887 
# Sumadas las variables tienen una mayor explicacion

data_set[, ChangeArena_sum_dsi0 := NULL]
data_set[, ChangeArena_sum_dsi1 := NULL]
data_set[, ChangeArena_sum_dsi2 := NULL]
data_set[, ChangeArena_sum_dsi3 := NULL]

valid_set[, ChangeArena_sum_dsi0 := NULL]
valid_set[, ChangeArena_sum_dsi1 := NULL]
valid_set[, ChangeArena_sum_dsi2 := NULL]
valid_set[, ChangeArena_sum_dsi3 := NULL]

eval_set[, ChangeArena_sum_dsi0 := NULL]
eval_set[, ChangeArena_sum_dsi1 := NULL]
eval_set[, ChangeArena_sum_dsi2 := NULL]
eval_set[, ChangeArena_sum_dsi3 := NULL]


rm(m0)
rm(m1)
rm(m2)
rm(m3)
rm(m4)
gc()
# Consideramos la posibilidad que el modelo trabaje con la escala logaritmica, (no hay valores negativos)
data_set$sum_ChangeArena_log <- log(data_set$sum_ChangeArena + 1 - min(data_set$sum_ChangeArena))
data_set[, sum_ChangeArena := NULL]

valid_set$sum_ChangeArena_log <- log(valid_set$sum_ChangeArena + 1 - min(valid_set$sum_ChangeArena))
valid_set[, sum_ChangeArena := NULL]

eval_set$sum_ChangeArena_log <- log(eval_set$sum_ChangeArena + 1 - min(eval_set$sum_ChangeArena))
eval_set[, sum_ChangeArena := NULL]


dim(data_set) #84
dim(valid_set)
dim(eval_set)
gc()

#----
# JoinTournament
# Toma siempre valores positivos, se puede hacer LOG

# JoinTournament_sum_dsi0
summary(data_set$JoinTournament_sum_dsi0)
str(data_set$JoinTournament_sum_dsi0)
# No tiene NA, tienen una distribucion asimetrica positiva

# JoinTournament_sum_dsi1
summary(data_set$JoinTournament_sum_dsi1)
str(data_set$JoinTournament_sum_dsi1)
# No tiene NA, tienen una distribucion asimetrica positiva

# JoinTournament_sum_dsi2
summary(data_set$JoinTournament_sum_dsi2)
str(data_set$JoinTournament_sum_dsi2)
# No tiene NA, tienen una distribucion asimetrica positiva

# JoinTournament_sum_dsi3
summary(data_set$JoinTournament_sum_dsi3)
str(data_set$JoinTournament_sum_dsi3)
# No tiene NA, tienen una distribucion asimetrica positiva

# Unifico el total de Join Tournament
data_set[, sum_JoinTournament := (JoinTournament_sum_dsi0 +JoinTournament_sum_dsi1 
                                  + JoinTournament_sum_dsi2 + JoinTournament_sum_dsi3)]


m0 <- lm(Label ~ JoinTournament_sum_dsi0, data = data_set)
m1 <- lm(Label ~ JoinTournament_sum_dsi1, data = data_set)
m2 <- lm(Label ~ JoinTournament_sum_dsi2, data = data_set)
m3 <- lm(Label ~ JoinTournament_sum_dsi3, data = data_set)
m4 <- lm(Label ~ sum_JoinTournament, data = data_set)

summary(m0) # Adjusted R-squared:  2.503e-05
summary(m1) # Adjusted R-squared:  2.679e-05
summary(m2) # Adjusted R-squared:  3.633e-14
summary(m3) # Adjusted R-squared:  2.898e-05 
summary(m4) # Adjusted R-squared:  5.389e-05 
# Estas variables explican muy poco de nuestra columna a predecir, asique las quitamos del data_set

data_set[, JoinTournament_sum_dsi0 := NULL]
data_set[, JoinTournament_sum_dsi1 := NULL]
data_set[, JoinTournament_sum_dsi2 := NULL]
data_set[, JoinTournament_sum_dsi3 := NULL]
data_set[, sum_JoinTournament := NULL]

valid_set[, JoinTournament_sum_dsi0 := NULL]
valid_set[, JoinTournament_sum_dsi1 := NULL]
valid_set[, JoinTournament_sum_dsi2 := NULL]
valid_set[, JoinTournament_sum_dsi3 := NULL]

eval_set[, JoinTournament_sum_dsi0 := NULL]
eval_set[, JoinTournament_sum_dsi1 := NULL]
eval_set[, JoinTournament_sum_dsi2 := NULL]
eval_set[, JoinTournament_sum_dsi3 := NULL]


rm(m0)
rm(m1)
rm(m2)
rm(m3)
rm(m4)
gc()

dim(data_set) #80
dim(valid_set)
dim(eval_set)
# ---

# QuitTournament
# Toma siempre valores positivos, se puede hacer log

# QuitTournament_sum_dsi0
summary(data_set$QuitTournament_sum_dsi0)
str(data_set$QuitTournament_sum_dsi0)
# No tiene NA, tienen una distribucion asimetrica positiva

# QuitTournament_sum_dsi1
summary(data_set$QuitTournament_sum_dsi1)
str(data_set$QuitTournament_sum_dsi1)
# No tiene NA, tienen una distribucion asimetrica positiva

# QuitTournament_sum_dsi2
summary(data_set$QuitTournament_sum_dsi2)
str(data_set$QuitTournament_sum_dsi2)
# No tiene NA, tienen una distribucion asimetrica positiva

# QuitTournament_sum_dsi3
summary(data_set$QuitTournament_sum_dsi3)
str(data_set$QuitTournament_sum_dsi3)
# No tiene NA, tienen una distribucion asimetrica positiva

# Unifico el total de QuitTournament
data_set[, sum_QuitTournament := (QuitTournament_sum_dsi0 +QuitTournament_sum_dsi1 
                                  + QuitTournament_sum_dsi2 + QuitTournament_sum_dsi3)]


m0 <- lm(Label ~ QuitTournament_sum_dsi0, data = data_set)
m1 <- lm(Label ~ QuitTournament_sum_dsi1, data = data_set)
m2 <- lm(Label ~ QuitTournament_sum_dsi2, data = data_set)
m3 <- lm(Label ~ QuitTournament_sum_dsi3, data = data_set)
m4 <- lm(Label ~ sum_QuitTournament, data = data_set)

summary(m0) # Adjusted R-squared:  0.01083
summary(m1) # Adjusted R-squared:  2.423e-05
summary(m2) # Adjusted R-squared:  2.348e-05
summary(m3) # Adjusted R-squared:  3.275e-05
summary(m4) # Adjusted R-squared:  6.016e-05
# Estas variables explican muy poco de nuestra columna a predecir, asique las quitamos del data_set

data_set[, QuitTournament_sum_dsi1 := NULL]
data_set[, QuitTournament_sum_dsi2 := NULL]
data_set[, QuitTournament_sum_dsi3 := NULL]
data_set[, sum_QuitTournament := NULL]

valid_set[, QuitTournament_sum_dsi1 := NULL]
valid_set[, QuitTournament_sum_dsi2 := NULL]
valid_set[, QuitTournament_sum_dsi3 := NULL]

eval_set[, QuitTournament_sum_dsi1 := NULL]
eval_set[, QuitTournament_sum_dsi2 := NULL]
eval_set[, QuitTournament_sum_dsi3 := NULL]


rm(m0)
rm(m1)
rm(m2)
rm(m3)
rm(m4)
gc()

data_set$QuitTournament_sum_dsi0_log <- log(data_set$QuitTournament_sum_dsi0 + 1 - min(data_set$QuitTournament_sum_dsi0))
data_set[, QuitTournament_sum_dsi0 := NULL]

valid_set$QuitTournament_sum_dsi0_log <- log(valid_set$QuitTournament_sum_dsi0 + 1 - min(valid_set$QuitTournament_sum_dsi0))
valid_set[, QuitTournament_sum_dsi0 := NULL]

eval_set$QuitTournament_sum_dsi0_log <- log(eval_set$QuitTournament_sum_dsi0 + 1 - min(eval_set$QuitTournament_sum_dsi0))
eval_set[, QuitTournament_sum_dsi0 := NULL]

dim(data_set) #77
dim(valid_set)
dim(eval_set)
# ---
# StartTournamentBattle 
# Toma siempre valores positivos, se puede hacer LOG

# StartTournamentBattle_sum_dsi0
summary(data_set$StartTournamentBattle_sum_dsi0)
str(data_set$StartTournamentBattle_sum_dsi0)
# No tiene NA, tienen una distribucion asimetrica positiva

# StartTournamentBattle_sum_dsi1
summary(data_set$StartTournamentBattle_sum_dsi1)
str(data_set$StartTournamentBattle_sum_dsi1)
# No tiene NA, tienen una distribucion asimetrica positiva

# StartTournamentBattle_sum_dsi2
summary(data_set$StartTournamentBattle_sum_dsi2)
str(data_set$StartTournamentBattle_sum_dsi2)
# No tiene NA, tienen una distribucion asimetrica positiva

# StartTournamentBattle_sum_dsi3
summary(data_set$StartTournamentBattle_sum_dsi3)
str(data_set$StartTournamentBattle_sum_dsi3)
# No tiene NA, tienen una distribucion asimetrica positiva

# Unifico el total StartTournamentBattle
data_set[, sum_StartTournamentBattle := (StartTournamentBattle_sum_dsi0 +StartTournamentBattle_sum_dsi1 
                                         + StartTournamentBattle_sum_dsi2 + StartTournamentBattle_sum_dsi3)]


m0 <- lm(Label ~ StartTournamentBattle_sum_dsi0, data = data_set)
m1 <- lm(Label ~ StartTournamentBattle_sum_dsi1, data = data_set)
m2 <- lm(Label ~ StartTournamentBattle_sum_dsi2, data = data_set)
m3 <- lm(Label ~ StartTournamentBattle_sum_dsi3, data = data_set)
m4 <- lm(Label ~ sum_StartTournamentBattle, data = data_set)

summary(m0) # Adjusted R-squared:  1.956e-05
summary(m1) # Adjusted R-squared:  1.907e-05
summary(m2) # Adjusted R-squared:  1.524e-05
summary(m3) # Adjusted R-squared:  1.993e-05
summary(m4) # Adjusted R-squared:  3.785e-05
# Explican muy poco de nuestra variable a predecir asique las quitamos del data_set

data_set[, StartTournamentBattle_sum_dsi0 := NULL]
data_set[, StartTournamentBattle_sum_dsi1 := NULL]
data_set[, StartTournamentBattle_sum_dsi2 := NULL]
data_set[, StartTournamentBattle_sum_dsi3 := NULL]
data_set[, sum_StartTournamentBattle := NULL]

valid_set[, StartTournamentBattle_sum_dsi0 := NULL]
valid_set[, StartTournamentBattle_sum_dsi1 := NULL]
valid_set[, StartTournamentBattle_sum_dsi2 := NULL]
valid_set[, StartTournamentBattle_sum_dsi3 := NULL]

eval_set[, StartTournamentBattle_sum_dsi0 := NULL]
eval_set[, StartTournamentBattle_sum_dsi1 := NULL]
eval_set[, StartTournamentBattle_sum_dsi2 := NULL]
eval_set[, StartTournamentBattle_sum_dsi3 := NULL]
gc()

rm(m0)
rm(m1)
rm(m2)
rm(m3)
rm(m4)
gc()

dim(data_set) #73
dim(valid_set)
dim(eval_set)

# ---

# WinTournamentBattle
# Toma siempre valores positivos, se puede hacer log

# WinTournamentBattle_sum_dsi0
summary(data_set$WinTournamentBattle_sum_dsi0)
str(data_set$WinTournamentBattle_sum_dsi0)
# No tiene NA, tienen una distribucion asimetrica positiva

# WinTournamentBattle_sum_dsi1
summary(data_set$WinTournamentBattle_sum_dsi1)
str(data_set$WinTournamentBattle_sum_dsi1)
# No tiene NA, tienen una distribucion asimetrica positiva

# WinTournamentBattle_sum_dsi2
summary(data_set$WinTournamentBattle_sum_dsi2)
str(data_set$WinTournamentBattle_sum_dsi2)
# No tiene NA, tienen una distribucion asimetrica positiva

# WinTournamentBattle_sum_dsi3
summary(data_set$WinTournamentBattle_sum_dsi3)
str(data_set$WinTournamentBattle_sum_dsi3)
# No tiene NA, tienen una distribucion asimetrica positiva

# Unifico el total de batallas ganadas
data_set[, sum_WinTournamentBattle:= (WinTournamentBattle_sum_dsi0 +WinTournamentBattle_sum_dsi1 
                                      + WinTournamentBattle_sum_dsi2 + WinTournamentBattle_sum_dsi3)]

valid_set[, sum_WinTournamentBattle:= (WinTournamentBattle_sum_dsi0 +WinTournamentBattle_sum_dsi1 
                                      + WinTournamentBattle_sum_dsi2 + WinTournamentBattle_sum_dsi3)]

eval_set[, sum_WinTournamentBattle:= (WinTournamentBattle_sum_dsi0 +WinTournamentBattle_sum_dsi1 
                                      + WinTournamentBattle_sum_dsi2 + WinTournamentBattle_sum_dsi3)]


m0 <- lm(Label ~ WinTournamentBattle_sum_dsi0, data = data_set)
m1 <- lm(Label ~ WinTournamentBattle_sum_dsi1, data = data_set)
m2 <- lm(Label ~ WinTournamentBattle_sum_dsi2, data = data_set)
m3 <- lm(Label ~ WinTournamentBattle_sum_dsi3, data = data_set)
m4 <- lm(Label ~ sum_WinTournamentBattle, data = data_set)

summary(m0) # Adjusted R-squared:  1.347e-05
summary(m1) # Adjusted R-squared:  1.335e-05
summary(m2) # Adjusted R-squared:  1.024e-05 
summary(m3) # Adjusted R-squared:  1.299e-05
summary(m4) # Adjusted R-squared:  2.596e-05
# Sumadas las variables tienen una mayor explicacion

data_set[, WinTournamentBattle_sum_dsi0 := NULL]
data_set[, WinTournamentBattle_sum_dsi1 := NULL]
data_set[, WinTournamentBattle_sum_dsi2 := NULL]
data_set[, WinTournamentBattle_sum_dsi3 := NULL]
data_set[, sum_WinTournamentBattle := NULL]

valid_set[, WinTournamentBattle_sum_dsi0 := NULL]
valid_set[, WinTournamentBattle_sum_dsi1 := NULL]
valid_set[, WinTournamentBattle_sum_dsi2 := NULL]
valid_set[, WinTournamentBattle_sum_dsi3 := NULL]
valid_set[, sum_WinTournamentBattle := NULL]

eval_set[, WinTournamentBattle_sum_dsi0 := NULL]
eval_set[, WinTournamentBattle_sum_dsi1 := NULL]
eval_set[, WinTournamentBattle_sum_dsi2 := NULL]
eval_set[, WinTournamentBattle_sum_dsi3 := NULL]
eval_set[, sum_WinTournamentBattle := NULL]

rm(m0)
rm(m1)
rm(m2)
rm(m3)
rm(m4)
gc()
dim(data_set) #69
dim(valid_set)
dim(eval_set)

# ---
# LoseTournamentBattle
# Toma siempre valores positivos, se puede hacer LOG

# LoseTournamentBattle_sum_dsi0
summary(data_set$LoseTournamentBattle_sum_dsi0)
str(data_set$LoseTournamentBattle_sum_dsi0)
# No tiene NA, tienen una distribucion asimetrica positiva

# LoseTournamentBattle_sum_dsi1
summary(data_set$LoseTournamentBattle_sum_dsi1)
str(data_set$LoseTournamentBattle_sum_dsi1)
# No tiene NA, tienen una distribucion asimetrica positiva

# LoseTournamentBattle_sum_dsi2
summary(data_set$LoseTournamentBattle_sum_dsi2)
str(data_set$LoseTournamentBattle_sum_dsi2)
# No tiene NA, tienen una distribucion asimetrica positiva

# LoseTournamentBattle_sum_dsi3
summary(data_set$LoseTournamentBattle_sum_dsi3)
str(data_set$LoseTournamentBattle_sum_dsi3)
# No tiene NA, tienen una distribucion asimetrica positiva

# Sumo la variable LoseTournamentBatle
data_set[, sum_LoseTournamentBattle:= (LoseTournamentBattle_sum_dsi0 +LoseTournamentBattle_sum_dsi1 
                                       + LoseTournamentBattle_sum_dsi2 + LoseTournamentBattle_sum_dsi3)]


m0 <- lm(Label ~ LoseTournamentBattle_sum_dsi0, data = data_set)
m1 <- lm(Label ~ LoseTournamentBattle_sum_dsi1, data = data_set)
m2 <- lm(Label ~ LoseTournamentBattle_sum_dsi2, data = data_set)
m3 <- lm(Label ~ LoseTournamentBattle_sum_dsi3, data = data_set)
m4 <- lm(Label ~ sum_LoseTournamentBattle, data = data_set)

summary(m0) # Adjusted R-squared:   2.658e-05
summary(m1) # Adjusted R-squared:   2.518e-05 
summary(m2) # Adjusted R-squared:   2.088e-05 
summary(m3) # Adjusted R-squared:   2.819e-05 
summary(m4) # Adjusted R-squared:   5.599e-0
# Las variables no explican mi variable a predecir, las saco del data set

data_set[, LoseTournamentBattle_sum_dsi0 := NULL]
data_set[, LoseTournamentBattle_sum_dsi1 := NULL]
data_set[, LoseTournamentBattle_sum_dsi2 := NULL]
data_set[, LoseTournamentBattle_sum_dsi3 := NULL]
data_set[, sum_LoseTournamentBattle := NULL]

valid_set[, LoseTournamentBattle_sum_dsi0 := NULL]
valid_set[, LoseTournamentBattle_sum_dsi1 := NULL]
valid_set[, LoseTournamentBattle_sum_dsi2 := NULL]
valid_set[, LoseTournamentBattle_sum_dsi3 := NULL]

eval_set[, LoseTournamentBattle_sum_dsi0 := NULL]
eval_set[, LoseTournamentBattle_sum_dsi1 := NULL]
eval_set[, LoseTournamentBattle_sum_dsi2 := NULL]
eval_set[, LoseTournamentBattle_sum_dsi3 := NULL]


rm(m0)
rm(m1)
rm(m2)
rm(m3)
rm(m4)
gc()

dim(data_set) # Tengo 65 variables
dim(valid_set)
dim(eval_set)
head(data_set)

# ----------------------------------------------------
# Analizamos Other Features

# Device Model
summary(data_set$device_model)
str(data_set$device_model)
# Tiene 9 NA, Factor w/37963 levels

# Platform
summary(data_set$platform)
str(data_set$platform)
# No tiene NA, Factor w/2 levels

# Id
summary(data_set$id)
str(data_set$id)
# No tiene NA

# user_id
summary(data_set$user_id)
str(data_set$user_id)
# No tiene NA
# La sacamos del data_set porque al ser el "nombre" de los usuarios no nos va a ayudar a predecir churn
# Y no vemos una importancia importante al correr nuestro modelo
data_set[, user_id := NULL]
valid_set[, user_id := NULL]
eval_set[, user_id := NULL]


# Install Date
summary(data_set$install_date)
str(data_set$install_date)
# No tiene NA

# Country
summary(data_set$country)
str(data_set$country)
# Tiene 72418 NAs, Factor w/249 levels

# -------------------------------------------------------
# Variables de gasto
# Analizamos su importancia

m0 <- lm(Label ~ soft_positive, data = data_set)
m1 <- lm(Label ~ soft_negative, data = data_set)
m2 <- lm(Label ~ hard_positive, data = data_set)
m3 <- lm(Label ~ hard_negative, data = data_set)

summary(m0) # Adjusted R-squared:  -1.955e-07 
summary(m1) # Adjusted R-squared:  -3.155e-07   
summary(m2) # Adjusted R-squared:  -3.051e-07 
summary(m3) # Adjusted R-squared:  -3.155e-07   

#--------------------------------------------------------
# Analisamos las variables categoricas
# Categorical 1
summary(data_set$categorical_1)
str(data_set$categorical_1)
# No tiene NAs, Factor w/25

# Categorical 2
summary(data_set$categorical_2)
str(data_set$categorical_2)
# No tiene NAs, Factor w/1180

# Categorical 3
summary(data_set$categorical_3)
str(data_set$categorical_3)
# No tiene NAs, Factor w/1614

# Categorical 4
summary(data_set$categorical_4)
str(data_set$categorical_4)
# No tiene NAs, Factor w/4851

# Categorical 5
summary(data_set$categorical_5)
str(data_set$categorical_5)
# No tiene NAs, Factor w/11910

# Categorical 6
summary(data_set$categorical_6)
str(data_set$categorical_6)
# No tiene NAs, Factor w/11

# Categorical 7
summary(data_set$categorical_7)
str(data_set$categorical_7)
# No tiene NAs, Factor w/353

m1 <- lm(Label ~ categorical_1, data = data_set) # Se queda sin memoria la PC
m2 <- lm(Label ~ categorical_2, data = data_set) # Se queda sin memoria la PC
m3 <- lm(Label ~ categorical_3, data = data_set) # Se queda sin memoria la PC
m4 <- lm(Label ~ categorical_4, data = data_set) # Se queda sin memoria la PC
m5 <- lm(Label ~ categorical_5, data = data_set) # Se queda sin memoria la PC
m6 <- lm(Label ~ categorical_6, data = data_set) # Se queda sin memoria la PC
m6 <- lm(Label ~ categorical_7, data = data_set) # Se queda sin memoria la PC

#no te deja operar con vectores de ese tamaÒo, por lo que ante la posibilidad
#de perder variables predictoreas se dejan
#--------------------------------------------------------
# Analisamos las variables Tutorial features
# TutorialStart
summary(data_set$TutorialStart)
str(data_set$TutorialStart)
# Variable binaria TRUE, FALSE

# TutorialStartPart1
summary(data_set$TutorialStartPart1)
str(data_set$TutorialStartPart1)
# Variable binaria TRUE, FALSE

# TutorialStartPart2
summary(data_set$TutorialStartPart2)
str(data_set$TutorialStartPart2)
# Variable binaria TRUE, FALSE

# TutorialStartPart3
summary(data_set$TutorialStartPart3)
str(data_set$TutorialStartPart3)
# Variable binaria TRUE, FALSE

# TutorialStartPart4
summary(data_set$TutorialStartPart4)
str(data_set$TutorialStartPart4)
# Variable binaria TRUE, FALSE

# TutorialStartPart5
summary(data_set$TutorialStartPart5)
str(data_set$TutorialStartPart5)
# Variable binaria TRUE, FALSE

# TutorialStartPart6
summary(data_set$TutorialStartPart6)
str(data_set$TutorialStartPart6)
# Variable binaria TRUE, FALSE

# TutorialFinish
summary(data_set$TutorialFinish)
str(data_set$TutorialFinish)
# Variable binaria TRUE, FALSE

m1 <- lm(Label ~ TutorialStart, data = data_set)          # Adjusted R-squared: 4.322e-05 
m2 <- lm(Label ~ TutorialStartPart1, data = data_set)     # Adjusted R-squared: 7.002e-05 
m3 <- lm(Label ~ TutorialStartPart2, data = data_set)     # Adjusted R-squared: 1.678e-06
m4 <- lm(Label ~ TutorialStartPart3, data = data_set)     # Adjusted R-squared: 5.081e-05
m5 <- lm(Label ~ TutorialStartPart4, data = data_set)     # Adjusted R-squared: 0.0001737
m6 <- lm(Label ~ TutorialStartPart5, data = data_set)     # Adjusted R-squared: 0.0003392 
m7 <- lm(Label ~ TutorialStartPart6, data = data_set)     # Adjusted R-squared: 0.0005756 
m8 <- lm(Label ~ TutorialFinish, data = data_set)         # Adjusted R-squared: 0.0006402 

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)
summary(m8)

rm(m1)
rm(m2)
rm(m3)
rm(m4)
rm(m5)
rm(m6)
rm(m7)
rm(m8)

# Tienen poca explicacion 
data_set[, TutorialStart := NULL]
data_set[, TutorialStartPart1 := NULL]
data_set[, TutorialStartPart2 := NULL]
data_set[, TutorialStartPart3 := NULL]

valid_set[, TutorialStart := NULL]
valid_set[, TutorialStartPart1 := NULL]
valid_set[, TutorialStartPart2 := NULL]
valid_set[, TutorialStartPart3 := NULL]

eval_set[, TutorialStart := NULL]
eval_set[, TutorialStartPart1 := NULL]
eval_set[, TutorialStartPart2 := NULL]
eval_set[, TutorialStartPart3 := NULL]


gc()
dim(data_set) #60
dim(valid_set)
dim(eval_set)

#--------------------------------------------------------
# Un poco de ingenieria de atributos

# Creamos la variable mejora

data_set[, mejora := (WinBattle_sum_dsi3 - WinBattle_sum_dsi0)]
data_set[, WinBattle_sum_dsi0 := NULL]
data_set[, WinBattle_sum_dsi3 := NULL]

valid_set[, mejora := (WinBattle_sum_dsi3 - WinBattle_sum_dsi0)]
valid_set[, WinBattle_sum_dsi0 := NULL]
valid_set[, WinBattle_sum_dsi3 := NULL]

eval_set[, mejora := (WinBattle_sum_dsi3 - WinBattle_sum_dsi0)]
eval_set[, WinBattle_sum_dsi0 := NULL]
eval_set[, WinBattle_sum_dsi3 := NULL]


# Creamos la variable gasto

data_set[, gasto := (soft_negative + hard_negative)]
valid_set[, gasto := (soft_negative + hard_negative)]
eval_set[, gasto := (soft_negative + hard_negative)]

data_set[, soft_negative := NULL]
data_set[, hard_negative := NULL]
valid_set[, soft_negative := NULL]
valid_set[, hard_negative := NULL]
eval_set[, soft_negative := NULL]
eval_set[, hard_negative := NULL]


dim(data_set) #58
dim(valid_set)
dim(eval_set)

head(data_set)

# Nombreamos "train_set" a este conjunto de datos porque es lo que vamos a usar para entrenar nuestro modelo
train_set <- data_set 
rm(data_set)
gc()

summary(train_set)
summary(valid_set)
summary(eval_set)

#Seleccionamos las variables coon las que vamos a estar haciendo las predicciones
#lo hacemos en base a una corrida previa donde identificamos las m·s importantes

VARS_TO_KEEP<- c("PiggyBankModifiedPoints_sum_dsi1_log","PiggyBankModifiedPoints_sum_dsi2_log","PiggyBankModifiedPoints_sum_dsi3_log", 
                  "EnterShop_sum_dsi3_log", "sum_WinBattle_log",
                   "LoseBattle_sum_dsi0_log","sum_UpgradeCard_log", "sum_StartSession_log",
                   "mejora", "id", "OpenChest_sum_dsi0_log","OpenChest_sum_dsi1_log","OpenChest_sum_dsi2_log","OpenChest_sum_dsi3_log", "platform",
                   "EnterDeck_sum_dsi0_log","EnterDeck_sum_dsi1_log","EnterDeck_sum_dsi2_log","EnterDeck_sum_dsi3_log", "LoseBattle_sum_dsi2_log", "install_date",
                   "LoseBattle_sum_dsi3_log", "LoseBattle_sum_dsi1_log", "EnterShop_sum_dsi2_log", "EnterShop_sum_dsi1_log","EnterShop_sum_dsi0_log",
                   "Label", "train_sample")


VARS_TO_KEEP_EVAL<- c("PiggyBankModifiedPoints_sum_dsi1_log","PiggyBankModifiedPoints_sum_dsi2_log","PiggyBankModifiedPoints_sum_dsi3_log", 
                 "EnterShop_sum_dsi3_log", "sum_WinBattle_log",
                 "LoseBattle_sum_dsi0_log","sum_UpgradeCard_log", "sum_StartSession_log",
                 "mejora", "id", "OpenChest_sum_dsi0_log","OpenChest_sum_dsi1_log","OpenChest_sum_dsi2_log","OpenChest_sum_dsi3_log", "platform",
                 "EnterDeck_sum_dsi0_log","EnterDeck_sum_dsi1_log","EnterDeck_sum_dsi2_log","EnterDeck_sum_dsi3_log", "LoseBattle_sum_dsi2_log", "install_date",
                 "LoseBattle_sum_dsi3_log", "LoseBattle_sum_dsi1_log", "EnterShop_sum_dsi2_log", "EnterShop_sum_dsi1_log","EnterShop_sum_dsi0_log",
                 "train_sample")

train_set <- train_set[, VARS_TO_KEEP, with = FALSE]
dim(train_set)

valid_set <- valid_set[, VARS_TO_KEEP, with = FALSE]

eval_set <- eval_set[, VARS_TO_KEEP_EVAL, with = FALSE]


#-------------------------------------------------------
#Una vez terminamos de analizar los datos y las variables, descartando algunas
#de ellas, usamos one hot encoding para preparar los datos para el modelo XGBoost

#Funcion para realizar One hot encoding 
one_hot_sparse <- function(data_set) {
  
  require(Matrix)
  
  created <- FALSE
  
  if (sum(sapply(data_set, is.numeric)) > 0) {  # Si hay, Pasamos los num√©ricos a una matriz esparsa (ser√?a raro que no estuviese, porque "Label"  es num√©rica y tiene que estar s√? o s√?)
    out_put_data <- as(as.matrix(data_set[,sapply(data_set, is.numeric), with = FALSE]), "dgCMatrix")
    created <- TRUE
  }
  
  if (sum(sapply(data_set, is.logical)) > 0) {  # Si hay, pasamos los l√≥gicos a esparsa y lo unimos con la matriz anterior
    if (created) {
      out_put_data <- cbind2(out_put_data,
                             as(as.matrix(data_set[,sapply(data_set, is.logical),
                                                   with = FALSE]), "dgCMatrix"))
    } else {
      out_put_data <- as(as.matrix(data_set[,sapply(data_set, is.logical), with = FALSE]), "dgCMatrix")
      created <- TRUE
    }
  }
  
  # Identificamos las columnas que son factor (OJO: el data.frame no deber√?a tener character)
  fact_variables <- names(which(sapply(data_set, is.factor)))
  
  # Para cada columna factor hago one hot encoding
  i <- 0
  
  for (f_var in fact_variables) {
    
    f_col_names <- levels(data_set[[f_var]])
    f_col_names <- gsub(" ", ".", paste(f_var, f_col_names, sep = "_"))
    j_values <- as.numeric(data_set[[f_var]])  # Se pone como valor de j, el valor del nivel del factor
    
    if (sum(is.na(j_values)) > 0) {  # En categ√≥ricas, trato a NA como una categor√?a m√°s
      j_values[is.na(j_values)] <- length(f_col_names) + 1
      f_col_names <- c(f_col_names, paste(f_var, "NA", sep = "_"))
    }
    
    if (i == 0) {
      fact_data <- sparseMatrix(i = c(1:nrow(data_set)), j = j_values,
                                x = rep(1, nrow(data_set)),
                                dims = c(nrow(data_set), length(f_col_names)))
      fact_data@Dimnames[[2]] <- f_col_names
    } else {
      fact_data_tmp <- sparseMatrix(i = c(1:nrow(data_set)), j = j_values,
                                    x = rep(1, nrow(data_set)),
                                    dims = c(nrow(data_set), length(f_col_names)))
      fact_data_tmp@Dimnames[[2]] <- f_col_names
      fact_data <- cbind(fact_data, fact_data_tmp)
    }
    
    i <- i + 1
  }
  
  if (length(fact_variables) > 0) {
    if (created) {
      out_put_data <- cbind(out_put_data, fact_data)
    } else {
      out_put_data <- fact_data
      created <- TRUE
    }
  }
  return(out_put_data)
}

#Hacemos one-hot-encoding sobre los conjuntos de train y test
#y dejamos los datos listos para correr el modelo
#train_set_aux<-train_set
#valid_set_aux<-valid_set
#eval_set_aux<-eval_set


#antes de hacer one-hot uno el modelo


train_set <- one_hot_sparse(train_set)
valid_set <- one_hot_sparse(valid_set)
eval_set <- one_hot_sparse(eval_set)




                                                     
                                                     