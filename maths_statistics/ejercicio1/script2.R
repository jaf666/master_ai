##########################################################
#### División del conjunto de datos
##########################################################

y = df_raw$SalePrice
trainIndex <- createDataPartition(y = y, p = 0.7, list = FALSE)
train_data <- df_raw[trainIndex, ] # El 70% para entrenar
test_data <- df_raw[-trainIndex, ] # El 30% para test

# Ahora realizo el preprocesado de datos, así me
# aseguro de que no habrá trampas en el training

# Calculo las medianas para utilizarlas luego
medianas_por_vecindario <- train_data %>%
  group_by(Neighborhood) %>%
  summarise(Median_Frontage = median(LotFrontage, na.rm=TRUE))

get_mode <- function(v) {
  uniqv <- unique(v)
  # Excluimos los NA para el cálculo
  uniqv <- na.omit(uniqv) 
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculo la moda de Electrical
moda <- get_mode(train_data$Electrical)

#####################
# 1. Cambios en train
#####################

# Fijo a "None" los valores "NaN"
na_to_none_cols_train <- c(
  "PoolQC", "MiscFeature", "Alley", "Fence", "FireplaceQu", 
  "GarageType", "GarageFinish", "GarageQual", "GarageCond", 
  "BsmtExposure", "BsmtFinType2", "BsmtQual", "BsmtCond", 
  "BsmtFinType1", "MasVnrType"
)
train_data <- train_data %>%
  # across() aplica la función (~fct_explicit_na(., na_level = "None")) 
  # a todas las columnas listadas en 'na_to_none_cols'.
  mutate(across(all_of(na_to_none_cols_train),
                ~fct_explicit_na(., na_level = "None")))

train_data <- train_data %>%
  mutate(MasVnrArea = replace_na(MasVnrArea, 0))

train_data$Electrical[is.na(train_data$Electrical)] <- moda
# Estimo los valores faltantes para los metros lineales de calle
# de una vivienda a partir de la mediana de esta variable en cada vecindario.
# Se podría hacer de una forma más sencilla utilizando la mediana global,
# pero es más preciso utilizar la de cada vecindario.

train_data <- train_data %>%
  # 2. Unir la tabla de medianas con el conjunto de entrenamiento
  left_join(medianas_por_vecindario, by = "Neighborhood") %>%
  # 3. Imputar los NAs usando la mediana calculada para ese vecindario
  mutate(LotFrontage = ifelse(is.na(LotFrontage),
                              Median_Frontage, LotFrontage)) %>%
  # 4. Eliminar la columna auxiliar de la mediana
  select(-Median_Frontage)

# Año del garaje
train_data <- train_data %>%
  mutate(GarageYrBlt = replace_na(GarageYrBlt, 0))
#####################
# 1. Cambios en test
#####################

# Definimos las columnas categóricas donde NA significa "None"
# (Ausencia de característica)
na_to_none_cols <- c(
  "PoolQC", "MiscFeature", "Alley", "Fence", "FireplaceQu",
  "GarageType", "GarageFinish", "GarageQual", "GarageCond",
  "BsmtExposure", "BsmtFinType2", "BsmtQual", "BsmtCond",
  "BsmtFinType1", "MasVnrType"
)

test_data <- test_data %>%
  # across() aplica la función (~fct_explicit_na(., na_level = "None"))
  # a todas las columnas listadas en 'na_to_none_cols'
  mutate(across(all_of(na_to_none_cols),
                ~fct_explicit_na(., na_level = "None")))

test_data <- test_data %>%
  mutate(MasVnrArea = replace_na(MasVnrArea, 0))
test_data <- test_data %>%
  mutate(GarageYrBlt = replace_na(GarageYrBlt, 0))

test_data <- test_data %>%
  # 2. Unir la tabla de medianas (calculada en train) con el conjunto de prueba
  left_join(medianas_por_vecindario, by = "Neighborhood") %>%
  # 3. Imputar los NAs usando la mediana calculada para ese vecindario
  mutate(LotFrontage = ifelse(is.na(LotFrontage),
                              Median_Frontage, LotFrontage)) %>%
  # 4. Eliminar la columna auxiliar de la mediana
  select(-Median_Frontage)

test_data$Electrical[is.na(test_data$Electrical)] <- moda

############# Comprobar nulos

# En train

na_percent <- colSums(is.na(train_data) / nrow(train_data) * 100)
na_percent_with_nans <- na_percent[na_percent > 0]

# Teniendo el porcentaje de nulos creo un df para visualizar todo mejor
na_train_data <- data.frame(Variable = names(na_percent_with_nans),
  Null_Percent = na_percent_with_nans
)
# Filtrar y ordenar el data frame
na_summary <- subset(na_train_data, Null_Percent > 0)
na_summary <- na_summary[order(na_summary$Null_Percent, decreasing = TRUE), ]
rownames(na_summary) <- NULL
print(na_summary)

# En test

na_percent <- colSums(is.na(test_data) / nrow(test_data) * 100)
na_percent_with_nans <- na_percent[na_percent > 0]

# Teniendo el porcentaje de nulos creo un df para visualizar todo mejor
na_test_data <- data.frame(Variable = names(na_percent_with_nans),
  Null_Percent = na_percent_with_nans
)
# Filtrar y ordenar el data frame
na_summary <- subset(na_test_data, Null_Percent > 0)
na_summary <- na_summary[order(na_summary$Null_Percent, decreasing = TRUE), ]
rownames(na_summary) <- NULL
print(na_summary)

# Codificación ordinal a aquellas variables ordinales de calidad/condición

# Aplicar one-hot encoding para pasar las variables categóricas nominales a
# binarias (dummy)

# Identifico las variables categóricas en el dataset
categorical_cols <- names(df)[sapply(df, is.character)]
categorical_cols
# Estandarización antes de usar PCA, Lasso y Ridge


# Modelado y evaluación

# Ejecutar el PCA

# Contruir 4 modelos de regresión
# - Regresión múltiple sobre PCA
# - Regresión Lasso
# - Regresión Ridge
# - Regresión regularizada sobre PCA

# Evaluación cruzada ajustando hiperparámetros de regularización

# Evaluación comparando modelos usando metricas como RSME y MAE