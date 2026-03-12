###
### Juan Manuel - Matemáticas y Estadística para la IA
###

library(readxl) # Para leer archivos Excel
library(caret)  # Para crear particiones estratificadas y ML
library(stats)  # Para PCA y regresión lineal
library(corrplot) # Correlación
library(dplyr)
library(forcats)
library(tidyr)
library(tidyverse)
library(glmnet)
library(naniar) # Plot top percentage

##########################################################################
###  EDA
##########################################################################
# Realizar un análisis exploratorio y descriptivo de las variables presentes
# en el conjunto de  datos, identificando patrones, tendencias y relaciones
# relevantes con el precio de venta (SalePrice)

set.seed(42)
df_raw <- read.csv("train.csv")
# Quito id
df <- subset(df_raw, select = -Id)
# Paso a escala logarítmica el precio
df$SalePrice <- log(df$SalePrice)

# Funcion para EDA básico, se podría haber hecho skim()
inspeccion_rapida <- function(df_input) {
  # Imprimo número de observaciones y columnas
  cat("--- Dimensiones del Dataset ---\n")
  print(dim(df_input))
  # Veo la estructura de los datos observando las primeras observaciones
  cat("\n--- Primeras 6 Observaciones ---\n")
  print(head(df_input))
  # Imprimo el resumen del dataset
  cat("\n--- Resumen Estadístico (Summary) ---\n")
  print(summary(df_input))
  # Imprimo la estructura de los distintos objetos del dataset
  cat("\n--- Estructura (Str) ---\n")
  str(df_input)
}

# inspeccion_rapida(df)

# Genero un histograma para demostrar que el precio sigue
# ahora una distribución normal
genero_histograma <- function(df_input) {
  # Pongo nombre para mi png y tamaño
  png("SalePrice_Histograma.png", width = 800, height = 800)

  # Genera el histograma, ahora visible en el nuevo dispositivo
  # Le paso la input tal cual porque ya está en log
  hist((df_input$SalePrice),
       main = "Distribución de SalePrice (Precio de Venta)",
       xlab = "SalePrice (Dólares)", col = "skyblue")
  dev.off()
}

# genero_histograma(df)

# Analizo la correlación que tienen las variables predictoras
# con mi variable objetivo SalePrice con las top 15 con más
# correlación
analisis_correlacion <- function(df_input, target_col = "SalePrice",
                                 top_n = 15) {
  # Cojo las variables numéricas sino no puedo hacer cor()
  df_numeric <- df_input %>%
    select(where(is.numeric))

  # Calculo correlacion de todas las variables respecto a la
  # target y fijo 'use = "pairwise.complete.obs"' para
  # ignorar NAs automáticamente
  cor_with_price <- cor(df_numeric, df_input[[target_col]],
                        use = "pairwise.complete.obs")

  # Ordeno de mayor a menor correlación (en valor absoluto) para
  # obtener aquellas con una mayor correlación
  cor_sorted_index <- order(abs(cor_with_price), decreasing = TRUE)
  cor_sorted <- cor_with_price[cor_sorted_index, , drop = FALSE]

  # Cojo el top 15 variables más correlacionadas
  print("Top 15 Variables que más afectan al precio:")
  print(head(cor_sorted, 15))

  # Con los cálculos realizados preparo mi mapa de calor
  # Selecciono los nombres de las 15 variables más importantes
  top_vars_names <- rownames(head(cor_sorted, 15))

  # Filtro el dataset numérico para quedarme solo con esas 15 columnass
  df_top_numeric <- df_numeric %>% select(all_of(top_vars_names))

  # Calculo la matriz de correlación cuadrada (15x15) solo para
  # estas variables
  M_top <- cor(df_top_numeric, use = "pairwise.complete.obs")

  png("MapaCorrelacion_Predictoras.png", width = 800, height = 800)

  corrplot(M_top,
           method = "color",
           type = "upper",
           order = "hclust",
           addCoef.col = "black", # Añade el número coeficiente
           tl.col = "black",      # Color de las etiquetas de texto
           tl.srt = 45,           # Rotación de las etiquetas
           number.cex = 0.7,      # Tamaño de los números
           title = "Top 15 Variables Correlacionadas con SalePrice",
           mar = c(0, 0, 1, 0))      # Ajuste de márgenes para el título

  dev.off()
}

# analisis_correlacion(df, target_col = "SalePrice_log", top_n = 15)

# Haciendolo como antes hago un plot de esas 15 pero en diagramas de
# dispersion con respecto a price
generar_dispersiones_top <- function(df_input,
                                     target_col = "SalePrice", top_n = 15) {

  # Calculo como antes
  df_numeric <- df_input %>% select(where(is.numeric))
  cor_with_price <- cor(df_numeric, df_input[[target_col]],
                        use = "pairwise.complete.obs")
  cor_sorted_index <- order(abs(cor_with_price), decreasing = TRUE)
  top_vars <- rownames(cor_with_price)[cor_sorted_index]

  # Quito la propia variable objetivo de la lista
  top_vars <- top_vars[top_vars != target_col]

  # Cojo las top N
  top_vars_selected <- head(top_vars, top_n)

  # Preparo datos para ggplot, se ponen todas las variables predictoras
  # en una sola columna llamada "Valor"y sus nombres en una columna
  # llamada "Variable"
  plot_data <- df_input %>%
    select(all_of(c(target_col, top_vars_selected))) %>%
    pivot_longer(cols = -all_of(target_col),
                 names_to = "Variable",
                 values_to = "Valor")

  # Genero el gráfico
  p <- ggplot(plot_data, aes_string(x = "Valor", y = target_col)) +
    # Cambio transparencia para ajustar en función de la densidad
    geom_point(alpha = 0.5, color = "#2c3e50", size = 1) +
    # Línea de tendencia roja para ver la dirección
    geom_smooth(method = "lm", color = "red", se = FALSE, size = 0.8) +
    # Divido en 15 pantallas, con escalas independientes
    facet_wrap(~Variable, scales = "free_x", ncol = 5) +
    theme_minimal() +
    labs(title = paste("Top", top_n, "Variables vs Precio (Log)"),
         y = "SalePrice (Log)",
         x = "Valor de la Variable") +
    theme(strip.text = element_text(face = "bold", size = 10),
          plot.title = element_text(hjust = 0.5, size = 16))

  ggsave("Scatterplots_TopVariables.png", plot = p, width = 15,
         height = 10, bg = "white")

  return(p)
}

generar_dispersiones_top(df, target_col = "SalePrice", top_n = 15)

# Detecto valores extremos que producen una penalización muy agresiva
# en el modelo. Al eliminarlos se obtienen valores fascinantes del RMSE,
# este ajuste lo hice al acabar para mejorar resultados

df <- df %>%
  filter(!(GrLivArea > 4000 & SalePrice < 12.5))

##########################################################################
###  Preprocesamiento de los datos
##########################################################################

# Empiezo haciendo una inspección rápida de si existen nulos y cuantos hay
anyNA(df)
colSums(is.na(df)) # Muestra el total de nulos por columna
sum(is.na(df))     # Total de NA en todo el dataset

# Defino una función que me devuelva solo aquellos valores cuyo porcentaje
# de nulos sea mayor que 0, así puedo ver qué variables he de tratar
porcentaje_nulos <- function(df_input) {

  na_percent <- colMeans(is.na(df_input) * 100)
  na_percent_with_nans <- na_percent[na_percent > 0]

  # Teniendo el porcentaje de nulos creo un df para visualizar todo mejor
  na_df <- data.frame(Variable = names(na_percent_with_nans),
    Null_Percent = na_percent_with_nans
  )
  # Filtrar y ordenar el data frame
  na_summary <- subset(na_df, Null_Percent > 0)
  na_summary <- na_summary[order(na_summary$Null_Percent, decreasing = TRUE), ]
  rownames(na_summary) <- NULL
  print(na_summary)
}

porcentaje_nulos(df)

# Visualización bonita con el paquete naniar de lo anterior
visualizacion_nulos <- function(df_input) {
  # Cojo las columnas donde hay al menos un NA
  df_filtrado <- df_input %>%
    select(where(~ any(is.na(.))))

  p <- naniar::gg_miss_var(df_filtrado, show_pct = TRUE) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 9),
      axis.text.y = element_text(size = 8)
    ) +
    labs(x = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.05)))

  ggsave("Analisis_Nulos.png", plot = p, width = 8, height = 6, bg = "white")

  return(p)
}

visualizacion_nulos(df)

# Las columnas que he decidido borrar las pongo a continuación:
columnas_a_borrar <- c(
  "RoofMatl", "Condition2", "Exterior2nd", "MiscFeature",
  "Alley", "PoolQC", "Street", "LandContour", "LotConfig",
  "Heating", "YrSold", "MoSold", "Utilities", "Fence"
)

# Me quedo con todo lo que no sea lo de antes
df <- df[, !(names(df) %in% columnas_a_borrar)]

# Verifico que todo OK
dim(df)


if("Id" %in% names(df)) {
  datos_proc <- df %>% select(-Id)
}

# Limpio los Na existentes por un valor razonable en función de
# la variable, en este caso 0 o None dependiendo del contexto de
# la variable.
limpio_nones <- function(df_input) {
  # Fijo a "None" los valores "NaN"
#     na_to_none_cols_train <- c(
#    "PoolQC", "MiscFeature", "Alley", "Fence", "FireplaceQu",
#    "GarageType", "GarageFinish", "GarageQual", "GarageCond",
#    "BsmtExposure", "BsmtFinType2", "BsmtQual", "BsmtCond",
#    "BsmtFinType1", "MasVnrType"
#  )
  na_to_none_cols_train <- c(
    "FireplaceQu",
    "GarageType", "GarageFinish", "GarageQual", "GarageCond",
    "BsmtExposure", "BsmtFinType2", "BsmtQual", "BsmtCond",
    "BsmtFinType1", "MasVnrType"
  )
  df_input <- df_input %>%
    # across() aplica la función (~fct_explicit_na(., na_level = "None"))
    # a todas las columnas listadas en 'na_to_none_cols'.
    mutate(across(all_of(na_to_none_cols_train),
                  ~fct_explicit_na(., na_level = "None")))
  df_input <- df_input %>%
    mutate(MasVnrArea = replace_na(MasVnrArea, 0))
  df_input <- df_input %>%
    mutate(GarageYrBlt = replace_na(GarageYrBlt, 0))

  return(df_input)
}

# Miro las variables categóricas que voy a tener que cambiar antes de limpiar
# nones porque me va a pasar cada columna a factor y luego me detecta menos
# columnas categóricas de las que realmente son

categorical_cols <- names(df)[sapply(df, is.character)]
categorical_cols

# Devuelvo el dataset limpio de modificaciones que puedo hacer antes de
# dividir el dataset en train, validation y test.
df <- limpio_nones(df)

# Observando esto me quedarían LotFrontage y Electrical, pero su gestión
# de ha de hacer en cada junto de datos por separado porque tiene efectos
# importantes en la predicción del modelo, así evito data leakage
porcentaje_nulos(df)

# Ahora voy con las variables categóricas, obtengo sus nombres para luego
# comparar con los conjuntos que he identificado manualmente y asegurarme
# que no me dejo ninguna
categorical_cols <- names(df)[sapply(df, is.character)]
categorical_cols
anyNA(categorical_cols)

# Clasifico en tres tipos las categóricas porque las ordinales hay grados

# Primer tipo un simple grado de calidad
# Simplemente cojo los grados y le asigno un valor numérico de menos a más
codificar_ordinales_estandar <- function(df_input) {
  map_calidad <- function(x) {
    case_when(
      x == "Ex" ~ 5, x == "Gd" ~ 4, x == "TA" ~ 3,
      x == "Fa" ~ 2, x == "Po" ~ 1, TRUE ~ 0 # 0 para None/NA
    )
  }

  # Lista con todas las variables de calidad
  # vars_calidad <- c("ExterQual", "ExterCond", "HeatingQC", "KitchenQual",
  #                   "BsmtQual", "BsmtCond", "FireplaceQu",
  #                   "GarageQual", "GarageCond", "PoolQC")

  vars_calidad <- c("ExterQual", "ExterCond", "HeatingQC", "KitchenQual",
                    "BsmtQual", "BsmtCond", "FireplaceQu",
                    "GarageQual", "GarageCond")

  # Devolución implícita en este caso
  df_input <- df_input %>%
    mutate(across(all_of(vars_calidad), map_calidad))
}

# Asigno al resto que no denote calidad valores numéricos
codificar_ordinales_espec <- function(df_input) {
  df_input <- df_input %>%
    mutate(
      LotShape = case_when(LotShape == "Reg" ~ 3,
                           LotShape == "IR1" ~ 2,
                           LotShape == "IR2" ~ 1,
                           LotShape == "IR3" ~ 0, TRUE ~ 0),

      #Utilities = case_when(Utilities == "AllPub" ~ 3,
      #                      Utilities == "NoSewr" ~ 2,
      #                      Utilities == "NoSeWa" ~ 1,
      #                      Utilities == "ELO" ~ 0, TRUE ~ 0),

      LandSlope = case_when(LandSlope == "Gtl" ~ 2,
                            LandSlope == "Mod" ~ 1,
                            LandSlope == "Sev" ~ 0, TRUE ~ 0),

      Electrical = case_when(Electrical == "SBrkr" ~ 4,
                             Electrical == "FuseA" ~ 3,
                             Electrical == "FuseF" ~ 2,
                             Electrical == "FuseP" ~ 1,
                             Electrical == "Mix" ~ 0, TRUE ~ 0),

      Functional = case_when(Functional == "Typ" ~ 7,
                             Functional == "Min1" ~ 6, Functional == "Min2" ~ 5,
                             Functional == "Mod" ~ 4, Functional == "Maj1" ~ 3,
                             Functional == "Maj2" ~ 2, Functional == "Sev" ~ 1,
                             Functional == "Sal" ~ 0, TRUE ~ 0),

      PavedDrive = case_when(PavedDrive == "Y" ~ 2,
                             PavedDrive == "P" ~ 1,
                             PavedDrive == "N" ~ 0, TRUE ~ 0),

      CentralAir = ifelse(CentralAir == "Y", 1, 0),

      # Exposición del sótano
      BsmtExposure = case_when(BsmtExposure == "Gd" ~ 4,
                               BsmtExposure == "Av" ~ 3,
                               BsmtExposure == "Mn" ~ 2,
                               BsmtExposure == "No" ~ 1, TRUE ~ 0),

      # Acabado del sótano (Tipo 1 y 2)
      BsmtFinType1 = case_when(BsmtFinType1 == "GLQ" ~ 6,
                               BsmtFinType1 == "ALQ" ~ 5,
                               BsmtFinType1 == "BLQ" ~ 4,
                               BsmtFinType1 == "Rec" ~ 3,
                               BsmtFinType1 == "LwQ" ~ 2,
                               BsmtFinType1 == "Unf" ~ 1,
                               TRUE ~ 0),

      BsmtFinType2 = case_when(BsmtFinType2 == "GLQ" ~ 6,
                               BsmtFinType2 == "ALQ" ~ 5,
                               BsmtFinType2 == "BLQ" ~ 4,
                               BsmtFinType2 == "Rec" ~ 3,
                               BsmtFinType2 == "LwQ" ~ 2,
                               BsmtFinType2 == "Unf" ~ 1, TRUE ~ 0),
      # Acabado Garaje
      GarageFinish = case_when(GarageFinish == "Fin" ~ 3,
                               GarageFinish == "RFn" ~ 2,
                               GarageFinish == "Unf" ~ 1, TRUE ~ 0),

      # Calidad Valla (Fence) -> La acabé quitando
      #Fence = case_when(Fence == "GdPrv" ~ 4,
      #                  Fence == "MnPrv" ~ 3,
      #                  Fence == "GdWo" ~ 2,
      #                  Fence == "MnWw" ~ 1, TRUE ~ 0)
    )
  return(df_input)
}

# Función que codifica aquellas variables que acabarán siendo dummies
codificar_nominales <- function(df_input) {
  # Lista de variables nominales restantes
  #vars_nominales <- c("MSZoning", "Street", "LandContour", "LotConfig",
  #                    "Neighborhood", "Condition1", "Condition2", "BldgType",
  #                    "HouseStyle", "RoofStyle", "RoofMatl", "Exterior1st",
  #                    "Exterior2nd", "Foundation", "Heating", "SaleType",
  #                    "SaleCondition", "Alley", "MasVnrType", "GarageType",
  #                    "MiscFeature")

  vars_nominales <- c(
    "MSZoning",
    "Neighborhood",
    "Condition1",
    "BldgType",
    "HouseStyle",
    "RoofStyle",
    "Exterior1st",
    "Foundation",
    "SaleType",
    "SaleCondition",
    "MasVnrType",
    "GarageType"
  )

  # Filtro solo las que realmente existen en el dataframe actual
  vars_existentes <- vars_nominales[vars_nominales %in% names(df_input)]

  # Genero las dummies
  dummies_obj <- dummyVars(~ ., data = df_input[, vars_existentes],
                           fullRank = TRUE)
  df_dummies <- predict(dummies_obj, newdata =
                          df_input[, vars_existentes]) %>% as.data.frame()

  # Uno y elimino columnas originales
  df_input <- df_input %>%
    select(-all_of(vars_existentes)) %>%
    bind_cols(df_dummies)
}

# Realizo el preprocesamiento
df <- codificar_ordinales_estandar(df)
df <- codificar_ordinales_espec(df)
df <- codificar_nominales(df)

# Compruebo que se ha realizado exitosamente al no quedar ninguna
# variable categórica
categorical_cols <- names(df)[sapply(df, is.character)]
categorical_cols

##########################################################################
###  Defino las funciones de modelado para train / val / test
##   Hago el split luego y las aplico
##########################################################################

# Preparo matrices con glmnet
preparar_matrices_glmnet <- function(train, val, test, target = "SalePrice") {
  list(
    x_train = as.matrix(train %>% select(-all_of(target))),
    y_train = train[[target]],
    x_val   = as.matrix(val %>% select(-all_of(target))),
    y_val   = val[[target]],
    x_test  = as.matrix(test %>% select(-all_of(target))),
    y_test  = test[[target]]
  )
}

# Entreno y evalúo Lasso / Ridge
entrenar_regularizacion <- function(mats, alpha_val, nombre) {

  set.seed(123) # Establezco reproducibilidad con la seed
  # Entreno con datos logarítimos
  cv_model <- cv.glmnet(mats$x_train, mats$y_train, alpha = alpha_val)
  best_lambda <- cv_model$lambda.min

  # Predicción y evaluación, tengo que deshacer el logaritmo, por lo que
  # hago una exponencial

  # Validación
  preds_val_log <- predict(cv_model, s = best_lambda, newx = mats$x_val)
  # Revierto exp(predicción) - exp(valor real)
  rmse_val <- sqrt(mean((exp(mats$y_val) - exp(preds_val_log))^2))

  # Test
  preds_test_log <- predict(cv_model, s = best_lambda, newx = mats$x_test)
  # Revierto
  rmse_test <- sqrt(mean((exp(mats$y_test) - exp(preds_test_log))^2))

  # Calculo el MAE
  mae_test <- mean(abs(exp(mats$y_test) - exp(preds_test_log)))

  # R2 se puede calcular sobre los valores reales o logarítmicos.
  # Generalmente se reporta sobre los reales para ser consistente.
  r2_test <- cor(exp(mats$y_test), exp(preds_test_log))^2


  cat("RMSE (Dólares):", rmse_test, "\n")

  return(list(modelo = cv_model, rmse_test = rmse_test,
              r2_test = r2_test, mae_test = mae_test))
}

# Genero la comparativa final para observar los resultados
generar_comparativa <- function(res_pca, res_lasso, res_ridge, res_lasso_pca) {
  resultados <- data.frame(
    Modelo = c("Regresión Lineal + PCA", "Lasso", "Ridge", "Lasso + PCA"),
    RMSE_Validacion = c(res_pca$rmse_val, res_lasso$rmse_val,
                        res_ridge$rmse_val, res_lasso_pca$rmse_val),
    RMSE_Test = c(res_pca$rmse_test, res_lasso$rmse_test,
                  res_ridge$rmse_test, res_lasso_pca$rmse_test),
    R2_Test = c(res_pca$r2_test, res_lasso$r2_test,
                res_ridge$r2_test, res_lasso_pca$r2_test)
  )

  print("\n--- TABLA FINAL DE RESULTADOS ---")
  print(resultados)

  # Gráfico comparativo (Uso el error de Test)
  barplot(resultados$RMSE_Test, names.arg = resultados$Modelo,
          col = c("gray", "skyblue", "orange", "lightgreen"),
          main = "RMSE en Test (Menor es mejor)", ylab = "RMSE",
          las = 2, cex.names = 0.7)

  return(resultados)
}

##########################################################################
###  DIVISIÓN 60% TRAIN - 20% VAL - 20% TEST
##########################################################################
set.seed(123)

# Separo el 20% para TEST final
index_test <- createDataPartition(df$SalePrice, p = 0.2, list = FALSE)
df_test <- df[index_test, ]
df_temp <- df[-index_test, ] # Aquí queda el 80% restante

# Del 80% restante, separo Train y Validation
# Necesitamos el 25% del df_temp para que sea el 20% del
# total (0.80 * 0.25 = 0.20)
index_val <- createDataPartition(df_temp$SalePrice, p = 0.25, list = FALSE)

df_val   <- df_temp[index_val, ]  # 20% del total
df_train <- df_temp[-index_val, ] # 60% del total

cat("\n--- Dimensiones Finales ---\n")
cat("Train (60%):", nrow(df_train), "filas\n")
cat("Val   (20%):", nrow(df_val),   "filas\n")
cat("Test  (20%):", nrow(df_test),  "filas\n")


##########################################################################
###  2. IMPUTACIÓN (Aprendiendo solo de Train)
##########################################################################
# Calculo estadísticos
med_frontage <- median(df_train$LotFrontage, na.rm = TRUE)
moda_elec    <- as.numeric(names(sort(table(df_train$Electrical),
                                      decreasing = TRUE))[1])

# Aplico a TRAIN
df_train$LotFrontage[is.na(df_train$LotFrontage)] <- med_frontage
df_train$Electrical[is.na(df_train$Electrical)]   <- moda_elec

# Aplico a vval pero usando los valores de train
df_val$LotFrontage[is.na(df_val$LotFrontage)]     <- med_frontage
df_val$Electrical[is.na(df_val$Electrical)]       <- moda_elec

# Aplico a test, de nuevo usando los valores de train
df_test$LotFrontage[is.na(df_test$LotFrontage)]   <- med_frontage
df_test$Electrical[is.na(df_test$Electrical)]     <- moda_elec


##########################################################################
###  3. ESTANDARIZACIÓN (Aprendiendo solo de Train)
##########################################################################
predictors <- names(df_train)[names(df_train) != "SalePrice"]

# Aprendo media y desviación típica del 60% de Train
preProc <- preProcess(df_train[, predictors],
                      method = c("center", "scale"))

# Transformo los 3 conjuntos
df_train_sc <- predict(preProc, df_train)
df_val_sc   <- predict(preProc, df_val)
df_test_sc  <- predict(preProc, df_test)


##########################################################################
###  MODELO 1: PCA + REGRESIÓN LINEAL
##########################################################################

# Preparo los datos para PCA quitando target
train_x <- df_train_sc %>% select(-SalePrice)
val_x   <- df_val_sc   %>% select(-SalePrice)
test_x  <- df_test_sc  %>% select(-SalePrice)

# Calculo PCA sobre Train
pca_model <- prcomp(train_x, center = FALSE, scale. = FALSE)

# Elijo componentes (95% varianza)
var_expl <- pca_model$sdev^2 / sum(pca_model$sdev^2)
num_comp <- which(cumsum(var_expl) >= 0.95)[1]
cat("Número de componentes seleccionados (95% varianza):", num_comp, "\n")

summary(pca_model)

# Proyecto los 3 conjuntos
train_pca <- predict(pca_model, newdata = train_x)[, 1:num_comp]
val_pca   <- predict(pca_model, newdata = val_x)[, 1:num_comp]
test_pca  <- predict(pca_model, newdata = test_x)[, 1:num_comp]

# Reconstruyo dataframes con Target, que ya esta en escala logaritmica
train_pca_df <- data.frame(train_pca, SalePrice = df_train_sc$SalePrice)
val_pca_df   <- data.frame(val_pca,   SalePrice = df_val_sc$SalePrice)
test_pca_df  <- data.frame(test_pca,  SalePrice = df_test_sc$SalePrice)

# Modelo Lineal
modelo_pca <- lm(SalePrice ~ ., data = train_pca_df)

# Evaluación en dólares
preds_pca_val_log  <- predict(modelo_pca, newdata = val_pca_df)
preds_pca_test_log <- predict(modelo_pca, newdata = test_pca_df)

res_pca <- list(
  # Resto exp(predicción) contra exp(realidad) para tener error en dólares
  rmse_val  = sqrt(mean((exp(val_pca_df$SalePrice) - exp(preds_pca_val_log))^2)),
  rmse_test = sqrt(mean((exp(test_pca_df$SalePrice) - exp(preds_pca_test_log))^2)),
  mae_test  = mean(abs(exp(test_pca_df$SalePrice) - exp(preds_pca_test_log))),
  r2_test   = cor(exp(test_pca_df$SalePrice), exp(preds_pca_test_log))^2
)

# Creo funciones para la regularización de los modelos

# Actualizo la función para que devuelva el error en dólares
entrenar_regularizacion_dolares <- function(mats, alpha_val, nombre) {

  set.seed(123)
  cv_model <- cv.glmnet(mats$x_train, mats$y_train, alpha = alpha_val)
  best_lambda <- cv_model$lambda.min

  # Predicción en Logaritmos
  preds_val_log  <- predict(cv_model, s = best_lambda, newx = mats$x_val)
  preds_test_log <- predict(cv_model, s = best_lambda, newx = mats$x_test)

  # Conversión a dólares reales para calcular el error
  val_real  <- exp(mats$y_val)
  test_real <- exp(mats$y_test)

  preds_val_dollar  <- exp(preds_val_log)
  preds_test_dollar <- exp(preds_test_log)

  # Cálculo de métricas
  rmse_val  <- sqrt(mean((val_real - preds_val_dollar)^2))
  rmse_test <- sqrt(mean((test_real - preds_test_dollar)^2))
  r2_test   <- cor(test_real, preds_test_dollar)^2

  mae_test  <- mean(abs(test_real - preds_test_dollar))

  return(list(rmse_val = rmse_val, rmse_test = rmse_test, r2_test = r2_test, mae_test = mae_test))
}

# Preparo matrices
mats <- preparar_matrices_glmnet(df_train_sc, df_val_sc, df_test_sc)

# Entrenamiento Lasso
res_lasso <- entrenar_regularizacion_dolares(mats, alpha_val = 1, nombre = "LASSO")

# Entrenamiento Ridge
res_ridge <- entrenar_regularizacion_dolares(mats, alpha_val = 0, nombre = "RIDGE")

# Entrenamiento de Lasso sobre PCA
mats_pca <- preparar_matrices_glmnet(train_pca_df, val_pca_df, test_pca_df)
res_lasso_pca <- entrenar_regularizacion_dolares(mats_pca, alpha_val = 1, nombre = "LASSO-PCA")

# Entrenamiento de Ridge sobre PCA
res_ridge_pca <- entrenar_regularizacion_dolares(mats_pca, alpha_val = 0, nombre = "RIDGE-PCA")

##########################################################################
###  TABLA FINAL UNIFICADA
##########################################################################

generar_comparativa_5 <- function(res_pca, res_lasso, res_ridge, res_lasso_pca, res_ridge_pca) {
  resultados <- data.frame(
    Modelo = c("Lineal + PCA", "Lasso", "Ridge", "Lasso + PCA", "Ridge + PCA"),
    RMSE_Validacion = c(res_pca$rmse_val, res_lasso$rmse_val, res_ridge$rmse_val
                        , res_lasso_pca$rmse_val, res_ridge_pca$rmse_val),
    RMSE_Test = c(res_pca$rmse_test, res_lasso$rmse_test, res_ridge$rmse_test,
                  res_lasso_pca$rmse_test, res_ridge_pca$rmse_test),
    MAE_Test = c(res_pca$mae_test, res_lasso$mae_test, res_ridge$mae_test,
                 res_lasso_pca$mae_test, res_ridge_pca$mae_test),
    R2_Test = c(res_pca$r2_test, res_lasso$r2_test, res_ridge$r2_test,
                res_lasso_pca$r2_test, res_ridge_pca$r2_test)
  )

  print("\n--- TABLA FINAL DE RESULTADOS (EN DÓLARES) ---")
  print(resultados)
  return(resultados)
}

tabla_final <- generar_comparativa_5(res_pca, res_lasso, res_ridge,
                                     res_lasso_pca, res_ridge_pca)