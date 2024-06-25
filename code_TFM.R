
#CHAPTER 2

#Read data: 
data=read.csv('Fifa 23 Players Data.csv')
#Examples positions.played and best.position: 
data[2:5,c(2,6)]
data[2:5,c(2,7)]
#See the positions considered in the dataset 
positions_list <- strsplit(data$Positions.Played, ",")
unique_positions <- unique(unlist(positions_list))
unique_positions
#Eliminate goalkeepers: 
data<- data[data$Best.Position != 'GK', ]
#Check no duplicated data 
nombres=table(data$name)
repeated <- names(nombres[nombres > 1])#names that are repeated
repeated
#Eliminate undesired variables: 
data=data[,-c(1,2,3:5,8,9,13:22,26:30,68:89)]#7 is best position, we keep it for EDA and then delete it 
#Dataset with horizontal positions: 
data2=data
data2$Position_horizontal <- ifelse(data$Best.Position %in% c("ST", "CF", "CAM", "CM", "CDM", "CB"), "center",
                                    ifelse(data$Best.Position %in% c("LW", "LM", "LWB", "LB"), "left",
                                           ifelse(data$Best.Position %in% c("RW", "RM", "RWB", "RB"), "right", NA)))
#Vertical positions
data2$Position_vertical <- ifelse(data2$Best.Position %in% c("ST", "CF", "LW", "RW"), "offensive",
                                  ifelse(data2$Best.Position %in% c("CB", "LB", "RB", "LWB", "RWB"), "defensive",
                                         ifelse(data2$Best.Position %in% c("CM", "CDM", "CAM", "LM", "RM"), "midfield", NA)))
#Proportions of each group
table(data2$Position_horizontal)
table(data2$Position_vertical)
prop.table(table(data2$Position_horizontal))
prop.table(table(data2$Position_vertical))
#See density function of all variables regarding horizontal positions
library(caret)
featurePlot(x = data2[, -c(1,2,6:10,46,47)],
            y = factor(data2$Position_horizontal),
            plot = "density",
            strip=strip.custom(par.strip.text=list(cex=0.7)),
            scales = list(x = list(relation="free"),
                          y = list(relation="free")))
#See density function of all variables regarding vertical  positions
featurePlot(x = data2[, -c(1,2,6:10,46,47)],
            y = factor(data2$Position_vertical),
            plot = "density",
            strip=strip.custom(par.strip.text=list(cex=0.7)),
            scales = list(x = list(relation="free"),
                          y = list(relation="free")))
#Plot marking: 
library(caret)
library(gridExtra)

# Nombre de la variable a mostrar en el eje x
var_name <- names(data2)[43]

# Crear los gráficos con ajuste en las leyendas y nombres de las variables en el eje x
plot1 <- featurePlot(x = data2[, c(43)],
                     y = factor(data2$Position_horizontal),
                     plot = "density",
                     strip = strip.custom(par.strip.text = list(cex = 0.7)),
                     scales = list(x = list(relation = "free"),
                                   y = list(relation = "free")),
                     auto.key = list(columns = 1, lines = TRUE, space = "right"))

plot2 <- featurePlot(x = data2[, c(43)],
                     y = factor(data2$Position_vertical),
                     plot = "density",
                     strip = strip.custom(par.strip.text = list(cex = 0.7)),
                     scales = list(x = list(relation = "free"),
                                   y = list(relation = "free")),
                     auto.key = list(columns = 1, lines = TRUE, space = "right"))

# Actualizar las etiquetas del eje x
plot1 <- update(plot1, xlab = var_name)
plot2 <- update(plot2, xlab = var_name)

# Mostrar los gráficos juntos con layout ajustado
grid.arrange(plot1, plot2, ncol = 2)
#Plot Preferred foot 
library(ggplot2)
library(gridExtra)
library(dplyr)

# Calcular proporciones para Position_horizontal
data2_horizontal <- data2 %>%
  group_by(Preferred.Foot, Position_horizontal) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

# Crear el gráfico de barras con etiquetas de proporción para Position_horizontal
plot1 <- ggplot(data = data2_horizontal, aes(x = Preferred.Foot, y = proportion, fill = Position_horizontal)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)), 
            position = position_fill(vjust = 0.5), size = 3) +
  labs(title = "Preferred Foot by Position Horizontal", y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# Calcular proporciones para Position_vertical
data2_vertical <- data2 %>%
  group_by(Preferred.Foot, Position_vertical) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

# Crear el gráfico de barras con etiquetas de proporción para Position_vertical
plot2 <- ggplot(data = data2_vertical, aes(x = Preferred.Foot, y = proportion, fill = Position_vertical)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)), 
            position = position_fill(vjust = 0.5), size = 3) +
  labs(title = "Preferred Foot by Position Vertical", y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# Mostrar los gráficos juntos con layout ajustado
grid.arrange(plot1, plot2, ncol = 2)
#Plot strenght and age 
# Cargar los paquetes necesarios
library(caret)
library(gridExtra)

# Nombre de la variable a mostrar en el eje x
var_name <- names(data2)[35]

# Crear los gráficos con ajuste en las leyendas y nombres de las variables en el eje x
plot1 <- featurePlot(x = data2[, c(35)],
                     y = factor(data2$Position_horizontal),
                     plot = "box",
                     strip = strip.custom(par.strip.text = list(cex = 0.7)),
                     scales = list(x = list(relation = "free"),
                                   y = list(relation = "free")),
                     auto.key = list(columns = 1, lines = TRUE, space = "right"))

plot2 <- featurePlot(x = data2[, c(35)],
                     y = factor(data2$Position_vertical),
                     plot = "box",
                     strip = strip.custom(par.strip.text = list(cex = 0.7)),
                     scales = list(x = list(relation = "free"),
                                   y = list(relation = "free")),
                     auto.key = list(columns = 1, lines = TRUE, space = "right"))

# Actualizar las etiquetas del eje x
plot1 <- update(plot1, xlab = var_name)
plot2 <- update(plot2, xlab = var_name)

# Mostrar los gráficos juntos con layout ajustado




# Nombre de la variable a mostrar en el eje x
var_name <- names(data2)[3]

# Crear los gráficos con ajuste en las leyendas y nombres de las variables en el eje x
plot3 <- featurePlot(x = data2[, c(3)],
                     y = factor(data2$Position_horizontal),
                     plot = "density",
                     strip = strip.custom(par.strip.text = list(cex = 0.7)),
                     scales = list(x = list(relation = "free"),
                                   y = list(relation = "free")),
                     auto.key = list(columns = 1, lines = TRUE, space = "right"))

plot4 <- featurePlot(x = data2[, c(3)],
                     y = factor(data2$Position_vertical),
                     plot = "density",
                     strip = strip.custom(par.strip.text = list(cex = 0.7)),
                     scales = list(x = list(relation = "free"),
                                   y = list(relation = "free")),
                     auto.key = list(columns = 1, lines = TRUE, space = "right"))

# Actualizar las etiquetas del eje x
plot3 <- update(plot3, xlab = var_name)
plot4 <- update(plot4, xlab = var_name)

# Mostrar los gráficos juntos con layout ajustado
grid.arrange(plot1, plot2,plot3,plot4,nrow=2, ncol = 2)
#Number of labels detected:
categorias_distintas <- unique(data$Positions.Played)
length(categorias_distintas)
#We consider a single ordering son equal labels in different orders are considered the same:
library(dplyr)
library(stringr)

# Supongamos que tu dataset se llama data y la columna con las combinaciones de posiciones se llama Positions.Played
# Reemplaza con tu dataset
# Crear una función para ordenar las posiciones dentro de cada combinación
normalize_positions <- function(positions) {
  sorted_positions <- str_split(positions, ",") %>%
    lapply(sort) %>%
    sapply(paste, collapse = ",")
  return(sorted_positions)
}

# Aplicar la función a la columna Positions.Played
data$Positions.Played <- normalize_positions(data$Positions.Played)

# Verificar los cambios
length(unique(data$Positions.Played))
#See categories with more frequency:
frecuencia_posiciones=table(data$Positions.Played)

df_frecuencia <- as.data.frame(frecuencia_posiciones)
colnames(df_frecuencia) <- c("Category", "Frequency")
df_frecuencia_ordenado <- df_frecuencia[order(-df_frecuencia$Frequency), ]
top_categorias <- head(df_frecuencia_ordenado, 10)
top_categorias
#See categories with less frequecy 
frecuencia_posiciones=table(data$Positions.Played)

df_frecuencia <- as.data.frame(frecuencia_posiciones)
colnames(df_frecuencia) <- c("Category", "Frequency")
df_frecuencia_ordenado <- df_frecuencia[order(df_frecuencia$Frequency), ]
top_categorias <- head(df_frecuencia_ordenado, 10)
top_categorias
#Eliminate variable best position, as we don't use it
data=data[,-2] #eliminate best.position once is already used for EDA  
#Range of values of variables: 
sapply(data[,-1], range,na.rm=TRUE)
#we eliminate the first column corresponding to positions played 
#See the class of every variable
str(data[,-1])
#Convert to factor: 
data$Weak.Foot.Rating  <- factor(data$Weak.Foot.Rating )
data$Skill.Moves <- factor(data$Skill.Moves) 
columnas_caracter <- sapply(data, is.character)
data[columnas_caracter] <- lapply(data[columnas_caracter], factor)
variables_categoricas <- names(data)[sapply(data, is.factor)]
cat('categorical variables:',variables_categoricas)
#Check we don't have NA's
sum(is.na(data))
#Preprocessing stages: divide train/test, create dummy variables and scale
library(caret)
set.seed(123)
trainIndex <- createDataPartition(data$Positions.Played, p = 0.7, list = FALSE)
data_train <- data[trainIndex, ]
data_test <- data[-trainIndex, ]
y_train=data_train$Positions.Played
y_test=data_test$Positions.Played
dummies_model = dummyVars(Positions.Played ~ ., data=data_train)
trainData_mat = predict(dummies_model, newdata=data_train)
data_train = data.frame(trainData_mat)
testData_mat = predict(dummies_model, data_test)
data_test=data.frame(testData_mat)
#en el proceso de dummies se pierden las Positions.played, así que las vuelvo a añadir
data_train$Positions.Played = y_train
data_test$Positions.Played = y_test
preProcess_range_model = preProcess(data_train, method='range')
data_train_scaled = predict(preProcess_range_model, newdata = data_train)
data_test_scaled <-predict(preProcess_range_model, data_test)
#Some libraries that I may use: 
library(tidyverse)
library(skimr)
library(mice)
library(VIM)
library(GGally)
library(MASS)
library(glmnet)
library(e1071) 
library(rpart)
library(pROC)
library(class)
library(randomForest)
library(caret)
library(kernlab)
#Hago hiper parameter tunning con KNN. 
k_values <- c(1,2,5,7,10:20)
# Crear una lista para almacenar las precisiones promedio para cada valor de K
list_accuracy <- numeric(length(k_values))
folds=5
set.seed(123)
fold_indices <- createFolds(data_train_scaled$Positions.Played, k = folds)
#esto me crea 5 listas de indices, cada una con indices de  un 20% de los datos del train 
#lo que yo hago es entrenar con 4 de ellas y testear con la otra, eso para cada i. 

# Realizar validación cruzada
for (K in k_values) {
  accuracies <- numeric(folds)
  
  for (i in 1:folds) {
    # Crear particiones de entrenamiento y prueba
    test_indices <- fold_indices[[i]]#esto guarda los índices de la carpeta i de las 5 carpetas que hay 
    train_indices <- setdiff(seq_len(nrow(data_train_scaled)), test_indices)#se queda con todos los demás índices
    #osea todas las carpetas menos la i, que se usa de testeo
    train_train <- data_train_scaled[train_indices, ]
    train_test <- data_train_scaled[test_indices, ]
    
    # Realizar predicciones con k-NN
    knn_pred <- knn(train = train_train[,-56], test = train_test[,-56], cl = train_train$Positions.Played, k = K)
    
    # Calcular la precisión
    confusion_matrix_knn <- confusionMatrix(knn_pred, train_test$Positions.Played)
    accuracies[i] <- confusion_matrix_knn$overall["Accuracy"]
  }
  
  # Calcular la precisión promedio para el valor actual de K
  list_accuracy[K] <- mean(accuracies)
}

# Encontrar el mejor valor de K
best_k <- which.max(list_accuracy)
print(best_k)
print(list_accuracy[best_k])
#Ahora uso el mejor para predecir 
knn_pred_top=knn(train =data_train_scaled[,-56], test = data_test_scaled[,-56], cl = data_train_scaled$Positions.Played,k=best_k)
confusion_matrix_knn_top=confusionMatrix(knn_pred_top, data_test_scaled$Positions.Played)
#Compute accuracy and the macro-measures described in the project with KNN predictions: 
Accuracy=confusion_matrix_knn_top$overall["Accuracy"]
class_metrics_knn_top <- confusion_matrix_knn_top$byClass
recall_per_class <- class_metrics_knn_top[, "Sensitivity"]
spe_per_class <- class_metrics_knn_top[, "Specificity"]
prec_pred_per_class <- class_metrics_knn_top[, "Precision"]
F1 <- class_metrics_knn_top[, "F1"]


# Calcular el macro averaged recall, ignorando nas para no ver los que están en train pero no en test 
macro_avg_recall <- mean(recall_per_class, na.rm = TRUE)

# Imprimir el resultado
cat('Accuracy=',Accuracy,'\n')
cat('Sensitivity=',mean(recall_per_class,na.rm = TRUE),'\n')
cat('Specificity',mean(spe_per_class,na.rm = TRUE),'\n')
cat('Precision',mean(prec_pred_per_class,na.rm = TRUE),'\n')
cat('F1 score',mean(F1,na.rm = TRUE))
#Now, with Decision Tress 
max_depth_values <- seq(1, 20, by = 2)
min_split_values <- seq(10, 50, by = 10)

# Crear una lista para almacenar las precisiones promedio para cada combinación de hiperparámetros
results <- data.frame(max_depth = numeric(),
                      min_split = numeric(),
                      accuracy = numeric())

# Crear los índices de los pliegues para la validación cruzada
set.seed(123)
folds <- createFolds(data_train_scaled$Positions.Played, k = 5)

# Realizar validación cruzada para cada combinación de hiperparámetros
for (max_depth in max_depth_values) {
  for (min_split in min_split_values) {
    accuracies <- numeric(length(folds))
    
    for (i in 1:length(folds)) {
      # Crear particiones de entrenamiento y prueba
      test_indices <- folds[[i]]
      train_indices <- setdiff(seq_len(nrow(data_train_scaled)), test_indices)
      train_train <- data_train_scaled[train_indices, ]
      train_test <- data_train_scaled[test_indices, ]
      
      # Entrenar el modelo con rpart
      dt_model <- rpart(Positions.Played ~ ., data = train_train,
                        control = rpart.control(minsplit = min_split, maxdepth = max_depth))
      
      # Realizar predicciones
      predictions <- predict(dt_model, train_test, type = "class")
      
      # Calcular la precisión
      confusion_matrix_dt <- confusionMatrix(predictions, train_test$Positions.Played)
      accuracies[i] <- confusion_matrix_dt$overall["Accuracy"]
    }
    
    # Calcular la precisión promedio para la combinación actual de hiperparámetros
    mean_accuracy <- mean(accuracies)
    
    # Almacenar los resultados en el data.frame
    results <- rbind(results, data.frame(max_depth = max_depth, min_split = min_split, accuracy = mean_accuracy))
  }
}

# Encontrar la mejor combinación de hiperparámetros
best_index <- which.max(results$accuracy)
best_max_depth <- results$max_depth[best_index]
best_min_split <- results$min_split[best_index]
best_accuracy <- results$accuracy[best_index]

# Imprimir los mejores hiperparámetros y la precisión correspondiente
print(paste("Best max_depth:", best_max_depth))
print(paste("Best min_split:", best_min_split))
print(paste("Best accuracy:", best_accuracy))
#use the best 
dt_model_top <- rpart(Positions.Played ~ ., data = data_train_scaled,
                      control = rpart.control(minsplit = best_min_split, maxdepth = best_max_depth))

predictions_dt_top <- predict(dt_model_top, data_test_scaled, type = "class")
confusion_matrix_dt_top=confusionMatrix(predictions_dt_top, data_test_scaled$Positions.Played)
#Compute accuracy and the macro-measures described in the project with dt predictions: 
Accuracy=confusion_matrix_dt_top$overall["Accuracy"]
class_metrics_dt_top <- confusion_matrix_dt_top$byClass
recall_per_class <- class_metrics_dt_top[, "Sensitivity"]
spe_per_class <- class_metrics_dt_top[, "Specificity"]
prec_pred_per_class <- class_metrics_dt_top[, "Precision"]
F1 <- class_metrics_dt_top[, "F1"]


# Calcular el macro averaged recall, ignorando nas para no ver los que están en train pero no en test 
macro_avg_recall <- mean(recall_per_class, na.rm = TRUE)

# Imprimir el resultado
cat('Accuracy=',Accuracy,'\n')
cat('Sensitivity=',mean(recall_per_class,na.rm = TRUE),'\n')
cat('Specificity',mean(spe_per_class,na.rm = TRUE),'\n')
cat('Precision',mean(prec_pred_per_class,na.rm = TRUE),'\n')
cat('F1 score',mean(F1,na.rm = TRUE))
#With Random Forest: 
#ESTE LO VOY A ENTRENAR CON MI FORMA MANUAL
# Definir los valores de los hiperparámetros a explorar
n_tree_values <- c(500,1000,1500)#, 500, 1000)
m_try_values <- seq(5,35, by =4)#hasta 30 lo hago

# Crear una lista para almacenar las precisiones promedio para cada combinación de hiperparámetros
results <- data.frame(n_tree = numeric(),
                      m_try = numeric(),
                      accuracy = numeric())

# Suponiendo que folds ya está definido

partitions=5

# Realizar validación cruzada para cada combinación de hiperparámetros
for (n_tree in n_tree_values) {
  for (m_try in m_try_values) {
    accuracies <- numeric(partitions)
    
    for (i in 1:partitions) {
      # Crear particiones de entrenamiento y prueba
      Index <- createDataPartition(data_train_scaled$Positions.Played, p = 0.7, list = FALSE)
      train_train <- data_train_scaled[Index, ]
      train_test <- data_train_scaled[-Index, ]
      
      # Entrenar el modelo con randomForest
      rf_model <- randomForest(Positions.Played ~ ., data = train_train, ntree = n_tree, mtry = m_try)
      
      # Realizar predicciones
      predictions_rf <- predict(rf_model, train_test)
      
      # Calcular la precisión
      confusion_matrix_rf <- confusionMatrix(predictions_rf, train_test$Positions.Played)
      accuracies[i] <- confusion_matrix_rf$overall["Accuracy"]
    }
    
    # Calcular la precisión promedio para la combinación actual de hiperparámetros
    mean_accuracy <- mean(accuracies)
    
    # Almacenar los resultados en el data.frame
    results <- rbind(results, data.frame(n_tree = n_tree, m_try = m_try, accuracy = mean_accuracy))
  }
}

# Encontrar la mejor combinación de hiperparámetros
best_index <- which.max(results$accuracy)
best_n_tree <- results$n_tree[best_index]
best_m_try <- results$m_try[best_index]
best_accuracy <- results$accuracy[best_index]

# Imprimir los mejores hiperparámetros y la precisión correspondiente
print(paste("Best n_tree:", best_n_tree))
print(paste("Best m_try:", best_m_try))
print(paste("Best accuracy:", best_accuracy))
#Evaluo en mis datos el mejor 
rf_model_top <- randomForest(Positions.Played ~ ., data =data_train_scaled,ntree=best_n_tree,mtry=best_m_try)
predictions_rf_top <- predict(rf_model_top, data_test_scaled)
confusion_matrix_rf_top=confusionMatrix(predictions_rf_top, data_test_scaled$Positions.Played)
#Saco las otras medidas 
Accuracy=confusion_matrix_rf_top$overall["Accuracy"]
class_metrics_rf_top <- confusion_matrix_rf_top$byClass
recall_per_class <- class_metrics_rf_top[, "Sensitivity"]
spe_per_class <- class_metrics_rf_top[, "Specificity"]
prec_pred_per_class <- class_metrics_rf_top[, "Precision"]
F1 <- class_metrics_rf_top[, "F1"]


# Calcular el macro averaged recall, ignorando nas para no ver los que están en train pero no en test 
macro_avg_recall <- mean(recall_per_class, na.rm = TRUE)

# Imprimir el resultado
cat('Accuracy=',Accuracy,'\n')
cat('Sensitivity=',mean(recall_per_class,na.rm = TRUE),'\n')
cat('Specificity',mean(spe_per_class,na.rm = TRUE),'\n')
cat('Precision',mean(prec_pred_per_class,na.rm = TRUE),'\n')
cat('F1 score',mean(F1,na.rm = TRUE))
#Calculating percentage of labels covered with RF: 
# Calcular la cobertura
real_levels_positions_played <- levels(droplevels(data_test_scaled$Positions.Played))
coverage <- length(unique(predictions_rf_top)) / length(unique(data_test_scaled$Positions.Played))
#aunque salga 692 levels cuando pongo unique, cuando pongo la longitud solo cuenta los que salen de verdad, 
#en este caso en el test hay 312 distintos, y se predicen unas 70 clases diferentes. 
# Mostrar la cobertura
print(coverage)
length(unique(predictions_rf_top))
length(unique(data_test_scaled$Positions.Played))
#With KNN
# Calcular la cobertura
coverage <- length(unique(knn_pred_top)) / length(unique(data_test_scaled$Positions.Played))

# Mostrar la cobertura
print(coverage)
length(unique(knn_pred_top))
length(unique(data_test_scaled$Positions.Played))
#Calculate the  classes with the highest number of accurately predicted observation with RF
aciertos=diag(table(predictions_rf_top,data_test_scaled$Positions.Played))
aciertos2=diag(confusion_matrix_rf_top$table)#las dos formas son equivalentes 
sum(aciertos)
sum(aciertos2)
aciertos[aciertos2>30]
sum(aciertos2[aciertos2>30])
#With KNN
aciertos=diag(confusion_matrix_knn_top$table)
sum(aciertos)
(aciertos[aciertos>16])
sum(aciertos[aciertos>16])
#Plots with frequencies of classes with predictions RF vs real observations: 
library(ggplot2)
library(gridExtra)

# Convertir las predicciones y las clases verdaderas en data frames para ggplot

true_df <- as.data.frame(table(data_test_scaled$Positions.Played))
pred_df_rf <- as.data.frame(table(predictions_rf_top))

# Crear gráficos de barras para mostrar la distribución
plot1 <- ggplot(pred_df_rf, aes(x = predictions_rf_top, y = Freq)) +
  geom_bar(stat = "identity", fill = "red", width = 2) +
  labs(title = "Predictions Random Forest", x = "Predicted class", y = "Frequency") +
  ylim(0, 1300) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

plot2 <- ggplot(true_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "black", width = 2) +
  labs(title = "Real observations", x = "Observed Class", y = "Frequency") +
  ylim(0, 1300) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
plot1
plot2
# CHAPTER 3
#prepare data to use the libraries. We need one binary column for each position, having for each observation 
#ones in the columns corresponding to positions where they can play, and 0 in the rest. 
top_observations_ready= rbind(data_train_scaled, data_test_scaled)
top_observations_ready$Positions.Played=as.character(top_observations_ready$Positions.Played)
#creo columnas binarias 
positions_list <- strsplit(top_observations_ready$Positions.Played, ",")
unique_positions <- unique(unlist(positions_list))
#aquí debería poner 1-0 directamente pero bueno lo dejo por si lo acabo cambiando 
new_data <- data.frame(matrix(FALSE, nrow = nrow(top_observations_ready), ncol = length(unique_positions)))
colnames(new_data) <-(unique_positions)
for (i in 1:nrow(top_observations_ready)) {
  new_data[i, unique_positions %in% unlist(positions_list[i])] <- TRUE
}

binary_data <- cbind(top_observations_ready, new_data)
#la variable 56 es positions.played, que ahora está repetida: 
binary_data=binary_data[,-56]
#We have TRUE/FALSE with the previous code, but we want ones and ceros: 
binary_data[, 56:69] <- lapply(binary_data[, 56:69], as.integer)
#Library utiml includes library mldr: we use it to get the data and see the measures. We have to indicate the labels
#(positions), that is our last 14 columns, and the regresors, the first 58, of the new binary data we have created,
#where the first 70% of the data correspond  to training partition and the rest to testing:
library(utiml)
data_mldr=mldr_from_dataframe(binary_data,c(56:69),c(1:58)) #así está en formato mldr:. 
summary(data_mldr)
#The plot with the number of observations that have 1, 2 and three labels, in the density subsection: 
plot(data_mldr, type = "CH",col =hcl.colors(data_mldr$measures$num.labels,palette = "Zissou 1"),xaxt = 'n')
labels <- c("1 label", "2 labels", "3 labels")
axis(1, at =c(1.25,1.75,2.75) , labels = labels,tick = FALSE, padj = -1) 
#Bar plot with the frequencies of each position: 
plot(data_mldr, type = "LB")
#IRLbl values for each position (and some extra that we do not use)
data_mldr$labels
#Bar plot with the IRLbl vales per position: 
positions <- c("RW", "CF", "ST", "LW", "CB", "LM", "CDM", "CM", "CAM", "LB", "RB", "RM", "LWB", "RWB")
IRLbl <- c(3.6, 9.96, 1.3, 3.6, 1.06, 1.84, 1.47, 1, 1.92, 2.27, 2.17, 1.84, 6.02, 6.11)
#IRLbl values seen in the code before
data_plot <- data.frame(Position = factor(positions, levels = positions), IRLbl = IRLbl)

# Asignar un color diferente a cada barra
colors <-rainbow(length(positions))

# Crear el gráfico de barras
ggplot(data_plot, aes(x = Position, y = IRLbl, fill = Position)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(title = "IRLbl Values for Different Positions", x = "Position", y = "IRLbl") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas del eje X para mejor legibilidad
    panel.grid.major = element_blank(),  # Quitar la cuadrícula mayor
    panel.grid.minor = element_blank()   # Quitar la cuadrícula menor
  )  # Rotar etiquetas del eje X para mejor legibilidad
#Plot with the scumble function depending on the constant that 
#represents the difference in IRLbl values between the two labels: 
# Función para calcular el valor de la función dada C
func <- function(C) {
  1 - 2 * sqrt(C) / (1 + C)
}

# Generar datos para graficar
C_values <- seq(0, 10, by = 0.1)  # Valores de C desde 0 hasta 10
values <- func(C_values)  # Calcular los valores de la función

# Crear un data frame con los valores
df <- data.frame(C = C_values, Value = values)

# Graficar usando ggplot2
ggplot(df, aes(x = C, y = Value)) +
  geom_line(color = "red") +labs(
    x = "C",
    y = "SCUMBLE"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = 0:10)
#Circular diagram with the links between positions that apprear together: 
plot(data_mldr, type = "LC")
#Now, the prediction part: 
#The traditional simple classification using the variable Best.Position 
#to evaluate the approach of predicting just one position is at the end of the document
#MLDR objects separating the training part (the first 11649 observations (recall we used cbin(train,test)
#for creating binary data) and the rest for testing so we keep the same partition)
data_mldr_train=mldr_from_dataframe(binary_data[c(1:11649),],c(56:69),c(1:58))
data_mldr_test=mldr_from_dataframe(binary_data[c(11650:16478),],c(56:69),c(1:58))
#Label propagation with hiper parameter tunning:
#We use function lp to tranform the mldr object into a datasrt with the same regresor columns,
# but instead of 14 binary columns, just one with the labelset of the combination of positions for each player: 
datalp=mldr_transform(data_mldr, type = "LP")
datalp$classLabel=factor(datalp$classLabel)
#Now, we separare the training and testing partitions: 
data_train_lp <- datalp[c(1:11649), ]
data_test_lp <- datalp[c(11650:16478), ]
#HIPER PARAM TUNNING CON RANDOM FOREST, DIRECTAMENTE CON 1500 ARBOLES: 
# Definir los valores de los hiperparámetros a explorar
m_try_values <- seq(5,35, by = 4)#hasta 30 lo hago

# Crear una lista para almacenar las precisiones promedio para cada combinación de hiperparámetros
results_2 <- data.frame(m_try = numeric(),
                        subset_accuracy = numeric(),hamming_loss,precision,recall)

# Suponiendo que folds ya está definido

partitions=5

# Realizar validación cruzada para cada combinación de hiperparámetros

for (m_try in m_try_values) {
  subset_accuracies <- numeric(partitions)
  hamming_losses <- numeric(partitions)
  precisions <- numeric(partitions)
  recalls <- numeric(partitions)
  
  for (i in 1:partitions) {
    # Crear particiones de entrenamiento y prueba
    Index <- createDataPartition(data_train_lp$classLabel, p = 0.7, list = FALSE)
    train_train <- data_train_lp[Index, ]
    train_test <- data_train_lp[-Index, ]
    train_test_mldr=data_mldr_train[-Index]
    
    # Entrenar el modelo con randomForest
    rf_model <- randomForest(classLabel ~ ., data = train_train, ntree = 1500, mtry = m_try)
    
    # Realizar predicciones
    predictions_rf <- predict(rf_model, train_test)
    predicciones_lista <- strsplit(as.character(predictions_rf ), "")
    predicciones_matriz <- do.call(rbind, predicciones_lista)
    predicciones_df <- as.data.frame(predicciones_matriz, stringsAsFactors = FALSE)
    colnames(predicciones_df) <- c("RW", "CF", "ST", "LW", "CB", "LM", "CDM", "CM", "CAM", "LB", "RB", "RM", "LWB", "RWB")
    predicciones_df <- predicciones_df %>% mutate(across(everything(), as.numeric))
    results <- suppressMessages(suppressWarnings(mldr_evaluate(train_test_mldr, as.matrix(predicciones_df))))
    subset_accuracies[i] <- results$subset_accuracy
    hamming_losses[i]=results$hamming_loss
    precisions[i]=results$precision
    recalls[i]=results$recall
    
  }
  
  # Calcular la precisión promedio para el valor actual de K
  mean_subset_accuracy <- mean(subset_accuracies)
  mean_hamming_loss=mean(hamming_losses)
  mean_precision=mean(precisions)
  mean_recall=mean(recalls)
  
  
  # Almacenar los resultados en el data.frame
  results_2 <- rbind(results_2, data.frame(m_try =m_try, subset_accuracy =mean_subset_accuracy,hamming_loss=mean_hamming_loss,precision=mean_precision,recall=mean_recall))
}


# Encontrar la mejor combinación de hiperparámetros
best_index_subset_accuracy <- which.max(results_2$subset_accuracy)
best_index_hamming_loss <- which.max(results_2$hamming_loss)
best_index_precision <- which.max(results_2$precision)
best_index_recall <- which.max(results_2$recall)
best_m_try_subset_accuracy <- results_2$m_try[best_index_subset_accuracy]
#best_accuracy <- results$accuracy[best_index]
best_m_try_hamming_loss <- results_2$m_try[best_index_hamming_loss]
#best_accuracy <- results$accuracy[best_index]
best_m_try_precision <-results_2$m_try[best_index_precision]
#best_accuracy <- results$accuracy[best_index]
best_m_try_recall <- results_2$m_try[best_index_recall]
#best_accuracy <- results$accuracy[best_index]

# Imprimir los mejores hiperparámetros y la precisión correspondiente

cat('best_m_try_subset_accuracy',best_m_try_subset_accuracy)


cat('best_m_try_hamming_loss',best_m_try_hamming_loss)


cat('best_m_try_precision ',best_m_try_precision )


cat('best_m_try_recall',best_m_try_recall)
# AHORA APLICO CADA UNO DE ELLOS A MIS DATOS. Accuracy y Preicsion dan el mismo 
rf_model_accuracy <- randomForest(classLabel ~ ., data = data_train_lp, ntree = 1500, mtry = best_m_try_subset_accuracy)
rf_model_hamming_loss <- randomForest(classLabel ~ ., data = data_train_lp, ntree = 1500, mtry = best_m_try_hamming_loss)
rf_model_precision <- randomForest(classLabel ~ ., data = data_train_lp, ntree = 1500, mtry = best_m_try_precision )
rf_model_recall <- randomForest(classLabel ~ ., data = data_train_lp, ntree = 1500, mtry = best_m_try_recall)
#Las predicciones 
predictions_rf_accuracy <- predict(rf_model_accuracy, data_test_lp)
predictions_rf_hamming_loss <- predict(rf_model_hamming_loss, data_test_lp)
predictions_rf_precision <- predict(rf_model_precision, data_test_lp)
predictions_rf_recall <- predict(rf_model_recall, data_test_lp)
#TIENE QUE ESTAR LA PREDICCION CON EL FORMATO DE LAS BINARIAS
predicciones_lista_accuracy <- strsplit(as.character(predictions_rf_accuracy), "")
predicciones_lista_hamming_loss <- strsplit(as.character(predictions_rf_hamming_loss), "")
predicciones_lista_precision <- strsplit(as.character(predictions_rf_precision), "")
predicciones_lista_recall <- strsplit(as.character(predictions_rf_recall), "")

# Crear una matriz a partir de la lista de predicciones
predicciones_matriz_accuracy <- do.call(rbind, predicciones_lista_accuracy)
predicciones_matriz_hamming_loss <- do.call(rbind, predicciones_lista_hamming_loss)
predicciones_matriz_precision <- do.call(rbind, predicciones_lista_precision)
predicciones_matriz_recall <- do.call(rbind, predicciones_lista_recall)

# Convertir a data frame y cambiar los nombres de las columnas
predicciones_df_accuracy <- as.data.frame(predicciones_matriz_accuracy, stringsAsFactors = FALSE)
predicciones_df_hamming_loss <- as.data.frame(predicciones_matriz_hamming_loss, stringsAsFactors = FALSE)
predicciones_df_precision <- as.data.frame(predicciones_matriz_precision, stringsAsFactors = FALSE)
predicciones_df_recall <- as.data.frame(predicciones_matriz_recall, stringsAsFactors = FALSE)

# Nombrar las columnas con las posiciones
colnames(predicciones_df_precision) <- c("RW", "CF", "ST", "LW", "CB", "LM", "CDM", "CM", "CAM", "LB", "RB", "RM", "LWB", "RWB")

# Convertir los valores a numéricos
predicciones_df_accuracy <- predicciones_df_accuracy %>% mutate(across(everything(), as.numeric))
predicciones_df_hamming_loss  <- predicciones_df_hamming_loss  %>% mutate(across(everything(), as.numeric))
predicciones_df_precision <- predicciones_df_precision %>% mutate(across(everything(), as.numeric))
predicciones_df_recall <- predicciones_df_recall %>% mutate(across(everything(), as.numeric))
#Miro la cardinality para ver cuantas predigo de media.
mean(rowSums(predicciones_df_accuracy))
#CON ESTO Y EL QUE VAYA A USAR HE DE MIRAR CUANTOS DE LAS 3 CLASES MAYORITARIAS ESTÁ PREDICIENDO. 
#lo del suppres evita los millones de mensajes que aparecen 
results_accuracy <- suppressMessages(suppressWarnings(mldr_evaluate(data_mldr_test, as.matrix(predicciones_df_accuracy))))
results_hamming_loss <- suppressMessages(suppressWarnings(mldr_evaluate(data_mldr_test, as.matrix(predicciones_df_hamming_loss))))
results_precision <- suppressMessages(suppressWarnings(mldr_evaluate(data_mldr_test, as.matrix(predicciones_df_precision))))
results_recall <- suppressMessages(suppressWarnings(mldr_evaluate(data_mldr_test, as.matrix(predicciones_df_recall))))
results_accuracy
#cf
sum(predicciones_df_accuracy$V2==1 & binary_data[11650:16478,]$CF==1)
sum(binary_data[11650:16478,]$CF==1)
#LWB
sum(predicciones_df_accuracy$V13==1 & binary_data[11650:16478,]$LWB==1)
sum(binary_data[11650:16478,]$LWB==1)
#RWB
sum(predicciones_df_accuracy$V14==1 & binary_data[11650:16478,]$RWB==1)
sum(binary_data[11650:16478,]$RWB==1)
results_hamming_loss
results_precision
results_recall
#We can also use the library utiml to do it automatically: 
model_automaticlp <- lp(data_mldr_train, "RF")
predictions_lp <- predict(model_automaticlp, data_mldr_test)
results <- multilabel_evaluate(data_mldr_test, predictions_lp)
round(results, 4)
#BINARY RELEVANCE:
#The following code creates 14 datasets, databr[[1]],...,databr[[14]] 
#each one with a column corresponding to each position
databr <- mldr_transform(data_mldr, type = "BR")
data_br_train=mldr_transform(data_mldr_train, type = "BR")
data_br_test=mldr_transform(data_mldr_test, type = "BR")
k_values <-c(1,5,10, 15, 20)
n_tree_values <-c(1500)
m_try_values <-c(5,10,17,25,35)#hasta 35 puse
#creo una función que dado data train data me haga cross validations con knn y rf con los parámetros elegidos
#y me devuelva los parámetros del mejor modelo. 
results <- list()
tune_and_evaluate <- function(train_data, response_col_name, k_values, n_tree_values, m_try_values, folds = 5) {
  # Crear los índices de los pliegues para la validación cruzada
  set.seed(123)
  fold_indices <- createFolds(train_data[,56], k = folds, list = TRUE)
  
  # Inicializar las variables para almacenar los mejores resultados
  best_accuracy <- 0
  best_model <- NULL                                        
  best_hyperparams <- list()
  # k-NN tuning
  for (k in k_values) {
    accuracies <- numeric(folds)
    for (i in 1:folds) {
      test_indices <- fold_indices[[i]]
      train_indices <- setdiff(seq_len(nrow(train_data)), test_indices)
      train_train <- train_data[train_indices, ]
      train_test <- train_data[test_indices, ]
      knn_pred <- knn(train = train_train[,-56], test = train_test[,-56], cl = train_train[,56], k = k)
      confusion_matrix_knn <- confusionMatrix(knn_pred,as.factor(train_test[,56]))
      accuracies[i] <- confusion_matrix_knn$overall["Accuracy"]
    }
    
    mean_accuracy <- mean(accuracies)
    if (mean_accuracy > best_accuracy) {
      best_accuracy <- mean_accuracy
      best_model <- "k-NN"
      best_hyperparams <- list(k = k)
    }
  }
  
  # Random Forest tuning
  for (n_tree in n_tree_values) {
    for (m_try in m_try_values) {
      accuracies <- numeric(folds)
      for (i in 1:folds) {
        test_indices <- fold_indices[[i]]
        train_indices <- setdiff(seq_len(nrow(train_data)), test_indices)
        train_train <- train_data[train_indices, ]
        train_test <- train_data[test_indices, ]
        formula=as.formula(paste(as.factor(response_col_name), "~ ."))
        #rf_model <- randomForest(formula~.,data = train_br)
        rf_model <- randomForest(formula,data = train_train,ntree = n_tree, mtry = m_try)
        
        #TEWNGO QUE PONERLO ASÍ PORQUE CON EL PUNTO COMO TENÍA QUE PONER LO DE COLNAME CAMBIABA LA COSA CON ~. Y DABA ERROR,
        #Y DEMÁS SI NO PONJÍA EL ASFACTOR LO TOMABA COMO NUMEROS Y DECIA QUE ERA REGRESION. 
        predictions_rf <- predict(rf_model, train_test)
        confusion_matrix_rf <- confusionMatrix(predictions_rf, as.factor(train_test[,56]))
        accuracies[i] <- confusion_matrix_rf$overall["Accuracy"]
      }
      
      mean_accuracy <- mean(accuracies)
      if (mean_accuracy > best_accuracy) {
        best_accuracy <- mean_accuracy
        best_model <- "Random Forest"
        best_hyperparams <- list(ntree = n_tree, mtry = m_try)
      }
    }
  }
  
  return(list(best_model = best_model, best_hyperparams = best_hyperparams, best_accuracy = best_accuracy))
}
for (i in 1:length(data_br_train)) { #for (i in 1:length(data_br_train))
  dataset <- data_br_train[[i]]#se queda con el dataset de la posición i-ésima
  response_col_name <- names(dataset)[56]  # Reemplazar con el nombre de la columna de respuesta binaria
  dataset[,response_col_name]=as.factor(binary_data[c(1:11649),response_col_name]) 
  print(i)
  
  # Realizar tuning y evaluación
  result <- tune_and_evaluate(dataset,response_col_name, k_values, n_tree_values, m_try_values)
  
  # Guardar los resultados
  results[[response_col_name]] <- result
}
result
for (name in names(results)) {
  cat("Results for", name, ":\n")
  print(results[[name]])
}
#Separo los dos de KNN para que me los de en orden
n_tree_value <-1500
m_try_values <- c(5,10,5,17,25,35,25,25,10,25,35,25)#lista de 12
K=15
#creo una función que dado data train data me haga cross validations con knn y rf con los parámetros elegidos
#y me devuelva los parámetros del mejor modelo. 
predictions <- data.frame(ID = 1:nrow(data_br_test[[1]]))
for (i in 1){
  #KNN
  train_br <- data_br_train[[i]]
  position <- names(train_br)[56]
  test_br=data_br_test[[i]]
  knn_pred <- knn(train = train_br[,-56], test = test_br[,-56], cl = train_br[,56], k = K)
  predictions[,position] <-knn_pred
}
for (i in 2:13) { #for (i in 1:length(data_br_train))
  train_br <- data_br_train[[i]]#se queda con el dataset de la posición i-ésima
  test_br=data_br_test[[i]]
  position <- names(train_br)[56]
  #train_br[,position]=as.factor(binary_data[c(1:11649),position])#quiza con as.factor(train_br[,position]) valdría
  train_br[,position]=as.factor(train_br[,position])
  formula=as.formula(paste(as.factor(position), "~ ."))
  #rf_model <- randomForest(formula~.,data = train_br)
  rf_model <- randomForest(formula,data = train_br, 
                           ntree =n_tree_value, mtry = m_try_values[i-1])#voy del 2 al 13 y quiero indices 1 a 12
  test_br[,position]=as.factor(test_br[,position])
  predictions_rf <- predict(rf_model, test_br)
  
  predictions[,position] <-predictions_rf
}

for (i in 14){
  #KNN
  train_br <- data_br_train[[i]]
  position <- names(train_br)[56]
  test_br=data_br_test[[i]]
  knn_pred <- knn(train = train_br[,-56], test = test_br[,-56], cl = train_br[,56], k = K)
  predictions[,position] <-knn_pred
}
final_predictions=predictions[,-1]
final_predictions<-final_predictions %>% mutate(across(everything(), as.numeric))
final_predictions=final_predictions-1 #me salían entre 1 y 2 
predictions_matrix <- as.matrix(final_predictions)

# Eliminar los nombres de las columnas
colnames(predictions_matrix) <- NULL

# Evaluar las predicciones
mldr_evaluate(data_mldr_test, predictions_matrix)
#results<- suppressMessages(suppressWarnings(mldr_evaluate(data_mldr_test, as.matrix(final_predictions))))
#Haicnedolo con la librería, de manera autmática: (añadir tambien la version de LP luego):
model <- br(data_mldr_train, "RF",probability=FALSE)
predictions_br <- predict(model, data_mldr_test,probability=FALSE)
results <- multilabel_evaluate(data_mldr_test, predictions_br)
round(results,4)
#This is the best prediction with BR and the one we use. 
#But we should check that we don't have players without any predicted position:
any(rowSums(predictions_br)==0)
mean(rowSums(predictions_br))
#predictions_br is an mlresult. The second colomn is CF, 13 is LWB and 14 RWB
#cf
sum(predictions_br[,2] & binary_data[11650:16478,]$CF==1)
sum(binary_data[11650:16478,]$CF==1)
#LWB
sum(predictions_br[,13]==1 & binary_data[11650:16478,]$LWB==1)
sum(binary_data[11650:16478,]$LWB==1)
#RWB
sum(predictions_br[,14]==1 & binary_data[11650:16478,]$RWB==1)
sum(binary_data[11650:16478,]$RWB==1)
#MLKNN
model_1=mlknn(data_mldr_train,k=1)
model_2=mlknn(data_mldr_train,k=2)
model_5=mlknn(data_mldr_train,k=5)
model_10=mlknn(data_mldr_train,k=10)
model_15=mlknn(data_mldr_train,k=15)
model_20=mlknn(data_mldr_train,k=20)
predictions_1 <- predict(model_1, data_mldr_test)
results_1 <- multilabel_evaluate(data_mldr_test, predictions_1)
round(results_1, 4)
predictions_2 <- predict(model_2, data_mldr_test)
results_2 <- multilabel_evaluate(data_mldr_test, predictions_2)
round(results_2, 4)
predictions_5 <- predict(model_5, data_mldr_test)
results_5 <- multilabel_evaluate(data_mldr_test, predictions_5)
round(results_5, 4)
predictions_10 <- predict(model_10, data_mldr_test)
results_10 <- multilabel_evaluate(data_mldr_test, predictions_10)
round(results_10, 4)
predictions_15 <- predict(model_15, data_mldr_test)
results_15 <- multilabel_evaluate(data_mldr_test, predictions_15)
round(results_15, 4)
predictions_20 <- predict(model_20, data_mldr_test)
results_20 <- multilabel_evaluate(data_mldr_test, predictions_20)
round(results_20, 4)
# Balancing 
library(mldr.resampling)
data_mldr_train_lpros=LPROS(data_mldr_train,P =25)
data_mldr_train_lprus=LPRUS(data_mldr_train,P=25)
data_mldr_train_mlros=MLROS(data_mldr_train,P=25) #ESTE NO LO USARÉ 
data_mldr_train_mlrus=MLRUS(data_mldr_train,P=25)
data_mldr_train_remedial=REMEDIAL(data_mldr_train)
data_mldr_train_remedial_lpros=LPROS(data_mldr_train_remedial,P=25)
summary(data_mldr_train)
summary(data_mldr_train_lpros)
summary(data_mldr_train_lprus)
summary(data_mldr_train_mlros) # este no puede sacar el scumble 
summary(data_mldr_train_mlrus)
summary(data_mldr_train_remedial)
summary(data_mldr_train_remedial_lpros)
#pruebo a cambiar el train y luego volver a juntar y volver a empezarlo: 
#hay que hacerlo porque si no al detectar lo nuevo salen errores al ver clases distintas 
data_completo_lpros=rbind(data_mldr_train_lpros$dataset,data_mldr_test$dataset)
data_completo_lpros=data_completo_lpros[,-c(70,71)]#quito scumble y la otra que salen del mldr automaticas

data_completo_lprus=rbind(data_mldr_train_lprus$dataset,data_mldr_test$dataset)
data_completo_lprus=data_completo_lprus[,-c(70,71)]
# a este le quito los nas 
data_completo_mlros=rbind(na.omit(data_mldr_train_mlros$dataset[,-c(70,71)]),data_mldr_test$dataset[,-c(70,71)])
#70 y 71 tenían NA en todo, por eso si no no me ponía nada en el train
data_completo_mlrus=rbind(data_mldr_train_mlrus$dataset,data_mldr_test$dataset)
data_completo_mlrus=data_completo_mlrus[,-c(70,71)]

data_completo_remedial=rbind(data_mldr_train_remedial$dataset,data_mldr_test$dataset)
data_completo_remedial=data_completo_remedial[,-c(70,71)]

data_completo_remedial_lpros=rbind(data_mldr_train_remedial_lpros$dataset,data_mldr_test$dataset)
data_completo_remedial_lpros=data_completo_remedial_lpros[,-c(70,71)]
#Vuelvo a crear los mldr a partir del nuevo dataset con los datos tranformados 
#LPROS: 
data_completo_lpros_mldr=mldr_from_dataframe(data_completo_lpros,c(56:69),c(1:58))
n=nrow(data_completo_lpros)
data_mldr_train_delcompleto_lpros=mldr_from_dataframe(data_completo_lpros[c(1:(n-4829)), ],c(56:69),c(1:58))
data_mldr_test_delcompleto_lpros=mldr_from_dataframe(data_completo_lpros[-c(1:(n-4829)), ],c(56:69),c(1:58))
#LPRUS
data_completo_lprus_mldr=mldr_from_dataframe(data_completo_lprus,c(56:69),c(1:58))
n=nrow(data_completo_lprus)
data_mldr_train_delcompleto_lprus=mldr_from_dataframe(data_completo_lprus[c(1:(n-4829)), ],c(56:69),c(1:58))
data_mldr_test_delcompleto_lprus=mldr_from_dataframe(data_completo_lprus[-c(1:(n-4829)), ],c(56:69),c(1:58))
#MLROS
data_completo_mlros_mldr=mldr_from_dataframe(data_completo_mlros,c(56:69),c(1:58))
n=nrow(data_completo_mlros)
data_mldr_train_delcompleto_mlros=mldr_from_dataframe(data_completo_mlros[c(1:(n-4829)), ],c(56:69),c(1:58))
data_mldr_test_delcompleto_mlros=mldr_from_dataframe(data_completo_mlros[-c(1:(n-4829)), ],c(56:69),c(1:58))
#MLRUS
data_completo_mlrus_mldr=mldr_from_dataframe(data_completo_mlrus,c(56:69),c(1:58))
n=nrow(data_completo_mlrus)
data_mldr_train_delcompleto_mlrus=mldr_from_dataframe(data_completo_mlrus[c(1:(n-4829)), ],c(56:69),c(1:58))
data_mldr_test_delcompleto_mlrus=mldr_from_dataframe(data_completo_mlrus[-c(1:(n-4829)), ],c(56:69),c(1:58))
#REMEDIAL
data_completo_remedial_mldr=mldr_from_dataframe(data_completo_remedial,c(56:69),c(1:58))
n=nrow(data_completo_remedial)
data_mldr_train_delcompleto_remedial=mldr_from_dataframe(data_completo_remedial[c(1:(n-4829)), ],c(56:69),c(1:58))
data_mldr_test_delcompleto_remedial=mldr_from_dataframe(data_completo_remedial[-c(1:(n-4829)), ],c(56:69),c(1:58))
#REMEDIAL and LPROS after REMEDIAL:
data_completo_remedial_lpros_mldr=mldr_from_dataframe(data_completo_remedial_lpros,c(56:69),c(1:58))
n=nrow(data_completo_remedial_lpros)
data_mldr_train_delcompleto_remedial_lpros=mldr_from_dataframe(data_completo_remedial_lpros[c(1:(n-4829)), ],c(56:69),c(1:58))
data_mldr_test_delcompleto_remedial_lpros=mldr_from_dataframe(data_completo_remedial_lpros[-c(1:(n-4829)), ],c(56:69),c(1:58))
#ESTE CODIGO ME CONFIRMA QUE EFECTIVAMENTE ESTÁ BIEN 
summary(data_mldr_train_delcompleto_lpros)
summary(data_mldr_train_lpros)

summary(data_mldr_train_delcompleto_lprus)
summary(data_mldr_train_lprus)

summary(data_mldr_train_delcompleto_mlrus)
summary(data_mldr_train_mlrus)

summary(data_mldr_train_delcompleto_remedial)
summary(data_mldr_train_remedial)
#y pongo el mlros tambien 
summary(data_mldr_train_delcompleto_mlros)

summary(data_mldr_train_delcompleto_remedial_lpros)
summary(data_mldr_train_remedial_lpros)
#REMEDIAL plot
plot(data_mldr_train_delcompleto_remedial, type = "LC")
#Plots of frequencies LPROS vs partition
labels <- c("RW", "CF", "ST", "LW", "CB", "LM", "CDM", "CM", "CAM", "LB", "RB", "RM", "LWB", "RWB")
train_counts <- data_mldr_train_delcompleto_lpros$labels$count
full_counts <- data_mldr_train$labels$count

# Crear data frames
train_df <- data.frame(Label = labels, Count = train_counts)
full_df <- data.frame(Label = labels, Count = full_counts)

# Colores rainbow
rainbow_colors <- rainbow(length(labels))

# Gráfico para el conjunto de entrenamiento
plot1 <- ggplot(train_df, aes(x = Label, y = Count, fill = Label)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rainbow_colors) +
  ylim(0, 4000) +
  labs(title = "Frecuencia de Etiquetas - Conjunto de Entrenamiento") +
  theme_minimal() +
  theme(legend.position = "none")

# Gráfico para el conjunto completo
plot2 <- ggplot(full_df, aes(x = Label, y = Count, fill = Label)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rainbow_colors) +
  ylim(0, 4000) +
  labs(title = "Frecuencia de Etiquetas - Conjunto Completo") +
  theme_minimal() +
  theme(legend.position = "none")
plot1
plot2
#LP predicitons: 
#lpros 
#con lpros 
model_lpros_lp <- lp(data_mldr_train_delcompleto_lpros, "RF")
predictions_lpros_lp <-predict(model_lpros_lp, data_mldr_test_delcompleto_lpros,prob=FALSE)
results_lp_lpros <- multilabel_evaluate(data_mldr_test_delcompleto_lpros, predictions_lpros_lp)
round(results_lp_lpros,4)
#predictions_br is an mlresult. The second colomn is CF, 13 is LWB and 14 RWB
#cf
sum(predictions_lpros_lp[,2] & binary_data[11650:16478,]$CF==1)
sum(binary_data[11650:16478,]$CF==1)
#LWB
sum(predictions_lpros_lp[,13]==1 & binary_data[11650:16478,]$LWB==1)
sum(binary_data[11650:16478,]$LWB==1)
#RWB
sum(predictions_lpros_lp[,14]==1 & binary_data[11650:16478,]$RWB==1)
sum(binary_data[11650:16478,]$RWB==1)
#lprus
#con lpros 
model_lprus_lp <- lp(data_mldr_train_delcompleto_lprus, "RF")
predictions_lprus_lp <-predict(model_lprus_lp, data_mldr_test_delcompleto_lprus)
results_lp_lprus <- multilabel_evaluate(data_mldr_test_delcompleto_lprus, predictions_lprus_lp)
round(results_lp_lprus,4)
#MLROS
model_mlros_lp <- lp(data_mldr_train_delcompleto_mlros, "RF")
predictions_mlros_lp <-predict(model_mlros_lp, data_mldr_test_delcompleto_mlros)
results_lp_mlros <- multilabel_evaluate(data_mldr_test_delcompleto_mlros, predictions_mlros_lp)
round(results_lp_mlros,4)
#MLRUS
model_mlrus_lp <- lp(data_mldr_train_delcompleto_mlrus, "RF")
predictions_mlrus_lp <-predict(model_mlrus_lp, data_mldr_test_delcompleto_mlrus)
results_lp_mlrus <- multilabel_evaluate(data_mldr_test_delcompleto_mlrus, predictions_mlrus_lp)
round(results_lp_mlrus,4)
#REMEDIAL
model_remedial_lp <- lp(data_mldr_train_delcompleto_remedial, "RF")
predictions_remedial_lp <-predict(model_remedial_lp, data_mldr_test_delcompleto_remedial,prob=FALSE)
results_lp_remedial <- multilabel_evaluate(data_mldr_test_delcompleto_remedial, predictions_remedial_lp)
round(results_lp_remedial,4)
#predictions_br is an mlresult. The second colomn is CF, 13 is LWB and 14 RWB
#cf
sum(predictions_remedial_lp [,2] & binary_data[11650:16478,]$CF==1)
sum(binary_data[11650:16478,]$CF==1)
sum(predictions_remedial_lp [,2]==1)
#LWB
sum(predictions_remedial_lp [,13]==1 & binary_data[11650:16478,]$LWB==1)
sum(binary_data[11650:16478,]$LWB==1)
#RWB
sum(predictions_remedial_lp [,14]==1 & binary_data[11650:16478,]$RWB==1)
sum(binary_data[11650:16478,]$RWB==1)
#REMEDIAL+LPROS: 
model_remedial_lpros_lp <- lp(data_mldr_train_delcompleto_remedial_lpros, "RF")
predictions_remedial_lpros_lp <-predict(model_remedial_lpros_lp, data_mldr_test_delcompleto_remedial,prob=FALSE)
results_lp_remedial_lpros <- multilabel_evaluate(data_mldr_test_delcompleto_remedial_lpros, predictions_remedial_lpros_lp)
round(results_lp_remedial_lpros,4)

#predictions_br is an mlresult. The second colomn is CF, 13 is LWB and 14 RWB
#cf
sum(predictions_remedial_lpros_lp [,2] & binary_data[11650:16478,]$CF==1)
sum(binary_data[11650:16478,]$CF==1)
#LWB
sum(predictions_remedial_lpros_lp [,13]==1 & binary_data[11650:16478,]$LWB==1)
sum(binary_data[11650:16478,]$LWB==1)
#RWB
sum(predictions_remedial_lpros_lp [,14]==1 & binary_data[11650:16478,]$RWB==1)
sum(binary_data[11650:16478,]$RWB==1)
#BINARY RELEVANCE AHORA: 
#lpros 
model_lpros_br <- br(data_mldr_train_delcompleto_lpros, "RF")
predictions_lpros_br <-predict(model_lpros_br, data_mldr_test_delcompleto_lpros,prob=FALSE)
results_br_lpros <- multilabel_evaluate(data_mldr_test_delcompleto_lpros, predictions_lpros_br)
round(results_br_lpros,4)
#predictions_br is an mlresult. The second colomn is CF, 13 is LWB and 14 RWB
#cf
sum(predictions_lpros_br[,2] & binary_data[11650:16478,]$CF==1)
sum(binary_data[11650:16478,]$CF==1)
#LWB
sum(predictions_lpros_br[,13]==1 & binary_data[11650:16478,]$LWB==1)
sum(binary_data[11650:16478,]$LWB==1)
#RWB
sum(predictions_lpros_br[,14]==1 & binary_data[11650:16478,]$RWB==1)
sum(binary_data[11650:16478,]$RWB==1)
#lprus
model_lprus_br <- br(data_mldr_train_delcompleto_lprus, "RF")
predictions_lprus_br <-predict(model_lprus_br, data_mldr_test_delcompleto_lprus)
results_br_lprus <- multilabel_evaluate(data_mldr_test_delcompleto_lprus, predictions_lprus_br)
round(results_br_lprus,4)
#MLROS
model_mlros_br <- br(data_mldr_train_delcompleto_mlros, "RF")
predictions_mlros_br <-predict(model_mlros_br, data_mldr_test_delcompleto_mlros)
results_br_mlros <- multilabel_evaluate(data_mldr_test_delcompleto_mlros, predictions_mlros_br)
round(results_br_mlros,4)
#MLRUS
model_mlrus_br <- br(data_mldr_train_delcompleto_mlrus, "RF")
predictions_mlrus_br <-predict(model_mlrus_br, data_mldr_test_delcompleto_mlrus)
results_br_mlrus <- multilabel_evaluate(data_mldr_test_delcompleto_mlrus, predictions_mlrus_br)
round(results_br_mlrus,4)
#REMEDIAL
model_remedial_br <- br(data_mldr_train_delcompleto_remedial, "RF")
predictions_remedial_br <-predict(model_remedial_br, data_mldr_test_delcompleto_remedial,prob=FALSE)
results_br_remedial <- multilabel_evaluate(data_mldr_test_delcompleto_remedial, predictions_remedial_br)
round(results_br_remedial,4)
#predictions_br is an mlresult. The second colomn is CF, 13 is LWB and 14 RWB
#cf
sum(predictions_remedial_br [,2] & binary_data[11650:16478,]$CF==1)
sum(binary_data[11650:16478,]$CF==1)
#LWB
sum(predictions_remedial_br [,13]==1 & binary_data[11650:16478,]$LWB==1)
sum(binary_data[11650:16478,]$LWB==1)
#RWB
sum(predictions_remedial_br [,14]==1 & binary_data[11650:16478,]$RWB==1)
sum(binary_data[11650:16478,]$RWB==1)
#REMEDIAL+LPROS
model_remedial_lpros_br <- br(data_mldr_train_delcompleto_remedial_lpros, "RF")
predictions_remedial_lpros_br <-predict(model_remedial_lpros_br, data_mldr_test_delcompleto_remedial,prob=FALSE)
results_br_remedial_lpros <- multilabel_evaluate(data_mldr_test_delcompleto_remedial_lpros, predictions_remedial_lpros_br)
round(results_br_remedial_lpros,4)
#predictions_br is an mlresult. The second colomn is CF, 13 is LWB and 14 RWB
#cf
sum(predictions_remedial_lpros_br [,1] & binary_data[11650:16478,]$CF==1)
sum(binary_data[11650:16478,]$CF==1)
#LWB
sum(predictions_remedial_lpros_br [,13]==1 & binary_data[11650:16478,]$LWB==1)
sum(binary_data[11650:16478,]$LWB==1)
#RWB
sum(predictions_remedial_lpros_br [,14]==1 & binary_data[11650:16478,]$RWB==1)
sum(binary_data[11650:16478,]$RWB==1)


# Simple classificaiton approach
#PREDECIR AHORA PARA BEST.POSITION A VER CUANTO SALE 
data=read.csv('Fifa 23 Players Data.csv')
data<- data[data$Best.Position != 'GK', ]
data=data[,-c(1,2,3:5,6,8,9,13:22,26:30,68:89)]#7 is best position, we keep it for EDA and then delete it 
#he quitado positions.played en su lugar 
data$Weak.Foot.Rating  <- factor(data$Weak.Foot.Rating )
data$Skill.Moves <- factor(data$Skill.Moves) 
columnas_caracter <- sapply(data, is.character)
data[columnas_caracter] <- lapply(data[columnas_caracter], factor)
variables_categoricas <- names(data)[sapply(data, is.factor)]
library(caret)
set.seed(123)
trainIndex <- createDataPartition(data$Best.Position, p = 0.7, list = FALSE)
data_train <- data[trainIndex, ]
data_test <- data[-trainIndex, ]
y_train=data_train$Best.Position
y_test=data_test$Best.Position
dummies_model = dummyVars(Best.Position ~ ., data=data_train)
trainData_mat = predict(dummies_model, newdata=data_train)
data_train = data.frame(trainData_mat)
testData_mat = predict(dummies_model, data_test)
data_test=data.frame(testData_mat)
#en el proceso de dummies se pierden las Positions.played, así que las vuelvo a añadir
data_train$Best.Position = y_train
data_test$Best.Position = y_test
preProcess_range_model = preProcess(data_train, method='range')
data_train_scaled = predict(preProcess_range_model, newdata = data_train)
data_test_scaled <-predict(preProcess_range_model, data_test)
library(tidyverse)
library(skimr)
library(mice)
library(VIM)
library(GGally)
library(MASS)
library(glmnet)
library(e1071) 
library(rpart)
library(pROC)
library(class)
library(randomForest)
library(caret)
library(kernlab)
best_n_tree=1500
best_m_try=35
rf_model_top <- randomForest(Best.Position ~ ., data =data_train_scaled,ntree=best_n_tree,mtry=best_m_try)
predictions_rf_top <- predict(rf_model_top, data_test_scaled)
confusion_matrix_rf_top=confusionMatrix(predictions_rf_top, data_test_scaled$Best.Position)
confusion_matrix_rf_top

knn_model_top <- knn(train=data_train_scaled[,-56],test=data_test_scaled[,-56],cl=data_train_scaled[,56],k=20)
confusion_matrix_knn_top=confusionMatrix(knn_model_top , data_test_scaled$Best.Position)
confusion_matrix_knn_top
