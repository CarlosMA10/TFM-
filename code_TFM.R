
###CHAPTER 2

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
library(gridExtra)

#Name of the variable to put in axis x
var_name <- names(data2)[43]

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

plot1 <- update(plot1, xlab = var_name)
plot2 <- update(plot2, xlab = var_name)
grid.arrange(plot1, plot2, ncol = 2) #plots together
#Plot Preferred foot 
library(ggplot2)
library(dplyr)
#calculate proportions for horizontal positions depending on variable positions.foot
data2_horizontal <- data2 %>%
  group_by(Preferred.Foot, Position_horizontal) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))
#barplot
plot1 <- ggplot(data = data2_horizontal, aes(x = Preferred.Foot, y = proportion, fill = Position_horizontal)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)), 
            position = position_fill(vjust = 0.5), size = 3) +
  labs(title = "Preferred Foot by Position Horizontal", y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# calculate proportions for vertical positions depending on variable positions.foot
data2_vertical <- data2 %>%
  group_by(Preferred.Foot, Position_vertical) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

#barplot
plot2 <- ggplot(data = data2_vertical, aes(x = Preferred.Foot, y = proportion, fill = Position_vertical)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)), 
            position = position_fill(vjust = 0.5), size = 3) +
  labs(title = "Preferred Foot by Position Vertical", y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
grid.arrange(plot1, plot2, ncol = 2)
#Plot strenght and age, we do the same:  
var_name <- names(data2)[35]
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

plot1 <- update(plot1, xlab = var_name)
plot2 <- update(plot2, xlab = var_name)
var_name <- names(data2)[3]
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

plot3 <- update(plot3, xlab = var_name)
plot4 <- update(plot4, xlab = var_name)
grid.arrange(plot1, plot2,plot3,plot4,nrow=2, ncol = 2)
#Number of labels detected:
categorias_distintas <- unique(data$Positions.Played)
length(categorias_distintas)
#We consider a single ordering so equal labels in different orders are considered the same:
#example: CAM,CM is considered different from CM,CAM if we don't do this
library(stringr)
normalize_positions <- function(positions) {
  sorted_positions <- str_split(positions, ",") %>%
    lapply(sort) %>%
    sapply(paste, collapse = ",")
  return(sorted_positions)
}

data$Positions.Played <- normalize_positions(data$Positions.Played)

#We can verify that now we have less classes, because those that were different for the ordering are now the same
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
data=data[,-2]  
#Range of values of variables: 
sapply(data[,-1], range,na.rm=TRUE)#we eliminate the first column corresponding to positions played 
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
#in the dummies procees we loose positions.played, so we add them again:
data_train$Positions.Played = y_train
data_test$Positions.Played = y_test
preProcess_range_model = preProcess(data_train, method='range')
data_train_scaled = predict(preProcess_range_model, newdata = data_train)
data_test_scaled <-predict(preProcess_range_model, data_test)
#Some libraries that I may use: not all of them were neccesary at the end
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
#Hiper parameter tunning for KNN:  
k_values <- c(1,2,5,7,10:20)
list_accuracy <- numeric(length(k_values))
folds=5
set.seed(123)
fold_indices <- createFolds(data_train_scaled$Positions.Played, k = folds)
#we just created 5 lists of indices, each with 20% of the train data. Nos, we 
#train, for each of them, the lasting 4, and use the other for testing, to finnally get 
#the average. That is, cross validation with 5 folds. We do it for each possible K, number of neighbours
for (K in k_values) {
  accuracies <- numeric(folds)
  
  for (i in 1:folds) {
    
    test_indices <- fold_indices[[i]]#indices from fold i 
    train_indices <- setdiff(seq_len(nrow(data_train_scaled)), test_indices)#rest of indices
    
    train_train <- data_train_scaled[train_indices, ]
    train_test <- data_train_scaled[test_indices, ]
    
    # We predict with KNN using K neighbours
    knn_pred <- knn(train = train_train[,-56], test = train_test[,-56], cl = train_train$Positions.Played, k = K)
    
    #Compute precision with the confucion matrix: 
    confusion_matrix_knn <- confusionMatrix(knn_pred, train_test$Positions.Played)
    accuracies[i] <- confusion_matrix_knn$overall["Accuracy"]
  }
  
  # Mean of the 5 results obtained for each fold:
  list_accuracy[K] <- mean(accuracies)
}

#Find K that led to the best accuracy:
best_k <- which.max(list_accuracy)
print(best_k)
print(list_accuracy[best_k])
#Use the best k for predicting our testing partition with our training partition: 
knn_pred_top=knn(train =data_train_scaled[,-56], test = data_test_scaled[,-56], cl = data_train_scaled$Positions.Played,k=best_k)
confusion_matrix_knn_top=confusionMatrix(knn_pred_top, data_test_scaled$Positions.Played)
#Compute accuracy and the macro-measures described in the project with KNN predictions: 
Accuracy=confusion_matrix_knn_top$overall["Accuracy"]
class_metrics_knn_top <- confusion_matrix_knn_top$byClass
recall_per_class <- class_metrics_knn_top[, "Sensitivity"]
spe_per_class <- class_metrics_knn_top[, "Specificity"]
prec_pred_per_class <- class_metrics_knn_top[, "Precision"]
F1 <- class_metrics_knn_top[, "F1"]
macro_avg_recall <- mean(recall_per_class, na.rm = TRUE)
#print the results: 
cat('Accuracy=',Accuracy,'\n')
cat('Sensitivity=',mean(recall_per_class,na.rm = TRUE),'\n')
cat('Specificity',mean(spe_per_class,na.rm = TRUE),'\n')
cat('Precision',mean(prec_pred_per_class,na.rm = TRUE),'\n')
cat('F1 score',mean(F1,na.rm = TRUE))

#Now, with Decision Tress

max_depth_values <- seq(1, 20, by = 2) #values of max depth
min_split_values <- seq(10, 50, by = 10) #values of min split 

results <- data.frame(max_depth = numeric(),
                      min_split = numeric(),
                      accuracy = numeric())

#Again, cross validation with 5 folds: 
set.seed(123)
folds <- createFolds(data_train_scaled$Positions.Played, k = 5)
for (max_depth in max_depth_values) {
  for (min_split in min_split_values) {
    accuracies <- numeric(length(folds))
    
    for (i in 1:length(folds)) {
      
      test_indices <- folds[[i]]
      train_indices <- setdiff(seq_len(nrow(data_train_scaled)), test_indices)
      train_train <- data_train_scaled[train_indices, ]
      train_test <- data_train_scaled[test_indices, ]
      
      #Train the model with the 4 folds
      dt_model <- rpart(Positions.Played ~ ., data = train_train,
                        control = rpart.control(minsplit = min_split, maxdepth = max_depth))
      
      # Predict with the fold i
      predictions <- predict(dt_model, train_test, type = "class")
      
      # Compute accuracy:
      confusion_matrix_dt <- confusionMatrix(predictions, train_test$Positions.Played)
      accuracies[i] <- confusion_matrix_dt$overall["Accuracy"]
    }
    
    
    mean_accuracy <- mean(accuracies)
    
    #We add the results to the data frame: 
    results <- rbind(results, data.frame(max_depth = max_depth, min_split = min_split, accuracy = mean_accuracy))
  }
}

# Find the best parameters: 
best_index <- which.max(results$accuracy)
best_max_depth <- results$max_depth[best_index]
best_min_split <- results$min_split[best_index]
best_accuracy <- results$accuracy[best_index]
print(paste("Best max_depth:", best_max_depth))
print(paste("Best min_split:", best_min_split))
print(paste("Best accuracy:", best_accuracy))

#use the best to predict our testing partition: 

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
macro_avg_recall <- mean(recall_per_class, na.rm = TRUE)
cat('Accuracy=',Accuracy,'\n')
cat('Sensitivity=',mean(recall_per_class,na.rm = TRUE),'\n')
cat('Specificity',mean(spe_per_class,na.rm = TRUE),'\n')
cat('Precision',mean(prec_pred_per_class,na.rm = TRUE),'\n')
cat('F1 score',mean(F1,na.rm = TRUE))

#With Random Forest: 

#Here instead of cross validation (can give problems when having classes in some folds that don't exist in others),
#we create 5 different partitions of our data train, and for each of them we train/test with the parameters and 
#compute the average. Similar to cross validation only know the 5 partitions aren't disjoint. The rest is the same: 
n_tree_values <- c(500,1000,1500)#number of trees
m_try_values <- seq(5,35, by =4)#number of variables selected
results <- data.frame(n_tree = numeric(),
                      m_try = numeric(),
                      accuracy = numeric())
partitions=5
for (n_tree in n_tree_values) {
  for (m_try in m_try_values) {
    accuracies <- numeric(partitions)
    
    for (i in 1:partitions) {
      
      Index <- createDataPartition(data_train_scaled$Positions.Played, p = 0.7, list = FALSE)
      train_train <- data_train_scaled[Index, ]
      train_test <- data_train_scaled[-Index, ]
      
      
      rf_model <- randomForest(Positions.Played ~ ., data = train_train, ntree = n_tree, mtry = m_try)
      
      
      predictions_rf <- predict(rf_model, train_test)
      
      
      confusion_matrix_rf <- confusionMatrix(predictions_rf, train_test$Positions.Played)
      accuracies[i] <- confusion_matrix_rf$overall["Accuracy"]
    }
    
    
    mean_accuracy <- mean(accuracies)
    
    
    results <- rbind(results, data.frame(n_tree = n_tree, m_try = m_try, accuracy = mean_accuracy))
  }
}


best_index <- which.max(results$accuracy)
best_n_tree <- results$n_tree[best_index]
best_m_try <- results$m_try[best_index]
best_accuracy <- results$accuracy[best_index]

print(paste("Best n_tree:", best_n_tree))
print(paste("Best m_try:", best_m_try))
print(paste("Best accuracy:", best_accuracy))
#Now we use the best parameters for our partition: 

rf_model_top <- randomForest(Positions.Played ~ ., data =data_train_scaled,ntree=best_n_tree,mtry=best_m_try)
predictions_rf_top <- predict(rf_model_top, data_test_scaled)
confusion_matrix_rf_top=confusionMatrix(predictions_rf_top, data_test_scaled$Positions.Played)
Accuracy=confusion_matrix_rf_top$overall["Accuracy"]
class_metrics_rf_top <- confusion_matrix_rf_top$byClass
recall_per_class <- class_metrics_rf_top[, "Sensitivity"]
spe_per_class <- class_metrics_rf_top[, "Specificity"]
prec_pred_per_class <- class_metrics_rf_top[, "Precision"]
F1 <- class_metrics_rf_top[, "F1"]
macro_avg_recall <- mean(recall_per_class, na.rm = TRUE)
cat('Accuracy=',Accuracy,'\n')
cat('Sensitivity=',mean(recall_per_class,na.rm = TRUE),'\n')
cat('Specificity',mean(spe_per_class,na.rm = TRUE),'\n')
cat('Precision',mean(prec_pred_per_class,na.rm = TRUE),'\n')
cat('F1 score',mean(F1,na.rm = TRUE))

#Calculating percentage of labels covered with RF: 
real_levels_positions_played <- levels(droplevels(data_test_scaled$Positions.Played))
coverage <- length(unique(predictions_rf_top)) / length(unique(data_test_scaled$Positions.Played))
print(coverage)
length(unique(predictions_rf_top))
length(unique(data_test_scaled$Positions.Played))
#Same with KNN
coverage <- length(unique(knn_pred_top)) / length(unique(data_test_scaled$Positions.Played))
print(coverage)
length(unique(knn_pred_top))
length(unique(data_test_scaled$Positions.Played))
#Calculate the  classes with the highest number of accurately predicted observation with RF
aciertos=diag(table(predictions_rf_top,data_test_scaled$Positions.Played))
aciertos2=diag(confusion_matrix_rf_top$table)#the two ways are equivalent
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
true_df <- as.data.frame(table(data_test_scaled$Positions.Played))
pred_df_rf <- as.data.frame(table(predictions_rf_top))

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

#### CHAPTER 3

#prepare data to use the libraries. We need one binary column for each position, having for each observation 
#ones/TRUE in the columns corresponding to positions where they can play, and 0/FALSE in the rest. 
top_observations_ready= rbind(data_train_scaled, data_test_scaled)#we combine all the data again
top_observations_ready$Positions.Played=as.character(top_observations_ready$Positions.Played)
positions_list <- strsplit(top_observations_ready$Positions.Played, ",")
unique_positions <- unique(unlist(positions_list))
#for each observation, we put TRUE in positions where he can play:   
new_data <- data.frame(matrix(FALSE, nrow = nrow(top_observations_ready), ncol = length(unique_positions)))
colnames(new_data) <-(unique_positions)
for (i in 1:nrow(top_observations_ready)) {
  new_data[i, unique_positions %in% unlist(positions_list[i])] <- TRUE
}

binary_data <- cbind(top_observations_ready, new_data)
#now we eliminate positions.played, we don't need it with the binary columns: 
binary_data=binary_data[,-56]
#We have TRUE/FALSE with the previous code, but in our case, ones and ceros is better for using library utiml 
binary_data[, 56:69] <- lapply(binary_data[, 56:69], as.integer)
#Library utiml includes library mldr: we use it to get the data and see the measures described in the project
#We have to indicate the labels (positions), that is, our last 14 columns, and the regressors, the first 55, 
#of the new binary data we have created,
#where the first 70% of the data correspond  to training partition and the rest to testing:
library(utiml)
data_mldr=mldr_from_dataframe(binary_data,c(56:69),c(1:55))
summary(data_mldr)
#Plot with the number of observations that have 1, 2 and three labels, in the density subsection: 
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
colors <-rainbow(length(positions))
ggplot(data_plot, aes(x = Position, y = IRLbl, fill = Position)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(title = "IRLbl Values for Different Positions", x = "Position", y = "IRLbl") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()  
  ) 
#Plot with the scumble function depending on the constant that 
#represents the difference in IRLbl values between the two labels: 
func <- function(C) {
  1 - 2 * sqrt(C) / (1 + C)
}
C_values <- seq(0, 10, by = 0.1) 
values <- func(C_values) 

df <- data.frame(C = C_values, Value = values)
ggplot(df, aes(x = C, y = Value)) +
  geom_line(color = "red") +labs(
    x = "C",
    y = "SCUMBLE"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = 0:10)
#Circular diagram with the links between positions that appear together: 
plot(data_mldr, type = "LC")

##Now, the prediction part: 
#The traditional simple classification using the variable Best.Position 
#to evaluate the approach of predicting just one position is at the end of this R code
#we create MLDR objects separating the training part (the first 11649 observations (recall we used cbin(train,test)
#for creating binary data) and the rest for testing so we keep the same partition)
#the firtst 70% (11649) observations, were the training partition used before: 
data_mldr_train=mldr_from_dataframe(binary_data[c(1:11649),],c(56:69),c(1:55))
data_mldr_test=mldr_from_dataframe(binary_data[c(11650:16478),],c(56:69),c(1:55))

#Label propagation with hiper parameter tunning:

#We use function lp to tranform the mldr object into a dataset with the same regresor columns,
# but instead of 14 binary columns, just one with the labelset of the combination of positions for each player: 
datalp=mldr_transform(data_mldr, type = "LP")
datalp$classLabel=factor(datalp$classLabel)
#Now, we separare the training and testing partitions: 
data_train_lp <- datalp[c(1:11649), ]
data_test_lp <- datalp[c(11650:16478), ]

#HIPER PARAM TUNNING WITH RANDOM FOREST and 1500 trees. Same process as in previous chapter:
#but now, we compute the best combination of parameters that gives the best of each measure 
#that is, best combination for accuracy, best for hamming loss, etc. At the end, we see that 
#all the combinations give a very similar result in all measures, so it wasn't really neccesary,
#we could just have maximized one of them
m_try_values <- seq(5,35, by = 4)#hasta 30 lo hago

results_2 <- data.frame(m_try = numeric(),
                        subset_accuracy = numeric(),hamming_loss,precision,recall)

partitions=5
for (m_try in m_try_values) {
  subset_accuracies <- numeric(partitions)
  hamming_losses <- numeric(partitions)
  precisions <- numeric(partitions)
  recalls <- numeric(partitions)
  
  for (i in 1:partitions) {
    
    Index <- createDataPartition(data_train_lp$classLabel, p = 0.7, list = FALSE)
    train_train <- data_train_lp[Index, ]
    train_test <- data_train_lp[-Index, ]
    train_test_mldr=data_mldr_train[-Index]
    
    
    rf_model <- randomForest(classLabel ~ ., data = train_train, ntree = 1500, mtry = m_try)
    predictions_rf <- predict(rf_model, train_test)
    #in order to compute the multilabel performance measures, we use the function mldr_evalute
    #it takes our mldr_object with the test data, and a matrix of 14 columns with the predictions
    #so we need to tranforms our predictions into a dataset and then into matrix: 
    #it may be that the predictions are directly a dataframe, but the following code standarizes 
    #the process so it is the same from now on: 
    predicciones_lista <- strsplit(as.character(predictions_rf ), "")
    predicciones_matriz <- do.call(rbind, predicciones_lista)
    predicciones_df <- as.data.frame(predicciones_matriz, stringsAsFactors = FALSE)
    colnames(predicciones_df) <- c("RW", "CF", "ST", "LW", "CB", "LM", "CDM", "CM", "CAM", "LB", "RB", "RM", "LWB", "RWB")
    predicciones_df <- predicciones_df %>% mutate(across(everything(), as.numeric))
    #now, we compute the meaures. mldr_evaluate gives a lot of messages while computing, that are not useful,
    #so we supress them: 
    results <- suppressMessages(suppressWarnings(mldr_evaluate(train_test_mldr, as.matrix(predicciones_df))))
    subset_accuracies[i] <- results$subset_accuracy
    hamming_losses[i]=results$hamming_loss
    precisions[i]=results$precision
    recalls[i]=results$recall
    
  }
  
  
  mean_subset_accuracy <- mean(subset_accuracies)
  mean_hamming_loss=mean(hamming_losses)
  mean_precision=mean(precisions)
  mean_recall=mean(recalls)
  
  
  
  results_2 <- rbind(results_2, data.frame(m_try =m_try, subset_accuracy =mean_subset_accuracy,hamming_loss=mean_hamming_loss,precision=mean_precision,recall=mean_recall))
}


# Find best combination for each measure: 
best_index_subset_accuracy <- which.max(results_2$subset_accuracy)
best_index_hamming_loss <- which.max(results_2$hamming_loss)
best_index_precision <- which.max(results_2$precision)
best_index_recall <- which.max(results_2$recall)
best_m_try_subset_accuracy <- results_2$m_try[best_index_subset_accuracy]

best_m_try_hamming_loss <- results_2$m_try[best_index_hamming_loss]

best_m_try_precision <-results_2$m_try[best_index_precision]

best_m_try_recall <- results_2$m_try[best_index_recall]


cat('best_m_try_subset_accuracy',best_m_try_subset_accuracy)


cat('best_m_try_hamming_loss',best_m_try_hamming_loss)


cat('best_m_try_precision ',best_m_try_precision )

cat('best_m_try_recall',best_m_try_recall)

#now we apply each of the combinations to our particular train/test partition and see the results: 

rf_model_accuracy <- randomForest(classLabel ~ ., data = data_train_lp, ntree = 1500, mtry = best_m_try_subset_accuracy)
rf_model_hamming_loss <- randomForest(classLabel ~ ., data = data_train_lp, ntree = 1500, mtry = best_m_try_hamming_loss)
rf_model_precision <- randomForest(classLabel ~ ., data = data_train_lp, ntree = 1500, mtry = best_m_try_precision )
rf_model_recall <- randomForest(classLabel ~ ., data = data_train_lp, ntree = 1500, mtry = best_m_try_recall)

predictions_rf_accuracy <- predict(rf_model_accuracy, data_test_lp)
predictions_rf_hamming_loss <- predict(rf_model_hamming_loss, data_test_lp)
predictions_rf_precision <- predict(rf_model_precision, data_test_lp)
predictions_rf_recall <- predict(rf_model_recall, data_test_lp)

#convert to dataframe and then to matrix in mldr_evaluate. We were a little repetivie, some of these steps 
#may be avoided and still get the same result: 

predicciones_lista_accuracy <- strsplit(as.character(predictions_rf_accuracy), "")
predicciones_lista_hamming_loss <- strsplit(as.character(predictions_rf_hamming_loss), "")
predicciones_lista_precision <- strsplit(as.character(predictions_rf_precision), "")
predicciones_lista_recall <- strsplit(as.character(predictions_rf_recall), "")

#matrix: 
predicciones_matriz_accuracy <- do.call(rbind, predicciones_lista_accuracy)
predicciones_matriz_hamming_loss <- do.call(rbind, predicciones_lista_hamming_loss)
predicciones_matriz_precision <- do.call(rbind, predicciones_lista_precision)
predicciones_matriz_recall <- do.call(rbind, predicciones_lista_recall)


predicciones_df_accuracy <- as.data.frame(predicciones_matriz_accuracy, stringsAsFactors = FALSE)
predicciones_df_hamming_loss <- as.data.frame(predicciones_matriz_hamming_loss, stringsAsFactors = FALSE)
predicciones_df_precision <- as.data.frame(predicciones_matriz_precision, stringsAsFactors = FALSE)
predicciones_df_recall <- as.data.frame(predicciones_matriz_recall, stringsAsFactors = FALSE)

colnames(predicciones_df_precision) <- c("RW", "CF", "ST", "LW", "CB", "LM", "CDM", "CM", "CAM", "LB", "RB", "RM", "LWB", "RWB")

predicciones_df_accuracy <- predicciones_df_accuracy %>% mutate(across(everything(), as.numeric))
predicciones_df_hamming_loss  <- predicciones_df_hamming_loss  %>% mutate(across(everything(), as.numeric))
predicciones_df_precision <- predicciones_df_precision %>% mutate(across(everything(), as.numeric))
predicciones_df_recall <- predicciones_df_recall %>% mutate(across(everything(), as.numeric))

#We see the cardinality of one of these predictions, just out of curiosity to see the mean number of positions
#that we are predicting. It should be more than one to be really distinct from traiditional one-position classifiers
mean(rowSums(predicciones_df_accuracy))
 
#see results of each prediciton
results_accuracy <- suppressMessages(suppressWarnings(mldr_evaluate(data_mldr_test, as.matrix(predicciones_df_accuracy))))
results_hamming_loss <- suppressMessages(suppressWarnings(mldr_evaluate(data_mldr_test, as.matrix(predicciones_df_hamming_loss))))
results_precision <- suppressMessages(suppressWarnings(mldr_evaluate(data_mldr_test, as.matrix(predicciones_df_precision))))
results_recall <- suppressMessages(suppressWarnings(mldr_evaluate(data_mldr_test, as.matrix(predicciones_df_recall))))
results_accuracy
results_hamming_loss
results_precision
results_recall
#now we check number of correct predicted positions in CF; LWB, and RWB. This is metioned in the section of 
#multilabel balancing techniques
sum(predicciones_df_accuracy$V2==1 & binary_data[11650:16478,]$CF==1)
sum(binary_data[11650:16478,]$CF==1)
#LWB
sum(predicciones_df_accuracy$V13==1 & binary_data[11650:16478,]$LWB==1)
sum(binary_data[11650:16478,]$LWB==1)
#RWB
sum(predicciones_df_accuracy$V14==1 & binary_data[11650:16478,]$RWB==1)
sum(binary_data[11650:16478,]$RWB==1)
#ALTERNATIVE: WITH LIBRARY UTIML, LP CAN BE DONE AUTOMATICCALY OVER MLDR OBJECTS: 
model_automaticlp <- lp(data_mldr_train, "RF")
predictions_lp <- predict(model_automaticlp, data_mldr_test)
#with this library we can directly use multilabel_evaluate, from utiml, that takes the mldr result, the object obtained 
#when predicting with autimatic models
results <- multilabel_evaluate(data_mldr_test, predictions_lp)
round(results, 4)
#the results are similar to those we obtained manually. 

#BINARY RELEVANCE:
#The following code creates 14 datasets, databr[[1]],...,databr[[14]] 
#each one with a column corresponding to each position
databr <- mldr_transform(data_mldr, type = "BR")
data_br_train=mldr_transform(data_mldr_train, type = "BR")
data_br_test=mldr_transform(data_mldr_test, type = "BR")
#we define values for hiper parameter tunning: 
k_values <-c(1,5,10, 15, 20)
n_tree_values <-c(1500)
m_try_values <-c(5,10,17,25,35)
#now we look for the best model for predicting each of the 14 columns, with KNN and RF.
#we first create the function that implements the hyper parameter tunning: 
results <- list()
tune_and_evaluate <- function(train_data, response_col_name, k_values, n_tree_values, m_try_values, folds = 5) {
  
  set.seed(123)
  fold_indices <- createFolds(train_data[,56], k = folds, list = TRUE)
  
  
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
        
        rf_model <- randomForest(formula,data = train_train,ntree = n_tree, mtry = m_try)
        
        
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
#now, we conduct the tunning for each of the 14 datasets: 
for (i in 1:length(data_br_train)) { 
  dataset <- data_br_train[[i]]#i^th dataset 
  response_col_name <- names(dataset)[56]  # name of the position predicted
  dataset[,response_col_name]=as.factor(binary_data[c(1:11649),response_col_name]) 
  print(i) #just to know the status of the process and how much time is left to finish
  
  
  result <- tune_and_evaluate(dataset,response_col_name, k_values, n_tree_values, m_try_values)
  
  
  results[[response_col_name]] <- result
}
result
for (name in names(results)) {
  cat("Results for", name, ":\n")
  print(results[[name]])
}
#We obtained columns 1 and 14 with KNN, K=15, and 2 to 12 with RF; with he 12 parameters written in m_try_values
n_tree_value <-1500
m_try_values <- c(5,10,5,17,25,35,25,25,10,25,35,25)
K=15
#now, we predict each column with the model and parameter obtained, and keep the predictions 
#for combining them aftewards
predictions <- data.frame(ID = 1:nrow(data_br_test[[1]]))
for (i in 1){
  #KNN
  train_br <- data_br_train[[i]]
  position <- names(train_br)[56]
  test_br=data_br_test[[i]]
  knn_pred <- knn(train = train_br[,-56], test = test_br[,-56], cl = train_br[,56], k = K)
  predictions[,position] <-knn_pred
}
for (i in 2:13) { 
  train_br <- data_br_train[[i]]
  test_br=data_br_test[[i]]
  position <- names(train_br)[56]
  
  train_br[,position]=as.factor(train_br[,position])
  formula=as.formula(paste(as.factor(position), "~ ."))
  
  rf_model <- randomForest(formula,data = train_br, 
                           ntree =n_tree_value, mtry = m_try_values[i-1])#we want indices 1 to 12, not 2 to 13
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
#eliminate first column of dataframe with predictions, corresponding to ID
final_predictions=predictions[,-1]
#prepare for using mldr_evaluate as always: 
final_predictions<-final_predictions %>% mutate(across(everything(), as.numeric))
final_predictions=final_predictions-1 #me salían entre 1 y 2 
predictions_matrix <- as.matrix(final_predictions)
colnames(predictions_matrix) <- NULL
mldr_evaluate(data_mldr_test, predictions_matrix)

#As with LP, we can also do it automatically with function br, giving the mldr_object to train. We use it
#indicating that each column is predicted with random forest: 
model <- br(data_mldr_train, "RF",probability=FALSE)
predictions_br <- predict(model, data_mldr_test,probability=FALSE)
#with this library we can directly use multilabel_evaluate, from utiml, that takes the mldr result, the object obtained 
#when predicting with autimatic models
results <- multilabel_evaluate(data_mldr_test, predictions_br)
round(results,4)
#This is the best prediction obtained with binary relevance and the one we used in the project. 
#We should check that we don't have players without any predicted position:
any(rowSums(predictions_br)==0)
#predictions_br is an mlresult. The second colomn is CF, 13 is LWB and 14 RWB
#we see the number of correct predictions in CF,LWB and RWB, for the resampling part: 
#cf
sum(predictions_br[,2] & binary_data[11650:16478,]$CF==1)
sum(binary_data[11650:16478,]$CF==1)
#LWB
sum(predictions_br[,13]==1 & binary_data[11650:16478,]$LWB==1)
sum(binary_data[11650:16478,]$LWB==1)
#RWB
sum(predictions_br[,14]==1 & binary_data[11650:16478,]$RWB==1)
sum(binary_data[11650:16478,]$RWB==1)

#Now, we use MLKNN described in the project. We can use the function mlknn included in the library utiml.
#These models are expensive in time and memory. For this reason, we directly used them for predicting our test 
#partitions with different values. The resutls for K=5,10,15 and 20 were very similar, and better than K=1,2
model_1=mlknn(data_mldr_train,k=1)
model_2=mlknn(data_mldr_train,k=2)
model_5=mlknn(data_mldr_train,k=5)
model_10=mlknn(data_mldr_train,k=10)
model_15=mlknn(data_mldr_train,k=15)
model_20=mlknn(data_mldr_train,k=20)
predictions_1 <- predict(model_1, data_mldr_test)
#with this library we can directly use multilabel_evaluate, from utiml, that takes the mldr result, the object obtained 
#when predicting with autimatic models
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

# Resampling algorithms: 

library(mldr.resampling) #this is the library with the methods
data_mldr_train_lpros=LPROS(data_mldr_train,P =25)#LPROS on training data, 25% 
data_mldr_train_lprus=LPRUS(data_mldr_train,P=25)#LPRUS on training data, 25%
data_mldr_train_mlros=MLROS(data_mldr_train,P=25)#MLROS on training data, 25%, this method gives some NAs
data_mldr_train_mlrus=MLRUS(data_mldr_train,P=25)#MLRuS on training data, 25%
data_mldr_train_remedial=REMEDIAL(data_mldr_train)#REMEDIAL
data_mldr_train_remedial_lpros=LPROS(data_mldr_train_remedial,P=25)#REMEDIAL+LPROS
summary(data_mldr_train)#see the measures of training data original and with the resampling: 
summary(data_mldr_train_lpros)
summary(data_mldr_train_lprus)
summary(data_mldr_train_mlros) 
summary(data_mldr_train_mlrus)
summary(data_mldr_train_remedial)
summary(data_mldr_train_remedial_lpros)
#we have some probelms predicting directly, because it detects new classes
#so, we use the new training data of each resmapling method, and put it togetehr with the test 
#data to create the new datasets, and repeat the process of prediction creating again the mldr objects
#using this new datasets: 
data_completo_lpros=rbind(data_mldr_train_lpros$dataset,data_mldr_test$dataset)
data_completo_lpros=data_completo_lpros[,-c(70,71)]#70 and 71 are SCUMBLE and another measure, not data
data_completo_lprus=rbind(data_mldr_train_lprus$dataset,data_mldr_test$dataset)
data_completo_lprus=data_completo_lprus[,-c(70,71)]
#MLROS we eliminate the NAs obtained  
data_completo_mlros=rbind(na.omit(data_mldr_train_mlros$dataset[,-c(70,71)]),data_mldr_test$dataset[,-c(70,71)])
data_completo_mlrus=rbind(data_mldr_train_mlrus$dataset,data_mldr_test$dataset)
data_completo_mlrus=data_completo_mlrus[,-c(70,71)]

data_completo_remedial=rbind(data_mldr_train_remedial$dataset,data_mldr_test$dataset)
data_completo_remedial=data_completo_remedial[,-c(70,71)]

data_completo_remedial_lpros=rbind(data_mldr_train_remedial_lpros$dataset,data_mldr_test$dataset)
data_completo_remedial_lpros=data_completo_remedial_lpros[,-c(70,71)]
#Now with the new datasets, mldr objects are created: 
#LPROS: 
data_completo_lpros_mldr=mldr_from_dataframe(data_completo_lpros,c(56:69),c(1:55))
n=nrow(data_completo_lpros)
data_mldr_train_delcompleto_lpros=mldr_from_dataframe(data_completo_lpros[c(1:(n-4829)), ],c(56:69),c(1:55))
data_mldr_test_delcompleto_lpros=mldr_from_dataframe(data_completo_lpros[-c(1:(n-4829)), ],c(56:69),c(1:55))
#LPRUS
data_completo_lprus_mldr=mldr_from_dataframe(data_completo_lprus,c(56:69),c(1:55))
n=nrow(data_completo_lprus)
data_mldr_train_delcompleto_lprus=mldr_from_dataframe(data_completo_lprus[c(1:(n-4829)), ],c(56:69),c(1:55))
data_mldr_test_delcompleto_lprus=mldr_from_dataframe(data_completo_lprus[-c(1:(n-4829)), ],c(56:69),c(1:55))
#MLROS
data_completo_mlros_mldr=mldr_from_dataframe(data_completo_mlros,c(56:69),c(1:55))
n=nrow(data_completo_mlros)
data_mldr_train_delcompleto_mlros=mldr_from_dataframe(data_completo_mlros[c(1:(n-4829)), ],c(56:69),c(1:55))
data_mldr_test_delcompleto_mlros=mldr_from_dataframe(data_completo_mlros[-c(1:(n-4829)), ],c(56:69),c(1:55))
#MLRUS
data_completo_mlrus_mldr=mldr_from_dataframe(data_completo_mlrus,c(56:69),c(1:55))
n=nrow(data_completo_mlrus)
data_mldr_train_delcompleto_mlrus=mldr_from_dataframe(data_completo_mlrus[c(1:(n-4829)), ],c(56:69),c(1:55))
data_mldr_test_delcompleto_mlrus=mldr_from_dataframe(data_completo_mlrus[-c(1:(n-4829)), ],c(56:69),c(1:55))
#REMEDIAL
data_completo_remedial_mldr=mldr_from_dataframe(data_completo_remedial,c(56:69),c(1:55))
n=nrow(data_completo_remedial)
data_mldr_train_delcompleto_remedial=mldr_from_dataframe(data_completo_remedial[c(1:(n-4829)), ],c(56:69),c(1:55))
data_mldr_test_delcompleto_remedial=mldr_from_dataframe(data_completo_remedial[-c(1:(n-4829)), ],c(56:69),c(1:55))
#REMEDIAL and LPROS after REMEDIAL:
data_completo_remedial_lpros_mldr=mldr_from_dataframe(data_completo_remedial_lpros,c(56:69),c(1:55))
n=nrow(data_completo_remedial_lpros)
data_mldr_train_delcompleto_remedial_lpros=mldr_from_dataframe(data_completo_remedial_lpros[c(1:(n-4829)), ],c(56:69),c(1:55))
data_mldr_test_delcompleto_remedial_lpros=mldr_from_dataframe(data_completo_remedial_lpros[-c(1:(n-4829)), ],c(56:69),c(1:55))
#now, we recompute the summaries to confirm that the results are the same: 
summary(data_mldr_train_delcompleto_lpros)
summary(data_mldr_train_lpros)

summary(data_mldr_train_delcompleto_lprus)
summary(data_mldr_train_lprus)

summary(data_mldr_train_delcompleto_mlrus)
summary(data_mldr_train_mlrus)

summary(data_mldr_train_delcompleto_remedial)
summary(data_mldr_train_remedial)
#we can now compute the measures of mlros too, e don't have nas  
summary(data_mldr_train_delcompleto_mlros)

summary(data_mldr_train_delcompleto_remedial_lpros)
summary(data_mldr_train_remedial_lpros)
#circular plot of the training partition after remedial: 
plot(data_mldr_train_delcompleto_remedial, type = "LC")
#Plots of frequencies LPROS vs partition
labels <- c("RW", "CF", "ST", "LW", "CB", "LM", "CDM", "CM", "CAM", "LB", "RB", "RM", "LWB", "RWB")
train_counts <- data_mldr_train_delcompleto_lpros$labels$count
full_counts <- data_mldr_train$labels$count

train_df <- data.frame(Label = labels, Count = train_counts)
full_df <- data.frame(Label = labels, Count = full_counts)

rainbow_colors <- rainbow(length(labels))

plot1 <- ggplot(train_df, aes(x = Label, y = Count, fill = Label)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rainbow_colors) +
  ylim(0, 4000) +
  labs(title = "Frecuencia de Etiquetas - Conjunto de Entrenamiento") +
  theme_minimal() +
  theme(legend.position = "none")

plot2 <- ggplot(full_df, aes(x = Label, y = Count, fill = Label)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = rainbow_colors) +
  ylim(0, 4000) +
  labs(title = "Frecuencia de Etiquetas - Conjunto Completo") +
  theme_minimal() +
  theme(legend.position = "none")
plot1
plot2

#now the predictions: is exactly the same process as before
#LP predicitons: 
#we use directly 1500 trees, and mtry as defaul as we saw the restuls were almost the same for different mtry values: 
#lpros 
model_lpros_lp <- lp(data_mldr_train_delcompleto_lpros, "RF")
predictions_lpros_lp <-predict(model_lpros_lp, data_mldr_test_delcompleto_lpros,prob=FALSE)
results_lp_lpros <- multilabel_evaluate(data_mldr_test_delcompleto_lpros, predictions_lpros_lp)
round(results_lp_lpros,4)
#now we see the number of correct predictions in CF,LWB and RWb
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
#we see the number of correct predictions in CF,LWB and RWB
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

#BINARY RELEVANCE
#lpros 
model_lpros_br <- br(data_mldr_train_delcompleto_lpros, "RF")
predictions_lpros_br <-predict(model_lpros_br, data_mldr_test_delcompleto_lpros,prob=FALSE)
results_br_lpros <- multilabel_evaluate(data_mldr_test_delcompleto_lpros, predictions_lpros_br)
round(results_br_lpros,4)

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

#REMEDIAL+LPROS
model_remedial_lpros_br <- br(data_mldr_train_delcompleto_remedial_lpros, "RF")
predictions_remedial_lpros_br <-predict(model_remedial_lpros_br, data_mldr_test_delcompleto_remedial,prob=FALSE)
results_br_remedial_lpros <- multilabel_evaluate(data_mldr_test_delcompleto_remedial_lpros, predictions_remedial_lpros_br)
round(results_br_remedial_lpros,4)



# Simple classificaiton approach
#PREDICT JUST BEST.POSITION, AS TRADITIONALLY DONE, TO COMPARE. It is just repeating what we did in the chapter 2 but 
#with this new variable instead of positions.played. We will use random forest with 1500 trees and KNN with K=20
data=read.csv('Fifa 23 Players Data.csv')
data<- data[data$Best.Position != 'GK', ]
data=data[,-c(1,2,3:5,6,8,9,13:22,26:30,68:89)]#6 is positions.player 
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
