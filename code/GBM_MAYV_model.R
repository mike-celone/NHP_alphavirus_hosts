################
# Michael Celone
# Date: DDMMM2023
# Code for analysis of potential primate reservoirs of Alphaviruses
#
# Step 1) Perform a grid-search to determine optimal parameters
# Step 2) Use optimal parameters to run the BRT model
# Step 3) Run the null model to calculate the corrected AUC
#
#################
# Import the clean dataset
prim_data_final <- read.csv("C:/Users/Mike/Dropbox/MAYV_NHP_prediction/prim_data_final_log.csv")

# Load necessary packages
packages <- c("gbm", "caret", "Matrix", "pdp", "caTools", "ROCR", "dplyr", 
              "foreach", "dismo", "tidyr")

sapply(packages, library, character.only = T)

# Save variable names
vars <- colnames(prim_data_final)[-c(1:3)]
label <- "vir_pos"     
species <- "species"

#--Set parameters for grid search--#
eta <- c(0.0001, 0.001, 0.01)         
max_depth <- c(2, 3, 4)               
n.minobsinnode <- c(2, 3, 4, 5)       
k_split <- 0.8                        
nrounds <- 100000                        

# Partition data into Train and Test
dp <- createDataPartition(prim_data_final$vir_pos, p = k_split)[[1]]
train <- prim_data_final[dp, ]
test <- prim_data_final[-dp, ]

# Set up the various parameter combinations using expand.grid function
grid <- expand.grid(shrinkage = eta, 
                    interaction.depth = max_depth, 
                    n.minobsinnode = n.minobsinnode,
                    optimal_trees = 0,    # a place to dump results
                    min_RMSE = 0)

# Set up cores for parallel processing
#detectCores()
#cl <- makeCluster(3, "SOCK")
#registerDoSNOW(cl)

# Grid search
for(i in 1:nrow(grid)) {
  
  # reproducibility
  set.seed(123)

  gbm.tune <- gbm(
        formula = vir_pos ~ . -species,
        distribution = "bernoulli",
        data = train,
        n.trees = nrounds,
        interaction.depth = grid$interaction.depth[i],
        shrinkage = grid$shrinkage[i],
        n.minobsinnode = grid$n.minobsinnode[i],
        bag.fraction = 0.5,
        train.fraction = 0.75,
        n.cores = NULL,
        verbose = TRUE)
  
  # add min training error and trees to grid
  grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

##################
# Code to run bootstrapped GBM models
##################

#Set the optimal parameters determined from a grid search

#---Set parameters---#
nruns <- 1          
eta <- 0.01          
max_depth <- 2             
n.minobsinnode <- 4    
k_split <- 0.8                     
nrounds <- 50000     
cv.folds <- 5        
#-------------------

#Create train/test partitions for each of the model runs
df <- foreach(i = 1:nruns) %do% {
  #Set a seed for each model
  set.seed(i)
  
  dp <- createDataPartition(prim_data_final$vir_pos, 
                            p = k_split)[[1]]
  train <- prim_data_final[dp, ]
  test <- prim_data_final[-dp, ]
  
  list(train, test)
}

#Run the models 
gbm.list <- lapply(1:nruns, function(m) {
  gbm(data = df[[m]][[1]],
      formula = vir_pos ~ . - species - wos_hits,
      distribution = "bernoulli",
      n.trees = nrounds,
      shrinkage = eta,
      cv.folds = cv.folds,
      interaction.depth = max_depth,
      n.minobsinnode = n.minobsinnode,
      bag.fraction = 0.5,
      n.cores = NULL,
      verbose = TRUE)
})

#Optimal number of trees
best.iter <- sapply(gbm.list, 
                    gbm.perf, 
                    method = "cv", 
                    plot.it = F)

#Replace infinity with 0
for(i in 1:nruns) {
  gbm.list[[i]]$var.levels <- lapply(gbm.list[[i]]$var.levels, 
                                     function(x) replace(x, is.infinite(x), 0))
}

#Generate predictions 
predictions <- do.call(rbind, lapply(1:nruns, function(j) {
  data.frame(predictions = predict(gbm.list[[j]], 
                                   newdata = prim_data_final, 
                                   n.trees = best.iter[j], 
                                   type = "response"),
             bootstrap_run = j,
             original_value = prim_data_final[, label],
             species = prim_data_final[, species])}))

output1 <- lapply(1:length(gbm.list), 
                  function(x) 
                    cbind(predict(gbm.list[[x]],
                                  newdata = df[[x]][[1]],
                                  n.trees = best.iter[x],
                                  type = "response"),
                          as.numeric(unlist(df[[x]][[1]][, label]))))

#AUC and RMSE for training data
eval_train <- sapply(output1, 
                     function(x) 
                       caTools::colAUC(x[,1],x[,2]))

rmse_train <- sapply(1:length(gbm.list), 
                     function(x) 
                       Metrics::rmse(actual = df[[x]][[1]][, "vir_pos"],
                                     predicted = predict(gbm.list[[x]],
                                                         newdata = df[[x]][[1]],
                                                         n.trees = best.iter[x],
                                                         type = "response")))

output2 <- lapply(1:length(gbm.list), 
                  function(x) 
                    cbind(
                      predict(gbm.list[[x]],
                              newdata = df[[x]][[2]],
                              n.trees = best.iter[x],
                              type = "response"),
                      as.numeric(unlist(df[[x]][[2]][, label]))))

#AUC and RMSE for testing data
eval_test <- sapply(output2, 
                    function(x) 
                      caTools::colAUC(x[,1],x[,2]))

rmse_test <- sapply(1:length(gbm.list), 
                    function(x) 
                      Metrics::rmse(actual = df[[x]][[2]][, label],
                                    predicted = predict(gbm.list[[x]],
                                                        newdata = df[[x]][[2]],
                                                        n.trees = best.iter[x],
                                                        type = "response")))

#Relative importance scores for the variables
df_importance <- do.call(rbind, 
                         lapply(gbm.list, 
                                function(j) 
                                  t(as.data.frame(summary(j)$rel.inf[match(vars, summary(j)$var)], 
                                                  optional = T))))

colnames(df_importance) <- vars

# Create a dataframe with the variable importance measures
df_imp <- df_importance %>%
  as.data.frame() %>%
  dplyr::select(where(is.numeric)) %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "imp") %>%
  mutate(imp=round(imp,2))

pd_out <- foreach(i = 1:nruns, .combine = "rbind") %do% {
  pd_out <- lapply(vars, function(m) dplyr::mutate(plot.gbm(gbm.list[[i]],
                                                            i.var = m,
                                                            return.grid = T,
                                                            type = "response"),
                                                   variable.name = m,
                                                   effect = "marginal.effect",
                                                   bootstrap_run = i))
  
  pd_out <- do.call(rbind, lapply(pd_out, function(m) {
    colnames(m)[1:2] <- c("x", "yhat")
    m}))
  pd_out
}

out1 <- cbind.data.frame(eta = eta,
                         max_depth = max_depth,
                         n.minobsinnode = n.minobsinnode,
                         n.trees = best.iter,
                         eval_train = eval_train,
                         eval_test = eval_test,
                         rmse_train = rmse_train,
                         rmse_test = rmse_test,
                         bootstrap_run = 1:nruns,
                         df_importance)

final.list <- list(out1, pd_out, predictions)

# Average of predictions across 10 model runs
vir <- combine %>% 
  dplyr::select(iucn2020_binomial, vir_pos)

final_preds <- predictions %>% 
  group_by(species) %>% 
  summarise(mean=mean(predictions)) %>%
  full_join(vir, by=c("species"="iucn2020_binomial"))

############
# Null Model
############

#Create train/test partitions for 10 model runs
df.null <- foreach(i = 1:nruns) %do% {
  #Set a seed for each model
  set.seed(i)
  
  dp <- createDataPartition(prim_data_final$vir_pos, p = k_split)[[1]]
  
  train <- prim_data_final[dp, ]
  train[, label] <- sample(x = train[, label], size = nrow(train), replace = F)
  test <- prim_data_final[-dp, ]
  test[, label] <- sample(x = test[, label], size = nrow(test), replace = F)
  
  list(train, test)
}

#Run the 10 models 
gbm.list.null <- lapply(1:nruns, function(m) {
  gbm(data= df.null[[m]][[1]],
      formula =  vir_pos ~ . - species,
      distribution = "bernoulli",
      n.trees = nrounds,
      shrinkage = eta,
      cv.folds = cv.folds,
      interaction.depth = max_depth,
      n.minobsinnode = n.minobsinnode,
      bag.fraction = 0.5,
      n.cores = NULL,
      verbose = TRUE)
})

#Optimal number of trees
best.iter.null <- sapply(gbm.list.null, gbm.perf, method = "cv", plot.it=F)

#Replace infinity with 0
for(i in 1:nruns) {
  gbm.list.null[[i]]$var.levels <- lapply(gbm.list.null[[i]]$var.levels, 
                                          function(x) replace(x, is.infinite(x), 0))
}

output.null <- lapply(1:length(gbm.list.null), 
                      function(x) cbind(predict(gbm.list.null[[x]],
                                                newdata = df.null[[x]][[2]],
                                                n.trees = best.iter.null[x],
                                                type = "response"),
                                        as.numeric(unlist(df.null[[x]][[2]][, label]))))

#AUC and RMSE for testing data
eval_test.null <- sapply(output.null, 
                         function(x) caTools::colAUC(x[,1],x[,2]))


# Final corrected AUC
mean(eval_test) - (mean(eval_test.null)-0.5)