################
# Michael Celone
# Date: DDMMM2023
# Code for analysis of Alphavirus primate hosts 
# Using Web of Science hits as the outcome to assess the impact of "studiedness" 
# 
# Model is WoS Hits ~ Primate traits, using a Poisson error distribution
# Step 1) Perform a grid-search to determine optimal parameters
# Step 2) Use optimal parameters to run the BRT model 
#
#################

# Import the clean dataset
prim_data_final <- read.csv("C:/Users/Mike/Dropbox/MAYV_NHP_prediction/prim_data_final_log.csv")

# Set the variable names and specify outcome variable
vars <- colnames(prim_data_final)[-c(1:3)]
label <- "wos_hits"   
species <- "species"

# Perform a grid-search to determine optimal parameters#

# Load necessary packages
packages <- c("gbm", "caret", "Matrix", "pdp", "caTools", "ROCR", "dplyr", 
              "foreach", "dismo", "tidyr")

sapply(packages, library, character.only = T)

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
  
  gbm.tune.wos <- gbm(
    formula = wos_hits ~ . - species - vir_pos,
    distribution = "poisson",
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
df.wos <- foreach(i = 1:nruns) %do% {
  #Set a seed for each model
  set.seed(i)
  
  dp <- createDataPartition(prim_data_final$wos_hits, 
                            p = k_split)[[1]]
  train <- prim_data_final[dp, ]
  test <- prim_data_final[-dp, ]
  
  list(train, test)
}

#Run the models 
gbm.list.wos <- lapply(1:nruns, function(m) {
  gbm(data = df.wos[[m]][[1]],
      formula = wos_hits ~ . - species - vir_pos,
      distribution = "poisson",
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
best.iter.wos <- sapply(gbm.list.wos, 
                        gbm.perf, 
                        method = "cv", 
                        plot.it = T)

#Replace infinity with 0
for(i in 1:nruns) {
  gbm.list.wos[[i]]$var.levels <- lapply(gbm.list.wos[[i]]$var.levels, 
                                     function(x) 
                                       replace(x, is.infinite(x), 0))
}

#Generate predictions 
predictions.wos <- do.call(rbind, lapply(1:nruns, function(j) {
  data.frame(predictions = predict(gbm.list.wos[[j]], 
                                   newdata = prim_data_final, 
                                   n.trees = best.iter.wos[j], 
                                   type = "response"),
             bootstrap_run = j,
             original_value = prim_data_final[, label],
             species = prim_data_final[, species])}))

output1 <- lapply(1:length(gbm.list.wos), 
                  function(x) 
                    cbind(predict(gbm.list.wos[[x]],
                                  newdata = df.wos[[x]][[1]],
                                  n.trees = best.iter.wos[x],
                                  type = "response"),
                          as.numeric(unlist(df.wos[[x]][[1]][, label]))))

# Relative importance scores for the variables
df_importance.wos <- do.call(rbind, 
                         lapply(gbm.list.wos, 
                                function(j) 
                                  t(as.data.frame(summary(j)$rel.inf[match(vars, summary(j)$var)], 
                                                  optional = T))))

colnames(df_importance.wos) <- vars

df_imp.wos <- df_importance.wos %>%
  as.data.frame() %>%
  dplyr::select(where(is.numeric)) %>%
  summarise(across(everything(), mean)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "imp") %>%
  mutate(imp=round(imp,2))
