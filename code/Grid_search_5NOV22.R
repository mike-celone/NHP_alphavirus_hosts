#-----------------------------------------------------#
#Perform a grid-search to determine optimal parameters#
#-----------------------------------------------------#

#-------------------------------------#
vars <- colnames(prim_data_final)[-17]#
label <- "Label"                      #
eta <- c(0.0001, 0.001, 0.01)         #
max_depth <- c(2, 3, 4)               #
n.minobsinnode <- c(2, 3, 4, 5)       #
k_split <- 0.8                        #
nrounds <- 10000                      #  
#-------------------------------------#

#Partition data into Train and Test
dp <- createDataPartition(prim_data_final$Label, p = k_split)[[1]]
train <- prim_data_final[dp, ]
test <- prim_data_final[-dp, ]

#Set up the various parameter combinations using expand.grid function
grid <- expand.grid(shrinkage = eta, 
                    interaction.depth = max_depth, 
                    n.minobsinnode = n.minobsinnode,
                    optimal_trees = 0,    # a place to dump results
                    min_RMSE = 0)

#Grid search
for(i in 1:nrow(grid)) {
  
  # reproducibility
  set.seed(123)

  gbm.tune <- gbm(
        formula = Label ~ .,
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