#Code to run bootstrapped GBM models
#Optimal parameters determined from a grid search

#-Set parameters-#
nruns <- 10
eta <- 0.01          
max_depth <- 2               
n.minobsinnode <- 2       
k_split <- 0.8                        
nrounds <- 100000
cv.folds <- 5
#-----------------#

#Create train/test partitions for 10 model runs
df <- foreach(i = 1:nruns) %do% {
  #Set a seed for each model
  set.seed(i)
  
  dp <- createDataPartition(prim_data_final$Label, p = k_split)[[1]]
  train <- prim_data_final[dp, ]
  test <- prim_data_final[-dp, ]
  
  list(train, test)
}

#Run the 10 models 
gbm.list <- lapply(1:nruns, function(m) {
  gbm(data= df[[m]][[1]],
      formula = Label ~ .,
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
best.iter <- sapply(gbm.list, gbm.perf, method = "cv", plot.it=F)

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
             original_value = prim_data_final[, label])}))

output1 <- lapply(1:length(gbm.list), function(x) cbind(predict(gbm.list[[x]],
                                                                newdata = df[[x]][[1]],
                                                                n.trees = best.iter[x],
                                                                type = "response"),
                                                        as.numeric(unlist(df[[x]][[1]][, label]))))

#AUC and RMSE for training data
eval_train <- sapply(output1, function(x) colAUC(x[,1],x[,2]))

rmse_train <- sapply(1:length(gbm.list), function(x) Metrics::rmse(actual = df[[x]][[1]][, label],
                                                                   predicted = predict(gbm.list[[x]],
                                                                                       newdata = DaFr[[x]][[1]],
                                                                                       n.trees = best.iter[x],
                                                                                       type = "response")))

output2 <- lapply(1:length(gbm.list), function(x) cbind(predict(gbm.list[[x]],
                                                                newdata = DaFr[[x]][[2]],
                                                                n.trees = best.iter[x],
                                                                type = "response"),
                                                        as.numeric(unlist(DaFr[[x]][[2]][, label]))))

#AUC and RMSE for testing data
eval_test <- sapply(output2, function(x) colAUC(x[,1],x[,2]))

rmse_test <- sapply(1:length(gbm.list), function(x) Metrics::rmse(actual = DaFr[[x]][[2]][, label],
                                                                  predicted = predict(gbm.list[[x]],
                                                                                      newdata = DaFr[[x]][[2]],
                                                                                      n.trees = best.iter[x],
                                                                                      type = "response")))

#Relative importance scores for the variables
df_importance <- do.call(rbind, 
                         lapply(gbm.list, 
                                function(j) t(as.data.frame(summary(j)$rel.inf[match(vars, summary(j)$var)], 
                                                            optional = T))))

colnames(df_importance) <- vars

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