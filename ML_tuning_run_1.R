### Run models for iteration 1

# define the tuning grids
tune.grid.avNNet <- expand.grid(size = c(2,3,5,10,20), decay = c(0.5,1,3,4,5,10), bag = c(TRUE,FALSE))
tune.grid.svmRdialWeights <- expand.grid(C = c(1,3,5,10,20), Weight = c(0.1,0.5,1,2,3,5,10), sigma = c(0.0005,0.001,0.005,0.01,0.05))
tune.grid.adaboost <- expand.grid(method = "Adaboost.M1", nIter = c(10,30,100,300,1000))
tune.grid.DeepNN <- expand.grid(complexity.multiplier = c(0.3,0.5,0.8,1,1.2),
                                activation.layer_1 = c('tanh', 'relu'),
                                activation.layer_2 = c('tanh', 'relu'),
                                activation.layer_3 = c('sigmoid', 'tanh', 'relu'),
                                activation.layer_4 = 'tanh',
                                activation.layer_5 = 'relu',
                                activation.layer_6 = 'relu',
                                rate.dropout_1 = c(0.3,0.4),
                                rate.dropout_2 = c(0.2,0.3),
                                rate.dropout_3 = 0.2,
                                rate.dropout_4 = 0.2,
                                rate.dropout_5 = 0.2,
                                rate.dropout_6 = 0.2,
                                Epochs = c(30,50))

# run the model building function
iter <- 1
deep.iter <- 1
ML.methods <- c("adaboost", "APACHE", "avNNet", "DeepNN", "glm", "parRF", "svmRadialWeights")
tuneModels(Iter = iter, DeepIter = deep.iter, MLmethods = ML.methods)