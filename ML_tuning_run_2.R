### Run models for iteration 2

# define the tuning grids 
tune.grid.adaboost <- expand.grid(method = "Adaboost.M1", nIter = c(3000, 5000))
tune.grid.DeepNN <- expand.grid(complexity.multiplier = c(seq(from = 3/16, to = 20/16, by = 1/16)),
                                activation.layer_1 = 'tanh',
                                activation.layer_2 = 'tanh',
                                activation.layer_3 = 'sigmoid',
                                activation.layer_4 = 'tanh',
                                activation.layer_5 = 'relu',
                                activation.layer_6 = 'relu',
                                rate.dropout_1 = c(0.4,0.5,0.6),
                                rate.dropout_2 = c(0.3,0.4,0.5),
                                rate.dropout_3 = c(0.2,0.3),
                                rate.dropout_4 = 0.2,
                                rate.dropout_5 = 0.2,
                                rate.dropout_6 = 0.2,
                                Epochs = 30)

# run the model building function
iter <- 2
deep.iter <- 1 + nrow(tune.grid.DeepNN) / (2 * no.parallel.cores)
ML.methods <- c("adaboost", "DeepNN")

tuneModels(Iter = iter, DeepIter = deep.iter, MLmethods = ML.methods)