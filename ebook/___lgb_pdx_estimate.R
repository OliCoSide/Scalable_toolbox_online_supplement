the_pdx_lightgbm_fun <- function(list_data, name, hyperparameter_grid){
  cat("Pdx for scenario: ", name, " \n")
  beg <- Sys.time()
  train_data <- list_data$train
  valid_data <- list_data$valid
  test_data <- list_data$test


  trn <- train_data %>% dplyr::select(X1, X2) %>% mutate(X2 = factor(X2, levels = 1:4))
  train_indices <- sample(seq_len(nrow(trn)), size = 0.8 * nrow(trn))  # Randomly select 80% of the data

  # Split the data
  sub_trn_trn <- trn[train_indices, ]  # 80% for training
  sub_trn_vld <- trn[-train_indices, ] # Remaining 20% for validation
  vld <- valid_data %>% dplyr::select(X1, X2) %>% mutate(X2 = factor(X2, levels = 1:4))
  tst <- test_data %>% dplyr::select(X1, X2) %>% mutate(X2 = factor(X2, levels = 1:4))

  ## convert categorical vars (if applicable)
  conversion <- lightgbm::lgb.convert_with_rules(data = sub_trn_trn, rules = NULL)
  sub_trn_trn_conv <- conversion$data
  sub_trn_vld_conv <-  lightgbm::lgb.convert_with_rules(data = sub_trn_vld, rules = conversion$rules)$data
  trn_conv <- lightgbm::lgb.convert_with_rules(data = trn, rules = conversion$rules)$data
  vld_conv <- lightgbm::lgb.convert_with_rules(data = vld, rules = conversion$rules)$data
  tst_conv <- lightgbm::lgb.convert_with_rules(data = tst, rules = conversion$rules)$data

  ## find the id of categorical vars
  categ_id <- names(conversion$rules) %>%
    sapply(function(n){which(n == names(sub_trn_trn_conv))})

  trn_lgb_dt <- lightgbm::lgb.Dataset(sub_trn_trn_conv %>% as.matrix,
                                      label = train_data$D[train_indices])
  vld_lgb_dt <- lightgbm::lgb.Dataset(sub_trn_vld_conv %>% as.matrix,
                                     label = train_data$D[-train_indices])


  # Function to train a single model and return validation MSE
  train_and_evaluate <- function(params, train_data, valid_data) {
    hyperparameters <- list(
      objective = 'binary',
      learning_rate = params$learning_rate,
      bagging_fraction = params$bagging_fraction,
      max_depth = params$max_depth,
      lambda_l1 = params$lambda_l1,
      lambda_l2 = params$lambda_l2,
      num_threads = detectCores(logical = FALSE)
    )

    lgb_data_list <- list(train = train_data, valid = valid_data)

    model <- lightgbm::lgb.train(
      params = hyperparameters,
      data = train_data,
      nrounds = 1200,
      valids = lgb_data_list,
      early_stopping_rounds = 100,
      verbose = -1
    )

    list(model = model, best_score = model$best_score)
  }

  # Perform grid search
  best_model <- NULL
  best_mse <- Inf
  train_data <- trn_lgb_dt
  valid_data <- vld_lgb_dt

  for (i in 1:nrow(hyperparameter_grid)) {
    cat("hyperparam ", i, "/", nrow(hyperparameter_grid), '\n')
    params <- hyperparameter_grid[i, ]
    result <- train_and_evaluate(params, train_data, valid_data)

    if (result$best_score < best_mse) {
      model <- result$model
      best_mse <- result$best_score
    }
  }


  cat("Best valid binary logloss:", model$best_score, " \n")
  cat("optimal ntree:", model$best_iter, " \n")
  cat("Training time:", Sys.time() - beg, " sec. \n")

  ## predict
  trn_pred <- model$predict(trn %>% as.matrix, rawscore = FALSE)
  vld_pred <- model$predict(vld %>% as.matrix, rawscore = FALSE)
  tst_pred <- model$predict(tst %>% as.matrix, rawscore = FALSE)

  ## Predict prob D = d
  predict_for_lgb <- function(newdata){
    pred <- model$predict(newdata %>%
                            dplyr::select(X1, X2) %>%
                            mutate(X2 = factor(X2, levels = 1:4)) %>%
                            as.matrix, rawscore = FALSE)
    1:nrow(newdata) %>% sapply(function(i){
      d <- newdata$D[i]
      ifelse(d, pred[i], 1 - pred[i])
    })
  }

  preds <- list('train' = trn_pred,
                'valid' = vld_pred,
                'test' = tst_pred)

  # ## If the folder do not exist...
  # if (!dir.exists('preds')) dir.create('preds')
  #
  # ## clean, then save them preds
  # preds %>% jsonlite::toJSON(., pretty = TRUE) %>% write(paste0('preds/', name, '_pdx.json'))

  to_return <- list('preds' = preds,
                    'pred_fun' = predict_for_lgb)
}
