the_given_tarif_lightgbm_fun <- function(list_data, name){
  cat("Given tariff for scenario: ", name, " \n")
  beg <- Sys.time()
  
  train_data <- list_data$train
  valid_data <- list_data$valid
  test_data <- list_data$test
  
  trn <- train_data %>% dplyr::select(X1, X2) %>% mutate(X1 = pmin(X1, 6),
                                                            X2 = ifelse(X2 == 1, 3, X2),
                                                            X2 = factor(X2, levels = 2:4))
  vld <- valid_data %>% dplyr::select(X1, X2) %>% mutate(X1 = pmin(X1, 6),
                                                            X2 = ifelse(X2 == 1, 3, X2),
                                                            X2 = factor(X2, levels = 2:4))
  tst <- test_data %>% dplyr::select(X1, X2) %>% mutate(X1 = pmin(X1, 6),
                                                           X2 = ifelse(X2 == 1, 3, X2),
                                                           X2 = factor(X2, levels = 2:4))
  
  ## convert categorical vars (if applicable)
  conversion <- lightgbm::lgb.convert_with_rules(data = trn, rules = NULL)
  
  ## extract data, and apply transformation to other datasets
  trn_conv <- conversion$data
  vld_conv <- lightgbm::lgb.convert_with_rules(data = vld, rules = conversion$rules)$data
  tst_conv <- lightgbm::lgb.convert_with_rules(data = tst, rules = conversion$rules)$data
  
  ## find the id of categorical vars
  categ_id <- names(conversion$rules) %>% 
    sapply(function(n){which(n == names(trn_conv))})
  
  ## transform into lgb.dataset (only training and valid)
  ## Attention, si on utilise l'exposition, on devrait l'inclure dans
  ## 'init_score' en incluant correctement la fonction de lien
  trn_lgb_dt <- lightgbm::lgb.Dataset(trn_conv %>% as.matrix,
                                      label = train_data$Y,
                                      categorical_feature = categ_id)
  vld_lgb_dt <- lightgbm::lgb.Dataset(vld_conv %>% as.matrix,
                                      label = valid_data$Y,
                                      categorical_feature = categ_id)
  
  ## setup training, and train. Insert optimization of hyperparameters here.
  ## Si tu utilises tweedie, il faut inclure le paramètres p quelque part ici. 
  hyperparameters <- list(objective = 'mse',
                          learning_rate = 0.01,
                          bagging_fraction = 0.75,
                          min_data_in_leaf = 32,
                          max_depth = 8,
                          num_threads = parallel::detectCores(logical = FALSE))
  
  ## Train
  lgb_data_list <- list(train = trn_lgb_dt, valid = vld_lgb_dt)
  model <- lightgbm::lgb.train(params = hyperparameters,
                               data = trn_lgb_dt,
                               nrounds = 1e3,
                               valids = lgb_data_list,
                               early_stopping_rounds = 100,
                               verbose = -1)
  
  cat("Best valid mse:", model$best_score, " \n")
  cat("optimal ntree:", model$best_iter, " \n")
  cat("Training time:", Sys.time() - beg, " sec. \n")
  
  ## predict
  trn_pred <- model$predict(trn_conv %>% as.matrix, rawscore = FALSE)
  vld_pred <- model$predict(vld_conv %>% as.matrix, rawscore = FALSE)
  tst_pred <- model$predict(tst_conv %>% as.matrix, rawscore = FALSE)
  
  ## the commercial factor (valid)
  unbalanced_commercial <- function(preds, X1, X2){
    discount_factor_X1 <- ifelse(X1 < 0, 0.95, 1)
    discount_factor_X2 <- ifelse(X2 == 2, 0.95, 1)
    
    return(
      preds * 
        discount_factor_X1 * 
        discount_factor_X2
    )
  }
  
  commercial_pred <- unbalanced_commercial(preds = vld_pred,
                                           X1 = vld_conv$X1,
                                           X2 = vld_conv$X2)
  
  pb_factor <- sum(vld_pred)/sum(commercial_pred)
  
  ## function to call the lgb later... counterfactual?
  predict_for_lgb <- function(newdata){
    conv_data <- lightgbm::lgb.convert_with_rules(newdata %>%
                                                    dplyr::select(X1, X2) %>%
                                                    mutate(X1 = pmin(X1, 6),
                                                           X2 = ifelse(X2 == 1, 3, X2),
                                                           X2 = factor(X2, levels = 2:4)),
                                                  rules = conversion$rules)$data
    the_technical_pred <- model$predict(conv_data %>% as.matrix, rawscore = FALSE)
    
    the_unb_commercial_pred <- unbalanced_commercial(preds = the_technical_pred,
                                                     X1 = newdata$X1,
                                                     X2 = newdata$X2)
    
    pb_factor * the_unb_commercial_pred
    
  }
  
  preds <- list('train' = trn_pred,
                'valid' = vld_pred,
                'test' = tst_pred)
  
  ## If the folder do not exist... 
  if (!dir.exists('preds')) dir.create('preds')
  
  ## save them preds
  preds %>% jsonlite::toJSON(., pretty = TRUE) %>%
    write(paste0('preds/', name, '_given_tarif.json'))
  
  to_return <- list('preds' = preds,
                    'rules' = conversion$rules,
                    'pred_fun' = predict_for_lgb)
}