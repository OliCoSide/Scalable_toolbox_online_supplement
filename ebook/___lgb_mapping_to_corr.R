the_mapping_to_corr_lightgbm_fun <- function(name){

  cat("Mapping to corr. for scenario: ", name, " \n")

  best_fn <- best_lgb[[name]]

  ## All the epsilon_y and best estimates and corrective
  beg_read <- Sys.time()
  eps_y <- jsonlite::fromJSON(paste0('transported/', name,
                                     '_bary_mapping.json'))
  to_train <- data.frame('mu_B' = best_fn$preds$train,
                         'D' = sims[[name]]$D,
                         'mu_C' = eps_y$train)
  to_valid <- data.frame('mu_B' = best_fn$preds$valid,
                         'D' = valid[[name]]$D,
                         'mu_C' = eps_y$valid)

  cat("Data import:", Sys.time() - beg_read, " sec. \n")
  beg_treat <- Sys.time()

  trn <- to_train %>% dplyr::select(mu_B, D) %>% mutate(D = factor(D, levels = 0:1))
  vld <- to_valid %>% dplyr::select(mu_B, D) %>% mutate(D = factor(D, levels = 0:1))

  ## convert categorical vars (if applicable)
  conversion <- lightgbm::lgb.convert_with_rules(data = trn, rules = NULL)

  ## extract data, and apply transformation to other datasets
  trn_conv <- conversion$data
  vld_conv <- lightgbm::lgb.convert_with_rules(data = vld, rules = conversion$rules)$data

  ## find the id of categorical vars
  categ_id <- names(conversion$rules) %>%
    sapply(function(n){which(n == names(trn_conv))})

  ## transform into lgb.dataset (only training and valid)
  ## Attention, si on utilise l'exposition, on devrait l'inclure dans 'init_score' en incluant correctement la fonction de lien
  trn_lgb_dt <- lightgbm::lgb.Dataset(trn_conv %>% as.matrix,
                                      label = to_train$mu_C,
                                      categorical_feature = categ_id)
  vld_lgb_dt <- lightgbm::lgb.Dataset(vld_conv %>% as.matrix,
                                      label = to_valid$mu_C,
                                      categorical_feature = categ_id)

  cat("Data conversion:", Sys.time() - beg_treat, " sec. \n")
  beg_train <- Sys.time()

  ## setup training, and train. Insert optimization of hyperparameters here.
  ## Si tu utilises tweedie, il faut inclure le paramètres p quelque part ici.
  hyperparameters <- list(objective = 'mse',
                          learning_rate = 0.03,
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

  cat("Lgb training :", Sys.time() - beg_train, " sec. \n")
  ## function to call the lgb later... counterfactual?
  predict_for_lgb <- function(newdata){
    conv_data <- lightgbm::lgb.convert_with_rules(newdata %>% mutate(D = factor(D, levels = 0:1)),
                                                  rules = conversion$rules)$data
    model$predict(conv_data %>% as.matrix, rawscore = FALSE)
  }

  return(predict_for_lgb)
}
