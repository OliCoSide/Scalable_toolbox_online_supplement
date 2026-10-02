# Load required libraries
library(evtree)
library(dplyr)
library(logging)
library(foreach)
library(doParallel)
library(rpart)
library(futile.logger)


# Configure logging
basicConfig(level = "INFO")

# Helper function to calculate relaxed accuracy
calculate_relaxed_accuracy <- function(proxy_vuln_t_te, reordered_test_leaf_indices, unique_proxy_t, tolerance = 1) {
  # Create and populate the accuracy table
  accuracy_table <- table(proxy_vuln_t_te, reordered_test_leaf_indices)
  # Sum the trace (main diagonal)
  trace_sum <- sum(diag(accuracy_table))

  # Sum adjacent diagonals (above and below the trace)
  adjacent_sum <- sum(diag(accuracy_table[-1, -ncol(accuracy_table)])) + sum(diag(accuracy_table[-nrow(accuracy_table), -1]))

  # Total number of samples
  total <- sum(accuracy_table)

  # Return strict and relaxed accuracy
  return(list(strict_accuracy = trace_sum / total, relaxed_accuracy = (trace_sum + adjacent_sum) / total))
}

calculate_binary_metrics <- function(y_true, y_pred, protected_label) {

  if(all(is.na(y_pred))){
    return(
      list(
        precision = NA,
        fnr = NA,
        acc = NA
      )
    )
  }
  # Step 1: Reclassify true labels (1 for protected level, 0 for else)
  y_true_binary <- ifelse(y_true == protected_label, 1, 0)

  # Step 2: Reclassify predicted labels
  # Find all predicted groups containing at least one protected label
  predicted_groups_with_protected <- unique(y_pred[y_true == protected_label])

  # Convert predicted labels to binary
  y_pred_binary <- ifelse(y_pred %in% predicted_groups_with_protected, 1, 0)

  # Step 3: Create a binary accuracy table
  accuracy_table <- table(y_true_binary, y_pred_binary)
  colnames(accuracy_table) <- c("Predicted Else", "Predicted Protected")
  rownames(accuracy_table) <- c("True Else", "True Protected")

  # Step 4: Extract metrics
  tp <- accuracy_table["True Protected", "Predicted Protected"]  # True Positives
  fp <- accuracy_table["True Else", "Predicted Protected"]       # False Positives
  fn <- accuracy_table["True Protected", "Predicted Else"]       # False Negatives

  # Calculate Precision and FNR
  precision <- tp / (tp + fp)
  fnr <- fn / (fn + tp)

  return(list(
    precision = precision,
    fnr = fnr,
    acc = sum(diag(accuracy_table))/sum(accuracy_table)
  ))
}

calculate_top_pct_recall <- function(predictions, truth, the_pct) {

  if(all(is.na(predictions))){
    return(
      list(recall = NA,
           precision = NA,
           accuracy = NA,
           effective_pct = NA)
    )
  }

  cutoff_pred <- quantile(predictions, 1 - the_pct)
  flag_pred <- predictions >= cutoff_pred

  cutoff_truth <- quantile(truth, 1 - the_pct)
  flag_truth <- truth >= cutoff_truth

  FN <- sum(!(flag_pred) & (flag_truth))
  TP <- sum((flag_pred) & (flag_truth))
  TN <- sum(!(flag_pred) & !(flag_truth))
  FP <- sum((flag_pred) & !(flag_truth))

  # Output results
  return(list(recall = TP / (TP + FN),
              precision = TP/ (TP + FP),
              accuracy = (TP + TN)/(TP + TN + FP + FN),
              effective_pct = (TP + FP)/(TP + TN + FP + FN))
         )
}


# Main function to process data
process_data_evtree_experiment <- function(preds) {
  to_return <- list()

  beg <- Sys.time()

  # Loop over all indices
  for (idx in seq_len(100)) {
    loginfo(sprintf("Processing index: %d", idx))

    # Extract train, valid, and test sets
    train_set <- preds[[idx]]$train
    valid_set <- preds[[idx]]$valid
    test_set <- preds[[idx]]$test

    # Prepare the data
    X_train <- train_set %>% select(X1, X2, D)
    X_test <- test_set %>% select(X1, X2, D)
    X_valid <- valid_set %>% select(X1, X2, D)

    train_set$resp <- train_set$proxy_vuln
    valid_set$resp <- valid_set$proxy_vuln

    y_train <- train_set$proxy_vuln
    y_valid <- valid_set$proxy_vuln

    proxy_vuln_t_tr <- train_set$proxy_vuln_t
    proxy_vuln_t_te <- test_set$proxy_vuln_t

    # Define hyperparameter grid
    param_grid <- expand.grid(minbucket = c(0.03, 0.05) * nrow(train_set),
                              maxdepth = c(3, 4),
                              alpha = c(1, 2),
                              ntrees = 25,
                              stringsAsFactors = FALSE)

    best_mse_overall <- Inf
    best_mse_with_8_leaves <- Inf
    best_mse_overall_r <- Inf
    best_mse_with_8_leaves_r <- Inf

    response <- "proxy_vuln"

    # Set up parallel backend
    ncores <- min(detectCores() - 1, nrow(param_grid))
    cl <- makeCluster(ncores)  # Use all cores except one
    registerDoParallel(cl)

    clusterExport(cl, c("param_grid", "evaluate_model", "flog.info", "flog.error",
                        "train_set", "valid_set", "test_set", "response"),
                  envir = environment())

    # Run grid search in parallel using the evaluate_model function
    # seq_len(nrow(param_grid))
    results <- foreach(params_idx = seq_len(nrow(param_grid)),
                       .packages = c("evtree",
                                     "rpart",
                                     "futile.logger")) %dopar% {
      params <- param_grid[params_idx, ]
      evaluate_model(params = params,
                     train_data = train_set,
                     valid_data = valid_set,
                     response_name = response)
    }

    stopCluster(cl)

    # Process results
    best_evtree_overall <- NULL
    best_evtree_with_8_leaves <- NULL
    best_rpart_overall <- NULL
    best_rpart_with_8_leaves <- NULL

    for (res in results) {
      if (!is.null(res)) {
        # Check for evtree
        if (res$evtree$val_mse < best_mse_overall) {
          best_mse_overall <- res$evtree$val_mse
          best_evtree_overall <- res$evtree
          best_params_overall <- res$params
        }
        if (res$evtree$val_mse < best_mse_with_8_leaves && res$evtree$leaves == 8) {
          best_mse_with_8_leaves <- res$evtree$val_mse
          best_evtree_with_8_leaves <- res$evtree
          best_params_with_8_leaves <- res$params
        }

        # Check for rpart
        if (res$rpart$val_mse < best_mse_overall_r) {
          best_mse_overall_r <- res$rpart$val_mse
          best_rpart_overall <- res$rpart
        }
        if (res$rpart_eight$val_mse < best_mse_with_8_leaves_r && res$rpart_eight$leaves == 8) {
          best_mse_with_8_leaves_r <- res$rpart_eight$val_mse
          best_rpart_with_8_leaves <- res$rpart_eight
        }
      }
    }

    if(!is.null(best_evtree_with_8_leaves$model)){
      test_leaf_indices <- predict(best_evtree_with_8_leaves$model, newdata = test_set, type = 'node')
      train_leaf_indices <- predict(best_evtree_with_8_leaves$model, newdata = train_set, type = 'node')
      sse_8 <- sum((proxy_vuln_t_te - predict(best_evtree_with_8_leaves$model, newdata = test_set))^2)
      test_e8 <- predict(best_evtree_with_8_leaves$model, newdata = test_set) %>% unname
    } else {
      test_leaf_indices <- rep(NA, nrow(test_set))
      train_leaf_indices <- rep(NA, nrow(train_set))
      test_e8 <- rep(NA, nrow(test_set))
      sse_8 <- NA
    }

    test_eo <- predict(best_evtree_overall$model, newdata = test_set) %>% unname
    test_r8 <- predict(best_rpart_with_8_leaves$model, newdata = test_set, type = "vector") %>% unname
    test_ro <- predict(best_rpart_overall$model, newdata = test_set, type = "vector") %>% unname

    pct_high_risk <- mean(proxy_vuln_t_te == max(proxy_vuln_t_te))

    iden_eo <- calculate_top_pct_recall(predictions = test_eo,
                                        truth = proxy_vuln_t_te,
                                        the_pct = pct_high_risk)
    iden_e8 <- calculate_top_pct_recall(predictions = test_e8,
                                        truth = proxy_vuln_t_te,
                                        the_pct = pct_high_risk)
    iden_ro <- calculate_top_pct_recall(predictions = test_ro,
                                        truth = proxy_vuln_t_te,
                                        the_pct = pct_high_risk)
    iden_r8 <- calculate_top_pct_recall(predictions = test_r8,
                                        truth = proxy_vuln_t_te,
                                        the_pct = pct_high_risk)

    test_leaf_indices_r <- round(predict(best_rpart_with_8_leaves$model, newdata = test_set, type = 'vector'), 3)
    train_leaf_indices_r <- round(predict(best_rpart_with_8_leaves$model, newdata = train_set, type = 'vector'), 3)
    sse_r8 <- sum((proxy_vuln_t_te - predict(best_rpart_with_8_leaves$model, newdata = test_set))^2)

    test_leaf_indices_any <- predict(best_evtree_overall$model, newdata = test_set, type = 'node')
    sse_any <- sum((proxy_vuln_t_te - predict(best_evtree_overall$model, newdata = test_set))^2)

    test_leaf_indices_rany <- predict(best_rpart_overall$model, newdata = test_set, type = 'vector')
    sse_rany <- sum((proxy_vuln_t_te - predict(best_rpart_overall$model, newdata = test_set))^2)

    sst <- sum((proxy_vuln_t_te - mean(proxy_vuln_t_te))^2)
    unique_proxy_t <- unique(proxy_vuln_t_tr)

    # Reorder leaves
    leaf_means <- tapply(proxy_vuln_t_tr, train_leaf_indices, mean)
    ordered_leaves <- leaf_means[order(leaf_means)]
    leaf_reordering_map <- setNames(seq_along(ordered_leaves), names(ordered_leaves))
    reordered_test_leaf_indices <- leaf_reordering_map[as.character(test_leaf_indices)]

    leaf_means_r <- tapply(proxy_vuln_t_tr, train_leaf_indices_r, mean)
    ordered_leaves_r <- leaf_means_r[order(leaf_means_r)]
    leaf_reordering_map_r <- setNames(seq_along(ordered_leaves_r), names(ordered_leaves_r))
    reordered_test_leaf_indices_r <- leaf_reordering_map_r[as.character(test_leaf_indices_r)]

    # Accuracy
    accuracy_metrics <- calculate_relaxed_accuracy(proxy_vuln_t_te, reordered_test_leaf_indices, unique_proxy_t)
    accuracy_metrics_r <- calculate_relaxed_accuracy(proxy_vuln_t_te, reordered_test_leaf_indices_r, unique_proxy_t)

    ## binary metrics
    binary_acc_top_any <- calculate_binary_metrics(y_true = proxy_vuln_t_te, y_pred = test_leaf_indices_any, protected_label = max(proxy_vuln_t_te))
    binary_acc_bottom_any <- calculate_binary_metrics(y_true = proxy_vuln_t_te, y_pred = test_leaf_indices_any, protected_label = min(proxy_vuln_t_te))
    binary_acc_top_8 <- calculate_binary_metrics(y_true = proxy_vuln_t_te, y_pred = reordered_test_leaf_indices, protected_label = max(proxy_vuln_t_te))
    binary_acc_bottom_8 <- calculate_binary_metrics(y_true = proxy_vuln_t_te, y_pred = reordered_test_leaf_indices, protected_label = min(proxy_vuln_t_te))

    binary_acc_top_rany <- calculate_binary_metrics(y_true = proxy_vuln_t_te, y_pred = test_leaf_indices_rany, protected_label = max(proxy_vuln_t_te))
    binary_acc_bottom_rany <- calculate_binary_metrics(y_true = proxy_vuln_t_te, y_pred = test_leaf_indices_rany, protected_label = min(proxy_vuln_t_te))
    binary_acc_top_r8 <- calculate_binary_metrics(y_true = proxy_vuln_t_te, y_pred = reordered_test_leaf_indices_r, protected_label = max(proxy_vuln_t_te))
    binary_acc_bottom_r8 <- calculate_binary_metrics(y_true = proxy_vuln_t_te, y_pred = reordered_test_leaf_indices_r, protected_label = min(proxy_vuln_t_te))

    # Store results
    to_return[[idx]] <- data.frame(
      num_leaf_8 = ifelse(all(is.na(test_e8)),
                          NA, length(unique(test_e8))),
      num_leaf_any = length(unique(test_eo)),
      minbucket_8 = best_params_with_8_leaves$minbucket/nrow(train_set),
      maxdepth_8 = best_params_with_8_leaves$maxdepth,

      minbucket_any = best_params_overall$minbucket/nrow(train_set),
      maxdepth_any = best_params_overall$maxdepth,

      num_leaf_r8 = length(unique(test_r8)),
      num_leaf_rany = length(unique(test_ro)),
      minbucket_r8 = best_params_with_8_leaves$minbucket/nrow(train_set),
      maxdepth_r8 = best_params_with_8_leaves$maxdepth,
      maxdepth_rany = max(rpart:::tree.depth(as.numeric(rownames(best_rpart_overall$model$frame)))),
      minbucket_rany = min(best_rpart_overall$model$frame[best_rpart_overall$model$frame$var == "<leaf>", ]$n)/nrow(train_set),

      alpha_any = best_params_overall$alpha,
      alpha_8 = ifelse(is.null(best_params_with_8_leaves$alpha), NA, best_params_with_8_leaves$alpha),

      cp_any = ifelse(length(best_rpart_overall$cp) == 0, 0, best_rpart_overall$cp),
      cp_8 = ifelse(length(best_rpart_with_8_leaves$cp) == 0, 0, best_rpart_with_8_leaves$cp),

      top_acc_any =  binary_acc_top_any$acc,
      top_prec_any =  binary_acc_top_any$precision,
        top_fnr_any =  binary_acc_top_any$fnr,
      bottom_acc_any =  binary_acc_bottom_any$acc,
      bottom_prec_any =  binary_acc_bottom_any$precision,
      bottom_fnr_any =  binary_acc_bottom_any$fnr,

      top_acc_rany =  binary_acc_top_rany$acc,
      top_prec_rany =  binary_acc_top_rany$precision,
      top_fnr_rany =  binary_acc_top_rany$fnr,
      bottom_acc_rany =  binary_acc_bottom_rany$acc,
      bottom_prec_rany =  binary_acc_bottom_rany$precision,
      bottom_fnr_rany =  binary_acc_bottom_rany$fnr,

      top_acc_8 =  binary_acc_top_8$acc,
      top_prec_8 =  binary_acc_top_8$precision,
      top_fnr_8 =  binary_acc_top_8$fnr,
      bottom_acc_8 =  binary_acc_bottom_8$acc,
      bottom_prec_8 =  binary_acc_bottom_8$precision,
      bottom_fnr_8 =  binary_acc_bottom_8$fnr,

      top_acc_r8 =  binary_acc_top_r8$acc,
      top_prec_r8 =  binary_acc_top_r8$precision,
      top_fnr_r8 =  binary_acc_top_r8$fnr,
      bottom_acc_r8 =  binary_acc_bottom_r8$acc,
      bottom_prec_r8 =  binary_acc_bottom_r8$precision,
      bottom_fnr_r8 =  binary_acc_bottom_r8$fnr,


      validation_mse_8 = best_mse_with_8_leaves,
      validation_mse_any = best_mse_overall,

      validation_mse_r8 = best_mse_with_8_leaves_r,
      validation_mse_rany = best_mse_overall_r,

      accuracy_test_8 = accuracy_metrics$strict_accuracy,
      relaxed_accuracy_test_8 = accuracy_metrics$relaxed_accuracy,
      r2_oracle_8 = 1 - sse_8 / sst,
      r2_oracle_any = 1 - sse_any / sst,

      accuracy_test_r8 = accuracy_metrics_r$strict_accuracy,
      relaxed_accuracy_test_r8 = accuracy_metrics_r$relaxed_accuracy,
      r2_oracle_r8 = 1 - sse_r8 / sst,
      r2_oracle_rany = 1 - sse_rany / sst,

      recall_top_pct_eo = iden_eo$recall,
      prec_top_pct_eo = iden_eo$precision,
      acc_top_pct_eo = iden_eo$accuracy,
      effpct_top_pct_eo = iden_eo$effective_pct,


      recall_top_pct_e8 = iden_e8$recall,
      prec_top_pct_e8 = iden_e8$precision,
      acc_top_pct_e8 = iden_e8$accuracy,
      effpct_top_pct_e8 = iden_e8$effective_pct,

      recall_top_pct_ro = iden_ro$recall,
      prec_top_pct_ro = iden_ro$precision,
      acc_top_pct_ro = iden_ro$accuracy,
      effpct_top_pct_ro = iden_ro$effective_pct,

      recall_top_pct_r8 = iden_r8$recall,
      prec_top_pct_r8 = iden_r8$precision,
      acc_top_pct_r8 = iden_r8$accuracy,
      effpct_top_pct_r8 = iden_r8$effective_pct,

      time_any = best_evtree_overall$time,
      time_8 = ifelse(is.null(best_evtree_with_8_leaves),
                      NA, best_evtree_with_8_leaves$time),
      time_rany = best_rpart_overall$time,
      time_r8 = best_rpart_with_8_leaves$time)
  }

  return(do.call(rbind, to_return))
}

