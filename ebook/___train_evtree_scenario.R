# Required libraries
library(doParallel)
library(evtree)
library(partykit)
library(futile.logger)

# Configure logger
flog.threshold(DEBUG) # Set the log level
flog.appender(appender.console()) # Log to console

# Main function to process populations and save results
process_populations <- function(preds_pop_stats, response_vars, param_grid, output_dir) {
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Perform grid search for each population and response variable
  final_results <- list()

  for (response in response_vars) {
    flog.info("Processing response variable: %s", response)
    response_results <- setNames(nm = names(preds_pop_stats)) %>% lapply(function(pop_name) {
      flog.info("Processing population: %s for response: %s", pop_name, response)

      # Check if model already exists
      output_file <- file.path(output_dir, paste0(pop_name, "_", response, "_model.rds"))
      if (file.exists(output_file)) {
        flog.info("Model for population %s and response %s already exists. Loading...", pop_name, response)
        return(readRDS(output_file))
      }

      # Extract train and validation sets
      train_data <- preds_pop_stats[[pop_name]]$train
      valid_data <- preds_pop_stats[[pop_name]]$valid
      train_data$resp <- train_data[[response]]
      valid_data$resp <- valid_data[[response]]
      for (resp_to_eliminate in setdiff(response_vars, response)) {
        train_data[[resp_to_eliminate]] <- NULL
        valid_data[[resp_to_eliminate]] <- NULL
      }

      # Verify data integrity
      if (is.null(train_data) || is.null(valid_data)) {
        flog.error("Train or validation data is missing for population %s", pop_name)
        return(NULL)
      }

      # Set up parallel backend
      cl <- makeCluster(detectCores() - 1) # Use all cores except one
      registerDoParallel(cl)
      # Export required variables to all workers
      clusterExport(cl, c("param_grid", "evaluate_model",
                          "train_data", "valid_data", "response"),
                    envir = environment())

      # Grid search in parallel
      # seq_len(nrow(param_grid))
      results <- foreach(params_idx = 1:nrow(param_grid),
                         .packages = c("evtree",
                                       "rpart",
                                       "futile.logger")) %dopar% {
                           params <- param_grid[params_idx, ]
                           evaluate_model(params, train_data, valid_data, response)
                         }

      # Stop the cluster
      stopCluster(cl)

      # Remove NULL results
      results <- results[!sapply(results, is.null)]

      # Extract the best model based on validation MSE
      if (length(results) == 0) {
        flog.error("No valid models were trained for population %s and response %s", pop_name, response)
        return(NULL)
      }

      best_result <- results[[which.min(sapply(results, function(res) res$evtree$val_mse))]]
      flog.info("Best params for population %s and response %s: %s", pop_name, response, toString(best_result$params))

      ready_to_return <- list(
        pop_name = pop_name,
        response = response,
        best_params = best_result$params,
        best_evtree = best_result$evtree,
        best_rpart = best_result$rpart,
        archive = results
      )

      # Save the results
      saveRDS(ready_to_return,
        file = output_file
      )

      # Return the best model information
      ready_to_return})

    final_results[[response]] <- response_results
  }


  # Return all results
  final_results
}

