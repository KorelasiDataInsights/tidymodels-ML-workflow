
###############################################################################################
### These tidymodels machine learning utilities were created by 
### Sydeaka P. Watson, PhD of Korelasi Data Insights, LLC
### These functions are open source and are available in the following GitHub repository:
###   https://github.com/korelasidata/tidymodels-ML-workflow
###############################################################################################




zero_variance <- function(vals) {
  vals %>% 
    .[is.na(vals)] %>%
    length(unique(.)) == 1
}

remove_zero_variance_fields <- function(dat) {
  zv_fields <- sapply(dat, zero_variance) %>% .[. == TRUE] %>% names
  
  if (length(zv_fields) == 0) {
    log_info("All fields had some variability. Returning dataframe with no changes.")
    return(dat)
  } else {
    log_info("The following fields were identified as having zero variance: {paste(zv_fields, collapse=', ')}")
    fields_to_keep <- colnames(dat)[!(colnames(dat) %in% zv_fields)]
    dat <- dat %>% select_at(fields_to_keep)
    log_info("Fields successfully removed")
  }
  
  return(dat)
}


model_specifications <- list(
  "xgboost" = boost_tree(engine = 'xgboost', trees = tune(),
                         tree_depth = tune(), min_n = tune(), learn_rate = tune(), 
                         mtry = tune()),
  
  "gbm" = boost_tree(engine = 'lightgbm', trees = tune(),
                     tree_depth = tune(), min_n = tune(), learn_rate = tune(), 
                     mtry = tune()),
  
  "random_forest" = rand_forest(trees = tune(), min_n = tune(), mtry = tune()) %>%
    set_engine("ranger", importance = "impurity")
  # set_engine("randomForest", importance = TRUE)
)



get_model_config <- function(model_formula, model_specifications, selected_algorithm, model_mode) {
  
  model_spec <- model_specifications[[selected_algorithm]] %>% 
    set_mode(model_mode)
  
  model_wflow <- workflow(model_formula, model_spec)
  
  if (selected_algorithm == "xgboost") {
    model_param_grid <- model_wflow %>% 
      extract_parameter_set_dials() %>% 
      update(
        trees = trees(c(100, 1500)),
        learn_rate = learn_rate(c(.00005, .5), trans= NULL),
        tree_depth = tree_depth(c(6, 20)),
        min_n = min_n(c(10, 60)),
        mtry = mtry(c(5, 40))
      )
  }
  
  if (selected_algorithm == "gbm") {
    model_param_grid <- model_wflow %>% 
      extract_parameter_set_dials() %>% 
      update(
        trees = trees(c(100, 1500)),
        learn_rate = learn_rate(c(.00005, .5), trans= NULL),
        tree_depth = tree_depth(c(6, 20)),
        min_n = min_n(c(10, 60)),
        mtry = mtry(c(5, 40))
      )
  }
  
  
  if (selected_algorithm == "random_forest") {
    model_param_grid <- model_wflow %>% 
      extract_parameter_set_dials() %>% 
      update(
        trees = trees(c(100, 1500)),
        min_n = min_n(c(10, 60)),
        mtry = mtry(c(5, 40))
      )
  }
  
  
  rtn <- list(
    model_spec = model_spec,
    model_wflow = model_wflow,
    model_param_grid = model_param_grid
  )
  
  return(rtn)
  
}

get_varimp <- function(selected_algorithm, final_model_fit, engine_specific_model_fit=NULL) {
  if (selected_algorithm %in% c("xgboost")) {
    df_varimp <- final_model_fit %>%
      extract_fit_parsnip() %>%
      vip::vi(object=.) %>%
      mutate(PctImportance = round(Importance / sum(Importance) * 100, 2))
    
    plot_varimp <- final_model_fit %>%
      extract_fit_parsnip() %>%
      vip::vip(geom = "col") +
      theme_bw()
  }
  
  
  if (selected_algorithm %in% c("random_forest")) {
    # ranger varimp
    df_varimp <- final_model_fit %>%
      extract_fit_parsnip() %>%
      vip::vi(object=.) %>%
      mutate(PctImportance = round(Importance / sum(Importance) * 100, 2))
    
    plot_varimp <- final_model_fit %>%
      extract_fit_parsnip() %>%
      vip::vip(geom = "col") +
      theme_bw()
    
    
    # randomForest varimp
    # type = either 1 or 2, specifying the type of importance measure 
    # (1 = mean decrease in accuracy, 2 = mean decrease in node impurity).
    # df_varimp <- engine_specific_model_fit %>%
    #   importance(type=2) %>% 
    #   data.frame(Variable = rownames(.), .) %>% 
    #   set_colnames(c("Variable", "Importance")) %>%
    #   mutate(PctImportance = round(Importance / sum(Importance) * 100, 2)) %>%
    #   arrange(desc(PctImportance))
    # 
    # plot_varimp <- df_varimp %>%
    #   head(10) %>%
    #   ggplot(aes(x = reorder(Variable, PctImportance), y = PctImportance)) +
    #   geom_bar(stat = "identity", col = "black", show.legend = F) +
    #   coord_flip() +
    #   scale_fill_grey() +
    #   theme_bw() + 
    #   ggtitle("Top 10 attributes") + 
    #   xlab("") + ylab("% importance")
  }
  
  
  
  if (selected_algorithm %in% c("gbm")) {
    tree_imp <- engine_specific_model_fit %>%
      lgb.importance(percentage = TRUE)
    
   df_varimp <- final_model_fit %>%
     rename(Variable = Feature, Importance = Gain) %>%
     select(Variable, Importance) %>%
     mutate(PctImportance = round(Importance / sum(Importance) * 100, 2)) %>%
     arrange(desc(PctImportance))
   
   plot_varimp <- df_varimp %>%
     head(10) %>%
     ggplot(aes())
  }
  
  
  
  
  if (selected_algorithm == "gbm") {
    # Applying varimp utils specific to lightgbm
    tree_imp <- engine_specific_model_fit %>%
      lgb.importance(percentage = TRUE)
    
    df_varimp <- tree_imp %>%
      rename(Variable = Feature, Importance = Gain) %>%
      select(Variable, Importance) %>%
      mutate(PctImportance = round(Importance / sum(Importance) * 100, 2)) %>%
      arrange(desc(PctImportance))
    
    plot_varimp <- df_varimp %>%
      head(10) %>%
      ggplot(aes(x = reorder(Variable, PctImportance), y = PctImportance)) +
      geom_bar(stat = "identity", col = "black", show.legend = F) +
      coord_flip() +
      scale_fill_grey() +
      theme_bw() + 
      ggtitle("Top 10 attributes") + 
      xlab("") + ylab("% importance")
  }
  
  return(list(
    df_varimp = df_varimp,
    plot_varimp = plot_varimp
  ))
  
}


plot_confusion_matrix <- function() {
  return(NULL)
  
  #cm <- pred_df %>% yardstick::conf_mat(Category, .pred_class)
  
  # Now compute the average confusion matrix across all folds in
  # terms of the proportion of the data contained in each cell.
  # First get the raw cell counts per fold using the `tidy` method
  library(tidyr)
  
  cells_per_resample <- pred_df %>%
    group_by(id) %>%
    conf_mat(truth=Category, estimate=.pred_class) %>%
    mutate(tidied = lapply(conf_mat, tidy)) %>%
    unnest(tidied)
  
  # Get the totals per resample
  counts_per_resample <- pred_df %>%
    group_by(id) %>%
    summarize(total = n()) %>%
    left_join(cells_per_resample, by = "id") %>%
    # Compute the proportions
    mutate(prop = value/total) %>%
    group_by(name) %>%
    # Average
    summarize(prop = mean(prop))
  
  counts_per_resample
  
  # Now reshape these into a matrix
  mean_cmat <- matrix(counts_per_resample$prop, byrow = TRUE, ncol = 4)
  rownames(mean_cmat) <- levels(hpc_cv$obs)
  colnames(mean_cmat) <- levels(hpc_cv$obs)
  
  round(mean_cmat, 3)
  
  # The confusion matrix can quickly be visualized using autoplot()
  library(ggplot2)
  
  autoplot(cm, type = "mosaic")
  autoplot(cm, type = "heatmap")
  
  
  
  cm <- caret::confusionMatrix(pred_df$.pred_class, pred_df$Category)
  cm_d <- as.data.frame(cm$table) # extract the confusion matrix values as data.frame
  cm_st <-data.frame(cm$overall) # confusion matrix statistics as data.frame
  cm_st$cm.overall <- round(cm_st$cm.overall,2) # round the values
  cm_d$diag <- cm_d$Prediction == cm_d$Reference # Get the Diagonal
  cm_d$ndiag <- cm_d$Prediction != cm_d$Reference # Off Diagonal     
  cm_d[cm_d == 0] <- NA # Replace 0 with NA for white tiles
  #cm_d$Reference <-  reverse.levels(cm_d$Reference) # diagonal starts at top left
  cm_d$ref_freq <- cm_d$Freq * ifelse(is.na(cm_d$diag),-1,1)
  
  
  plt1 <-  ggplot(data = cm_d, aes(x = Prediction , y =  Reference, fill = Freq))+
    scale_x_discrete(position = "top") +
    geom_tile( data = cm_d,aes(fill = ref_freq)) +
    scale_fill_gradient2(guide = FALSE ,low="red3",high="orchid4", midpoint = 0,na.value = 'white') +
    geom_text(aes(label = Freq), color = 'black', size = 3)+
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position = "none",
          panel.border = element_blank(),
          plot.background = element_blank(),
          axis.line = element_blank(),
    )
  
  plt2 <- tableGrob(cm_st)
  
  # TO DO: Need to export this plot somehow. `grid.arrange` plots to console only. Value of `plot_predictions` is a tableGrob
  plot_predictions <- grid.arrange(plt1, plt2, nrow = 1, ncol = 2, 
                                   top = grid::textGrob("Confusion Matrix", 
                                                        gp = grid::gpar(fontsize=25,font=1)))
}











plot_param <- function(metric_df, param, metric_name='rmse') {
  metric_df %>%
    filter(.metric == metric_name) %>%
    arrange_at('mean') %>%
    ggplot(aes_string(x=param, y='mean')) +
    geom_point() + 
    xlab(param) + 
    ylab('') + 
    ggtitle(glue::glue("{metric_name} vs {param}"))
}









# Helper function to get a single model fit on a bootstrap resample
fit_model_on_bootstrap <- function(split, best_wflow) {
  best_wflow %>% 
    fit(data = analysis(split))
}



# Helper function to get prediction intervals
# boot_model <- boot_models$model[[1]]
# input_data <- dat_train_and_val[1:3,]
# predict(boot_model, new_data = input_data)
bootstrap_pred_intervals <- function(boot_models, input_data, lower_pct = .05, upper_pct = 0.95) {
  # Get predictions on all input cases using all bootstrap models
  pred_df <- boot_models %>%
    mutate(preds = map(model, \(mod) predict(mod, new_data=input_data)))
  
  # Combine predictions across bootstraps into a matrix
  pred_matrix <- bind_cols(pred_df$preds, .name_repair="minimal") %>%
    as.matrix %>% t
  
  # Compute upper and lower confidence bounds
  pred_intervals <- pred_matrix %>% apply(2, quantile, probs=c(lower_pct, upper_pct)) %>% t
  
  return(pred_intervals)
}

# bootstrap_pred_intervals(boot_models, input_data, lower_pct = .05, upper_pct = 0.95)




