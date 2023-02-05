#-----------------------------------------------------------------------------
#function: Extract code change dimension metrics
#param: data, code change and code structrue metrics
#param: code change dimension names
# return data frame with code change dimension metrics
#-----------------------------------------------------------------------------
evaluate_remove_code_change_dimension <- function(df_train_all_dimension=NA, df_test_all_dimension=NA, code_change_dimension_names=NA){
  df_train_no_code_change_dimension <- dplyr::select(df_train_all_dimension, -one_of(code_change_dimension_names))
  df_test_no_code_change_dimension <- dplyr::select(df_test_all_dimension, -one_of(code_change_dimension_names))
  
  fit_tf_idf <- buildModel(data = df_train_no_code_change_dimension, model_name = model_choice)
  evaluateModel(fit_tf_idf, df_test_no_code_change_dimension, names(df_train_no_code_change_dimension), model_name = model_choice)
}

evaluate_keep_code_change_dimension <- function(df_train_all_dimension=NA, df_test_all_dimension=NA, code_change_dimension_names=NA){
  code_change_dimension_names <- c(code_change_dimension_names, 'regression')
  
  df_train_code_change_dimension <- dplyr::select(df_train_all_dimension, one_of(code_change_dimension_names))
  df_test_code_change_dimension <- dplyr::select(df_test_all_dimension, one_of(code_change_dimension_names))
  
  fit_tf_idf <- buildModel(data = df_train_code_change_dimension, model_name = model_choice)
  evaluateModel(fit_tf_idf, df_test_code_change_dimension, names(df_train_code_change_dimension), model_name = model_choice)
}


#-----------------------------------------------------------------------------
#function: Extract code structure dimension metrics
#param: data, code change and code structrue metrics
#param: code structure dimension names
# return data frame with code structure dimension metrics
#-----------------------------------------------------------------------------
evaluate_remove_code_structure_dimension <- function(df_train_all_dimension=NA, df_test_all_dimension=NA, code_structure_dimension_names=NA){
  df_train_no_code_structure_dimension <- dplyr::select(df_train_all_dimension, -one_of(code_structure_dimension_names))
  df_test_no_code_structure_dimension <- dplyr::select(df_test_all_dimension, -one_of(code_structure_dimension_names))
  
  fit_tf_idf <- buildModel(data = df_train_no_code_structure_dimension, model_name = model_choice)
  evaluateModel(fit_tf_idf, df_test_no_code_structure_dimension, names(df_train_no_code_structure_dimension), model_name = model_choice)
}

evaluate_keep_code_structure_dimension <- function(df_train_all_dimension=NA, df_test_all_dimension=NA, code_structure_dimension_names=NA){
  code_structure_dimension_names <- c(code_structure_dimension_names, 'regression')
  
  df_train_code_structure_dimension <- dplyr::select(df_train_all_dimension, one_of(code_structure_dimension_names))
  df_test_code_structure_dimension <- dplyr::select(df_test_all_dimension, one_of(code_structure_dimension_names))
  
  fit_tf_idf <- buildModel(data = df_train_code_structure_dimension, model_name = model_choice)
  evaluateModel(fit_tf_idf, df_test_code_structure_dimension, names(df_train_code_structure_dimension), model_name = model_choice)
}

#-----------------------------------------------------------------------------
#function: Extract code token dimension metrics
#param: data, code change, code structrue metrics, and token metrics
#param: non code token dimension names
# return data frame with token dimension metrics
#-----------------------------------------------------------------------------
evaluate_remove_code_token_dimension <- function(df_train_no_token_dimension=NA, df_test_no_token_dimension=NA){
  fit_tf_idf <- buildModel(data = df_train_no_token_dimension, model_name = model_choice)
  evaluateModel(fit_tf_idf, df_test_no_token_dimension, names(df_train_no_token_dimension), model_name = model_choice)
}

evaluate_keep_code_token_dimension <- function(df_train_token_dimension=NA, df_test_token_dimension=NA){
  fit_tf_idf <- buildModel(data = df_train_token_dimension, model_name = model_choice)
  evaluateModel(fit_tf_idf, df_test_token_dimension, names(df_train_token_dimension), model_name = model_choice)
}

#-----------------------------------------------------------------------------
#function: Extract configuration option dimension metrics
#param: data, code change, code structrue metrics, and token metrics
# return data frame with configuration option dimension metrics
#-----------------------------------------------------------------------------
evaluate_remove_configuration_dimension <- function(df_train_no_config_dimension=NA, df_test_no_config_dimension=NA){
  fit_tf_idf <- buildModel(data = df_train_no_config_dimension, model_name = model_choice)
  evaluateModel(fit_tf_idf, df_test_no_config_dimension, names(df_train_no_config_dimension), model_name = model_choice)
}

evaluate_keep_configuration_dimension <- function(df_train_config_dimension=NA, df_test_config_dimension=NA){
  fit_tf_idf <- buildModel(data = df_train_config_dimension, model_name = model_choice)
  evaluateModel(fit_tf_idf, df_test_config_dimension, names(df_train_config_dimension), model_name = model_choice)
}

#-----------------------------------------------------------------------------
#function: evaluate all dimension metrics
#param: data, code change, code structrue metrics, and token metrics
# return data frame with configuration option dimension metrics
#-----------------------------------------------------------------------------
evaluate_all_dimension <- function(df_train_all_dimension=NA, df_test_all_dimension=NA){
  fit_tf_idf <- buildModel(data = df_train_all_dimension, model_name = model_choice)
  evaluateModel(fit_tf_idf, df_test_all_dimension, names(df_train_all_dimension), model_name = model_choice)
}

#-----------------------------------------------------------------------------
#function: k fold evaluation
#param: data
# return precision, recall
#-----------------------------------------------------------------------------
crossDimensionEvaluation <- function(data=NA, evaluate_file=NA){
  # data <- df_cassandra_code_label
  # 100 times
  for(round in 1:10){
    sample_folds <- split(sample(1:nrow(data)), 1:10)
    # folds <- createFolds(y=data$runtime, k=10) 
    for(i in 1:10){
      print(paste0(i," cross validation"))
      # segment your data by i-th fold 
      training_data <- data[-unlist(sample_folds[i]),]
      test_data <- data[unlist(sample_folds[i]),]
      
      # get different dimensions metrics data from train data
      df_train_code_tfidf <- tf_idf_code_token(data = training_data, non_code_metric_names = non_code_metric_names)
      df_train_all_dimension <- calculate_option_metric(df_train_code_tfidf)
      token_names <- setdiff(names(df_train_code_tfidf), non_code_metric_names)
      config_names <- setdiff(names(df_train_all_dimension), names(df_train_code_tfidf))
      # token metrics and config metrics
      df_train_no_token_dimension <- dplyr::select(df_train_all_dimension, -one_of(token_names))
      df_train_token_dimension <- dplyr::select(df_train_all_dimension, one_of(c(token_names, "regression")))
      df_train_no_config_dimension <- df_train_code_tfidf
      df_train_config_dimension <- dplyr::select(df_train_all_dimension, one_of(c(config_names, "regression")))
      
      # get different dimensions metrics data from test data
      df_test_code_tfidf <- tf_idf_code_token(data = test_data, non_code_metric_names = non_code_metric_names)
      df_test_all_dimension <- calculate_option_metric(df_test_code_tfidf)
      token_names <- setdiff(names(df_test_code_tfidf), non_code_metric_names)
      config_names <- setdiff(names(df_test_all_dimension), names(df_test_code_tfidf))
      #
      df_test_no_token_dimension <- dplyr::select(df_test_all_dimension, -one_of(token_names))
      df_test_token_dimension <- dplyr::select(df_test_all_dimension, one_of(c(token_names, "regression")))
      df_test_no_config_dimension <- df_test_code_tfidf
      df_test_config_dimension <- dplyr::select(df_test_all_dimension, one_of(c(config_names, "regression")))
      
      
      # evaluate removing one dimension metrics
      print('Evaluate all dimension metrics...')
      cat('removing any dimension metrics\n', file=evaluate_file, append=T)
      all_result <- evaluate_all_dimension(df_train_all_dimension, df_test_all_dimension)
      print('Evaluate all metrics without code change dimension...')
      code_change_result <- evaluate_remove_code_change_dimension(df_train_all_dimension, df_test_all_dimension, code_change_dimension)
      print('Evaluate all metrics without code structure dimension...')
      code_structure_result <- evaluate_remove_code_structure_dimension(df_train_all_dimension, df_test_all_dimension, code_structure_dimension)
      print('Evaluate all metrics without code token dimension...')
      code_token_result <- evaluate_remove_code_token_dimension(df_train_no_token_dimension, df_test_no_token_dimension)
      print('Evaluate all metrics without configuration dimension...')
      configuration_result <- evaluate_remove_configuration_dimension(df_train_no_config_dimension, df_test_no_config_dimension)
      
      merge_results <- c(all_result, code_change_result, code_structure_result, code_token_result, configuration_result)
      
      cat(paste(merge_results, collapse=" "), '\n', file=evaluate_file, append=T)
      
      # evaluate keeping only one dimension metrics
      print('Evaluate only code change dimension...')
      cat('Keeping only one dimension metrics\n', file=evaluate_file, append=T)
      code_change_result <- evaluate_keep_code_change_dimension(df_train_all_dimension, df_test_all_dimension, code_change_dimension)
      print('Evaluate obly code structure dimension...')
      code_structure_result <- evaluate_keep_code_structure_dimension(df_train_all_dimension, df_test_all_dimension, code_structure_dimension)
      print('Evaluate only code token dimension...')
      code_token_result <- evaluate_keep_code_token_dimension(df_train_no_token_dimension, df_test_no_token_dimension)
      print('Evaluate only configuration dimension...')
      configuration_result <- evaluate_keep_configuration_dimension(df_train_no_config_dimension, df_test_no_config_dimension)
      
      merge_results <- c(all_result, code_change_result, code_structure_result, code_token_result, configuration_result)
      cat(paste(merge_results, collapse=" "), '\n', file=evaluate_file, append=T)
    }
  }
  
}


#-----------------------------------------------------------------------------
#function: building model to calculate important metrics
#param1: data, dataframe
#important metrics
#-----------------------------------------------------------------------------
importantMetric <- function(data, importance_file=NA){
  # data <- df_hadoop_tfidf_config
  # remove the column that the sum==0
  data <- data[,!(names(data)%in% c("release", "ESDIFF", "commithash", "Test", "Option"))]
  nans <- sapply(data, function(x) all(is.nan(x)))
  data <- data[,!nans]
  data <- data.frame(data)
  
  # remove the column that the sum==0
  data <- data[,colSums(data)!=0]
  # remove the class label column
  independant <- data[,!(names(data)%in%c('regression'))]
  # calculate correlation
  correlations <- cor(independant, method="pearson")
  correlations[is.na(correlations)] <- 1 #remove na columns
  highCor <- findCorrelation(correlations,cutoff = .7)
  low_cor_names <- names(independant[,-highCor])
  low_cor_data <- independant[(names(independant)%in%low_cor_names)]
  
  # get the metric name
  factors <- names(low_cor_data)
  fla <- paste("regression ~", paste(factors, collapse="+"))
  form <- as.formula(fla)
  # get the metric name
  metric_names <-names(low_cor_data)
  # build model and calculate important metrics
  #booting statics function
  statistic_fun <- function(data, indices){
    print("bootstrap and build model")
    d <- data[indices,]
    fit <-randomForest(form, d, ntree = 100, importance = TRUE)
    importance_result <- importance(fit, type=1) 
    return(importance_result)
  }
  # bootstrap 10 iterations, R=10
  boot_result <- boot(data = data, statistic = statistic_fun, R=100)
  # extract the importance 
  importance_metric <- boot_result$t
  # add the metric name to the importance variable
  colnames(importance_metric) <- metric_names
  importance_metric <- data.frame(importance_metric)
  importance_metric <- importance_metric[,colSums(importance_metric)!=0]
  
  return(importance_metric)
}

#-----------------------------------------------------------------------------
#function: plot boxplot in important dimenssion metrics
#param1: data, dataframe
#important metrics
#-----------------------------------------------------------------------------
boxplot_dimenssion_importance <- function(dimenssion_importance_file=NA, metrics=NA, subject=NA){
  col_names <- c('all_pre', 'all_rel', 'all_auc',
                 'd1_pre', 'd1_rel', 'd1_auc',
                 'd2_pre', 'd2_rel', 'd2_auc',
                 'd3_pre', 'd3_rel', 'd3_auc',
                 'd4_pre', 'd4_rel', 'd4_auc')
  
  # dimenssion_importance_file <- hadoop_rq3_dimension_importance_file
  data_dimenssion_importance <- read.csv(dimenssion_importance_file, header = F)
  # remove NA rows
  data_dimenssion_importance <- data_dimenssion_importance[complete.cases(data_dimenssion_importance),]
  colnames(data_dimenssion_importance) <- col_names
  data_dimenssion_importance <- data.frame(lapply(data_dimenssion_importance, function(x) as.numeric(as.character(x))))
  
  ########### remove one dimenssion
  print("plot removing one dimension boxplot")
  odd_rows <- seq(1, nrow(data_dimenssion_importance), 2)
  data_remove_dimension_all <- data_dimenssion_importance[odd_rows,]
  for (metric_index in seq(length(metrics))){
    # get remove dimension auc data for each performance metric
    metric_start <- metric_index*100 - 99
    metric_end <- metric_index*100
    data_remove_dimension <- data_remove_dimension_all[metric_start:metric_end,]
    # box plot
    metric <- metrics[metric_index]
    boxplot_file <- paste0(subject,'/data/RQ2/',metric,'-',subject,'remove-importance.pdf')
    pdf(boxplot_file)
    boxplot(data_remove_dimension$all_auc, data_remove_dimension$d1_auc, data_remove_dimension$d2_auc, data_remove_dimension$d3_auc, data_remove_dimension$d4_auc, 
            names=c('All', 'CC', 'CS', 'CT', 'CON'),cex.axis=2)
    dev.off()
  }
  
  ########### keep only one dimension
  print("plot keeping one dimension boxplot")
  even_rows <- seq(2, nrow(data_dimenssion_importance), 2)
  data_keep_dimenssion_all <- data_dimenssion_importance[even_rows,]
  for (metric_index in seq(length(metrics))){
    # get remove dimension auc data for each performance metric
    metric_start <- metric_index*100 - 99
    metric_end <- metric_index*100
    data_keep_dimension <- data_keep_dimenssion_all[metric_start:metric_end,]
    # box plot
    metric <- metrics[metric_index]
    boxplot_file <- paste0(subject,'/data/RQ2/',metric,'-',subject,'keep-importance.pdf')
    pdf(boxplot_file)
    boxplot(data_keep_dimension$all_auc, data_keep_dimension$d1_auc, data_keep_dimension$d2_auc, data_keep_dimension$d3_auc, data_keep_dimension$d4_auc, 
            names=c('All', 'CC', 'CS', 'CT', 'CON'),cex.axis=2)
    dev.off()
  }
}

