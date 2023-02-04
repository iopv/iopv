model_choice <- "randomForest"
# model_choice <- "regression"
# model_choice <- "xgboost"
subject <- "cassandra"
metrics <- c('runtime', 'cpu', 'mem', 'ioread', 'iowrite')


############ (2) code token metrics in cassandra
df_cassandra <- fread(cassandra_metric_file, sep = ',')
colnames(df_cassandra) <- metric_names

cassandra_evaluate_file <- paste0("cassandra/data/RQ1/evaluate-", model_choice, ".csv")
cat("",file = cassandra_evaluate_file, append = FALSE) # clear the output file
for (metric in metrics){
  print(metric)
  cat(metric, '\n', file=cassandra_evaluate_file, append=T)
  other_counters <- metrics[which(metrics != metric)]
  filter_column <- names(df_cassandra)[!names(df_cassandra) %in% other_counters]
  # filter_column <- !names(df_cassandra) %in% other_counters
  df_metric_data <- subset(df_cassandra, select=filter_column)
  names(df_metric_data)[names(df_metric_data) == metric] <- "ESDIFF"
  
  df_cassandra_code_embedding <- fread(cassandra_embedding_file, sep = ',', header = T)
  
  # add label
  df_cassandra_label <- splitEffectSize(df_metric_data, "ESDIFF")
  df_cassandra_code_label <- spread(data = df_cassandra_label, key = code, value = codeNum, fill = 0)
  
  # k fold evaluation
  print(paste0("Tf idf ", model_choice))
  cat(paste0("Tf idf ", model_choice, "\n"), file = cassandra_evaluate_file, append = T)
  crossTfidfEvaluation(df_cassandra_code_label, model = model_choice)

  print(paste0("PCA ", model_choice))
  cat(paste0("PCA ", model_choice, "\n"), file = cassandra_evaluate_file, append = T)
  crossTfidfPCAEvaluation(df_cassandra_code_label, model = model_choice)
  
  print(paste0("Code embedding ", model_choice))
  cat(paste0("Code embedding ", model_choice, "\n"), file = cassandra_evaluate_file, append = T)
  crossEmbeddingEvaluation(df_cassandra_code_label, df_cassandra_code_embedding, model = model_choice)
}


############ (3) code token metrics in hadoop
df_hadoop <- fread(hadoop_metric_file, sep = ',')
colnames(df_hadoop) <- metric_names
hadoop_evaluate_file <- paste0("hadoop/data/RQ1/evaluate-", model_choice, ".csv")

cat("",file = hadoop_evaluate_file, append = FALSE) # clear the output file
for (metric in metrics){
  print(metric)
  cat(metric, '\n', file=hadoop_evaluate_file, append=T)
  other_counters <- metrics[which(metrics != metric)]
  filter_column <- names(df_hadoop)[!names(df_hadoop) %in% other_counters]
  # filter_column <- !names(df_cassandra) %in% other_counters
  df_metric_data <- subset(df_hadoop, select=filter_column)
  names(df_metric_data)[names(df_metric_data) == metric] <- "ESDIFF"
  
  df_hadoop_code_embedding <- fread(hadoop_embedding_file, sep = ',', header = T)
  
  # add label
  df_hadoop_label <- splitEffectSize(df_metric_data, "ESDIFF")
  df_hadoop_code_label <- spread(data = df_hadoop_label, key = code, value = codeNum, fill = 0)
  
  # k fold evaluation
  print(paste0("Tf idf ", model_choice))
  cat(paste0("Tf idf ", model_choice, "\n"), file = hadoop_evaluate_file, append = T) 
  crossTfidfEvaluation(df_hadoop_code_label, model = model_choice)
  
  print(paste0("PCA ", model_choice))
  cat(paste0("PCA ", model_choice, "\n"), file = hadoop_evaluate_file, append = T) 
  crossTfidfPCAEvaluation(df_hadoop_code_label, model = model_choice)
  
  print(paste0("Code embedding ", model_choice))
  cat(paste0("Code embedding ", model_choice, "\n"), file = hadoop_evaluate_file, append = T)
  crossEmbeddingEvaluation(df_hadoop_code_label, df_hadoop_code_embedding, model = model_choice)
}

########### (4) generate deep learning data (For NN and CNN, implementation using python)
df_hadoop <- fread(hadoop_metric_file, sep = ',')
colnames(df_hadoop) <- metric_names
df_hadoop_code_embedding <- fread(hadoop_embedding_file, sep = ',', header = T)
df_hadoop_all_label <- splitEffectSizeAll(df_hadoop)
df_hadoop_code_label <- spread(data = df_hadoop_all_label, key = code, value = codeNum, fill = 0)
generateDLdata(data = df_hadoop_code_label, project = subject, df_code_embedding = df_hadoop_code_embedding)

df_cassandra <- fread(cassandra_metric_file, sep = ',')
colnames(df_cassandra) <- metric_names
df_cassandra_code_embedding <- fread(cassandra_embedding_file, sep = ',', header = T)
df_cassandra_all_label <- splitEffectSizeAll(df_cassandra)
df_cassandra_code_label <- spread(data = df_cassandra_all_label, key = code, value = codeNum, fill = 0)
generateDLdata(data = df_cassandra_code_label, project = subject, df_code_embedding = df_cassandra_code_embedding)

model_choice <- "randomForest"
perf_metrics <- c('runtime', 'cpu', 'mem', 'ioread', 'iowrite')

############ dimension importance  in cassandra
# (1) remove one dimension, and keep only one dimension
df_cassandra <- fread(cassandra_metric_file, sep = ',')
colnames(df_cassandra) <- metric_names
cat("",file = cassandra_dimension_importance_file, append = FALSE) # clear the output file
for(metric in perf_metrics){
  print(metric)
  cat(metric, '\n', file=cassandra_dimension_importance_file, append=T)
  other_counters <- perf_metrics[which(perf_metrics != metric)]
  filter_column <- names(df_cassandra)[!names(df_cassandra) %in% other_counters]
  
  df_metric_data <- subset(df_cassandra, select=filter_column)
  names(df_metric_data)[names(df_metric_data) == metric] <- "ESDIFF"
  # add label
  df_cassandra_label <- splitEffectSize(df_metric_data, "ESDIFF")
  df_cassandra_code_label <- spread(data = df_cassandra_label, key = code, value = codeNum, fill = 0)
  
  crossDimensionEvaluation(df_cassandra_code_label, evaluate_file = cassandra_dimension_importance_file)
}

############ dimension importance in hadoop
# (1)  remove one dimension, and keep only one dimension
df_hadoop <- fread(hadoop_metric_file, sep = ',')
colnames(df_hadoop) <- metric_names
cat("",file = hadoop_dimension_importance_file, append = FALSE) # clear the output file
for(metric in perf_metrics){
  print(metric)
  
  cat(metric, '\n', file=hadoop_dimension_importance_file, append=T)
  other_counters <- perf_metrics[which(perf_metrics != metric)]
  filter_column <- names(df_hadoop)[!names(df_hadoop) %in% other_counters]
  
  df_metric_data <- subset(df_hadoop, select=filter_column)
  names(df_metric_data)[names(df_metric_data) == metric] <- "ESDIFF"
  # add label
  df_hadoop_label <- splitEffectSize(df_metric_data, "ESDIFF")
  df_hadoop_code_label <- spread(data = df_hadoop_label, key = code, value = codeNum, fill = 0)
  
  crossDimensionEvaluation(df_hadoop_code_label, evaluate_file = hadoop_dimension_importance_file)
}

############ individual metric importance in hadoop, not use any more
df_hadoop <- fread(hadoop_metric_file, sep = ',')
colnames(df_hadoop) <- metric_names
for(metric in perf_metrics){
  print(metric)
  
  importance_file <- paste0(hadoop_individual_importance_file, metric, '.csv')
  other_counters <- perf_metrics[which(perf_metrics != metric)]
  filter_column <- names(df_hadoop)[!names(df_hadoop) %in% other_counters]
  
  df_metric_data <- subset(df_hadoop, select=filter_column)
  names(df_metric_data)[names(df_metric_data) == metric] <- "ESDIFF"
  # add label
  df_hadoop_label <- splitEffectSize(df_metric_data, "ESDIFF")
  df_hadoop_code_label <- spread(data = df_hadoop_label, key = code, value = codeNum, fill = 0)
  
  df_hadoop_tfidf <- tf_idf_code_token(data = df_hadoop_code_label, non_code_metric_names = non_code_metric_names)
  df_hadoop_tfidf_config <- calculate_option_metric(data = df_hadoop_tfidf)
  # importance metrics
  importance_metric <- importantMetric(data = df_hadoop_tfidf_config)
  
  # SK ESD group
  sk_esd_rank <- sk_esd(importance_metric) 
  group_importance_metrics <-bind_rows(importance_metric,sk_esd_rank$groups)
  # 
  write.table(group_importance_metrics, file = importance_file, sep = ",", row.names = FALSE)
  # calculate the average importance for each metric
  sum_metric_ranks <- colSums(importance_metric)
  sum_metric_nonzero <- colSums(importance_metric != 0)   # handle the try error, if all the ranks of one metric is the same
  average_metrics_rank <- sum_metric_ranks/sum_metric_nonzero
  # replace NaN with 0
  average_metrics_rank[is.nan(average_metrics_rank)] <- 0
  # remove zero from the rank
  average_metrics_rank <- average_metrics_rank[average_metrics_rank!=0]
  # order the rank with metric
  order_metrics_rank <- sort(average_metrics_rank, decreasing = T)
  write.table(order_metrics_rank, file = importance_file, sep = ",", row.names = TRUE, append = TRUE)
  
}

############ individual metric importance in cassandra, not use any more
df_cassandra <- fread(cassandra_metric_file, sep = ',')
colnames(df_cassandra) <- metric_names
for(metric in perf_metrics){
  print(metric)
  
  importance_file <- paste0(cassandra_individual_importance_file, metric, '.csv')
  
  other_counters <- perf_metrics[which(perf_metrics != metric)]
  filter_column <- names(df_cassandra)[!names(df_cassandra) %in% other_counters]
  
  df_metric_data <- subset(df_cassandra, select=filter_column)
  names(df_metric_data)[names(df_metric_data) == metric] <- "ESDIFF"
  # add label
  df_cassandra_label <- splitEffectSize(df_metric_data, "ESDIFF")
  df_cassandra_code_label <- spread(data = df_cassandra_label, key = code, value = codeNum, fill = 0)
  
  df_cassandra_tfidf <- tf_idf_code_token(data = df_cassandra_code_label, non_code_metric_names = non_code_metric_names)
  df_cassandra_tfidf_config <- calculate_option_metric(data = df_cassandra_tfidf)
  # importance metrics
  importance_metric <- importantMetric(data = df_cassandra_tfidf_config)
  
  # SK ESD group
  sk_esd_rank <- sk_esd(importance_metric) 
  group_importance_metrics <-bind_rows(importance_metric,sk_esd_rank$groups)
  # 
  write.table(group_importance_metrics, file = importance_file, sep = ",", row.names = FALSE)
  # calculate the average importance for each metric
  sum_metric_ranks <- colSums(importance_metric)
  sum_metric_nonzero <- colSums(importance_metric != 0)   # handle the try error, if all the ranks of one metric is the same
  average_metrics_rank <- sum_metric_ranks/sum_metric_nonzero
  # replace NaN with 0
  average_metrics_rank[is.nan(average_metrics_rank)] <- 0
  # remove zero from the rank
  average_metrics_rank <- average_metrics_rank[average_metrics_rank!=0]
  # order the rank with metric
  order_metrics_rank <- sort(average_metrics_rank, decreasing = T)
  write.table(order_metrics_rank, file = importance_file, sep = ",", row.names = TRUE, append = TRUE)
}

############ boxplot metric dimenssion importance
subject <- 'hadoop'
boxplot_dimenssion_importance(hadoop_dimension_importance_file, metrics = perf_metrics, subject = subject)
