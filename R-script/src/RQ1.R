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

