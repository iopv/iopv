#-----------------------------------------------------------------------------
#function: perform Ckmeans.1d.dp to 1d cluster, require(Ckmeans.1d.dp)
#param: df
# test data: 
# x <- c(rnorm(20, sd=0.3), rnorm(20, mean=1, sd=0.3), rnorm(20, mean=2, sd=0.3))
#-----------------------------------------------------------------------------
# splitEffectSize <- function(effect_size_diff){
#   effect_size_diff <- as.numeric(effect_size_diff)
#   kmeans.1d <- Ckmeans.1d.dp(effect_size_diff, k = 2)
#   # Get break points
#   cluster <- kmeans.1d$cluster # group index
#   breaks <- tapply(effect_size_diff, cluster, max) # cal breaks based on index group
#   data.frame(breaks = breaks)
# }

#-----------------------------------------------------------------------------
#calculate effect size breaks in one performance counters
# size_column_name is the name of performance counters, like cpu
#-----------------------------------------------------------------------------
splitEffectSize <- function(data, size_column_name){
  effect_size_diff <- as.numeric(data[[size_column_name]])
  breaks <- Ckmeans.1d.dp(effect_size_diff, k = 2)
  # data.frame(breaks$cluster)
  # data <- data[,(names(data)%in%c(metric_names,'release'))]
  # cbind(data, regression=breaks$cluster-1)
  data.frame(data, regression=breaks$cluster-1)
}

#-----------------------------------------------------------------------------
#calculate effect size breaks in all performance counters
#-----------------------------------------------------------------------------
splitEffectSizeAll <- function(data){
  runtime_effect_size_diff <- as.numeric(data$runtime)
  cpu_effect_size_diff <- as.numeric(data$cpu)
  mem_effect_size_diff <- as.numeric(data$mem)
  io_read_effect_size_diff <- as.numeric(data$ioread)
  io_write_effect_size_diff <- as.numeric(data$iowrite)
  
  runtime <- Ckmeans.1d.dp(runtime_effect_size_diff, k = 2)
  cpu <- Ckmeans.1d.dp(cpu_effect_size_diff, k = 2)
  mem <- Ckmeans.1d.dp(mem_effect_size_diff, k = 2)
  ioread <- Ckmeans.1d.dp(io_read_effect_size_diff, k = 2)
  iowrite <- Ckmeans.1d.dp(io_write_effect_size_diff, k = 2)
  
  data$runtime <- runtime$cluster-1
  data$cpu <- cpu$cluster-1
  data$mem <- mem$cluster-1
  data$ioread <- ioread$cluster-1
  data$iowrite <- iowrite$cluster-1
  return(data)
  # data.frame(data, runtime=runtime$cluster-1, cpu=cpu$cluster-1, mem=mem$cluster-1, ioread=ioread$cluster-1, iowrite=iowrite$cluster-1)
}

#-----------------------------------------------------------------------------
#function: calculate tf-idf (term frequency, inverse document frequency)
#param: data, containing term frequency of each term
#param: non_code_metric_names, non code metric names in one conter, or all 
#     performance counters
# return data frame with tf-idf
#-----------------------------------------------------------------------------
tf_idf_code_token <- function(data, non_code_metric_names=NA){
  # debug
  # data <- training_data

  # data_code_stopword <- dplyr::select(data_code_spread, -one_of(stopwords))
  
  data_non_code_metric <- dplyr::select(data, one_of(non_code_metric_names))
  data_code_metric <- dplyr::select(data, -one_of(non_code_metric_names))
  data_tf_idf <- data.frame(lapply(data_code_metric, function(x) {x*log(nrow(data_code_metric)/sum(x>0)+1)}))
  data_tf_idf[is.na(data_tf_idf)] <- 0
  
  # correlation and redanduncy analysis
  correlations <- cor(data_tf_idf, method="pearson")
  correlations[is.na(correlations)] <- 1 #remove na columns
  highCor <- findCorrelation(correlations, cutoff = .7)
  low_cor_names <- names(data_tf_idf[,-highCor])
  low_cor_data <- data_tf_idf[(names(data_tf_idf)%in%low_cor_names)]
  # remove the redun, it is time consuming
  # redun_obj <- redun(~.,data=low_cor_data,nk=0)
  # after_redun <- low_cor_data[,!(names(low_cor_data)%in%redun_obj $Out)]
  # rank the tf-idf
  tf_idf_max <- sapply(low_cor_data, max)
  tf_idf_rank <- sort(tf_idf_max,decreasing = T)
  top_code_token_names <- names(tf_idf_rank)[1:100]  # top 100 code tokens
  # 
  data_tf_idf_top_tokens <- dplyr::select(data_tf_idf, one_of(top_code_token_names))
  # add token dimension prefix
  colnames(data_tf_idf_top_tokens) <- paste0('token-', names(data_tf_idf_top_tokens))
  
  data_tf_idf_final <- data.frame(data_non_code_metric, data_tf_idf_top_tokens)
  return(data_tf_idf_final)
}

#-----------------------------------------------------------------------------
#function: calculate tf-idf and then use PCA to filter tokens
#param: data, containing term frequency of each term
# return data frame with tf-idf
#-----------------------------------------------------------------------------
tf_idf_PCA_code_token <- function(data, non_code_metric_names=NA){
  # data <- test_data  # debug
  # tf idf calculation
  data_non_code_metric <- dplyr::select(data, one_of(non_code_metric_names))
  data_code_metric <- dplyr::select(data, -one_of(non_code_metric_names))
  data_tf_idf <- data.frame(lapply(data_code_metric, function(x) {x*log(nrow(data_code_metric)/sum(x>0)+1)}))
  
  # PCA analysis
  data_tf_idf[is.na(data_tf_idf)] <- 0
  data_tf_idf <- data_tf_idf[,colSums(data_tf_idf)!=0]
  data_pca <- prcomp(x=data_tf_idf, center = FALSE, scale.=TRUE)
  std_dev <- data_pca$sdev   # compute standard deviation of each principal component
  data_pca_var <- std_dev^2  # compute variance
  data_pca_var_pro <- data_pca_var/sum(data_pca_var)  # proportion of variance explained
  data_pca_var_pro_cum <- cumsum(data_pca_var_pro)  # Cumulative Proportion of Variance Explained
  
  data_pca_filter <- data_pca$x[,data_pca_var_pro_cum<0.95]
  # combine tradictional metrics to the token pcas
  
  data_merge <- data.frame(data_non_code_metric, data_pca_filter)
  return(data_merge)
}

#-----------------------------------------------------------------------------
#function: code embedding
#param: data, code embedding vecotr
#return data frame with metrics with code embedding metrics
#-----------------------------------------------------------------------------
code_embedding <- function(data, data_code_embedding, non_code_metric_names=NA){
  # data_code_embedding <- df_cassandra_code_embedding
  # data_code_spread <- spread(data = data, key = code, value = codeNum, fill = 0)
  
  code_token_names <- data_code_embedding$code_token
  # code_token_names <- intersect(names(data_code_spread), as.character(data_code_embedding$code_token))  # common tokens
  # data_code_embedding <- filter(data_code_embedding, code_token %in% code_token_names)
  
  data_code_metric <- dplyr::select(data, one_of(code_token_names))
  # data_non_code_metric <- dplyr::select(data_code_spread, one_of(c("release", "ESDIFF", "commithash", "Test", "Option")))
  data_non_code_metric <- dplyr::select(data, one_of(non_code_metric_names))
  
  data_tf_idf <- data.frame(lapply(data_code_metric, function(x) {x*log(nrow(data_code_metric)/sum(x>0)+1)}))
  data_tf_idf[is.na(data_tf_idf)] <- 0
  
  data_tf_idf_embedding <- data.frame(as.matrix(data_tf_idf) %*% as.matrix(data_code_embedding[,-1]))
  
  data_tf_idf_embed_final <- data.frame(data_non_code_metric, data_tf_idf_embedding)
  return(data_tf_idf_embed_final)
}

#-----------------------------------------------------------------------------
#function: Add option metric
#param: dataframe, metric withour option metric
#return data frame with metrics with option metrics
#-----------------------------------------------------------------------------
calculate_option_metric <- function(data){
  data_test <- data["Option"]
  data_test$row_num <- seq(1,nrow(data_test),by=1) # row identifier
  data_test$count_no_option <- 1

  data_option <- separate(data = data_test, col=Option, into=c("a","b","c","d","e","f", "g"), sep="[-._]")  # sep is -,.,and _
  data_option[is.na(data_option)] = '0'
  
  data_option_spread <- spread(data = data_option, key=a, value=count_no_option, fill = 0)
  data_option_spread$count_no_option <- 1
  data_option_spread2 <- spread(data = data_option_spread, key=b, value = count_no_option, fill = 0)
  data_option_spread2$count_no_option <- 1
  data_option_spread3 <- spread(data = data_option_spread2, key=c, value = count_no_option, fill = 0)
  data_option_spread3$count_no_option <- 1
  data_option_spread4 <- spread(data = data_option_spread3, key=d, value = count_no_option, fill = 0)
  data_option_spread4$count_no_option <- 1
  data_option_spread5 <- spread(data = data_option_spread4, key=e, value = count_no_option, fill = 0)
  data_option_spread5$count_no_option <- 1
  data_option_spread6 <- spread(data = data_option_spread5, key=f, value = count_no_option, fill = 0)
  data_option_spread6$count_no_option <- 1
  data_option_spread7 <- spread(data = data_option_spread6, key=g, value = count_no_option, fill = 0)
  
  data_option_final <- data_option_spread7[,!names(data_option_spread7)%in%c("row_num", "0")]
  data_option_final <- data_option_final[,colSums(data_option_final)!=0]
  # correlation analysis
  correlations <- cor(data_option_final, method="pearson")
  correlations[is.na(correlations)] <- 1 #remove na columns
  highCor <- findCorrelation(correlations, cutoff = .7)
  low_cor_names <- names(data_option_final[,-highCor])
  low_cor_data <- data_option_final[(names(data_option_final)%in%low_cor_names)]
  # add config prefix
  colnames(low_cor_data) <- paste0('config-', names(low_cor_data))
  
  data_option_final <- data.frame(data, low_cor_data)
  
  return(data_option_final)
}

#-----------------------------------------------------------------------------
#function: generate deep learning data
#param: data
#-----------------------------------------------------------------------------
generateDLdata <- function(data, project=NA, df_code_embedding=NA){
  print('Generating code TFIDF data...')
  tf_idf_data <- tf_idf_code_token(data, non_dl_code_metric_names)
  tf_idf_file <- paste0(project,'/data/dl-data/code_tfidf.csv')
  write.csv(tf_idf_data, file=tf_idf_file, row.names=FALSE)
  
  print('Generating code PCA data...')
  tf_idf_pca_data <- tf_idf_PCA_code_token(data, non_dl_code_metric_names)
  tf_idf_pca_file <- paste0(project,'/data/dl-data/code_pca.csv')
  write.csv(tf_idf_pca_data, file=tf_idf_pca_file, row.names=FALSE)
  
  print('Generating code embedding data...')
  tf_idf_embedding_data <- code_embedding(data, df_code_embedding, non_dl_code_metric_names)
  tf_idf_embedding_file <- paste0(project,'/data/dl-data/code_embedding.csv')
  write.csv(tf_idf_embedding_data, file=tf_idf_embedding_file, row.names=FALSE)
}

#-----------------------------------------------------------------------------
#function: build model
#param: data
# return built model
#-----------------------------------------------------------------------------
buildModel <- function(data, model_name = NA){
  # (1) preprocess data
  # data <- training_data
  # metrics <- data[,!(names(data)%in% c("release", "ESDIFF", "commithash", "Test", "Option", 'regression'))]
  # regression <- data$regression
  # scale the metrics data
  # metrics <- scale(metrics,center = FALSE, scale = TRUE)
  # data <- data.frame(metrics,regression)   # cbind, if we use data.frame(), columns may contain ., like funtion -> funtion.
  # remove the NaN clos
  data <- data[,!(names(data)%in% c("release", "ESDIFF", "commithash", "Test", "Option"))]
  nans <- sapply(data, function(x) all(is.nan(x)))
  data <- data[,!nans]
  data <- data.frame(data)
  
  # (2) correlation and redundancy analysis
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
  # remove the redun, it is time consuming
  # redun_obj <- redun(~.,data=low_cor_data,nk=0)
  # after_redun <- low_cor_data[,!(names(low_cor_data)%in%redun_obj $Out)]
  
  # after_redun$regression <- data$regression
  # (3) build model
  # construct the formula
  factors <- names(independant)
  fla <- paste("regression ~", paste(factors, collapse="+"))
  form <- as.formula(fla)
  if (model_name == 'regression'){
    # build logistic regression model
    fit <- glm(formula = form,data = data,family = binomial(link="logit"))
  }
  if(model_name == 'randomForest'){
    # build randomForest model
    fit <- randomForest(formula=form, data=data, ntree=100, importance=TRUE)
  }
  if(model_name == 'xgboost'){
    # build xgboost model
    class_data <- data$regression  # label
    fit <- xgboost(data = as.matrix(low_cor_data), label = class_data, nrounds = 50, objective = "binary:logistic", max_depth = 6, eta = 0.5, eval_metric = "auc", verbose = 0)
    
  }

  return(fit)
}

#-----------------------------------------------------------------------------
#function: model evaluation
#param: model, testing data
# return precision, recall
#-----------------------------------------------------------------------------
evaluateModel <- function(model, data_test, train_features, model_name = NA) {
  # data_test <- df_test_tf_idf_option
  # model <- fit_tf_idf
  # train_features <- names(df_train_tf_idf_option)
  
  ### add the extra tokens from training data in testing data, default all value is 0
  names_in_train_not_test <- setdiff(train_features, names(data_test))
  df_virtual <- data.frame(matrix(-1, ncol = length(names_in_train_not_test), nrow = nrow(data_test)))
  colnames(df_virtual) <- names_in_train_not_test
  
  data_test <- data_test[,!(names(data_test)%in% c("release", "ESDIFF", "commithash", "Test", "Option"))]
  ### if test columns less than training columns
  data_test <- data.frame(data_test, df_virtual)  #
  
  true_regression <- data_test$regression
  # # scale the metrics data

  if(model_name == 'xgboost'){
    data_test <- data_test[model$feature_names]  # the order of columns in test need to be the same to the training data columns
    data_test <- as.matrix(data_test)
  }
  predictions <- predict(model, newdata=data_test, type="response")
  # print (predictions)
  # precision and recall
  # glm
  TP = sum((predictions>0.5) & (true_regression==1))
  precision = TP / sum((predictions>0.5))
  recall = TP / sum(true_regression)
  # random forest
  # TP_rf = sum(predictions=="TRUE" & (data_test$regression==1))
  # precision = TP_rf / sum((predictions=="TRUE"))
  # recall = TP_rf / sum(data_test$regression)
  # calculate AUC
  roc_obj <- roc(true_regression, predictions)
  auc <- auc(roc_obj)
  print (c(precision,recall,auc))
  return(c(precision,recall,auc))
}



#-----------------------------------------------------------------------------
#function: k fold evaluation
#param: data
# return precision, recall
#-----------------------------------------------------------------------------
crossTfidfEvaluation <- function(data, model_name = NA){
  # data <- df_cassandra_code_label
  # i <- 1
  sample_folds <- split(sample(1:nrow(data)),1:10)
  for(i in 1:10){
    # print(paste0(i," cross validation"))
    # segment your data by i-th fold 
    training_data <- data[-unlist(sample_folds[i]),]
    test_data <- data[unlist(sample_folds[i]),]
    # train data
    df_train_tf_idf <- tf_idf_code_token(data = training_data, non_code_metric_names = non_code_metric_names)  # tokens
    df_train_tf_idf_option <- calculate_option_metric(df_train_tf_idf)

    # building model
    fit_tf_idf <- buildModel(data = df_train_tf_idf_option, model_name = model_name)

    # test_data
    df_test_tf_idf <- tf_idf_code_token(data = test_data, non_code_metric_names = non_code_metric_names)  # tokens
    df_test_tf_idf_option <- calculate_option_metric(df_test_tf_idf)
    
    evaluate_result <- evaluateModel(fit_tf_idf, df_test_tf_idf_option, names(df_train_tf_idf_option), model_name = model_name)
    cat(paste(evaluate_result,collapse=" "), '\n', file=cassandra_evaluate_file, append=T)
  }
}

crossTfidfPCAEvaluation <- function(data, model_name = NA){
  sample_folds <- split(sample(1:nrow(data)),1:10)
  for(i in 1:10){
    training_data <- data[-unlist(sample_folds[i]),]
    test_data <- data[unlist(sample_folds[i]),]
    # train data
    df_train_pca <- tf_idf_PCA_code_token(data = training_data, non_code_metric_names = non_code_metric_names)  # tokens
    df_train_pca_option <- calculate_option_metric(df_train_pca)
    # building model
    fit_pca <- buildModel(data = df_train_pca_option, model_name = model_name)
    
    # test_data
    df_test_pca <- tf_idf_PCA_code_token(data = test_data, non_code_metric_names = non_code_metric_names)  # tokens
    df_test_pca_option <- calculate_option_metric(df_test_pca)
    
    evaluate_result <- evaluateModel(fit_pca, df_test_pca_option, names(df_train_pca_option), model_name = model_name)
    cat(paste(evaluate_result,collapse=" "), '\n', file=cassandra_evaluate_file, append=T)
  }
}

crossEmbeddingEvaluation <- function(data, df_code_embedding,  model_name = NA){
  sample_folds <- split(sample(1:nrow(data)),1:10)
  for(i in 1:10){
    training_data <- data[-unlist(sample_folds[i]),]
    test_data <- data[unlist(sample_folds[i]),]
    # train data
    df_embedding_tf_idf <- code_embedding(data = training_data, df_code_embedding, non_code_metric_names = non_code_metric_names)  # tokens
    df_embedding_tf_idf_option <- calculate_option_metric(df_embedding_tf_idf)
    # building model
    fit_embedding <- buildModel(data = df_embedding_tf_idf_option, model_name = model_name)
    
    # test_data
    df_test_embedding <- code_embedding(data = test_data, df_code_embedding, non_code_metric_names = non_code_metric_names)  # tokens
    df_test_embedding_option <- calculate_option_metric(df_test_embedding)
    
    evaluate_result <- evaluateModel(fit_embedding, df_test_embedding_option, names(df_embedding_tf_idf_option), model_name = model_name)
    cat(paste(evaluate_result,collapse=" "), '\n', file=cassandra_evaluate_file, append=T)
  }
}

crossEvaluation <- function(data, df_code_embedding){
  sample_folds <- split(sample(1:nrow(data)),1:10)
  # add label
  
  for(i in 1:10){
    # print(paste0(i," cross validation"))
    # segment your data by i-th fold 
    training_data <- data[-unlist(sample_folds[i]),]
    test_data <- data[unlist(sample_folds[i]),]
    # train data
    df_train_tf_idf <- tf_idf_code_token(data = training_data)  # tokens
    df_train_pca <- tf_idf_PCA_code_token(data = training_data) # PCA of tokens
    df_train_embedding <- code_embedding(data = training_data, df_code_embedding)  # embedding
    
    # building model
    fit_tf_idf <- buildModel(data = df_train_tf_idf)
    fit_pca <- buildModel(data = df_train_pca)
    fit_embeeding <- buildModel(data = df_train_embedding)
    
    # R_square <- 1- fit$deviance/fit$null.deviance
    
    # test_data
    df_test_tf_idf <- tf_idf_code_token(data = test_data)  # tokens
    df_test_pca <- tf_idf_PCA_code_token(data = test_data) # PCA of tokens
    df_test_embedding <- code_embedding(data = test_data, df_code_embedding)  # embedding
    
    evaluateModel(fit_tf_idf, df_test_tf_idf)
    evaluateModel(fit_pca, df_test_pca)
    evaluateModel(df_test_embedding, df_test_embedding)
  }
}
#-----------------------------------------------------------------------------
#function: mining frequent associated rules
#param: data
# return precision, recall
#-----------------------------------------------------------------------------
mineAssociateRule <- function(){
  ########## mining associated rules
  data_option <- data.frame(table(data$option))
  
  data_diff <- filter(data, max_level %in% c("large", "medium") & default_level %in% c('trivial', 'small'))
  data_diff <- filter(data, min_level %in% c("trivial") & max_level %in% c('large', "medium"))
  # data_diff <- rbind(data_diff1, data_diff2)
  
  data_diff_option <- data.frame(table(data_diff$option))
  
  data_two_option <- inner_join(data_diff_option,data_option, by=c("Var1"))
  data_two_option$rate <- data_two_option$Freq.x/data_two_option$Freq.y
  
  data_training <- filter(data_diff, release %in% c('3.0.11', '3.0.12', '3.0.13', '3.0.14'))
  data_testing <- filter(data_diff, release == '3.0.15')
  
  # build logistic regression model
  fit <- buildModel(data = data_trainset)
  
  R_square <- 1- fit$deviance/fit$null.deviance
  
  evaluateModel(fit, data_testset)
  
  #################### using arules library
  data_training_inspect <- subset(data_training, select=c("test_class", "option", "change_method"))
  data_testing_inspect <- subset(data_testing, select=c("test_class", "option", "change_method"))
  
  #### test_class and option rules
  # the number of tests in testing data not in training data
  test_notin_training_data <- setdiff(data_testing_inspect$test_class, data_training_inspect$test_class)
  
  # find only rules with option column in the right-hand-side
  data_training_transaction <- as(data_training_inspect, "transactions")
  optionItems <- grep("^option=", itemLabels(data_training_transaction), value = TRUE)
  testItems <- grep("^test_class=", itemLabels(data_training_transaction), value = TRUE)
  methodItems <- grep("^change_method=", itemLabels(data_training_transaction), value = TRUE)
  
  rules_training <- apriori(data_training_inspect,
                            parameter=list(support=0.003, confidence=0.5, minlen=2),
                            appearance = list(lhs = testItems, rhs = optionItems))
  data_training_rule <- as(rules_training, "data.frame")
  
  final_rules <- splitRule(data_training_rule)
  
  TP <- inner_join(final_rules, data_testing_inspect, by = c("test_class", "option"))
  TFP <- left_join(final_rules, data_testing_inspect, by = c('test_class'))  # TP + FP
  TNP <- left_join(data_testing_inspect, final_rules, by = c('test_class'))  # TP + FN
  
  
  precision_test <- nrow(TP)/nrow(TFP)
  recall_test <- nrow(TP)/nrow(TNP)
  
  #### change method and option rules
  rules_training <- apriori(data_training_inspect,
                            parameter=list(support=0.005, confidence=0.5, minlen=2),
                            appearance = list(lhs = methodItems, rhs = optionItems))
  data_training_rule <- as(rules_training, "data.frame")
  final_rules <- splitRule(data_training_rule)
  
  TP <- inner_join(final_rules, data_testing_inspect, by = c("change_method", "option"))
  TFP <- left_join(final_rules, data_testing_inspect, by = c('change_method'))  # TP + FP
  TNP <- left_join(data_testing_inspect, final_rules, by = c('change_method'))  # TP + FN
  
  
  precision_method <- nrow(TP)/nrow(TFP)
  recall_method <- nrow(TP)/nrow(TNP)
  
  #### test, change method, and option
  
  rules_training <- apriori(data_training_inspect, parameter=list(support=0.002, confidence=0.5, minlen=3), appearance = list(rhs = optionItems))
  data_training_rule <- as(rules_training, "data.frame")
  final_rules <- splitRule(data_training_rule)
  
  TP <- inner_join(final_rules, data_testing_inspect, by = c("test_class", "option"))
  TFP <- inner_join(final_rules, data_testing_inspect, by = c("test_class"))  # TP + FP
  TNP <- left_join(data_testing_inspect, final_rules, by = c("test_class"))  # TP + FN
  
  precision_test_method <- nrow(TP)/nrow(TFP)
  recall_test_method <- nrow(TP)/nrow(TNP)
  
  
  ########## hadoop associated
  data <- fread(hadoop_runtime_diff_file, sep = ',')
  data_diff <- filter(data, max_level %in% c("large", "medium") & default_level %in% c('trivial', 'small'))
  data_training <- filter(data_diff, release %in% c('2.6.1', '2.6.2', '2.6.3', '2.6.4', '2.7.1', '2.7.2'))
  data_testing <- filter(data_diff, release == '2.7.3')
  data_training_inspect <- subset(data_training, select=c("test_class", "option"))
  data_testing_inspect <- subset(data_testing, select=c("test_class", "option"))
  data_training_transaction <- as(data_training_inspect, "transactions")
  optionItems <- grep("^option=", itemLabels(data_training_transaction), value = TRUE)
  testItems <- grep("^test_class=", itemLabels(data_training_transaction), value = TRUE)
  methodItems <- grep("^change_method=", itemLabels(data_training_transaction), value = TRUE)
  
  rules_training <- apriori(data_training_inspect,
                            parameter=list(support=0.003, confidence=0.5, minlen=2),
                            appearance = list(lhs = testItems, rhs = optionItems))
  data_training_rule <- as(rules_training, "data.frame")
  
  final_rules <- splitRule(data_training_rule)
  
  TP <- inner_join(final_rules, data_testing_inspect, by = c("test_class", "option"))
  TFP <- left_join(final_rules, data_testing_inspect, by = c('test_class'))  # TP + FP
  TNP <- left_join(data_testing_inspect, final_rules, by = c('test_class'))  # TP + FN
  
  
  precision_test <- nrow(TP)/nrow(TFP)
  recall_test <- nrow(TP)/nrow(TNP)
}

#-----------------------------------------------------------------------------
#function: non k-fold cross evaluation
#param: data
# return precision, recall
#-----------------------------------------------------------------------------
old_version_model <- function(){
  # cassandra
  # old version, non k-fold evalution
  df_cassandra_tf_idf <- tf_idf_code_token(data = df_cassandra)  # tokens
  df_cassandra_pca <- tf_idf_PCA_code_token(data = df_cassandra) # PCA of tokens
  df_cassandra_tf_idf_embedding <- code_embedding(data = df_cassandra, df_cassandra_code_embedding)  # embedding
  
  
  df_cassandra_label <- splitEffectSize(df_cassandra_tf_idf, 'ESDIFF')
  df_cassandra_label <- splitEffectSize(df_cassandra_pca, 'ESDIFF')
  df_cassandra_label <- splitEffectSize(df_cassandra_tf_idf_embedding, 'ESDIFF')
  
  # normal train and test data
  # df_cassandra_trainset <- filter(df_cassandra, release %in% c('3.0.11', '3.0.12', '3.0.13', '3.0.14'))
  # df_cassandra_testset  <- filter(df_cassandra, release == '3.0.15')
  
  fit_cassandra <- buildModel(data = df_cassandra_trainset)
  R_square <- 1- fit_cassandra$deviance/fit_cassandra$null.deviance
  
  evaluateModel(fit_cassandra, df_cassandra_testset)
  # hadoop
  # old version
  df_hadoop_tf_idf <- tf_idf_code_token(data = df_hadoop)
  df_hadoop_pca<- tf_idf_PCA_code_token(data = df_hadoop)
  df_hadoop_tf_idf_embedding <- code_embedding(data = df_hadoop, df_hadoop_code_embedding)
  
  # rf-idf 
  df_hadoop_label <- splitEffectSize(df_hadoop_tf_idf, 'ESDIFF')
  # rf-idf using PCA
  df_hadoop_label <- splitEffectSize(df_hadoop_pca, 'ESDIFF')
  # code embedding
  df_hadoop_label <- splitEffectSize(df_hadoop_tf_idf_embedding, 'ESDIFF')
  
  df_hadoop_trainset <- filter(df_hadoop_label, release %in% c("2.6.1", "2.6.2", "2.6.3", "2.6.4", "2.7.1", "2.7.2"))
  df_hadoop_testset  <- filter(df_hadoop_label, release == "2.7.3")
  
  # k fold evaluation
  crossEvaluation(df_hadoop_label)
  
  fit_hadoop <- buildModel(data = df_hadoop_trainset)
  R_square <- 1- fit_hadoop$deviance/fit_hadoop$null.deviance
  
  evaluateModel(fit_hadoop, df_hadoop_testset)
}

#-----------------------------------------------------------------------------
#function: preprocess data to filter code token so that only keeping the frequency
# code token
#param: data, rank rates
# return filtered code token names
#-----------------------------------------------------------------------------
filter_code_token <- function(data, ratio=NA){
  df_code_count <- ddply(data, .(code), summarise, count=sum(codeNum))
  df_code_count <- df_code_count[order(df_code_count$count),]
  code_sum <- sum(df_code_count$count)
  
  for (code_count in df_code_count$count){
    df_code_filter <- filter(df_code_count, count > code_count)
    code_ratio <- sum(df_code_filter$count)/code_sum
    if (code_ratio < ratio)
      break
  }
  df_code_filter <- filter(df_code_filter,nchar(df_code_filter$code) >= 2) # filter one letter code
  df_code_filter <- filter(data, code %in% df_code_filter$code)
  return (df_code_filter)  # codes considered
}

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
