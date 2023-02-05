#-----------------------------------------------------------------------------
#function: Generating file name and import data
#param: subject, performance metric name
#-----------------------------------------------------------------------------
importdata <- function(subject=NA, metric=NA){
  # file name generation
  # "cassandra/data/runtime-label.csv"
  data_file <- paste0(subject, '/data/', metric, '-label.csv')
  
  # import five counter the data
  if(subject == 'cassandra'){
    data <- fread(data_file, sep=",", header = FALSE, select = c("V1","V4","V5","V6","V9","V10","V11","V15","V16","V48"))
    colnames(data) <- cassandra_column_names
    data <- filter(data, data$commit!=data$cur_par_commit)  # keep the current commit
  }else{
    data <- fread(data_file, sep=",", header = FALSE, select = c("V1","V2","V3","V4","V37"))
    colnames(data) <- hadoop_column_names
    # odd <- seq(1,nrow(data),2)  # get odd rows
    # data <- data[odd,]
  }
  return (data)
}

#-----------------------------------------------------------------------------
#function: Generating effect size difference
#param: subject, performance metric name
#-----------------------------------------------------------------------------
diffEffectsize <- function(subject=NA, metric=NA){
  data <- importdata(metric = metric, subject = subject)
  if (subject == 'cassandra'){
    df_diff_effectsize <- ddply(data, 
                                .(release,commit,cur_par_commit,test_class,option), 
                                differentiate_effect_size)
  }
  else{
    df_diff_effectsize <- ddply(data, 
                                .(release,commit,test_class,option), 
                                differentiate_effect_size)
  }
  return(df_diff_effectsize)
}

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
#function: Plot threshold of one-dimension clustering diff effect size 
#param: subject, performance metric name
#-----------------------------------------------------------------------------
plot_threshold_cluster <-function(subject=NA, metrics=NA){
  
  for (metric in metrics){
    print(metric)
    data_diff_effect_size <- diffEffectsize(metric = metric, subject = subject)
    effect_size_diff <- as.numeric(data_diff_effect_size$large_diff)
    breaks <- Ckmeans.1d.dp(effect_size_diff, k = 2)
    data_cluster <- data.frame(diff=effect_size_diff, cluster=breaks$cluster)
    data_cluster1 <- filter(data_cluster, cluster == 1)
    split_point <- max(data_cluster1$diff)
    # plot
    plot_file <- paste0(subject,'/data/PQ1/',metric,'-',subject,'-cluster.pdf')
    pdf(plot_file)
    p <- ggplot(data_cluster, aes(x=diff)) + geom_density() + geom_vline(aes(xintercept=mean(split_point)), linetype="dashed") + labs(title="",x="", y = "") + theme(panel.grid = element_blank(),panel.background = element_rect(fill = "white"),panel.border = element_rect(colour = "black", fill=NA, size=0.6),text = element_text(size=40))
    print(p)
    dev.off()
  }
  
}

#-----------------------------------------------------------------------------
#function: Preliminary Study based function
# Preliminary Study: calculate IoPV data from all dimensions, all perf metrics
# param: data
# return: a dataframe that contains all IoPV data
#-----------------------------------------------------------------------------
calculate_iopv <- function(metrics=NA, subject=NA){
  for (metric in metrics) {
    data_diff_effect_size <- diffEffectsize(metric = metric, subject = subject)
    ###### calculate IoPV
    min_effect_size <- data_diff_effect_size$min
    max_effect_size <- data_diff_effect_size$max
    cto_iopv_regression <- (min_effect_size<0.333 & max_effect_size>0.333)  # regression and non-regression
    cto_iopv_improvement <- (min_effect_size<(-0.333) & max_effect_size>(-0.333))  # improvement and non-improvement
    cto_iopv_combine <- (cto_iopv_regression | cto_iopv_improvement)
    # initial iopv data
    data_iopv_based <- unite(data_diff_effect_size, col=commit_test_option, commit, test_class, option, sep = '#')
    if (metric == 'runtime'){ 
      data_iopv_combine <- dplyr::select(data_iopv_based, commit_test_option)
      data_iopv_combine[[paste0('iopv_', metric)]] <- cto_iopv_combine
    }else{
      diff_commit_test_option <- setdiff(data_iopv_combine$commit_test_option, data_iopv_based$commit_test_option)
      data_iopv_combine <- filter(data_iopv_combine, !(commit_test_option %in% diff_commit_test_option))
      # add column iopv to each performance metric
      data_iopv_combine[[paste0('iopv_', metric)]] <- cto_iopv_combine
    }
  }
  # cto suffer from IoPV in any of performance metric
  cto_iopv_any_perf_metric <- (data_iopv_combine$iopv_runtime | data_iopv_combine$iopv_cpu | data_iopv_combine$iopv_mem | data_iopv_combine$iopv_ioread | data_iopv_combine$iopv_iowrite)
  data_iopv_combine$iopv_any <- cto_iopv_any_perf_metric
  data_iopv_combine <- separate(data = data_iopv_combine, col = commit_test_option, into = c("commit","test_class","option"), sep = '#')
  return(data_iopv_combine)
}

#-----------------------------------------------------------------------------
#function: Preliminary Study
# Preliminary Study: RQ1, percentage of CTO, commit, option suffer from IoPV
# param: dada
#-----------------------------------------------------------------------------
count_cto_iopv <- function(data_iopv=NA){
  
  # Number of CTO suffers from IoPV in any performance counter
  num_iopv_any <- sum(data_iopv$iopv_any)
  print(paste0("Percentage of CTO suffers from IoPV in any performance metric"))
  print(num_iopv_any/nrow(data_iopv))
  
  # Percentage of commit suffers from IoPV in any performance counter
  data_iopv_commit <- filter(data_iopv, iopv_any=='TRUE')
  num_commit_iopv <- length(unique(data_iopv_commit$commit))
  percent_commit_iopv <- num_commit_iopv/length(unique(data_iopv$commit))
  print(paste0("Percentage of commit suffers from IoPV in any performance metric"))
  print(percent_commit_iopv)
  
  # Percentage of option suffers from IoPV in any performance counter
  num_option_iopv <- length(unique(data_iopv_commit$option))
  percent_option_iopv <- num_option_iopv/length(unique(data_iopv$option))
  print(paste0("Percentage of option suffers from IoPV in any performance metric"))
  print(percent_option_iopv)

}

#-----------------------------------------------------------------------------
#function: Preliminary Study
# Preliminary Study: RQ1, percentage of test and options in each commit suffer from IoPV
# param: dada
#-----------------------------------------------------------------------------
count_testoption_percent_commit_iopv <- function(data_iopv=NA){
  # percentage of test and options in each commit suffer from IoPV in any performance metric
  data_iopv_commit <- ddply(data_iopv, .(commit), function(data){
    total_test_option <- nrow(data)
    iopv_test_option <- sum(data$iopv_any)
    percent_iopv <- iopv_test_option/total_test_option
    data.frame(total_test_option=total_test_option, iopv_test_option=iopv_test_option, percent_iopv=percent_iopv)
  })
  # median of percentage of test option in each commit suffer from IoPV
  median_percent_iopv <- median(data_iopv_commit$percent_iopv)
  print("median of percentage of test option in each commit suffer from IoPV")
  print(median_percent_iopv)
}

#-----------------------------------------------------------------------------
#function: Preliminary Study
# Preliminary Study: RQ1, figure of test and options in each commit suffer from IoPV
# param: iopv data, Figure3, and figure4
#-----------------------------------------------------------------------------
plot_testoption_percent_commit_iopv <- function(data_iopv=NA, subject=NA, metrics=NA){
  # percentage of test and options in each commit suffer from IoPV
  # data_iopv <- data_hadoop_iopv
  data_iopv_commit <- ddply(data_iopv, .(commit), function(data){
    total_test_option <- nrow(data)
    iopv_test_option_runtime <- sum(data$iopv_runtime)
    iopv_test_option_cpu <- sum(data$iopv_cpu)
    iopv_test_option_mem <- sum(data$iopv_mem)
    iopv_test_option_ioread <- sum(data$iopv_ioread)
    iopv_test_option_iowrite <- sum(data$iopv_iowrite)
    percent_iopv_runtime <- iopv_test_option_runtime/total_test_option
    percent_iopv_cpu <- iopv_test_option_cpu/total_test_option
    percent_iopv_mem <- iopv_test_option_mem/total_test_option
    percent_iopv_ioread <- iopv_test_option_ioread/total_test_option
    percent_iopv_iowrite <- iopv_test_option_iowrite/total_test_option
    
    data.frame(total_test_option=total_test_option, percent_iopv_runtime, percent_iopv_cpu, percent_iopv_mem, percent_iopv_ioread, percent_iopv_iowrite)
  })
  # boxplot in each performance metric
  for (metric in metrics){
    boxplot_file <- paste0(subject,'/data/PQ1/',metric,'-',subject,'-boxplot.pdf')
    pdf(file=boxplot_file)
    boxplot(data_iopv_commit[[paste0('percent_iopv_', metric)]], 1-data_iopv_commit[[paste0('percent_iopv_', metric)]], names=c("IoPV","non-IoPV"),cex.axis=3)
    dev.off()
  }
}

#-----------------------------------------------------------------------------
#function: the number of CTO that the default value has no regression but the
#          min or max option value has regression
# for Table2
# param: data
#-----------------------------------------------------------------------------
count_cto_default_no_regression <- function(metrics=NA, subject=NA){
  for (metric in metrics) {
    print(metric)
    data_diff_effect_size <- diffEffectsize(metric = metric, subject = subject)
    ###### calculate medium, and large regression
    default <- data_diff_effect_size$default
    max <- data_diff_effect_size$max
    all_regression <- (default<0.33 & max>0.333)
    large_regression <- (default<0.33 & max>0.474)
    num_cto_large_regression <- sum(large_regression)
    num_cto_medium_regression <- sum(all_regression) - num_cto_large_regression
    
    result <- c(num_cto_large_regression, num_cto_medium_regression)
    print(result)
    # combine cto regression in each performance metric
    data_reg_based <- unite(data_diff_effect_size, col=commit_test_option, commit, test_class, option, sep = '#')
    if (metric == 'runtime'){ 
      data_reg_combine <- dplyr::select(data_reg_based, commit_test_option)
      data_reg_combine[[paste0('reg_', metric)]] <- all_regression
    }else{
      diff_commit_test_option <- setdiff(data_reg_combine$commit_test_option, data_reg_based$commit_test_option)
      data_reg_combine <- filter(data_reg_combine, !(commit_test_option %in% diff_commit_test_option))
      # add column iopv to each performance metric
      data_reg_combine[[paste0('reg_', metric)]] <- all_regression
    }
  }
  # cto has regression in any of performance metric
  reg_any_perf_metric <- (data_reg_combine$reg_runtime | data_reg_combine$reg_cpu | data_reg_combine$reg_mem | data_reg_combine$reg_ioread | data_reg_combine$reg_iowrite)
  data_reg_combine$reg_any <- reg_any_perf_metric
  data_reg_combine <- separate(data = data_reg_combine, col = commit_test_option, into = c("commit","test_class","option"), sep = '#')
  # Percentage of cto has regression in any of performance metric
  num_reg_any_perf_metric <- sum(data_reg_combine$reg_any)
  total_cto <- nrow(data_reg_combine)
  print("percentage of cto has regression in any of performance metric")
  print(paste0("num_reg_any_perf_metric: ",num_reg_any_perf_metric, "  total_cto :", total_cto))
  print(num_reg_any_perf_metric/total_cto)
  
  # Percentage of commit has regression in any of performance metric
  data_commit_reg <- ddply(data_reg_combine, .(commit), function(data) { commit_reg <- (sum(data$reg_any)>0) })
  num_reg_commit <- sum(data_commit_reg$V1)
  total_commit <- nrow(data_commit_reg)
  print("Percentage of commit has regression in any of performance metric")
  print(paste0("num_reg_commit: ",num_reg_commit, "  total_commit :", total_commit))
  print(num_reg_commit/total_commit)
  
  return(data_reg_combine)
}

#-----------------------------------------------------------------------------
#function: the number of CTO that the default value has improvement but the
#          other value has regression
# for Table3
# param: data
#-----------------------------------------------------------------------------
count_cto_default_improvement <- function(metrics=NA, subject=NA){
  for (metric in metrics) {
    print(metric)
    data_diff_effect_size <- diffEffectsize(metric = metric, subject = subject)
    ###### calculate medium, and large regression
    default <- data_diff_effect_size$default
    max <- data_diff_effect_size$max
    all_regression <- (default<(-0.333) & max>0.333)   # default is improvement, other has regression
    large_regression <- (default<(-0.333) & max>0.474)
    num_cto_large_regression <- sum(large_regression)
    num_cto_medium_regression <- sum(all_regression) - num_cto_large_regression
    
    result <- c(num_cto_large_regression, num_cto_medium_regression)
    print(result)
    # combine cto regression in each performance metric
    data_reg_based <- unite(data_diff_effect_size, col=commit_test_option, commit, test_class, option, sep = '#')
    if (metric == 'runtime'){ 
      data_reg_combine <- dplyr::select(data_reg_based, commit_test_option)
      data_reg_combine[[paste0('reg_', metric)]] <- all_regression
    }else{
      diff_commit_test_option <- setdiff(data_reg_combine$commit_test_option, data_reg_based$commit_test_option)
      data_reg_combine <- filter(data_reg_combine, !(commit_test_option %in% diff_commit_test_option))
      # add column iopv to each performance metric
      data_reg_combine[[paste0('reg_', metric)]] <- all_regression
    }
  }
  # cto has regression in any of performance metric
  reg_any_perf_metric <- (data_reg_combine$reg_runtime | data_reg_combine$reg_cpu | data_reg_combine$reg_mem | data_reg_combine$reg_ioread | data_reg_combine$reg_iowrite)
  data_reg_combine$reg_any <- reg_any_perf_metric
  data_reg_combine <- separate(data = data_reg_combine, col = commit_test_option, into = c("commit","test_class","option"), sep = '#')
  # Percentage of cto has regression in any of performance metric
  num_reg_any_perf_metric <- sum(data_reg_combine$reg_any)
  total_cto <- nrow(data_reg_combine)
  print("percentage of cto has improvement in default but regression in others in any of performance metric")
  print(paste0("num_impro_any_perf_metric: ",num_reg_any_perf_metric, "  total_cto :", total_cto))
  print(num_reg_any_perf_metric/total_cto)
  
  # Percentage of commit has regression in any of performance metric
  data_commit_reg <- ddply(data_reg_combine, .(commit), function(data) { 
    commit_reg <- (sum(data$reg_any)>0)
    total_cto <- nrow(data)
    reg_cto <- sum(data$reg_any)
    percent_cto_commit <- reg_cto/total_cto
    data.frame(total_cto=total_cto, reg_cto=reg_cto, commit_reg=commit_reg, percent_cto_commit=percent_cto_commit)
    })
  num_reg_commit <- sum(data_commit_reg$commit_reg)
  total_commit <- nrow(data_commit_reg)
  print("Percentage of commit has regression in any of performance metric")
  print(num_reg_commit/total_commit)
  # percentage of cto in each commit
  percentage_cto_per_commit <- median(data_commit_reg$percent_cto_commit)
  print("Median percentage of cto in each commit has improvement in default but regression in other values in any of performance metric")
  print(percentage_cto_per_commit)
  
  return(data_reg_combine)
}

#-----------------------------------------------------------------------------
#function: the number of CTO that the default value has regression but the
#          min or max option value has non regression (improvement)
# for Table4
# param: data
#-----------------------------------------------------------------------------
count_cto_default_regression <- function(metrics=NA, subject=NA){
  for (metric in metrics) {
    print(metric)
    data_diff_effect_size <- diffEffectsize(metric = metric, subject = subject)
    ###### calculate medium, and large regression
    default <- data_diff_effect_size$default
    min <- data_diff_effect_size$min
    all_regression <- (min<0.333 & default>0.333)
    large_regression <- (min<0.333 & default>0.474)
    num_cto_large_regression <- sum(large_regression)
    num_cto_medium_regression <- sum(all_regression) - num_cto_large_regression
    
    result <- c(num_cto_large_regression, num_cto_medium_regression)
    print(result)
    # combine cto regression in each performance metric
    data_reg_based <- unite(data_diff_effect_size, col=commit_test_option, commit, test_class, option, sep = '#')
    if (metric == 'runtime'){ 
      data_reg_combine <- dplyr::select(data_reg_based, commit_test_option)
      data_reg_combine[[paste0('reg_', metric)]] <- all_regression
    }else{
      diff_commit_test_option <- setdiff(data_reg_combine$commit_test_option, data_reg_based$commit_test_option)
      data_reg_combine <- filter(data_reg_combine, !(commit_test_option %in% diff_commit_test_option))
      # add column reg to each performance metric
      data_reg_combine[[paste0('reg_', metric)]] <- all_regression
    }
  }
  
  # cto has regression in any of performance metric
  reg_any_perf_metric <- (data_reg_combine$reg_runtime | data_reg_combine$reg_cpu | data_reg_combine$reg_mem | data_reg_combine$reg_ioread | data_reg_combine$reg_iowrite)
  data_reg_combine$reg_any <- reg_any_perf_metric
  data_reg_combine <- separate(data = data_reg_combine, col = commit_test_option, into = c("commit","test_class","option"), sep = '#')
  # Percentage of cto has regression in any of performance metric
  num_reg_any_perf_metric <- sum(data_reg_combine$reg_any)
  total_cto <- nrow(data_reg_combine)
  print("percentage of cto has regression in default but non-regression in others in any of performance metric")
  print(paste0("num_impro_any_perf_metric: ",num_reg_any_perf_metric, "  total_cto :", total_cto))
  print(num_reg_any_perf_metric/total_cto)
  
  return(data_reg_combine)
}

#-----------------------------------------------------------------------------
#function: count IoPV in dimemsion of commit, test, cto
# for table5
# param: dada of IoPV
#-----------------------------------------------------------------------------
count_dimemsion_iopv <- function(data_iopv=NA, metrics=NA){
  metrics <- c(metrics, 'any')
  commit_total <- length(unique(data_iopv$commit))
  test_total <- length(unique(data_iopv$test_class))
  option_total <- length(unique(data_iopv$option))
  for (metric in metrics){
    iopv_metric <- paste0('iopv_',metric)
    data_iopv_metric <- filter(data_iopv, data_iopv[[iopv_metric]] == TRUE)
    commit_iopv <- length(unique(data_iopv_metric$commit))
    test_iopv <- length(unique(data_iopv_metric$test_class))
    option_iopv <- length(unique(data_iopv_metric$option))
    result <- c(commit_total, commit_iopv, test_total, test_iopv, option_total, option_iopv)
    print(result)
  }
}

#-----------------------------------------------------------------------------
#function: count regression in dimemsion of commit, test, option, values
# unique unit tests, option with regression in all performance metrics
# for table6
# param: dada
#-----------------------------------------------------------------------------
count_dimemsion_regression <- function(data){
  data_filter <- filter(data, effect_size > 0.333)
  commit_total <- length(unique(data$commit))
  commit_reg <- length(unique(data_filter$commit))
  test_total <- length(unique(data$test_class))
  test_reg <- length(unique(data_filter$test_class))
  option_total <- length(unique(data$option))
  option_reg <- length(unique(data_filter$option))
  value_total <- nrow(data)
  value_reg <- nrow(data_filter)
  result <- c(commit_total, commit_reg, test_total, test_reg, option_total, option_reg, value_total, value_reg)
  
  print(result)
  reg_test_option <- list(unique(data_filter$test_class), unique(data_filter$option))
}


#-----------------------------------------------------------------------------
#function: commit option Jaccard distance picture
# for figure2
# param: dada diff effect size
#-----------------------------------------------------------------------------

binarySimilarity <- function(x, y) {
  # x and y are 2 binary (0-1) vectors
  shared <- sum(x&y)  # tuple occurs together
  # shared <- sum(x==y)
  total <- sum(x|y)
  return(shared/total)
  # Jarccard distance between two consecutive commits
  # option_total <- ncol(data_spread_option) - 1
  # commit_jaccard_distance <- rollapply(seq(nrow(data_spread_option)), 2, function(tuple){
  #   tuple1 <- tuple[1]
  #   tuple2 <- tuple[2]
  #   cur_commit <- as.numeric(data_spread_option[tuple1,-1])
  #   next_commit <- as.numeric(data_spread_option[tuple2,-1])
  #   option_same <- sum(cur_commit == next_commit)
  #   return(option_same/option_total)
  # })
  # 
  # plot(x=commit_jaccard_distance)
  # dev.new()
  # plot(seq(nrow(data_spread_option)-1),commit_jaccard_distance,xlim = c(1,nrow(data_spread_option)-1),ylim=c(0.5,1), type = 'o')
  # dev.off()
}

jaccard_commit_distance <- function(data, plot_file=NA){
  if (debug == TRUE){
    data <- data_diff_effect_size
  }
  data_filter <- ddply(data, .(commit, option), function(data_option){
    max_diff_effect_size <- max(data_option$large_diff)
  })
  data_label <- splitEffectSize(data_filter, size_column_name = "V1")
  data_label["V1"] <- NULL
  data_spread_option <- spread(data_label, key=option, value=regression,fill = 0)
  
  df_similarity_initial <- t(data_spread_option[,-1])
  nvar <- ncol(df_similarity_initial)
  simMatrix <- matrix(nrow = nvar, ncol = nvar)
  for(i in 1:nvar)  for (j in 1:nvar) {
    simMatrix[i,j] <- binarySimilarity(df_similarity_initial[,i], df_similarity_initial[,j])
  }  
  # dimnames(simMatrix) <- list(names(df_similarity_initial), names(df_similarity_initial))
  jaccard_dist <- 1 - simMatrix
  
  # the dist default function is not applicable in this situation
  # jaccard_dist <- as.matrix(dist(data_spread_option[,-1], method = "JACCARD"))
  
  # coul <- colorRampPalette(brewer.pal(8, "Spectral"))(15) #change color
  coul <- colorRampPalette(colors = c("lightBlue","blue"))(256)
  pdf(plot_file)
  # par(mar=c(1,1,1,1))
  heatmap.2(jaccard_dist,
          Rowv=F, Colv=F, # no cluster
          trace="none", # no trace for cell
          labRow=F, labCol=F, # no xaxis and yaxis label
          cexRow=1,cexCol=1,
          key=FALSE, keysize=0.1, # no legend
          col=coul, # color
          margins = c(1, 1)
          )
  dev.off()
}
#-----------------------------------------------------------------------------
#function: test Jaccard distance, based on commit and option both
# for figure2, figure3
# param: dada diff effect size
#-----------------------------------------------------------------------------
jaccard_test_commit_distance <- function(data, plot_file=NA){
  if (debug == TRUE){
    data <- data_diff_effect_size
  }
  data_filter <- ddply(data, .(test_class, option), function(data_option){
    max_diff_effect_size <- max(data_option$large_diff)
  })
  data_label <- splitEffectSize(data_filter, size_column_name = "V1")
  data_label["V1"] <- NULL
  data_spread_option <- spread(data_label, key=option, value=regression,fill = 0)
  
  df_similarity_initial <- t(data_spread_option[,-1])
  df_similarity_initial <- data_spread_option[,-1]
  nvar <- ncol(df_similarity_initial)
  simMatrix <- matrix(nrow = nvar, ncol = nvar)
  for(i in 1:nvar)  for (j in 1:nvar) {
    simMatrix[i,j] <- binarySimilarity(df_similarity_initial[,i], df_similarity_initial[,j])
  }  
  
  jaccard_dist <- 1 - simMatrix
  
  coul <- colorRampPalette(colors = c("lightBlue","blue"))(256)
  
  pdf(plot_file)
  # par(mar=c(1,1,1,1))
  heatmap.2(jaccard_dist,
            Rowv=F, Colv=F, # no cluster
            trace="none", # no trace for cell
            labRow=F, labCol=F, # no xaxis and yaxis label
            cexRow=1,cexCol=1,
            key=FALSE, keysize=0.1, # no legend
            col=coul, # color
            margins = c(1, 1)
  )
  dev.off()
}
#-----------------------------------------------------------------------------
#function: commit Jaccard distance, based on test and option seperately
# for figure2, figure3
# param: dada diff effect size
#-----------------------------------------------------------------------------
jaccard_commit_testoption_distance <- function(data, plot_file=NA){
  if (debug == TRUE){
    data <- data_diff_effect_size
    plot_file <- plot_commit_testoption_file
  }
  data_merge_test_option <- unite(data = data, col = test_option, test_class, option)
  data_filter <- ddply(data_merge_test_option, .(commit, test_option), function(data_option){
    max_diff_effect_size <- max(data_option$large_diff)
  })
  data_label <- splitEffectSize(data_filter, size_column_name = "V1")
  data_label["V1"] <- NULL
  data_merge_test_option_diff <- unite(data = data_label, col = test_option_diff_tuple, test_option, regression)
  
  df_commit_tuple_count <- ddply(data_merge_test_option_diff, .(commit, test_option_diff_tuple), tally)
  data_commit_spread_tuple <- spread(df_commit_tuple_count, key=test_option_diff_tuple, value=n,fill = 0)
  
  df_similarity_initial <- t(data_commit_spread_tuple[,-1])
  nvar <- ncol(df_similarity_initial)
  simMatrix <- matrix(nrow = nvar, ncol = nvar)
  for(i in 1:nvar)  for (j in 1:nvar) {
    simMatrix[i,j] <- binarySimilarity(df_similarity_initial[,i], df_similarity_initial[,j])
  }  

  jaccard_dist <- 1 - simMatrix
  
  coul <- colorRampPalette(colors = c("white", "black"))(256)
  
  pdf(plot_file)
  # par(mar=c(1,1,1,1))
  heatmap.2(jaccard_dist,
            Rowv=F, Colv=F, # no cluster
            trace="none", # no trace for cell
            labRow=F, labCol=F, # no xaxis and yaxis label
            cexRow=1,cexCol=1,
            key=FALSE, keysize=0.1, # no legend
            col=coul, # color
            margins = c(1, 1)
  )
  dev.off()
}

effect_size_label <- function(effect_size_v){
  if (effect_size_v > 0.474)
    return('large')
  else if(effect_size_v > 0.333)
    return('medium')
  else if(effect_size_v > 0.147)
    return('small')
  else
    return('trivial')
}

differentiate_effect_size <- function(data){
  effect_sizes <- data$effect_size
  
  max_es = max(effect_sizes)
  min_es = min(effect_sizes)
  default_es = effect_sizes[1]  # the first option value is the default value
  large_diff_es <- max_es - min_es
  default_diff_es <- max_es - default_es
  data.frame(max = max_es, max_level=effect_size_label(max_es), min= min_es, min_level=effect_size_label(min_es), default=default_es,default_level=effect_size_label(default_es), large_diff= large_diff_es, default_diff = default_diff_es)
}

group_test_option <- function(data){
  max_min_diff <- max(data$large_diff)
  max_default_diff <- max(data$default_diff)
  data.frame(max_min_diff=max_min_diff, max_default_diff=max_default_diff)
}

#-----------------------------------------------------------------------------
#function: calculating effect size distribution 
# param: dada
#-----------------------------------------------------------------------------
# 
effect_size_distribution <- function(data){
  data$effect_size_diff <- data$max - data$min
  
  top_options <- c('tombstone_failure_threshold', 'tombstone_warn_threshold', 'file_cache_size_in_mb', 'read_request_timeout_in_ms',
                   'buffer_pool_use_heap_if_exhausted', 'concurrent_materialized_view_writes', 'memtable_allocation_type',
                   'memtable_heap_space_in_mb', 'memtable_offheap_space_in_mb', 'memtable_flush_writers', 'concurrent_counter_writes',
                   'concurrent_reads', 'concurrent_writes')
  
  lapply(top_options, function(top_option) {
    option_effect_diff <- filter(data, option == top_option)
    pdf(paste0(getwd(),'/cassandra/data/plot/',top_option,'.pdf'))
    p <- ggplot(option_effect_diff, aes(x=effect_size_diff)) + geom_density(color = "black", fill = "gray")
    print(p)
    dev.off()
  })
  
  tombstone_failure_threshold_effect_diff <- filter(data, option == 'tombstone_failure_threshold')
  
  hist(tombstone_failure_threshold_effect_diff$effect_size_diff,breaks = 100)
  p <- ggplot(tombstone_failure_threshold_effect_diff, aes(x=effect_size_diff)) + geom_density(color = "black", fill = "gray")
}
