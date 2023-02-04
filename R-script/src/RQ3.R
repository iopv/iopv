# main entry
debug <- FALSE
if(debug){
  metric <- 'runtime'
}
subject <- 'hadoop'
metrics <- c('runtime', 'cpu', 'mem', 'ioread', 'iowrite')

# Discretizing CTO into IoPV and non-IoPV: Figure 1 and Figure 2 in appendix plot threshold
plot_threshold_cluster(subject = subject, metrics = metrics)

# calculate IoPV data
data_cassandra_iopv <- calculate_iopv(metrics = metrics, subject = 'cassandra')
data_hadoop_iopv <- calculate_iopv(metrics = metrics, subject = 'hadoop')

# Preliminary Study: PQ1, percentage of CTO, commit, option suffer from IoPV
data_iopv <- data_cassandra_iopv
data_iopv <- data_hadoop_iopv
# percentage of CTO, commit, option suffer from IoPV
count_cto_iopv(data_iopv)

# Preliminary Study: PQ1, percentage of tests and options in each commit suffer from IoPV
count_testoption_percent_commit_iopv(data_iopv)

# Preliminary Study: PQ1, Figure 3 and Figure 4 in appendix, boxplot of percentage of tests and options in each commit suffer from IoPV
plot_testoption_percent_commit_iopv(data_iopv, subject = subject, metrics = metrics)

# Preliminary Study: PQ1, table3, CTO has no regression in default but regression in other values.
data_cto_no_reg <- count_cto_default_no_regression(metrics = metrics, subject = subject)

# Preliminary Study: PQ1, table4, CTO has improvement in default and regression in other values.
data_cto_imp <- count_cto_default_improvement(metrics = metrics, subject = subject)

# Preliminary Study: PQ1, table5, CTO has regression in default and non-regression in other values.
data_cto_reg <- count_cto_default_regression(metrics = metrics, subject = subject)

# Preliminary Study: PQ2, table6, number of unique commits, tests, options suffer fromIoPV.
count_dimemsion_iopv(data_iopv, metrics = metrics)


# Preliminary Study: PQ2, Figure 3 and Figure 4, jaccard commit distance calculation
for(metric in metrics){
  if(debug == TRUE) { metric <- 'runtime' }
  
  print(metric)
  data <- importdata(metric = metric, subject = subject)
  if (subject == 'cassandra'){
    data_diff_effect_size <- ddply(data, .(release,commit,cur_par_commit,test_class,option,change_class,change_method), differentiate_effect_size)
  }else{
    data_diff_effect_size <- ddply(data, .(release,commit,test_class,option), differentiate_effect_size)
  }
  #plot_test_commitoption_file <- paste0(subject,'/data/PQ2/',subject,'-',metric,'-testX.pdf')
  #jaccard_test_commit_distance(data = data_diff_effect_size, plot_file = plot_test_commitoption_file)
  
  plot_commit_testoption_file <- paste0(subject,'/data/PQ2/',subject,'-',metric,'-commitX.pdf')
  jaccard_commit_testoption_distance(data = data_diff_effect_size, plot_file = plot_commit_testoption_file)
  # break  # test response time metric
}

# generate label diff data for each counter, for RQ2
for (metric in metrics){
  print(metric)
  data <- importdata(metric = metric, subject = subject)
  if (subject == 'cassandra'){
    data_diff_effect_size <- ddply(data, .(release,commit,cur_par_commit,test_class,option,change_class,change_method), differentiate_effect_size)
    data_group_test_option <- ddply(data_diff_effect_size, .(release,commit,cur_par_commit,test_class,option), group_test_option)
  }else{
    data_diff_effect_size <- ddply(data, .(release,commit,test_class,option), differentiate_effect_size)
    data_group_test_option <- ddply(data_diff_effect_size, .(release,commit,test_class,option), group_test_option)
  }
  excel_file <- paste0(subject, '/data/', metric, '-labeldiff.csv')
  write.csv(data_group_test_option, excel_file, row.names = FALSE)
}

plot_commit_test_distribution(data_diff_effect_size, diff_name = "large_diff", save_file = data_file)

