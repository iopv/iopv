setwd('running-environment')
#####################################################################
#import library
#####################################################################
#search library
necessary <- c('stringr','tidyr','plyr','BAMMtools','data.table','MASS','RecordLinkage','TTR','rgl',
               'zoo','microbenchmark','ggplot2','reshape','gtools','rattle','cluster',
               'parallel','inline','qdap','dendextend','effsize','base',"rdist")
installed <- necessary %in% installed.packages()[, 'Package']
if (length(necessary[!installed]) >=1)
  install.packages(necessary[!installed])

library(stringr)
library(tidyr)
library(dplyr)
library(plyr)
library(BAMMtools)
library(data.table)
library(MASS) #rlm model
library(RecordLinkage)
library(zoo) #rollapply
library(ggplot2)
library(reshape) #column to matrix
library(gtools)#ascii
library(R.utils) #lines
library(rattle) #centre.hclust
library(cluster)#pam function
library(factoextra)
library(rdist)
library(TTR)
library(rgl)
library(ggplot2)
library(caret)
library(Hmisc)
library(pROC)
library(randomForest)
library(xgboost)
library(boot)
library(ScottKnottESD)

#source("https://bioconductor.org/biocLite.R")
library(Biostrings)   # biocLite("Biostrings") #installation #lcprefix
library(Biobase)  # biocLite("Biobase") #installation
library(parallel) #parallel coputing
library(inline) #inline cpp program
library(qdap) #multiple repalce, mgsub
library(effsize)
library(base) #readlines
library(arules) # association rule mining
# library(fdm2id)
library(recommenderlab)
library(gplots) # heatmap
library(RColorBrewer)
library(philentropy)


require(Ckmeans.1d.dp)
#####################################################################
#import R script defined by own implementation
#####################################################################
# source('lib/tool.R')
# source('lib/strTool.R')

#####################################################################
#setup execution environment
#####################################################################
#setup time calculation digits
options(digits.secs=6)
options(warn=-1)
#####################################################################

#####################################################################
#setup dir or file of cassandra data
cassandra_metric_file <- "cassandra/data/metric.csv"
cassandra_embedding_file <- "cassandra/data/code_token_embedding_sg_hs.csv"  # code_token_embedding_sg_ns.csv
metric_names <- c("release", "runtime", "cpu", "mem", "ioread", "iowrite", "commithash", "Test", "Option", "NS", "ND", "NF", "Entropy", "NM","NCM","NIM","SOCT", "SUMC", "NCMM", "SOCM", "cyclomatic", "Fanin", "Fanout", "code", "codeNum")
non_code_metric_names <- c("regression", "release", "ESDIFF", "commithash", "Test", "Option", "NS", "ND", "NF", "Entropy", "NM","NCM","NIM","SOCT", "SUMC", "NCMM", "SOCM", "cyclomatic", "Fanin", "Fanout")
non_dl_code_metric_names <- c(non_code_metric_names, 'runtime', 'cpu', 'mem', 'ioread', 'iowrite')

#####################################################################
#setup dir or file of hadoop data
hadoop_column_names <- c("release", "commit", "test_class", "option", "effect_size")

hadoop_metric_file <- "hadoop/data/metric.csv"
hadoop_embedding_file <- "hadoop/data/code_token_embedding_sg_ns.csv"  # code_token_embedding_sg_ns.csv
metric_names <- c("release", "runtime", "cpu", "mem", "ioread", "iowrite", "commithash", "Test", "Option", "NS", "ND", "NF", "Entropy", "NM","NCM","NIM","SOCT", "SUMC", "NCMM", "SOCM", "cyclomatic", "Fanin", "Fanout", "code", "codeNum")
non_code_metric_names <- c("regression", "release", "ESDIFF", "commithash", "Test", "Option", "NS", "ND", "NF", "Entropy", "NM","NCM","NIM","SOCT", "SUMC", "NCMM", "SOCM", "cyclomatic", "Fanin", "Fanout")

#####################################################################
code_change_dimension <- c("NS", "ND", "NF", "Entropy", "NM","NCM","NIM","SOCT", "SUMC", "NCMM", "SOCM")
code_structure_dimension <- c("cyclomatic", "Fanin", "Fanout")
cassandra_dimension_importance_file <- "cassandra/data/RQ2/dimension_importance_100.csv"
cassandra_individual_importance_file <- "cassandra/data/RQ2/importance_.csv"
hadoop_dimension_importance_file <- "hadoop/data/RQ2/dimension_importance_100.csv"
hadoop_individual_importance_file <- "hadoop/data/RQ2/importance_"
