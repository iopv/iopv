# Replication package for FSE submission.

## Structure of the replication package. 

There are two types of folders in this repo: 1) folders for **Code**: `R-script` and `py-script` and 2) folders for **Data and Results**: `cassandra` and `hadoop`. Below, we give detailed descriptions of this two types of folders.

- **Code**
    1. R-script
        - lib        <br />
            This folder includes the packages needed to install.  <br /> 
            Before running the scripts in src, please import R environment `setup.R` and utils for each research question in `pq-lib.R`, `rq1-lib.R`, and `rq2-lib.R.`.<br />
        - src  <br />
            Each script (P/R)Q.R corresponds to the research question of the paper.
     2. py-script
        - dnn model <br />
            `dnn.py` will build neural network model
        - cnn model <br />
            `cnn.py` will build convolutional neural network model
        - combination option <br />
            `sample_combination.py` will calculate the consistent performance result between individual and combination options.
        - canadadra-run-script <br />
            The folder includes scripts and data used to collect performance data of interactions of configuration options in project Cassandra
        - hadoop-run-script <br />
            The folder includes scripts and data used to collect performance data of interactions of configuration options in project Hadoop
- **Data and Results**
    1. cassandra
        - `X-label.csv`  <br />
            Five performance metrics measurement data with statistical analysis, used in RQ1 and RQ2
        - `metric.csv` <br />
            The extracted metrics used to build models in RQ1 and RQ2           
        - perf-measurement  <br />
            This folder contains the raw performance measurement data, including five performance metrics.  <br />
            `runtime.out`: response time measurement data  <br />
            `performance.out`: performance metric data, including CPU, Memory, and Disk IO data.  <br />
        - DL <br />
            The preprocessed data for deep learning model.
        - (P/R)Q1/2 <br />
            Our results corresponding to each preliminary and research question are in this forder. 
    2. hadoop <br />
            The file structure is the same as cassandra. 

