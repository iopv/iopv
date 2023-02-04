import numpy as np
import pandas as pd
import tensorflow as tf
from sklearn.model_selection import KFold


def get_data_set(csv_data_file=None, performance_metric=None):
    # csv_data_file = "../../cassandra/data/dl-data/tf_idf.csv"
    df = pd.read_csv(csv_data_file)

    target = df[performance_metric]

    df = df.drop(['release', 'commithash', 'Test', 'Option', 'runtime', 'cpu', 'mem', 'ioread', 'iowrite'], axis=1)

    data_set = tf.data.Dataset.from_tensor_slices((df.values, target.values))

    X = df.values
    Y = target.values
    # train_dataset = data_set.shuffle(len(df)).batch(1)
    return X, Y


def get_compiled_model(ncols=None):
    """
    Compile CNN model
    :param
        clos: number of columns in X
    :return:
    """
    # print(ncols)
    model = tf.keras.Sequential([
        tf.keras.layers.Conv1D(2, 2, activation='relu', input_shape=(ncols, 1)),
        tf.keras.layers.MaxPool1D(pool_size=2),
        tf.keras.layers.MaxPool1D(pool_size=2),
        tf.keras.layers.Flatten(),
        tf.keras.layers.Dense(1, activation='sigmoid')
    ])

    model.compile(optimizer=tf.keras.optimizers.Adam(0.001),
                  loss='binary_crossentropy',
                  metrics=[tf.keras.metrics.Precision(), tf.keras.metrics.Recall(), tf.keras.metrics.AUC()])
    return model


def fold_evaluation():
    """
    Use scikit-learn to implement K-fold data.
    :return:
    """
    project = 'cassandra'
    code_process_techniques = ['tfidf', 'pca', 'embedding']
    performance_metrics = ['runtime', 'cpu', 'mem', 'ioread', 'iowrite']

    debug = False
    if debug:
        code_process_techniques = ['tfidf']
        performance_metrics = ['runtime']

    for code_process_technique in code_process_techniques:
        print("Processing data: ", code_process_technique)
        evaluation_result_file = open('../{}/data/RQ1/dnn_{}_result.csv'.format(project, code_process_technique), 'w')
        result_buffer = []

        for performance_metric in performance_metrics:
            auc_buffer = []
            precision_buffer = []
            recall_buffer = []
            print("CNN model in performance metric data: ", performance_metric)
            result_buffer.append(performance_metric)
            result_buffer.append('\n')

            data_file = "../{}/data/DL/code_{}.csv".format(project, code_process_technique)
            X, Y = get_data_set(csv_data_file=data_file, performance_metric=performance_metric)

            n_split = 10
            for train_index, test_index in KFold(n_split).split(X):
                x_train, x_test = X[train_index], X[test_index]
                y_train, y_test = Y[train_index], Y[test_index]

                x_train = np.expand_dims(x_train, axis=2)
                x_test = np.expand_dims(x_test, axis=2)

                model = get_compiled_model(X.shape[1])
                model.fit(x_train, y_train, epochs=10, batch_size=100, verbose=2)

                scores = model.evaluate(x_test, y_test, batch_size=100)

                precision = scores[1]
                recall = scores[2]
                auc = scores[3]
                if auc != 0:
                    if auc < 0.5:
                        auc = 1 - auc
                    auc_buffer.append(auc)
                if precision != 0:
                    precision_buffer.append(precision)
                if recall != 0:
                    recall_buffer.append(recall)

                # result_buffer.extend([precision, recall, auc])
                # result_buffer.append('\n')
                print("%s: %.2f%%" % (model.metrics_names[1], scores[1] * 100))
                print("%s: %.2f%%" % (model.metrics_names[2], scores[2] * 100))
                print("%s: %.2f%%" % (model.metrics_names[3], scores[3] * 100))
            # average evaluation result
            auc_average = sum(auc_buffer)/len(auc_buffer)
            if len(precision_buffer) == 0:
                precision_average = 0
            else:
                precision_average = sum(precision_buffer)/len(precision_buffer)
            if len(recall_buffer) == 0:
                recall_average = 0
            else:
                recall_average = sum(recall_buffer)/len(recall_buffer)
            print("average results: ", precision_average, recall_average, auc_average)
            result_buffer.extend([precision_average, recall_average, auc_average])
            result_buffer.append('\n')

        evaluation_result_file.write(','.join(map(str, result_buffer)))


fold_evaluation()