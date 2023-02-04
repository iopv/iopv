import json
import pandas as pd

project = 'cassandra'
perf_metrics = ['runtime', 'mem', 'cpu', 'ioread', 'iowrite']
combination_result_file = '../{}/data/combination_results.json'.format(project)


with open(combination_result_file, 'r') as f:
    combination_results = json.load(f)
    f.close()

print('Processing combination result...')
# [ {commit+test+option1+option1_value: [effect size, true/false],
#                 commit+test+option2+option2_value: [effect size, true/false],
#                 commit+test+option3+option3_value: [effect size, true/false],
#                 commit+test+option4+option4_value: [effect size, true/false]} ]
combination_dict_list = []
for commit, combination_result in combination_results.items():
    combination_dict = {}
    commit_test_option = commit.split(',')
    cur_commit, par_commit, test = commit_test_option[0], commit_test_option[1], commit_test_option[2]
    option1, option1_value = commit_test_option[3], commit_test_option[4]
    option2, option2_value = commit_test_option[5], commit_test_option[6]
    option3, option3_value = commit_test_option[7], commit_test_option[8]
    option4, option4_value = commit_test_option[9], commit_test_option[10]

    if combination_result['runtime'][2] is None:
        runtime_com_label = False
    else:
        runtime_com_label = abs(combination_result['runtime'][2]) > 0.33

    combination_dict[par_commit + test + option1 + str(option1_value).lower()] = runtime_com_label
    combination_dict[par_commit + test + option2 + str(option2_value).lower()] = runtime_com_label
    combination_dict[par_commit + test + option3 + str(option3_value).lower()] = runtime_com_label
    combination_dict[par_commit + test + option4 + str(option4_value).lower()] = runtime_com_label
    combination_dict_list.append(combination_dict)
    # combination_dict.clear()

print('Processing one option result...')
for perf_metric in perf_metrics:
    print('Processing in metric ', perf_metric)
    single_runtime_result_file = '../{}/data/{}-label.csv'.format(project, perf_metric)
    # Result of only one option
    perf_result = pd.read_csv(single_runtime_result_file)
    # {commit+test+option+option_value: true/false }
    one_option_dict = {}
    if project == 'cassandra':
        for i, test in perf_result.iterrows():
            if i % 2 == 0:
                cur_commit = test[14]
            if i % 2 == 1:
                par_commit = test[14]
                test_case = test[4]
                option_name = test[3]
                option_value = test[15]
                p_value = test[-2]
                effect_size = test[-1]

                one_option_dict[par_commit + test_case + option_name + str(option_value).lower()] = abs(effect_size) > 0.33

    elif project == 'hadoop':
        for i, test in perf_result.iterrows():
            if i % 2 == 0:
                cur_commit = test[1]
            if i % 2 == 1:
                par_commit = test[1]
                test_case = test[2]
                option_name = test[3]
                option_value = test[4]
                effect_size = test[-1]

                one_option_dict[par_commit + test_case + option_name + str(option_value).lower()] = abs(effect_size) > 0.33

    print('Comparing one option result to combination result...')
    # check the results of combination in single option results
    diff_one_com_option = 0
    for com_index, combination_dict in enumerate(combination_dict_list):
        for option_key, com_result in combination_dict.items():  # one sample combination result, for options
            # get one option result
            if not com_result and one_option_dict.get(option_key):  # combination is false, not regression
                one_option_result = one_option_dict[option_key]
                if one_option_result:  # one option is true, combination is true
                    diff_one_com_option += 1
                    break
    print('result of one option is not the same to combination:', diff_one_com_option)
