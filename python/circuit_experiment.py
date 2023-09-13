import os
import subprocess

from datetime import datetime

now = datetime.now()


def run_experiment(cho_file, iters, train, test, results_filename):
    # generate data
    data_csv = open('data.csv', "w")
    stderr_log = open('stderr.log', "w")

    iters = str(iters)
    train = str(train)
    test = str(test)

    p1 = subprocess.Popen(['/usr/bin/time', '-f', '\'%e\'', 'cabal', 'exec', 'd-tree-data', '--',
                           iters, train, test, '-f', cho_file],
                          stdout=data_csv, stderr=stderr_log)
    p1.wait()

    stderr_log.close()
    data_csv.close()

    with open('stderr.log', "r") as f:
        data_time = float(f.read().replace('\'', ''))

    print('time to generate data:', data_time)

    # run d-trees
    stdout_log = open('stdout.log', "w")
    stderr_log = open('stderr.log', "w")

    p2 = subprocess.Popen(['/usr/bin/time', '-f', '\'%e\'', 'python', '--',
                           'python/d-tree-csv.py', 'data.csv',
                           iters, train, test],
                          stdout=stdout_log, stderr=stderr_log)
    p2.wait()

    stderr_log.close()
    stdout_log.close()

    with open('stdout.log', "r") as f:
        pval = float(f.read().replace('\'', ''))

    with open('stderr.log', "r") as f:
        d_tree_time = float(f.read().replace('\'', ''))

    print('time to run d_trees:', d_tree_time)
    print('p-val:', pval)

    with open(results_filename, 'a') as f:
        f.write(f'{iters},{train},{test},{pval},{data_time},{d_tree_time}\n')


iters = [64, 128]
trains = [2**i for i in range(7, 11)]

cho_names = [
'adder64_beaver.cho',
'adder64_gmw.cho',
'aes_128_beaver.cho',
'aes_128_gmw.cho',
'divide64_beaver.cho',
'divide64_gmw.cho',
'FP-add_beaver.cho',
'FP-add_gmw.cho',
'FP-div_beaver.cho',
'FP-div_gmw.cho',
'FP-mul_beaver.cho',
'FP-mul_gmw.cho',
'mult64_beaver.cho',
'mult64_gmw.cho',
'sha256_beaver.cho',
'sha256_gmw.cho']

for cho_name in cho_names:
    dt_string = now.strftime("%d-%m-%Y_%H:%M:%S")
    filename = f'results/circuit_results_{cho_name}_{dt_string}.csv'
    with open(filename, 'w') as f:
        f.write('iters,train_size,test_size,pval,data_time,d_tree_time\n')

    for iters in iters:
        for train in trains:
            print(iters, train, int(train/2))

            run_experiment(cho_name, iters, train, int(train/2), filename)
