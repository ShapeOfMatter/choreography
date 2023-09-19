import os
import subprocess
import sys

from datetime import datetime

now = datetime.now()

def gen_circuit_file(circuit_file, cho_file, protocol_type, config):
    bias_sharing, bias_and, accidental_secret, accidental_gate = config

    circuit_filename = 'circuits/' + circuit_file

    p1 = subprocess.Popen(['python', f'python/{protocol_type}_circuit_generator.py',
                           circuit_filename, cho_file,
                           '--bias_sharing', str(bias_sharing),
                           '--bias_and', str(bias_and),
                           '--accidental_secret', str(accidental_secret),
                           '--accidental_gate', str(accidental_gate)])
    p1.wait()

def run_experiment(cho_filename, iters, train, test, results_filename,
                   protocol_type, config, circuit_name):
    # generate data
    data_csv = open('data.csv', "w")
    stderr_log = open('stderr.log', "w")

    iters = str(iters)
    train = str(train)
    test = str(test)

    p1 = subprocess.Popen(['/usr/bin/time', '-f', '\'%e\'', 'cabal', 'exec', 'd-tree-data', '--',
                           iters, train, test, '-f', cho_filename],
                          stdout=data_csv, stderr=stderr_log)
    p1.wait()

    stderr_log.close()
    data_csv.close()

    with open('stderr.log', "r") as f:
        data_time = float(f.read().replace('\'', ''))

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

    config_str = ','.join([str(x) for x in config])
    with open(results_filename, 'a') as f:
        line = f'{circuit_name},{protocol_type},{iters},{train},{test},{pval},{data_time},{d_tree_time},{config_str}\n'
        print(line)
        f.write(line)


iters = [64, 128]
trains = [2**i for i in range(7, 10)]

circuit_names = [
    'adder64.txt',
    'mult64.txt',
    'aes_128.txt',
    'divide64.txt',
    'FP-add.txt',
    'FP-mul.txt',
    'sha256.txt',
    'FP-div.txt']

protocol_types = ['gmw', 'beaver']


dt_string = now.strftime("%d-%m-%Y_%H:%M:%S")
filename = f'results/circuit_results_{dt_string}.csv'
with open(filename, 'w') as f:
    f.write('circuit,protocol,iters,train_size,test_size,pval,data_time,d_tree_time,bias_sharing,bias_and,accidental_secret,accidental_gate\n')

TRIALS = 3

configs = [
    (0, 0, 0, 0), # secure
]
configs += [(i, 0, 0, 0) for i in range(1, 4)]
configs += [(0, i, 0, 0) for i in range(1, 4)]
configs += [(0, 0, 1/i, 0) for i in range(2, 5)]
configs += [(0, 0, 0, 1/i) for i in range(2, 5)]

cho_file = 'tmp.cho'
for circuit_name in circuit_names:
    print('Running on:', circuit_name)
    for protocol_type in protocol_types:
        for c in configs:
            gen_circuit_file(circuit_name, cho_file, protocol_type, c)

            for i in iters:
                for train in trains:
                    for _ in range(TRIALS):
                        print(i, train, int(train/2))

                        run_experiment(cho_file, i, train, int(train/2), filename,
                                       protocol_type, c, circuit_name)
