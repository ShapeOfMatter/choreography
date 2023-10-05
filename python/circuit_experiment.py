from argparse import (ArgumentParser, FileType)
from collections import Counter
from csv import DictReader
from dataclasses import dataclass
from datetime import datetime
from itertools import chain
import math
import os
from pprint import PrettyPrinter
import subprocess
import sys


launch_time = datetime.now()


@dataclass(frozen=True)
class SecurityConfiguration:
    bias_sharing : int = 0
    bias_and : int = 0
    accidental_secret : float = 0
    accidental_gate : float = 0
    def __str__(self):
        return f"{self.bias_sharing},{self.bias_and},{self.accidental_secret},{self.accidental_gate}"


argp = ArgumentParser(description="Build various circuits and test them at various powers")
# We don't use `FileType` arguments because we don't want these handles open all the time.
argp.add_argument("--iters", "-i", type=int, action="extend", nargs="+", help="Values of \"iterations\" at which to test the circuits.")
argp.add_argument("--trains", "-t", type=int, action="extend", nargs="+", help="Values of \"training N\" at which to test the circuits.")
argp.add_argument("--testing-ratio", type=int, default=2,
                  help="Circuits will be tested with \"testing N\" set to t/testing-ratio for each t in --trains.")
argp.add_argument("--circuits", "-c", type=str, action="extend", nargs="+", help="Circuit files to parse into CHO and then test."
                                                                                 +" A \"circuits/\" prefix will be added!")
argp.add_argument("--protocols", "-p", type=str, action="extend", nargs="+", help="The protocols with which to implement each circuit.")
argp.add_argument('--output', "-o", type=str, default="results/circuit_results_{t}.csv",
                  help="Path to the CSV file to write data to. Will be overwritten if it already exists."
                       +" The pattern \"\{t\}\" will be replaced with the current time.")
argp.add_argument("--repititions", "-R", type=int, default=3, help="How many times to repeat each test on each circuit.")
argp.add_argument("--cho-temp", type=str, default='tmp.cho', help="The temp-file to which to save the generated CHO protocol.")
argp.add_argument("--pre-existing", "-P", type=str, action="extend", nargs="+", default=[],
                  help="CSV files containing data from prior runs, which can be skipped.")
argp.add_argument("--log", type=FileType('w', 1, encoding='utf_8', errors='strict'), default="-", help="An extra log file to use instead of STD_OUT.")
args = argp.parse_args()

log = PrettyPrinter(width=120, stream=args.log).pprint
log(args)
iters = args.iters or [64, 128]
trains = args.trains or [2**i for i in range(7, 10)]
testing_ratio = args.testing_ratio
circuit_names = args.circuits or ['adder_1.txt',
                                  'adder_2.txt',
                                  'adder_3.txt',
                                  'adder_4.txt',
                                  'adder_5.txt',
                                  'adder_6.txt',
                                  'adder_7.txt',
                                  'adder_8.txt',
                                  'adder_9.txt',
                                  'adder_10.txt',
                                  'adder_11.txt',
                                  'adder_12.txt',
                                  'adder_13.txt',
                                  'adder_14.txt',
                                  'adder_15.txt',
                                  'adder_16.txt',
                                  'adder_17.txt',
                                  'adder_18.txt',
                                  'adder_19.txt',
                                  'adder_20.txt',
                                  'adder_21.txt',
                                  'adder_22.txt',
                                  'adder_23.txt',
                                  'adder_24.txt',
                                  'adder_25.txt',
                                  'adder_26.txt',
                                  'adder_27.txt',
                                  'adder_28.txt',
                                  'adder_29.txt',
                                  'adder_30.txt',
                                  'adder_31.txt',
                                  'adder_32.txt',
                                  'adder64.txt',
                                  'mult64.txt',
                                  'aes_128.txt',
                                  'divide64.txt',
                                  'FP-add.txt',
                                  'FP-mul.txt',
                                  'sha256.txt',
                                  'FP-div.txt']
protocol_types = args.protocols or ['gmw', 'beaver']
filename = args.output.format(t=launch_time.strftime("%d-%m-%Y_%H:%M:%S"))
TRIALS = args.repititions
cho_file = args.cho_temp
log((iters, trains, testing_ratio, circuit_names, protocol_types, filename, TRIALS, cho_file))

configs = [
    SecurityConfiguration(), # secure
    *( SecurityConfiguration(bias_sharing      = i  ) for i in range(1, 4) ),  # for consistency i should make these args too, but it's a chore.
    *( SecurityConfiguration(bias_and          = i  ) for i in range(1, 4) ),
    *( SecurityConfiguration(accidental_secret = 1/i) for i in range(2, 5) ),
    *( SecurityConfiguration(accidental_gate   = 1/i) for i in range(2, 5) )
]
log(configs)

def scrape_prior(csv_name):
    with open(csv_name, newline='') as f:
        dr = DictReader(f)
        for row in dr:
            yield (row["circuit"],
                   row["protocol"],
                   int(row["iters"]),
                   int(row["train_size"]),
                   int(row["test_size"]),
                   SecurityConfiguration(bias_sharing=int(row["bias_sharing"]),
                                         bias_and=int(row["bias_and"]),
                                         accidental_secret=float(row["accidental_secret"]),
                                         accidental_gate=float(row["accidental_gate"])))

already_done = Counter(chain.from_iterable(scrape_prior(p) for p in args.pre_existing))
log(already_done)


try:
    with open("./cores") as cores_file:
        CORES = int(cores_file.read())
except FileNotFoundError:
    exit(f"{__file__} was unable to find a file named \"./cores\" to tell it how many machine cores to use.")
except ValueError:
    exit(f"{__file__} was unable to parse \"./cores\" as an integer.")
assert math.log(CORES, 2).is_integer(), "We trust the division of work across reasonable-powers-of-two cores;" \
                                          + f" other values (e.g. {CORES}) are likely to cause bugs."
log(f"CORES={CORES}")


def gen_circuit_file(circuit_file, cho_file, protocol_type, config):
    circuit_filename = 'circuits/' + circuit_file

    process = ['python', f'python/{protocol_type}_circuit_generator.py',
                           circuit_filename, cho_file,
                           '--bias_sharing', str(config.bias_sharing),
                           '--bias_and', str(config.bias_and),
                           '--accidental_secret', str(config.accidental_secret),
                           '--accidental_gate', str(config.accidental_gate)]
    log("Generating CHO:")
    log(process)
    p1 = subprocess.Popen(process)
    p1.wait()
    log("Finished generating CHO.")

def run_experiment(cho_filename, iters, train, test, results_filename,
                   protocol_type, config, circuit_name):
    # generate data
    iters_per_core = int(iters) // CORES
    iters_per_core = str(iters_per_core)
    iters = str(iters)
    train = str(train)
    test = str(test)

    pids = []
    for i in range(CORES):
        data_csv = open(f'data{i}.csv', "w")
        stderr_log = open(f'stderr{i}.log', "w")

        p1 = subprocess.Popen(['/usr/bin/time', '-f', '\'%e\'', 'cabal', 'exec', 'd-tree-data', '--',
                            iters_per_core, train, test, '-f', cho_filename],
                            stdout=data_csv, stderr=stderr_log)
        pids.append((p1, data_csv, stderr_log))

    for pid, data_csv, stderr_log in pids:
        pid.wait()
        stderr_log.close()
        data_csv.close()

    def retrieve_time(i):
        with open(f'stderr{i}.log', "r") as f:
            return float(f.read().replace('\'', ''))

    data_time = max(retrieve_time(i) for i in range(CORES))

    # run d-trees
    stdout_log = open('stdout.log', "w")
    stderr_log = open('stderr.log', "w")

    data_files = [f'data{i}.csv' for i in range(CORES)]
    proc_spec = ['/usr/bin/time', '-f', '\'%e\'', 'python', '--',
                 'python/d-tree-csv.py'] + data_files + \
                [iters, train, test]
    p2 = subprocess.Popen(proc_spec,
                          stdout=stdout_log, stderr=stderr_log)
    p2.wait()

    stderr_log.close()
    stdout_log.close()

    with open('stdout.log', "r") as f:
        pval = float(f.read().replace('\'', ''))

    with open('stderr.log', "r") as f:
        d_tree_time = float(f.read().replace('\'', ''))

    with open(results_filename, 'a') as f:
        line = f'{circuit_name},{protocol_type},{iters},{train},{test},{pval},{data_time},{d_tree_time},{config}\n'
        log(line)
        f.write(line)


with open(filename, 'w') as f:
    f.write('circuit,protocol,iters,train_size,test_size,pval,data_time,d_tree_time,bias_sharing,bias_and,accidental_secret,accidental_gate\n')

for circuit_name in circuit_names:
    log(f"Running on: {circuit_name}")
    for protocol_type in protocol_types:
        for c in configs:
            gen_circuit_file(circuit_name, cho_file, protocol_type, c)

            for i in iters:
                for train in trains:
                    testing = int(train/testing_ratio)
                    done = already_done[(circuit_name, protocol_type, i, train, testing, c)]
                    for rep in range(done, TRIALS):
                        log((i, train, testing, rep))
                        run_experiment(cho_file, i, train, testing, filename, protocol_type, c, circuit_name)

