from argparse import (ArgumentParser, FileType)
from joblib import Parallel, delayed
import numpy as np
import pandas as pd
from scipy.stats import ttest_ind
from sklearn import tree
from sklearn.multioutput import ClassifierChain
from sys import stderr

argp = ArgumentParser(description="Run the decision tree test on a csv of data.")
argp.add_argument("file", type=FileType('r', 1, encoding='utf_8', errors='strict'))
argp.add_argument("iterations", type=int)
argp.add_argument("trainingN", type=int)
argp.add_argument("testingN", type=int)
args = argp.parse_args()

NUM_ITERS = args.iterations
NUM_TRAIN = args.trainingN
PER_ITER = NUM_TRAIN + args.testingN

df = pd.read_csv(args.file)

if len(df) != NUM_ITERS * PER_ITER:
    print(f"Using {PER_ITER} rows for {NUM_ITERS} iterations, but there are {len(df)} rows!",
          file=stderr)

ideal_cols = []
view_cols = []
honest_cols = []

for c in df.columns:
    if c.startswith('i_'): ideal_cols.append(c)
    elif c.startswith('v_'): view_cols.append(c)
    elif c.startswith('h_'): honest_cols.append(c)
    else: raise Exception('unknown column type', c)

features1 = df[ideal_cols].to_numpy()
features2 = df[ideal_cols + view_cols].to_numpy()
labels = df[honest_cols].to_numpy()

def run_iter(features1, features2, labels):
    features1_train = features1[:NUM_TRAIN]
    features1_test = features1[NUM_TRAIN:PER_ITER]
    features2_train = features2[:NUM_TRAIN]
    features2_test = features2[NUM_TRAIN:PER_ITER]
    labels_train = labels[:NUM_TRAIN]
    labels_test = labels[NUM_TRAIN:PER_ITER]

    model1 = ClassifierChain(tree.DecisionTreeClassifier(), order='random')
    model1.fit(features1_train, labels_train)

    model2 = ClassifierChain(tree.DecisionTreeClassifier(), order='random')
    model2.fit(features2_train, labels_train)

    p1 = model1.predict(features1_test)
    p2 = model2.predict(features2_test)
    score1 = np.linalg.norm(p1 - labels_test, ord=1, axis=1).sum() / len(p1)
    score2 = np.linalg.norm(p2 - labels_test, ord=1, axis=1).sum() / len(p1)

    return (score2, score1)

results = Parallel(n_jobs=4)(delayed(run_iter)(f1, f2, l) for f1, f2, l in \
                             zip(np.array_split(features1, NUM_ITERS),
                                 np.array_split(features2, NUM_ITERS),
                                 np.array_split(labels, NUM_ITERS)))
r = np.array(results)
print(r)

print('T-test result:', ttest_ind(r[:,0], r[:,1], alternative='less'))

