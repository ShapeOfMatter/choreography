import numpy as np
import pandas as pd
from sklearn import tree
from sklearn.multioutput import ClassifierChain
from scipy.stats import ttest_ind
from joblib import Parallel, delayed

NUM_ITERS = 2

df = pd.read_csv('miniFunc.csv')

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
    num_train = int(len(features1) * .5)
    features1_train = features1[num_train:]
    features1_test = features1[:num_train]
    features2_train = features2[num_train:]
    features2_test = features2[:num_train]
    labels_train = labels[num_train:]
    labels_test = labels[:num_train]

    model1 = ClassifierChain(tree.DecisionTreeClassifier(), order='random')
    model1.fit(features1_train, labels_train)

    model2 = ClassifierChain(tree.DecisionTreeClassifier(), order='random')
    model2.fit(features2_train, labels_train)

    a1 = model1.score(features1_test, labels_test)
    a2 = model2.score(features2_test, labels_test)

    return (a1, a2)

results = Parallel(n_jobs=4)(delayed(run_iter)(f1, f2, l) for f1, f2, l in zip(np.split(features1, NUM_ITERS),
                                                                               np.split(features2, NUM_ITERS),
                                                                               np.split(labels, NUM_ITERS)))
r = np.array(results)
print(r)

print('T-test result:', ttest_ind(r[:,0], r[:,1], alternative='less'))

