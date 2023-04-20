from dataclasses import dataclass
import numpy as np
from sklearn import tree

def xor(a, b):
    return (a + b) % 2

def land(a, b):
    return a * b

def lnot(a):
    return (a + 1) % 2


@dataclass(frozen=True)
class Trace:
    inputs: np.ndarray
    tapes: np.ndarray
    views: np.ndarray
    outputs: np.ndarray
