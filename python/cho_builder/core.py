from dataclasses import (dataclass, field, fields)
from typing import Callable

@dataclass(frozen=True)
class ImplementationDetails:
    bias_sharing: int = field(default=0, metadata={'help': "Bias randomness used in secret sharing"})
    bias_and: int = field(default=0, metadata={'help': "Bias randomness used for AND gates"})
    accidental_secret: int = field(default=0, metadata={'help': "Rate of accidentally sending secret inputs to corrupt party"})
    accidental_gate: int = field(default=0, metadata={'help': "Rate of accidentally sending shares of and-gate outputs to corrupt party"})

    @classmethod
    def from_dict(cls, d):
        return cls(**{f.name: f.type(d[f.name]) for f in fields(cls)})
    @classmethod
    def from_args(cls, args):
        return cls.from_dict(vars(args))
    def as_csv_data(self):
        return f"{self.bias_sharing},{self.bias_and},{self.accidental_secret},{self.accidental_gate}"

@dataclass(frozen=True)
class GateGenerators:
    and_gate: Callable[[str, str], str]
    xor_gate: Callable[[str, str], str]
    inv_gate: Callable[[str], str]
