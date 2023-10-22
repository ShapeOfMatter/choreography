from argparse import (ArgumentParser, FileType)
from cycler import cycler
from dataclasses import dataclass, field, fields
import matplotlib.pyplot as plt
import numpy as np
import os
import pandas as pd
import re
import warnings

@dataclass(frozen=True, order=True)
class Breakage:
    bias_sharing: int = field(default=0, metadata={'help': "Bias randomness used in secret sharing",
                                                   'defaults': [0,1,2,3]})
    bias_and: int = field(default=0, metadata={'help': "Bias randomness used for AND gates",
                                               'defaults': [0,1,2,3]})
    accidental_secret: int = field(default=0, metadata={'help': "Rate of accidentally sending secret inputs to corrupt party",
                                                        'defaults': [0,1,2,3]})
    accidental_gate: int = field(default=0, metadata={'help': "Rate of accidentally sending shares of and-gate outputs to corrupt party",
                                                      'defaults': [0,1,2,3]})

    @classmethod
    def from_dict(cls, d):
        return cls(**{f.name: f.type(d[f.name]) for f in fields(cls)})
    @classmethod
    def from_args(cls, args):
        return cls.from_dict(vars(args))
    def short(self):
        return ",".join(f"{f.name}={getattr(self, f.name)}"
                        for f in fields(self)
                        if getattr(self, f.name) != f.default
                       ) or "secure"
    def linestyle(self):
        retval = {}
        kinds = [f.name for f in fields(self) if getattr(self, f.name) != f.default]
        kind_colors = dict(zip((f.name for f in fields(self)), ['r', 'b', 'm', 'y', 'k', 'c']))
        if 0 == len(kinds):
            retval['color'] = 'green'
            retval['dashes'] = (None, None)
            retval['linewidth'] = 2
            retval['zorder'] = 0
        elif 1 == len(kinds):
            k = kinds[0]
            v = getattr(self, k)
            retval['color'] = kind_colors[k]
            retval['dashes'] = (v * 2, v * 2)
            retval['linewidth'] = 1
            retval['zorder'] = v
        elif 2 == len(kinds):
            k1, k2 = kinds
            v1, v2 = (getattr(self, k) for k in kinds)
            retval['color'] = kind_colors[k1]
            retval['gapcolor'] = kind_colors[k2]
            retval['dashes'] = (v1 * 2, v2 * 2)
            retval['linewidth'] = 1.3
            retval['zorder'] = 100 + v1 + v2
        else:
            raise ValueError("Un-colorable breakage value", self)
        return retval

SECURE = Breakage()

def get_broken(data, breakage):
    return data[  (data['bias_sharing'] == breakage.bias_sharing)
                & (data['bias_and'] == breakage.bias_and)
                & (data['accidental_secret'] == breakage.accidental_secret)
                & (data['accidental_gate'] == breakage.accidental_gate)]

@dataclass(frozen=True)
class FigureParams:
    circuit_name_form: str
    protocol: str
    output_kind: str
    breakages: list[Breakage]
    def short(self):
        return f"{self.circuit_name_form.format('*')}, {self.protocol}, {self.output_kind}, {[b.short() for b in self.breakages]}"
    def filename(self, destination, suffix):
        base = self.circuit_name_form.format('').rsplit('.', maxsplit=1)[0].strip(' _')
        breakages = '_'.join(f.name
                             for f in fields(Breakage)
                             if any(getattr(b, f.name) != f.default for b in self.breakages)
                            ) or 'secure'
        name = f"{base}_{self.protocol}_{self.output_kind}_{breakages}.{suffix}"
        return os.path.join(destination, name)

@dataclass(frozen=True)
class GraphParams:
    iters: int
    train_size: int
    min_width: int
    max_width: int
    def short(self):
        return ",".join(f"{f.name}={getattr(self, f.name)}"
                        for f in fields(self))

def make_figure(p: FigureParams, destination, image_format, data, title):
    relevant = data[  (data['protocol'] == p.protocol)
                    & (data['outputs'] == p.output_kind)
                    & (data['circuit'].str.fullmatch(p.circuit_name_form.format('\d+')))]
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", pd.errors.SettingWithCopyWarning)
        relevant['bit_width'] = relevant['circuit'].str.extract(p.circuit_name_form.format('(\d+)'), expand=False).astype(int)
    iterss = sorted(frozenset(relevant['iters'].astype(int)))
    train_sizes = sorted(frozenset(relevant['train_size'].astype(int)))
    bit_widths = sorted(frozenset(relevant['bit_width']))

    fig = plt.figure(figsize=(18,12), layout='compressed')
    fig.suptitle(f"{title} ({p.short()})")
    axs = fig.subplot_mosaic([[GraphParams(iters=iters,
                                           train_size=train_size,
                                           min_width=relevant['bit_width'].min(),
                                           max_width=relevant['bit_width'].max())
                               for train_size in train_sizes]
                              for iters in iterss],
                             sharex=True, sharey=True)
    handles = []
    labels = []
    for gp, ax in axs.items():
        new_handles, new_labels = make_graph(ax, gp, p, relevant)
        handles.extend(new_handles)
        labels.extend(new_labels)

    unique_hl = [(h, l) for i, (h, l) in enumerate(zip(handles, labels)) if l not in labels[:i]]
    fig.legend(*zip(*unique_hl))
    fig.savefig(p.filename(destination, image_format))
    plt.close(fig)


def make_graph(ax, gp: GraphParams, fp: FigureParams, data):
    print(fp.short(), gp.short())
    ax.set_title(gp.short())
    ax.set_xlim(gp.min_width, gp.max_width)
    ax.set_ylim(0, 1)
    ax.spines['top'].set_visible(False)
    ax.spines['bottom'].set_visible(False)
    df_graph = data[(data['iters'] == gp.iters) & (data['train_size'] == gp.train_size)]
    if 0 != len(df_graph):
        results = {b: get_broken(df_graph, b) for b in fp.breakages}
        for b, v in results.items():
            df_g = v.groupby('bit_width')
            print(frozenset(df_g['pval'].count()))
            if 0 != len(df_g):
                means = df_g['pval'].mean()
                stds = df_g['pval'].std()
                x = means.index
                y = means
                style = b.linestyle()
                ax.plot(x, y, label=b.short(), **style)
                ax.fill_between(x, y-stds, y+stds, alpha=.25, linewidth=0, color=style['color'])
            else:
                print(f"skipping {fp} {gp} {b} because it doesn't seem to have any relevant data.")
    else:
        print(f"skipping {fp} {gp} because it doesn't seem to have any relevant data.")
    return ax.get_legend_handles_labels()

help_message = "Graph results from circuit_experiment.py"
argp = ArgumentParser(description=help_message)
argp.add_argument('--data-files', type=str, nargs='+', default=['\dev\stdin'])
argp.add_argument('--destination', type=str, required=True)
argp.add_argument('--circuit-forms', type=str, nargs='+', required=True,
                  help="A python format string for the circuit names (including suffix), with a single unnamed field for the bit width")
argp.add_argument('--max-breakage-value', type=int, default=4, help="For every breakage kind, (try to) plot breakage-value lines from 0-N.")


def main():
    args = argp.parse_args()
    df = pd.concat(pd.read_csv(f) for f in args.data_files)
    print(df)
    os.makedirs(args.destination, exist_ok=True)
    for form in args.circuit_forms:
        relevant = df[(df['circuit'].str.fullmatch(form.format('\d+')))]
        cho_details = frozenset((row.protocol, row.outputs) for row in relevant.itertuples())
        for (protocol, output_kind) in cho_details:
            for f in fields(Breakage):
                make_figure(FigureParams(circuit_name_form=form,
                                         protocol=protocol,
                                         output_kind=output_kind,
                                         breakages=[Breakage(**{f.name: v}) for v in range(0, args.max_breakage_value + 1)]),
                            args.destination,
                            'svg',
                            df,
                            title=str(args.data_files))


if __name__ == '__main__':
    main()
