from circuit_generator import gensym, emit, gen_circuit, gen_randomness
import random
from argparse import (ArgumentParser, FileType)

argp = ArgumentParser(description="Run the decision tree test on a csv of data.")
argp.add_argument("input_file", type=FileType('r', 1, encoding='utf_8', errors='strict'))
argp.add_argument("output_file", type=FileType('w', 1, encoding='utf_8', errors='strict'))
argp.add_argument("--bias_sharing", action="store", type=int, default=0,
                  help="Bias randomness used in secret sharing")
argp.add_argument("--bias_and", action="store", type=int, default=0,
                  help="Bias randomness used for AND gates")
argp.add_argument("--accidental_secret", action="store", type=float, default=0.0,
                  help="Rate of accidentally sending secret inputs to corrupt party")
argp.add_argument("--accidental_gate", action="store", type=float, default=0.0,
                  help="Rate of accidentally sending shares of gate outputs to corrupt party")
args = argp.parse_args()

config = {
    'bias_sharing': args.bias_sharing,
    'bias_and': args.bias_and,
    'accidental_secret': args.accidental_secret,
    'accidental_gate': args.accidental_gate,
    }

def gen_xor(a, b):
    out = gensym('g')
    emit(f'{out}_1 = {a}_1 + {b}_1')
    emit(f'{out}_2 = {a}_2 + {b}_2')
    if random.random() < config['accidental_gate']:
        emit(f'SEND {out}_2 TO P1 -- accidental send to corrupt')
    return out

def gen_inv(a):
    out = gensym('g')
    emit(f'{out}_1 = {a}_1 + 1')
    emit(f'{out}_2 = {a}_2 + 1')
    if random.random() < config['accidental_gate']:
        emit(f'SEND {out}_2 TO P1 -- accidental send to corrupt')
    return out

def gen_and(a, b):
    out = gensym('g')

    emit(f'DO and_beaver(P1({a}_1, {b}_1), P2({a}_2, {b}_2)) GET({out}_1=out1, {out}_2=out2)')
    if random.random() < config['accidental_gate']:
        emit(f'SEND {out}_2 TO P1 -- accidental send to corrupt')
    return out

generators = {'AND': gen_and, 'XOR': gen_xor, 'INV': gen_inv}

share_randomness_var, share_randomness_defs = gen_randomness(config['bias_sharing'], 'P1')
a1, a1_defs = gen_randomness(config['bias_and'], 'P3')
a2, a2_defs = gen_randomness(config['bias_and'], 'P3')
b1, b1_defs = gen_randomness(config['bias_and'], 'P3')
b2, b2_defs = gen_randomness(config['bias_and'], 'P3')
c1, c1_defs = gen_randomness(config['bias_and'], 'P3')
c2, c2_defs = gen_randomness(config['bias_and'], 'P3')

header = f"""
MACRO secret_share(P1(x), P2()) AS
  {share_randomness_defs}
  s1 = {share_randomness_var}
  s2 = x + s1
  SEND s1 TO P2
ENDMACRO

MACRO and_beaver(P1(x2, y2), P2(x1, y1)) AS
  -- generate beaver triple
  {a1_defs}
  a1 = {a1}
  {a2_defs}
  a2 = {a2}
  {b1_defs}
  b1 = {b1}
  {b2_defs}
  b2 = {b2}
  {c1_defs}
  c1 = {c1}

  c2 = ((a1 + a2) ^ (b1 + b2)) + c1

  -- deal shares of beaver triple
  SEND a1 TO P1
  SEND b1 TO P1
  SEND c1 TO P1

  SEND a2 TO P2
  SEND b2 TO P2
  SEND c2 TO P2

  -- use beaver triple to evaluate the gate
  d1 = x2 + a1
  d2 = x1 + a2
  SEND d1 TO P2
  SEND d2 TO P1
  d = d1 + d2
  e1 = y2 + b1
  e2 = y1 + b2
  SEND e1 TO P2
  SEND e2 TO P1
  e = e1 + e2

  out1 = (d ^ e) + (d ^ b1) + (e ^ a1) + c1
  out2 = (d ^ e) + (d ^ b2) + (e ^ a2) + c2
ENDMACRO

MACRO reveal(P1(x1), P2(x2)) AS
  SEND x1 TO P2
  SEND x2 TO P1
  y = x1 + x2
ENDMACRO
"""

gen_circuit(config, generators, header, args.input_file, args.output_file)
