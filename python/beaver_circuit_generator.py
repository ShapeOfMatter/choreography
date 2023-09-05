from circuit_generator import gensym, emit, gen_circuit, gen_randomness
import random

config = {
    'bias_sharing': 0,
    'bias_and': 0,
    'accidental_secret': 0.0,
    'accidental_gate': 1.0,
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
    ta_1 = gensym('t')
    ta_2 = gensym('t')
    tb_1 = gensym('t')
    tb_2 = gensym('t')
    tc_1 = gensym('t')
    tc_2 = gensym('t')

    a_1 = random.randint(0, 1)
    a_2 = random.randint(0, 1)
    b_1 = random.randint(0, 1)
    b_2 = random.randint(0, 1)
    c_1 = random.randint(0, 1)
    c_2 = ((a_1 ^ a_2) & (b_1 ^ b_2)) ^ c_1
    assert (a_1 ^ a_2) & (b_1 ^ b_2) == c_1 ^ c_2

    for name, val in [(ta_1, a_1), (ta_2, a_2), (tb_1, b_1), (tb_2, b_2), (tc_1, c_1), (tc_2, c_2)]:
        emit(f'{name} = {val}')

    emit(f'DO and_beaver(P1({a}_1, {b}_1, {ta_1}, {tb_1}, {tc_1}), P2({a}_2, {b}_2, {ta_2}, {tb_2}, {tc_2})) GET({out}_1=out1, {out}_2=out2)')
    if random.random() < config['accidental_gate']:
        emit(f'SEND {out}_2 TO P1 -- accidental send to corrupt')
    return out

generators = {'AND': gen_and, 'XOR': gen_xor, 'INV': gen_inv}

share_randomness_var, share_randomness_defs = gen_randomness(config['bias_sharing'], 'P1')
and_randomness_var, and_randomness_defs = gen_randomness(config['bias_and'], 'P2')

header = f"""
MACRO secret_share(P1(x), P2()) AS
  {share_randomness_defs}
  s1 = {share_randomness_var}
  s2 = x + s1
  SEND s1 TO P2
ENDMACRO

MACRO and_beaver(P1(x2, y2, a1, b1, c1), P2(x1, y1, a2, b2, c2)) AS
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

gen_circuit(config, generators, header)
