import sys

assert len(sys.argv) == 3


circuit_filename = sys.argv[1]
output_filename = sys.argv[2]
f = open(output_filename, "w")

with open(circuit_filename, "r") as c:
    circuit_lines = c.readlines()


num_gates, num_wires = [int(x) for x in circuit_lines[0].strip().split(' ')]
inputs_line = circuit_lines[1].strip().split(' ')
outputs_line = circuit_lines[2].strip().split(' ')

input_bits = [int(x) for x in inputs_line[1:]]
output_bits = [int(x) for x in outputs_line[1:]]

input_wires = []
current_wire = 0
for bw in input_bits:
    input_wires.append(list(range(current_wire, current_wire+bw)))
    current_wire += bw

assert len(input_wires) == 2
assert len(output_bits) == 1

gate_lines = circuit_lines[4:]

gn = 0
def gensym(x):
    global gn
    gn += 1
    return f'{x}{gn}'

def emit(s=''):
    f.write(s + '\n')

def gen_xor(a, b):
    out = gensym('g')
    emit(f'{out}_1 = {a}_1 + {b}_1')
    emit(f'{out}_2 = {a}_2 + {b}_2')
    return out

def gen_inv(a):
    out = gensym('g')
    emit(f'{out}_1 = {a}_1 + 1')
    emit(f'{out}_2 = {a}_2 + 1')
    return out

def gen_and(a, b):
    out = gensym('g')
    emit(f'DO and_gmw(P1({a}_1, {b}_1), P2({a}_2, {b}_2)) GET({out}_1=out1, {out}_2=out2)')
    return out


header = """
MACRO secret_share(P1(x), P2()) AS
  s1 = FLIP @P1
  s2 = x + s1
  SEND s1 TO P2
ENDMACRO

MACRO and_gmw(P1(x1, y1), P2(x2, y2)) AS
  out1 = FLIP @P1
  g1_s2_00 = out1 + ((x1 + 0) ^ (y1 + 0))
  g1_s2_01 = out1 + ((x1 + 0) ^ (y1 + 1))
  g1_s2_10 = out1 + ((x1 + 1) ^ (y1 + 0))
  g1_s2_11 = out1 + ((x1 + 1) ^ (y1 + 1))
  out2 = OBLIVIOUSLY [[g1_s2_00, g1_s2_01]?y2, [g1_s2_10, g1_s2_11]?y2]?x2 FOR P2
ENDMACRO

MACRO reveal(P1(x1), P2(x2)) AS
  SEND x1 TO P2
  SEND x2 TO P1
  y = x1 + x2
ENDMACRO
"""
emit(header)

xs = [f'x{i}' for i in input_wires[0]]
ys = [f'y{i}' for i in input_wires[1]]

emit('-- Read secrets')
for xi in xs:
    emit(f'{xi} = SECRET @P1')

emit()
for yi in ys:
    emit(f'{yi} = SECRET @P2')

emit()
emit('-- Set up shares')
for xi in xs:
    emit(f'DO secret_share(P1({xi}), P2()) GET({xi}_1=s2, {xi}_2=s1)')

emit()
for yi in ys:
    emit(f'DO secret_share(P2({yi}), P1()) GET({yi}_1=s1, {yi}_2=s2)')

##################################################
# Gates
##################################################
emit()
emit('-- Circuit evaluation')

wire_names = {}
for x, i in zip(xs, input_wires[0]):
    wire_names[i] = x
for y, i in zip(ys, input_wires[1]):
    wire_names[i] = y

for gl in gate_lines:
    gate_params = gl.strip().split(' ')
    gate_type = gate_params[-1]

    if gate_type == 'INV':
        inputs, outputs, in1, out, t = gate_params
        out_name = gen_inv(wire_names[int(in1)])
        wire_names[int(out)] = out_name

    elif gate_type == 'XOR':
        inputs, outputs, in1, in2, out, t = gate_params
        out_name = gen_xor(wire_names[int(in1)],
                           wire_names[int(in2)])
        wire_names[int(out)] = out_name

    elif gate_type == 'AND':
        inputs, outputs, in1, in2, out, t = gate_params
        out_name = gen_and(wire_names[int(in1)],
                           wire_names[int(in2)])
        wire_names[int(out)] = out_name

    elif gate_type == '':
        pass
    else:
        raise Exception('unknown gate', gate_params)

output_wires= list(sorted(wire_names.keys()))[-output_bits[0]:]
output_names = [wire_names[k] for k in output_wires]

emit()
emit('-- Reveal output')
rs = [f'r{i}' for i in range(len(output_names))]
for o, r in zip(output_names, rs):
    emit(f'DO reveal(P1({o}_1), P2({o}_2)) GET({r}=y)')
    emit(f'OUTPUT {r}')

f.close()
