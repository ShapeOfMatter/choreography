f = open("adder_32.cho", "w")


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

def gen_and(a, b):
    out = gensym('g')
    emit(f'DO andGMW(P1({a}_1, {b}_1), P2({a}_2, {b}_2)) GET({out}_1=out1, {out}_2=out2)')
    return out

def gen_adder(xs, ys):
    outs = []
    carry = None

    for a, b in zip(reversed(xs), reversed(ys)):
        # ab = a + b
        gab = gen_xor(a, b)

        # out = ab + carry % 2
        if carry is None:
            out = gab
        else:
            out = gen_xor(gab, carry)

        outs.append(out)
        
        # carry = ((a + carry) * (b + carry) + carry) % 2
        if carry is None:
            carry1 = a
            carry2 = b
        else:
            carry1 = gen_xor(a, carry)
            carry2 = gen_xor(b, carry)
        carry3 = gen_and(carry1, carry2)
        if carry is None:
            carry = carry3
        else:
            carry = gen_xor(carry3, carry)

    return outs

header = """
MACRO secretShare(P1(x), P2()) AS
  s1 = FLIP @P1
  s2 = x + s1
  SEND s1 TO P2
ENDMACRO

MACRO andGMW(P1(x1, y1), P2(x2, y2)) AS
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
  OUTPUT y
ENDMACRO
"""
emit(header)

xs = [f'x{i}' for i in range(32)]
ys = [f'y{i}' for i in range(32)]

emit('-- Read secrets')
for xi in xs:
    emit(f'{xi} = SECRET @P1')

emit()
for yi in ys:
    emit(f'{yi} = SECRET @P2')

emit()
emit('-- Set up shares')
for xi in xs:
    emit(f'DO secretShare(P1({xi}, P2) GET({xi}_1=s1, {xi}_2=s2)')

emit()
for yi in ys:
    emit(f'DO secretShare(P2({yi}, P1) GET({yi}_1=s1, {yi}_2=s2)')

emit()
emit('-- Adder circuit')
outs = gen_adder(xs, ys)

emit()
emit('-- Reveal output')
for o in outs:
    emit(f'DO reveal(P1({o}_1), P2({o}_2)) GET()')

f.close()
