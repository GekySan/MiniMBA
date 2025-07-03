from z3 import *

LEN         = 8
TARGET_HASH = 0xF859A9A0
ASCII_MIN   = 0x20
ASCII_MAX   = 0x7e

chars = [ BitVec(f'c{i}', 8) for i in range(LEN) ]

printables = [ And(chars[i] >= ASCII_MIN, chars[i] <= ASCII_MAX) for i in range(LEN) ]

h = BitVecVal(0, 32)
for c in chars:
    h = (h * 31 + ZeroExt(24, c)) & 0xffffffff  # 32 bits

hash_eq = (h == TARGET_HASH)

s = Solver()
s.add(printables + [hash_eq])

if s.check() == sat:
    m = s.model()
    result = ''.join(chr(m[c].as_long()) for c in chars)
    print(result)
else:
    print("KO")
