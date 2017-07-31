class Bit(object):
    pass

class Zero(Bit):
    pass

class One(Bit):
    pass

class Byte(object):
    def __init__(self, bit7, bit6, bit5, bit4, bit3, bit2, bit1, bit0):
        self.bits = (bit7, bit6, bit5, bit4, bit3, bit2, bit1, bit0)

UInt8 = Byte

class UInt32(object):
    def __init__(self, byte3, byte2, byte1, byte0):
        self.bytes = (byte3, byte2, byte1, byte0)

class Bool(object):
    def __init__(self, bit):
        self.bit = bit

BTrue = Bool(One)
BFalse = Bool(Zero)

def _if(b, t, f):
    return t if b.bit is One else f

print _if(BTrue, "hello", "goodbye")

# ergo, we need:
# - case anaysis / branching
# - sum types
# - product types

cTrue = lambda x, _: x
cFalse = lambda _, y: y

c0 = lambda f, x: x

def succ(n):
    return lambda f, x: n(f, f(x))

inc = lambda n: n + 1

c1 = succ(c0)
c2 = succ(c1)

def add(m, n):
    return lambda f, x: m(f, n(f, x))

c3 = add(c1, c2)


def mul(m, n):
    return lambda f, x: m(lambda y: n(f, y), x)

c6 = mul(c3, c3)

print c6(inc, 0)
