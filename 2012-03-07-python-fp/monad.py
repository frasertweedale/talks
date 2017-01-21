import operator
import functools
import itertools
from functional import compose

# iteration via recursion
def fsum(xs):
    return xs[0] + fsum(xs[1:]) if xs else 0
print fsum(range(10))

# map, reduce, functools.partial
print map(functools.partial(operator.__add__, 1), [1,2,3])
print reduce(operator.__mul__, [1,2,3], 1)
factorial = lambda n: reduce(operator.__mul__, range(1, n + 1), 1)
print factorial(6)

# function composition
succ = lambda x: x + 1
square = lambda x: x**2
squaresucc = compose(square, succ)
print squaresucc(5)

# division by zero
class Maybe(object):
    @classmethod
    def ret(cls, x): return Just(x)

class Nothing(Maybe):
    def __init__(self): pass
    def __rshift__(self, f): return self
    def __repr__(self): return 'Nothing()'

class Just(Maybe):
    def __init__(self, x): self._x = x
    def __rshift__(self, f):
        return f(self._x)
    def __repr__(self): return 'Just({!r})'.format(self._x)

def mdiv(n, d):
    return Nothing() if not d else Just(n / d)

print Just(10) >> (lambda x: Just(2) >> (lambda y: mdiv(x, y)))
print Just(10) >> (lambda x: Just(0) >> (lambda y: mdiv(x, y)))

def divby(d):
    return lambda n: mdiv(n, d)

print Just(10) >> divby(2)
print Just(10) >> divby(0)
print Just(10) >> divby(0) >> divby(2)
print Just(16) >> divby(2) >> divby(2)

class List(object):
    def __init__(self, xs): self._xs = tuple(xs)
    def __rshift__(self, f):
        return type(self)(itertools.chain.from_iterable(map(f, self)))
    def __iter__(self): return iter(self._xs)
    def __repr__(self): return '{}({!r})'.format(type(self).__name__, self._xs)
    def __len__(self): return len(self._xs)
    @classmethod
    def ret(cls, xs): return cls([xs])

print List([0,1])
print List([]) >> (lambda x: List(['bad', 'mad', 'rad']))
print List([1,2,3]) >> (lambda x: List([]))
print List([3,4,5]) >> (lambda x: List([x, -x]))

class Set(List):
    def __init__(self, xs): self._xs = set(xs)
    def __or__(self, other): return type(self)(self._xs | other._xs)
    def __sub__(self, other): return type(self)(self._xs - other._xs)

def move_knight((c, r)):
    return Set(
        newpos for newpos in [
            (c + 2, r - 1), (c + 2, r + 1),
            (c - 2, r - 1), (c - 2, r + 1),
            (c + 1, r - 2), (c + 1, r + 2),
            (c - 1, r - 2), (c - 1, r + 2),
        ] if all(x in xrange(8) for x in newpos)
    )

print move_knight((6, 2))

in_3 = lambda pos: Set.ret(pos) >> move_knight >> move_knight >> move_knight
can_reach_in_3 = lambda start, end: end in in_3(start)

print can_reach_in_3((6, 2), (6, 1))
print can_reach_in_3((6, 2), (7, 3))

def furthest_posns(posns):
    """Calculate the furthest position by number of moves from start pos."""
    return posns if len(posns) == 64 else \
        furthest_posns(posns | (posns >> move_knight)) - posns

print furthest_posns(Set.ret((0,0)))
print furthest_posns(Set.ret((1,1)))
print furthest_posns(Set.ret((2,2)))
print furthest_posns(Set.ret((3,3)))
print furthest_posns(Set.ret((7,7)))
