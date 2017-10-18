from hypothesis import given
from hypothesis.strategies import integers, lists

def rev(l):
    return list(reversed(l))

@given(integers())
def test_rev_unit(x):
    assert rev([x]) == [x]

@given(lists(elements=integers()), lists(elements=integers()))
def test_rev_app(xs, ys):
    assert rev(xs + ys) == rev(ys) + rev(xs)

if __name__ == '__main__':
    test_rev_unit()
    test_rev_app()
