from pyqcy import *

def rev(l):
    return list(reversed(l))

@qc
def prop_rev_unit(x=int_()):
    assert rev([x]) == [x]

@qc
def prop_rev_app(xs=list_(of=int), ys=list_(of=int)):
    assert rev(xs + ys) == rev(ys) + rev(xs)

if __name__ == '__main__':
    main()
