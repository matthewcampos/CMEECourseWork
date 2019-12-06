##Try this first
"""Understanding scope and how to manipulate variables"""

__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
__version__ = '0.0.1'

_a_global=10 # a global variable

if _a_global >=5:
    _b_global=_a_global+5 # also a global variable

def a_function():
    _a_global=5 # a local variable that replaces the global variable outside

    if _a_global>=5:
        _b_global=_a_global+5 # also a local variable

    _a_local=4

    print("Inside the function, the value of _a_global is", _a_global)
    print("Inside the fucntion, the value of _b_global is", _b_global)
    print("Inside the function, the value of _a_local is", _a_local)

    return None

a_function()

print("Outside the function, the value of _a_global is", _a_global)
print("Outside the function, the value of _b_global is", _b_global)
#-----------------------------------------------------------------------------
_a_global=10

def a_function():
    _a_local=4

    print("Inside the function, the value of _a_local is", _a_local)
    print("Inside the function, the value of _a_global is", _a_global)

    return None

a_function()

print("Outside the functino, the value of _a_global is", _a_global)
#-----------------------------------------------------------------------------
_a_global=10

print("Outside the function, the value of _a_global is", _a_global)

def a_function():
    global _a_global #keyword that modifies the value of the variable and sets it as the global outside
    _a_global=5
    _a_local=4

    print("Inside the function, the value of _a_global is", _a_global)
    print("Inside the function, the value of _a_local is", _a_local)

    return None

a_function()

print("Outside the function, the value of _a_global now is", _a_global)
#-----------------------------------------------------------------------------
def a_function():
    _a_global=10

    def _a_function2():
        global _a_global
        _a_global=20

    print("Before calling a_function, the value of _a_global is", _a_global)

    _a_function2()

    print("After calling _a_function2, the value of _a_global is", _a_global)

a_function()

print("The value of _a_global in main workspace/namespace is", _a_global)
#using the global keyword inside _a_function2 resulted in changing the value of _a_global in the main worlspace but within the scope of _a_function, remained 10
#-----------------------------------------------------------------------------
_a_global=10

def a_function():
    def _a_function2():
        global _a_global
        _a_global=20

    print("Before calling a_function, the value of _a_global is", _a_global)

    _a_function2()

    print("After calling a_function2, value of _a_global is", _a_global)

a_function()

print("The value of a_global in main workspace / namespace is ", _a_global)
