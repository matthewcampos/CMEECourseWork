"""Debugging practice"""

__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
__version__ = '0.0.1'

def makeabug(x):
    """using ipdb to identify bug in function"""
    y = x**4
    z = 0.
    import ipdb; ipdb.set_trace()
    y = y/z
    return y

makeabug(25)
