"""
Enum definitions used in Fortran modules

Generated using :mod:`fgen`
"""
from __future__ import annotations

from enum import Enum


class OneDimensionalHandlingOption(Enum):
    """
    Options for fgen's 1D handling
    """

    NotSpecified = -1
    """No handling has been specified"""

    LinearSpline = 1
    """Linear spline is assumed between points"""

    QuadraticSpline = 2
    """Quadratic spline is assumed between points"""

    CubicSpline = 3
    """Cubic spline is assumed between points"""

    Previous = 10
    """The value is equal to the last defined point (a type of constant spline)"""

    Next = 11
    """The value is equal to the next defined point (a type of constant spline)"""
