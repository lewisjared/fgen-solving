# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.14.5
#   kernelspec:
#     display_name: Python 3 (ipykernel)
#     language: python
#     name: python3
# ---

# %% [markdown]
# # Basic demo
#
# This notebook gives a basic demonstration of how to use Fortran-based solving utilities.

# %%
import pint

Q = pint.get_application_registry().Quantity

# %%
import fgen_solving
from fgen_solving.derived_type import DerivedType

# %%
print(f"You are using fgen_solving version {fgen_solving.__version__}")

# %% [markdown]
# The auto-generated Python wrappers give Python access to derived types defined in Fortran.

# %%
dt = DerivedType.from_build_args(base=Q(2, "m"))
dt

# %%
dt.base

# %%
dt.add(Q(3, "cm"))
