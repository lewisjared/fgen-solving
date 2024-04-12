"""
Python wrapper of the Fortran module ``fgen_rk_w``

``fgen_rk_w`` is itself a wrapper
around the Fortran module ``fgen_rk``.
"""
from __future__ import annotations

from typing import Any

import fgen_runtime.exceptions as fgr_excs
from attrs import define
from fgen_runtime.base import (
    INVALID_INSTANCE_INDEX,
    FinalizableWrapperBase,
    FinalizableWrapperBaseContext,
    check_initialised,
    execute_finalize_on_fail,
)
from fgen_runtime.formatting import (
    to_html,
    to_pretty,
    to_str,
)
from fgen_runtime.units import verify_units

try:
    from fgen_solving._lib import fgen_rk_w  # type: ignore
except (ModuleNotFoundError, ImportError) as exc:
    raise fgr_excs.CompiledExtensionNotFoundError("fgen_solving._lib") from exc

_UNITS: dict[str, str] = {
    "rtol": "dimensionless",
    "atol": "dimensionless",
    "h": "yr",
}


@define
class RK4Stepper(FinalizableWrapperBase):
    """
    Wrapper around the Fortran :class:`RK4Stepper`

    Implementation of a Runge-Kutta 4th order stepper
    """

    @property
    def exposed_attributes(self) -> tuple[str, ...]:
        """
        Attributes exposed by this wrapper
        """
        return (
            "rtol",
            "atol",
            "h",
        )

    def __str__(self) -> str:
        """
        String representation of self
        """
        return to_str(
            self,
            self.exposed_attributes,
        )

    def _repr_pretty_(self, p: Any, cycle: bool) -> None:
        """
        Pretty representation of self

        Used by IPython notebooks and other tools
        """
        to_pretty(
            self,
            self.exposed_attributes,
            p=p,
            cycle=cycle,
        )

    def _repr_html_(self) -> str:
        """
        html representation of self

        Used by IPython notebooks and other tools
        """
        return to_html(
            self,
            self.exposed_attributes,
        )

    # Class methods
    @classmethod
    @verify_units(
        None,
        (
            None,
            _UNITS["rtol"],
            _UNITS["atol"],
            _UNITS["h"],
        ),
    )
    def from_build_args(
        cls,
        rtol: float,
        atol: float,
        h: float,
    ) -> RK4Stepper:
        """
        Initialise from build arguments

        This also creates a new connection to a Fortran object.
        The user is responsible for releasing this connection
        using :attr:`~finalize` when it is no longer needed.
        Alternatively a :obj:`~RK4StepperContext`
        can be used to handle the finalisation using a context manager.

        Parameters
        ----------
        rtol
            Relative tolerance to use when deciding step size

        atol
            Absolute tolerance to use when deciding step size

        h
            Step size

        Returns
        -------
            Built (i.e. linked to Fortran and initialised)
            :obj:`RK4Stepper`

        See Also
        --------
        :meth:`RK4StepperContext.from_build_args`
        """
        out = cls.from_new_connection()
        execute_finalize_on_fail(
            out,
            fgen_rk_w.instance_build,
            rtol=rtol,
            atol=atol,
            h=h,
        )

        return out

    @classmethod
    def from_new_connection(cls) -> RK4Stepper:
        """
        Initialise from a new connection

        The user is responsible for releasing this connection
        using :attr:`~finalize` when it is no longer needed.
        Alternatively a :obj:`~RK4StepperContext`
        can be used to handle the finalisation using a context manager.

        Returns
        -------
            A new instance with a unique instance index

        Raises
        ------
        WrapperErrorUnknownCause
            If a new instance could not be allocated

            This could occur if too many instances are allocated at any one time
        """
        instance_index = fgen_rk_w.get_free_instance_number()
        if instance_index == INVALID_INSTANCE_INDEX:
            raise fgr_excs.WrapperErrorUnknownCause(  # noqa: TRY003
                f"Could not create instance of {cls.__name__}. "
            )

        return cls(instance_index)

    # Finalisation
    @check_initialised
    def finalize(self) -> None:
        """
        Close the connection with the Fortran module
        """
        fgen_rk_w.instance_finalize(self.instance_index)
        self._uninitialise_instance_index()

    # Attribute getters and setters
    @property
    @check_initialised
    @verify_units(
        _UNITS["rtol"],
        (None,),
    )
    def rtol(self) -> float:
        """
        Relative tolerance to use when deciding step size

        Returns
        -------
            Attribute value, retrieved from Fortran.

            The value is a copy of the derived type's data.
            Changes to this value will not be reflected
            in the underlying instance of the derived type.
            To make changes to the underlying instance, use the setter instead.
        """
        # Wrapping rtol
        # Strategy: WrappingStrategyDefault(
        #     magnitude_suffix='_m',
        # )
        rtol: float = fgen_rk_w.iget_rtol(
            self.instance_index,
        )

        return rtol

    @property
    @check_initialised
    @verify_units(
        _UNITS["atol"],
        (None,),
    )
    def atol(self) -> float:
        """
        Absolute tolerance to use when deciding step size

        Returns
        -------
            Attribute value, retrieved from Fortran.

            The value is a copy of the derived type's data.
            Changes to this value will not be reflected
            in the underlying instance of the derived type.
            To make changes to the underlying instance, use the setter instead.
        """
        # Wrapping atol
        # Strategy: WrappingStrategyDefault(
        #     magnitude_suffix='_m',
        # )
        atol: float = fgen_rk_w.iget_atol(
            self.instance_index,
        )

        return atol

    @property
    @check_initialised
    @verify_units(
        _UNITS["h"],
        (None,),
    )
    def h(self) -> float:
        """
        Step size

        Returns
        -------
            Attribute value, retrieved from Fortran.

            The value is a copy of the derived type's data.
            Changes to this value will not be reflected
            in the underlying instance of the derived type.
            To make changes to the underlying instance, use the setter instead.
        """
        # Wrapping h
        # Strategy: WrappingStrategyDefault(
        #     magnitude_suffix='_m',
        # )
        h: float = fgen_rk_w.iget_h(
            self.instance_index,
        )

        return h


@define
class RK4StepperNoSetters(FinalizableWrapperBase):
    """
    Wrapper around the Fortran :class:`RK4Stepper`

    This wrapper has no setters so can be used for representing objects
    that have no connection to the underlying Fortran
    (i.e. changing their values/attributes
    will have no effect on the underlying Fortran).
    For example, derived type attribute values that are allocatable.

    Implementation of a Runge-Kutta 4th order stepper
    """

    @property
    def exposed_attributes(self) -> tuple[str, ...]:
        """
        Attributes exposed by this wrapper
        """
        return (
            "rtol",
            "atol",
            "h",
        )

    def __str__(self) -> str:
        """
        String representation of self
        """
        return to_str(
            self,
            self.exposed_attributes,
        )

    def _repr_pretty_(self, p: Any, cycle: bool) -> None:
        """
        Pretty representation of self

        Used by IPython notebooks and other tools
        """
        to_pretty(
            self,
            self.exposed_attributes,
            p=p,
            cycle=cycle,
        )

    def _repr_html_(self) -> str:
        """
        html representation of self

        Used by IPython notebooks and other tools
        """
        return to_html(
            self,
            self.exposed_attributes,
        )

    # Class methods
    @classmethod
    @verify_units(
        None,
        (
            None,
            _UNITS["rtol"],
            _UNITS["atol"],
            _UNITS["h"],
        ),
    )
    def from_build_args(
        cls,
        rtol: float,
        atol: float,
        h: float,
    ) -> RK4StepperNoSetters:
        """
        Initialise from build arguments

        This also creates a new connection to a Fortran object.
        The user is responsible for releasing this connection
        using :attr:`~finalize` when it is no longer needed.
        Alternatively a :obj:`~RK4StepperNoSettersContext`
        can be used to handle the finalisation using a context manager.

        Parameters
        ----------
        rtol
            Relative tolerance to use when deciding step size

        atol
            Absolute tolerance to use when deciding step size

        h
            Step size

        Returns
        -------
            Built (i.e. linked to Fortran and initialised)
            :obj:`RK4StepperNoSetters`

        See Also
        --------
        :meth:`RK4StepperNoSettersContext.from_build_args`
        """
        out = cls.from_new_connection()
        execute_finalize_on_fail(
            out,
            fgen_rk_w.instance_build,
            rtol=rtol,
            atol=atol,
            h=h,
        )

        return out

    @classmethod
    def from_new_connection(cls) -> RK4StepperNoSetters:
        """
        Initialise from a new connection

        The user is responsible for releasing this connection
        using :attr:`~finalize` when it is no longer needed.
        Alternatively a :obj:`~RK4StepperNoSettersContext`
        can be used to handle the finalisation using a context manager.

        Returns
        -------
            A new instance with a unique instance index

        Raises
        ------
        WrapperErrorUnknownCause
            If a new instance could not be allocated

            This could occur if too many instances are allocated at any one time
        """
        instance_index = fgen_rk_w.get_free_instance_number()
        if instance_index == INVALID_INSTANCE_INDEX:
            raise fgr_excs.WrapperErrorUnknownCause(  # noqa: TRY003
                f"Could not create instance of {cls.__name__}. "
            )

        return cls(instance_index)

    # Finalisation
    @check_initialised
    def finalize(self) -> None:
        """
        Close the connection with the Fortran module
        """
        fgen_rk_w.instance_finalize(self.instance_index)
        self._uninitialise_instance_index()

    # Attribute getters
    @property
    @check_initialised
    @verify_units(
        _UNITS["rtol"],
        (None,),
    )
    def rtol(self) -> float:
        """
        Relative tolerance to use when deciding step size

        Returns
        -------
            Attribute value, retrieved from Fortran.

            The value is a copy of the derived type's data.
            Changes to this value will not be reflected
            in the underlying instance of the derived type.
            To make changes to the underlying instance, use the setter instead.
        """
        # Wrapping rtol
        # Strategy: WrappingStrategyDefault(
        #     magnitude_suffix='_m',
        # )
        rtol: float = fgen_rk_w.iget_rtol(
            self.instance_index,
        )

        return rtol

    @property
    @check_initialised
    @verify_units(
        _UNITS["atol"],
        (None,),
    )
    def atol(self) -> float:
        """
        Absolute tolerance to use when deciding step size

        Returns
        -------
            Attribute value, retrieved from Fortran.

            The value is a copy of the derived type's data.
            Changes to this value will not be reflected
            in the underlying instance of the derived type.
            To make changes to the underlying instance, use the setter instead.
        """
        # Wrapping atol
        # Strategy: WrappingStrategyDefault(
        #     magnitude_suffix='_m',
        # )
        atol: float = fgen_rk_w.iget_atol(
            self.instance_index,
        )

        return atol

    @property
    @check_initialised
    @verify_units(
        _UNITS["h"],
        (None,),
    )
    def h(self) -> float:
        """
        Step size

        Returns
        -------
            Attribute value, retrieved from Fortran.

            The value is a copy of the derived type's data.
            Changes to this value will not be reflected
            in the underlying instance of the derived type.
            To make changes to the underlying instance, use the setter instead.
        """
        # Wrapping h
        # Strategy: WrappingStrategyDefault(
        #     magnitude_suffix='_m',
        # )
        h: float = fgen_rk_w.iget_h(
            self.instance_index,
        )

        return h


@define
class RK4StepperContext(FinalizableWrapperBaseContext):
    """
    Context manager for :class:`RK4Stepper`
    """

    @classmethod
    def from_build_args(
        cls,
        *args: Any,
        **kwargs: Any,
    ) -> RK4StepperContext:
        """
        Docstrings to be handled as part of #223
        """
        return cls(
            RK4Stepper.from_build_args(*args, **kwargs),
        )


@define
class RK4StepperNoSettersContext(FinalizableWrapperBaseContext):
    """
    Context manager for :class:`RK4StepperNoSetters`
    """

    @classmethod
    def from_build_args(
        cls,
        *args: Any,
        **kwargs: Any,
    ) -> RK4StepperNoSettersContext:
        """
        Docstrings to be handled as part of #223
        """
        return cls(
            RK4StepperNoSetters.from_build_args(*args, **kwargs),
        )
