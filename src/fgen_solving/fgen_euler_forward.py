"""
Python wrapper of the Fortran module ``fgen_euler_forward_w``

``fgen_euler_forward_w`` is itself a wrapper
around the Fortran module ``fgen_euler_forward``.
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
    from fgen_solving._lib import fgen_euler_forward_w  # type: ignore
except (ModuleNotFoundError, ImportError) as exc:
    raise fgr_excs.CompiledExtensionNotFoundError("fgen_solving._lib") from exc

_UNITS: dict[str, str] = {
    "step_size": "yr",
}


@define
class EulerForwardStepper(FinalizableWrapperBase):
    """
    Wrapper around the Fortran :class:`EulerForwardStepper`

    Implementation of a forward Euler stepper
    """

    @property
    def exposed_attributes(self) -> tuple[str, ...]:
        """
        Attributes exposed by this wrapper
        """
        return ("step_size",)

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
            _UNITS["step_size"],
        ),
    )
    def from_build_args(
        cls,
        step_size: float,
    ) -> EulerForwardStepper:
        """
        Initialise from build arguments

        This also creates a new connection to a Fortran object.
        The user is responsible for releasing this connection
        using :attr:`~finalize` when it is no longer needed.
        Alternatively a :obj:`~EulerForwardStepperContext`
        can be used to handle the finalisation using a context manager.

        Parameters
        ----------
        step_size
            Step size

        Returns
        -------
            Built (i.e. linked to Fortran and initialised)
            :obj:`EulerForwardStepper`

        See Also
        --------
        :meth:`EulerForwardStepperContext.from_build_args`
        """
        out = cls.from_new_connection()
        execute_finalize_on_fail(
            out,
            fgen_euler_forward_w.instance_build,
            step_size=step_size,
        )

        return out

    @classmethod
    def from_new_connection(cls) -> EulerForwardStepper:
        """
        Initialise from a new connection

        The user is responsible for releasing this connection
        using :attr:`~finalize` when it is no longer needed.
        Alternatively a :obj:`~EulerForwardStepperContext`
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
        instance_index = fgen_euler_forward_w.get_free_instance_number()
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
        fgen_euler_forward_w.instance_finalize(self.instance_index)
        self._uninitialise_instance_index()

    # Attribute getters and setters
    @property
    @check_initialised
    @verify_units(
        _UNITS["step_size"],
        (None,),
    )
    def step_size(self) -> float:
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
        # Wrapping step_size
        # Strategy: WrappingStrategyDefault(
        #     magnitude_suffix='_m',
        # )
        step_size: float = fgen_euler_forward_w.iget_step_size(
            self.instance_index,
        )

        return step_size


@define
class EulerForwardStepperNoSetters(FinalizableWrapperBase):
    """
    Wrapper around the Fortran :class:`EulerForwardStepper`

    This wrapper has no setters so can be used for representing objects
    that have no connection to the underlying Fortran
    (i.e. changing their values/attributes
    will have no effect on the underlying Fortran).
    For example, derived type attribute values that are allocatable.

    Implementation of a forward Euler stepper
    """

    @property
    def exposed_attributes(self) -> tuple[str, ...]:
        """
        Attributes exposed by this wrapper
        """
        return ("step_size",)

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
            _UNITS["step_size"],
        ),
    )
    def from_build_args(
        cls,
        step_size: float,
    ) -> EulerForwardStepperNoSetters:
        """
        Initialise from build arguments

        This also creates a new connection to a Fortran object.
        The user is responsible for releasing this connection
        using :attr:`~finalize` when it is no longer needed.
        Alternatively a :obj:`~EulerForwardStepperNoSettersContext`
        can be used to handle the finalisation using a context manager.

        Parameters
        ----------
        step_size
            Step size

        Returns
        -------
            Built (i.e. linked to Fortran and initialised)
            :obj:`EulerForwardStepperNoSetters`

        See Also
        --------
        :meth:`EulerForwardStepperNoSettersContext.from_build_args`
        """
        out = cls.from_new_connection()
        execute_finalize_on_fail(
            out,
            fgen_euler_forward_w.instance_build,
            step_size=step_size,
        )

        return out

    @classmethod
    def from_new_connection(cls) -> EulerForwardStepperNoSetters:
        """
        Initialise from a new connection

        The user is responsible for releasing this connection
        using :attr:`~finalize` when it is no longer needed.
        Alternatively a :obj:`~EulerForwardStepperNoSettersContext`
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
        instance_index = fgen_euler_forward_w.get_free_instance_number()
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
        fgen_euler_forward_w.instance_finalize(self.instance_index)
        self._uninitialise_instance_index()

    # Attribute getters
    @property
    @check_initialised
    @verify_units(
        _UNITS["step_size"],
        (None,),
    )
    def step_size(self) -> float:
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
        # Wrapping step_size
        # Strategy: WrappingStrategyDefault(
        #     magnitude_suffix='_m',
        # )
        step_size: float = fgen_euler_forward_w.iget_step_size(
            self.instance_index,
        )

        return step_size


@define
class EulerForwardStepperContext(FinalizableWrapperBaseContext):
    """
    Context manager for :class:`EulerForwardStepper`
    """

    @classmethod
    def from_build_args(
        cls,
        *args: Any,
        **kwargs: Any,
    ) -> EulerForwardStepperContext:
        """
        Docstrings to be handled as part of #223
        """
        return cls(
            EulerForwardStepper.from_build_args(*args, **kwargs),
        )


@define
class EulerForwardStepperNoSettersContext(FinalizableWrapperBaseContext):
    """
    Context manager for :class:`EulerForwardStepperNoSetters`
    """

    @classmethod
    def from_build_args(
        cls,
        *args: Any,
        **kwargs: Any,
    ) -> EulerForwardStepperNoSettersContext:
        """
        Docstrings to be handled as part of #223
        """
        return cls(
            EulerForwardStepperNoSetters.from_build_args(*args, **kwargs),
        )
