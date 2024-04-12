"""
Python wrapper of the Fortran module ``fgen_time_w``

``fgen_time_w`` is itself a wrapper
around the Fortran module ``fgen_time``.
"""
from __future__ import annotations

from typing import Any

import fgen_runtime.exceptions as fgr_excs
import numpy as np
import pint
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
    from fgen_solving._lib import fgen_time_w  # type: ignore
except (ModuleNotFoundError, ImportError) as exc:
    raise fgr_excs.CompiledExtensionNotFoundError("fgen_solving._lib") from exc

UR: pint.registry.UnitRegistry = pint.get_application_registry()
"""Unit registry to use"""


@define
class TimeAxis(FinalizableWrapperBase):
    """
    Wrapper around the Fortran :class:`TimeAxis`

    Class for handling time axes
    """

    @property
    def exposed_attributes(self) -> tuple[str, ...]:
        """
        Attributes exposed by this wrapper
        """
        return (
            "values",
            "value_last_bound",
            "units",
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
            None,
            None,
        ),
    )
    def from_build_args(
        cls,
        values: pint.registry.UnitRegistry.Quantity,
        value_last_bound: pint.registry.UnitRegistry.Quantity,
    ) -> TimeAxis:
        """
        Initialise from build arguments

        This also creates a new connection to a Fortran object.
        The user is responsible for releasing this connection
        using :attr:`~finalize` when it is no longer needed.
        Alternatively a :obj:`~TimeAxisContext`
        can be used to handle the finalisation using a context manager.

        Parameters
        ----------
        values
            values

        value_last_bound
            value to use for last bound

        units
            units of :attr:`values` and :attr:`value_last_bound`

        Returns
        -------
            Built (i.e. linked to Fortran and initialised)
            :obj:`TimeAxis`

        See Also
        --------
        :meth:`TimeAxisContext.from_build_args`
        """
        # Extract units from the input, make sure they're all the same
        provided_units = {
            name: str(v.units)
            for name, v in (
                ("values", values),
                ("value_last_bound", value_last_bound),
            )
        }

        provided_units_values = list(provided_units.values())
        if len(set(provided_units_values)) > 1:
            msg = f"More than one unit provided. Received: {provided_units}"
            raise ValueError(msg)

        unit_for_fortran = provided_units_values[0]

        # Wrapping values
        # Strategy: WrappingStrategyArrayDeferredSize(
        #     magnitude_suffix='_m',
        #     shape_suffix='_shape',
        #     shape_callable_suffix='_shape',
        # )
        values_m = values.to(unit_for_fortran).m
        # Wrapping value_last_bound
        # Strategy: WrappingStrategyDefault(
        #     magnitude_suffix='_m',
        # )
        value_last_bound_m = value_last_bound.to(unit_for_fortran).m

        out = cls.from_new_connection()
        execute_finalize_on_fail(
            out,
            fgen_time_w.instance_build,
            values=values_m,
            value_last_bound=value_last_bound_m,
            units=unit_for_fortran,
        )

        return out

    @classmethod
    def from_new_connection(cls) -> TimeAxis:
        """
        Initialise from a new connection

        The user is responsible for releasing this connection
        using :attr:`~finalize` when it is no longer needed.
        Alternatively a :obj:`~TimeAxisContext`
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
        instance_index = fgen_time_w.get_free_instance_number()
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
        fgen_time_w.instance_finalize(self.instance_index)
        self._uninitialise_instance_index()

    # Attribute getters and setters
    @property
    @check_initialised
    @verify_units(
        None,
        (None,),
    )
    def values(self) -> pint.registry.UnitRegistry.Quantity:
        """
        values

        Returns
        -------
            Attribute value, retrieved from Fortran.

            The value is a copy of the derived type's data.
            Changes to this value will not be reflected
            in the underlying instance of the derived type.
            To make changes to the underlying instance, use the setter instead.
        """
        # Wrapping values
        # Strategy: WrappingStrategyArrayDeferredSize(
        #     magnitude_suffix='_m',
        #     shape_suffix='_shape',
        #     shape_callable_suffix='_shape',
        # )
        from fgen_runtime.arrays import ftype_to_np

        call_args = (self.instance_index,)

        (
            values_nd,
            values_dtype,
            values_shape,
        ) = fgen_time_w.iget_values_shape(*call_args)
        if values_nd == 0:
            # Nice example of how error handling could work
            # via basic integers or a derived type
            # (raise on Fortran side, check then process on Python side,
            # train track style).
            raise fgr_excs.UnallocatedMemoryError("values")

        values_ctype_scalar = np.ctypeslib.as_ctypes_type(ftype_to_np(values_dtype.strip().decode()))
        values_array = np.asfortranarray(np.empty(shape=values_shape[:values_nd], dtype=values_ctype_scalar))

        fgen_time_w.iget_values(*call_args, values=values_array)

        values: pint.registry.UnitRegistry.Quantity = UR.Quantity(values_array, self.units)

        return values

    @property
    @check_initialised
    @verify_units(
        None,
        (None,),
    )
    def value_last_bound(self) -> pint.registry.UnitRegistry.Quantity:
        """
        value to use for last bound

        Returns
        -------
            Attribute value, retrieved from Fortran.

            The value is a copy of the derived type's data.
            Changes to this value will not be reflected
            in the underlying instance of the derived type.
            To make changes to the underlying instance, use the setter instead.
        """
        # Wrapping value_last_bound
        # Strategy: WrappingStrategyDefault(
        #     magnitude_suffix='_m',
        # )
        value_last_bound_raw = fgen_time_w.iget_value_last_bound(
            self.instance_index,
        )
        value_last_bound: pint.registry.UnitRegistry.Quantity = UR.Quantity(value_last_bound_raw, self.units)

        return value_last_bound

    @property
    @check_initialised
    @verify_units(
        None,
        (None,),
    )
    def units(self) -> str:
        """
        units of :attr:`values` and :attr:`value_last_bound`

        Returns
        -------
            Attribute value, retrieved from Fortran.

            The value is a copy of the derived type's data.
            Changes to this value will not be reflected
            in the underlying instance of the derived type.
            To make changes to the underlying instance, use the setter instead.
        """
        # Wrapping units
        # Strategy: WrappingStrategyCharacterDeferredSize(
        #     length_suffix='_length',
        #     length_callable_suffix='_length',
        # )
        call_args = (self.instance_index,)
        units_length = fgen_time_w.iget_units_length(*call_args)
        units_raw = fgen_time_w.iget_units(
            *call_args,
            n=units_length,
        )

        units: str = units_raw.decode()

        return units

    # Wrapped methods
    @check_initialised
    @verify_units(
        None,
        (None,),
    )
    def repr(
        self,
    ) -> str:
        """
        String representation of self

        Returns
        -------
            String representation of self
        """
        # Wrapping res
        # Strategy: WrappingStrategyCharacterDeferredSize(
        #     length_suffix='_length',
        #     length_callable_suffix='_length',
        # )
        call_args = (self.instance_index,)
        res_length = fgen_time_w.i_repr_length(*call_args)
        res_raw = fgen_time_w.i_repr(
            *call_args,
            n=res_length,
        )

        res: str = res_raw.decode()

        return res

    @check_initialised
    @verify_units(
        None,
        (None,),
    )
    def get_bounds(
        self,
    ) -> pint.registry.UnitRegistry.Quantity:
        """
        Get the bounds

        Returns
        -------
            The bounds of self's data
        """
        # Wrapping bounds
        # Strategy: WrappingStrategyArrayDeferredSize(
        #     magnitude_suffix='_m',
        #     shape_suffix='_shape',
        #     shape_callable_suffix='_shape',
        # )
        from fgen_runtime.arrays import ftype_to_np

        call_args = (self.instance_index,)

        (
            bounds_nd,
            bounds_dtype,
            bounds_shape,
        ) = fgen_time_w.i_get_bounds_shape(*call_args)
        if bounds_nd == 0:
            # Nice example of how error handling could work
            # via basic integers or a derived type
            # (raise on Fortran side, check then process on Python side,
            # train track style).
            raise fgr_excs.UnallocatedMemoryError("bounds")

        bounds_ctype_scalar = np.ctypeslib.as_ctypes_type(ftype_to_np(bounds_dtype.strip().decode()))
        bounds_array = np.asfortranarray(np.empty(shape=bounds_shape[:bounds_nd], dtype=bounds_ctype_scalar))

        fgen_time_w.i_get_bounds(*call_args, bounds=bounds_array)

        bounds: pint.registry.UnitRegistry.Quantity = UR.Quantity(bounds_array, self.units)

        return bounds


@define
class TimeAxisNoSetters(FinalizableWrapperBase):
    """
    Wrapper around the Fortran :class:`TimeAxis`

    This wrapper has no setters so can be used for representing objects
    that have no connection to the underlying Fortran
    (i.e. changing their values/attributes
    will have no effect on the underlying Fortran).
    For example, derived type attribute values that are allocatable.

    Class for handling time axes
    """

    @property
    def exposed_attributes(self) -> tuple[str, ...]:
        """
        Attributes exposed by this wrapper
        """
        return (
            "values",
            "value_last_bound",
            "units",
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
            None,
            None,
        ),
    )
    def from_build_args(
        cls,
        values: pint.registry.UnitRegistry.Quantity,
        value_last_bound: pint.registry.UnitRegistry.Quantity,
    ) -> TimeAxisNoSetters:
        """
        Initialise from build arguments

        This also creates a new connection to a Fortran object.
        The user is responsible for releasing this connection
        using :attr:`~finalize` when it is no longer needed.
        Alternatively a :obj:`~TimeAxisNoSettersContext`
        can be used to handle the finalisation using a context manager.

        Parameters
        ----------
        values
            values

        value_last_bound
            value to use for last bound

        units
            units of :attr:`values` and :attr:`value_last_bound`

        Returns
        -------
            Built (i.e. linked to Fortran and initialised)
            :obj:`TimeAxisNoSetters`

        See Also
        --------
        :meth:`TimeAxisNoSettersContext.from_build_args`
        """
        # Extract units from the input, make sure they're all the same
        provided_units = {
            name: str(v.units)
            for name, v in (
                ("values", values),
                ("value_last_bound", value_last_bound),
            )
        }

        provided_units_values = list(provided_units.values())
        if len(set(provided_units_values)) > 1:
            msg = f"More than one unit provided. Received: {provided_units}"
            raise ValueError(msg)

        unit_for_fortran = provided_units_values[0]

        # Wrapping values
        # Strategy: WrappingStrategyArrayDeferredSize(
        #     magnitude_suffix='_m',
        #     shape_suffix='_shape',
        #     shape_callable_suffix='_shape',
        # )
        values_m = values.to(unit_for_fortran).m
        # Wrapping value_last_bound
        # Strategy: WrappingStrategyDefault(
        #     magnitude_suffix='_m',
        # )
        value_last_bound_m = value_last_bound.to(unit_for_fortran).m

        out = cls.from_new_connection()
        execute_finalize_on_fail(
            out,
            fgen_time_w.instance_build,
            values=values_m,
            value_last_bound=value_last_bound_m,
            units=unit_for_fortran,
        )

        return out

    @classmethod
    def from_new_connection(cls) -> TimeAxisNoSetters:
        """
        Initialise from a new connection

        The user is responsible for releasing this connection
        using :attr:`~finalize` when it is no longer needed.
        Alternatively a :obj:`~TimeAxisNoSettersContext`
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
        instance_index = fgen_time_w.get_free_instance_number()
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
        fgen_time_w.instance_finalize(self.instance_index)
        self._uninitialise_instance_index()

    # Attribute getters
    @property
    @check_initialised
    @verify_units(
        None,
        (None,),
    )
    def values(self) -> pint.registry.UnitRegistry.Quantity:
        """
        values

        Returns
        -------
            Attribute value, retrieved from Fortran.

            The value is a copy of the derived type's data.
            Changes to this value will not be reflected
            in the underlying instance of the derived type.
            To make changes to the underlying instance, use the setter instead.
        """
        # Wrapping values
        # Strategy: WrappingStrategyArrayDeferredSize(
        #     magnitude_suffix='_m',
        #     shape_suffix='_shape',
        #     shape_callable_suffix='_shape',
        # )
        from fgen_runtime.arrays import ftype_to_np

        call_args = (self.instance_index,)

        (
            values_nd,
            values_dtype,
            values_shape,
        ) = fgen_time_w.iget_values_shape(*call_args)
        if values_nd == 0:
            # Nice example of how error handling could work
            # via basic integers or a derived type
            # (raise on Fortran side, check then process on Python side,
            # train track style).
            raise fgr_excs.UnallocatedMemoryError("values")

        values_ctype_scalar = np.ctypeslib.as_ctypes_type(ftype_to_np(values_dtype.strip().decode()))
        values_array = np.asfortranarray(np.empty(shape=values_shape[:values_nd], dtype=values_ctype_scalar))

        fgen_time_w.iget_values(*call_args, values=values_array)

        values: pint.registry.UnitRegistry.Quantity = UR.Quantity(values_array, self.units)

        return values

    @property
    @check_initialised
    @verify_units(
        None,
        (None,),
    )
    def value_last_bound(self) -> pint.registry.UnitRegistry.Quantity:
        """
        value to use for last bound

        Returns
        -------
            Attribute value, retrieved from Fortran.

            The value is a copy of the derived type's data.
            Changes to this value will not be reflected
            in the underlying instance of the derived type.
            To make changes to the underlying instance, use the setter instead.
        """
        # Wrapping value_last_bound
        # Strategy: WrappingStrategyDefault(
        #     magnitude_suffix='_m',
        # )
        value_last_bound_raw = fgen_time_w.iget_value_last_bound(
            self.instance_index,
        )
        value_last_bound: pint.registry.UnitRegistry.Quantity = UR.Quantity(value_last_bound_raw, self.units)

        return value_last_bound

    @property
    @check_initialised
    @verify_units(
        None,
        (None,),
    )
    def units(self) -> str:
        """
        units of :attr:`values` and :attr:`value_last_bound`

        Returns
        -------
            Attribute value, retrieved from Fortran.

            The value is a copy of the derived type's data.
            Changes to this value will not be reflected
            in the underlying instance of the derived type.
            To make changes to the underlying instance, use the setter instead.
        """
        # Wrapping units
        # Strategy: WrappingStrategyCharacterDeferredSize(
        #     length_suffix='_length',
        #     length_callable_suffix='_length',
        # )
        call_args = (self.instance_index,)
        units_length = fgen_time_w.iget_units_length(*call_args)
        units_raw = fgen_time_w.iget_units(
            *call_args,
            n=units_length,
        )

        units: str = units_raw.decode()

        return units

    # Wrapped methods
    @check_initialised
    @verify_units(
        None,
        (None,),
    )
    def repr(
        self,
    ) -> str:
        """
        String representation of self

        Returns
        -------
            String representation of self
        """
        # Wrapping res
        # Strategy: WrappingStrategyCharacterDeferredSize(
        #     length_suffix='_length',
        #     length_callable_suffix='_length',
        # )
        call_args = (self.instance_index,)
        res_length = fgen_time_w.i_repr_length(*call_args)
        res_raw = fgen_time_w.i_repr(
            *call_args,
            n=res_length,
        )

        res: str = res_raw.decode()

        return res

    @check_initialised
    @verify_units(
        None,
        (None,),
    )
    def get_bounds(
        self,
    ) -> pint.registry.UnitRegistry.Quantity:
        """
        Get the bounds

        Returns
        -------
            The bounds of self's data
        """
        # Wrapping bounds
        # Strategy: WrappingStrategyArrayDeferredSize(
        #     magnitude_suffix='_m',
        #     shape_suffix='_shape',
        #     shape_callable_suffix='_shape',
        # )
        from fgen_runtime.arrays import ftype_to_np

        call_args = (self.instance_index,)

        (
            bounds_nd,
            bounds_dtype,
            bounds_shape,
        ) = fgen_time_w.i_get_bounds_shape(*call_args)
        if bounds_nd == 0:
            # Nice example of how error handling could work
            # via basic integers or a derived type
            # (raise on Fortran side, check then process on Python side,
            # train track style).
            raise fgr_excs.UnallocatedMemoryError("bounds")

        bounds_ctype_scalar = np.ctypeslib.as_ctypes_type(ftype_to_np(bounds_dtype.strip().decode()))
        bounds_array = np.asfortranarray(np.empty(shape=bounds_shape[:bounds_nd], dtype=bounds_ctype_scalar))

        fgen_time_w.i_get_bounds(*call_args, bounds=bounds_array)

        bounds: pint.registry.UnitRegistry.Quantity = UR.Quantity(bounds_array, self.units)

        return bounds


@define
class TimeAxisContext(FinalizableWrapperBaseContext):
    """
    Context manager for :class:`TimeAxis`
    """

    @classmethod
    def from_build_args(
        cls,
        *args: Any,
        **kwargs: Any,
    ) -> TimeAxisContext:
        """
        Docstrings to be handled as part of #223
        """
        return cls(
            TimeAxis.from_build_args(*args, **kwargs),
        )


@define
class TimeAxisNoSettersContext(FinalizableWrapperBaseContext):
    """
    Context manager for :class:`TimeAxisNoSetters`
    """

    @classmethod
    def from_build_args(
        cls,
        *args: Any,
        **kwargs: Any,
    ) -> TimeAxisNoSettersContext:
        """
        Docstrings to be handled as part of #223
        """
        return cls(
            TimeAxisNoSetters.from_build_args(*args, **kwargs),
        )
