"""
Python wrapper of the Fortran module ``fgen_timeseries_w``

``fgen_timeseries_w`` is itself a wrapper
around the Fortran module ``fgen_timeseries``.
"""
from __future__ import annotations

from typing import Any, Union

import fgen_runtime.exceptions as fgr_excs
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

from fgen_solving.enums import OneDimensionalHandlingOption

try:
    from fgen_solving._lib import fgen_timeseries_w  # type: ignore
except (ModuleNotFoundError, ImportError) as exc:
    raise fgr_excs.CompiledExtensionNotFoundError("fgen_solving._lib") from exc

try:
    from fgen_solving.fgen_time import TimeAxis, TimeAxisNoSetters
except (ModuleNotFoundError, ImportError) as exc:
    raise fgr_excs.RelativePythonModuleNotFoundError(
        requesting_python_module=__name__,
        requested_from_python_module="fgen_solving.fgen_time",
        requested="TimeAxis, TimeAxisNoSetters",
    ) from exc
try:
    from fgen_solving.fgen_values_bounded import ValuesBounded, ValuesBoundedNoSetters
except (ModuleNotFoundError, ImportError) as exc:
    raise fgr_excs.RelativePythonModuleNotFoundError(
        requesting_python_module=__name__,
        requested_from_python_module="fgen_solving.fgen_values_bounded",
        requested="ValuesBounded, ValuesBoundedNoSetters",
    ) from exc

UR: pint.registry.UnitRegistry = pint.get_application_registry()
"""Unit registry to use"""


@define
class Timeseries(FinalizableWrapperBase):
    """
    Wrapper around the Fortran :class:`Timeseries`

    Timeseries handling container
    """

    @property
    def exposed_attributes(self) -> tuple[str, ...]:
        """
        Attributes exposed by this wrapper
        """
        return (
            "name",
            "values",
            "time",
            "spline",
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
            None,
            None,
        ),
    )
    def from_build_args(
        cls,
        name: str,
        values: Union[
            ValuesBounded,
            ValuesBoundedNoSetters,
        ],
        time: Union[
            TimeAxis,
            TimeAxisNoSetters,
        ],
        spline: OneDimensionalHandlingOption,
    ) -> Timeseries:
        """
        Initialise from build arguments

        This also creates a new connection to a Fortran object.
        The user is responsible for releasing this connection
        using :attr:`~finalize` when it is no longer needed.
        Alternatively a :obj:`~TimeseriesContext`
        can be used to handle the finalisation using a context manager.

        Parameters
        ----------
        name
            Name of the timeseries

        values
            values

        time
            Time axis

        spline
            Kind of spline to assume when performing operations

        Returns
        -------
            Built (i.e. linked to Fortran and initialised)
            :obj:`Timeseries`

        See Also
        --------
        :meth:`TimeseriesContext.from_build_args`
        """
        # Wrapping values
        # Strategy: WrappingStrategyDerivedType(
        #     no_setters_class_suffix='NoSetters',
        #     instance_index_suffix='_instance_index',
        # )
        values_instance_index = values.instance_index
        # Wrapping time
        # Strategy: WrappingStrategyDerivedType(
        #     no_setters_class_suffix='NoSetters',
        #     instance_index_suffix='_instance_index',
        # )
        time_instance_index = time.instance_index
        # Wrapping spline
        # Strategy: WrappingStrategyEnum(
        #     int_value_suffix='_int_value',
        # )
        spline_int_value = spline.value

        out = cls.from_new_connection()
        execute_finalize_on_fail(
            out,
            fgen_timeseries_w.instance_build,
            name=name,
            values_instance_index=values_instance_index,
            time_instance_index=time_instance_index,
            spline=spline_int_value,
        )

        return out

    @classmethod
    def from_new_connection(cls) -> Timeseries:
        """
        Initialise from a new connection

        The user is responsible for releasing this connection
        using :attr:`~finalize` when it is no longer needed.
        Alternatively a :obj:`~TimeseriesContext`
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
        instance_index = fgen_timeseries_w.get_free_instance_number()
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
        fgen_timeseries_w.instance_finalize(self.instance_index)
        self._uninitialise_instance_index()

    # Attribute getters and setters
    @property
    @check_initialised
    @verify_units(
        None,
        (None,),
    )
    def name(self) -> str:
        """
        Name of the timeseries

        Returns
        -------
            Attribute value, retrieved from Fortran.

            The value is a copy of the derived type's data.
            Changes to this value will not be reflected
            in the underlying instance of the derived type.
            To make changes to the underlying instance, use the setter instead.
        """
        # Wrapping name
        # Strategy: WrappingStrategyCharacterDeferredSize(
        #     length_suffix='_length',
        #     length_callable_suffix='_length',
        # )
        call_args = (self.instance_index,)
        name_length = fgen_timeseries_w.iget_name_length(*call_args)
        name_raw = fgen_timeseries_w.iget_name(
            *call_args,
            n=name_length,
        )

        name: str = name_raw.decode()

        return name

    @property
    @check_initialised
    @verify_units(
        None,
        (None,),
    )
    def values(self) -> ValuesBoundedNoSetters:
        """
        values

        Returns
        -------
            Attribute value, retrieved from Fortran.
        """
        # Wrapping values
        # Strategy: WrappingStrategyDerivedType(
        #     no_setters_class_suffix='NoSetters',
        #     instance_index_suffix='_instance_index',
        # )
        values_instance_index = fgen_timeseries_w.iget_values(
            self.instance_index,
        )
        values: ValuesBoundedNoSetters = ValuesBoundedNoSetters(values_instance_index)

        return values

    @property
    @check_initialised
    @verify_units(
        None,
        (None,),
    )
    def time(self) -> TimeAxisNoSetters:
        """
        Time axis

        Returns
        -------
            Attribute value, retrieved from Fortran.
        """
        # Wrapping time
        # Strategy: WrappingStrategyDerivedType(
        #     no_setters_class_suffix='NoSetters',
        #     instance_index_suffix='_instance_index',
        # )
        time_instance_index = fgen_timeseries_w.iget_time(
            self.instance_index,
        )
        time: TimeAxisNoSetters = TimeAxisNoSetters(time_instance_index)

        return time

    @property
    @check_initialised
    @verify_units(
        None,
        (None,),
    )
    def spline(self) -> OneDimensionalHandlingOption:
        """
        Kind of spline to assume when performing operations

        Returns
        -------
            Attribute value, retrieved from Fortran.

            The value is a copy of the derived type's data.
            Changes to this value will not be reflected
            in the underlying instance of the derived type.
            To make changes to the underlying instance, use the setter instead.
        """
        # Wrapping spline
        # Strategy: WrappingStrategyEnum(
        #     int_value_suffix='_int_value',
        # )
        spline_int_value: int = fgen_timeseries_w.iget_spline(
            self.instance_index,
        )

        spline = OneDimensionalHandlingOption(spline_int_value)

        return spline

    # Wrapped methods
    @check_initialised
    @verify_units(
        None,
        (None,),
    )
    def get_time_units(
        self,
    ) -> str:
        """
        Get the time units, without having to chain in Python

        Returns
        -------
            Time units
        """
        # Wrapping tunits
        # Strategy: WrappingStrategyCharacterDeferredSize(
        #     length_suffix='_length',
        #     length_callable_suffix='_length',
        # )
        call_args = (self.instance_index,)
        tunits_length = fgen_timeseries_w.i_get_time_units_length(*call_args)
        tunits_raw = fgen_timeseries_w.i_get_time_units(
            *call_args,
            n=tunits_length,
        )

        tunits: str = tunits_raw.decode()

        return tunits

    @check_initialised
    @verify_units(
        None,
        (None,),
    )
    def get_values_units(
        self,
    ) -> str:
        """
        Get the values units, without having to chain in Python

        Returns
        -------
            values units
        """
        # Wrapping vunits
        # Strategy: WrappingStrategyCharacterDeferredSize(
        #     length_suffix='_length',
        #     length_callable_suffix='_length',
        # )
        call_args = (self.instance_index,)
        vunits_length = fgen_timeseries_w.i_get_values_units_length(*call_args)
        vunits_raw = fgen_timeseries_w.i_get_values_units(
            *call_args,
            n=vunits_length,
        )

        vunits: str = vunits_raw.decode()

        return vunits

    @check_initialised
    @verify_units(
        None,
        (
            None,
            None,
        ),
    )
    def get_value_at_time(
        self,
        time: pint.registry.UnitRegistry.Quantity,
    ) -> pint.registry.UnitRegistry.Quantity:
        """
        Get a value at a specific time

        Parameters
        ----------
        time
            Time for which to get the value

        Returns
        -------
            The value for time t
        """
        # Wrapping time
        # Strategy: WrappingStrategyDefault(
        #     magnitude_suffix='_m',
        # )
        time_m = time.to(self.get_time_units()).m

        # Wrapping value
        # Strategy: WrappingStrategyDefault(
        #     magnitude_suffix='_m',
        # )
        value_raw = fgen_timeseries_w.i_get_value_at_time(
            self.instance_index,
            time=time_m,
        )
        value: pint.registry.UnitRegistry.Quantity = UR.Quantity(value_raw, self.get_values_units())

        return value

    @check_initialised
    @verify_units(
        None,
        (
            None,
            None,
            None,
        ),
    )
    def interpolate(
        self,
        time_axis_new: Union[
            TimeAxis,
            TimeAxisNoSetters,
        ],
        allow_extrapolation: bool,
    ) -> TimeseriesNoSetters:
        """
        Interpolate onto a new time axis

        Parameters
        ----------
        time_axis_new
            New time axis onto which to interpolate

        allow_extrapolation
            Should extrapolation be allowed or not?

        Returns
        -------
            Interpolation
        """
        # Wrapping time_axis_new
        # Strategy: WrappingStrategyDerivedType(
        #     no_setters_class_suffix='NoSetters',
        #     instance_index_suffix='_instance_index',
        # )
        time_axis_new_instance_index = time_axis_new.instance_index

        # Wrapping res
        # Strategy: WrappingStrategyDerivedType(
        #     no_setters_class_suffix='NoSetters',
        #     instance_index_suffix='_instance_index',
        # )
        res_instance_index = fgen_timeseries_w.i_interpolate(
            self.instance_index,
            time_axis_new_instance_index=time_axis_new_instance_index,
            allow_extrapolation=allow_extrapolation,
        )
        res: TimeseriesNoSetters = TimeseriesNoSetters(res_instance_index)

        return res

    @check_initialised
    @verify_units(
        None,
        (
            None,
            None,
        ),
    )
    def differentiate(
        self,
        name_res: str,
    ) -> TimeseriesNoSetters:
        """
        Differentiate

        Parameters
        ----------
        name_res
            Name of the result of the differentiation

        Returns
        -------
            Derivative
        """
        # Wrapping res
        # Strategy: WrappingStrategyDerivedType(
        #     no_setters_class_suffix='NoSetters',
        #     instance_index_suffix='_instance_index',
        # )
        res_instance_index = fgen_timeseries_w.i_differentiate(
            self.instance_index,
            name_res=name_res,
        )
        res: TimeseriesNoSetters = TimeseriesNoSetters(res_instance_index)

        return res

    @check_initialised
    @verify_units(
        None,
        (
            None,
            None,
            None,
        ),
    )
    def integrate(
        self,
        c: pint.registry.UnitRegistry.Quantity,
        name_res: str,
    ) -> TimeseriesNoSetters:
        """
        integrate

        Parameters
        ----------
        c
            Constant of integration

        name_res
            Name of the result of the integration

        Returns
        -------
            Integral
        """
        # Wrapping c
        # Strategy: WrappingStrategyDefault(
        #     magnitude_suffix='_m',
        # )
        c_m = c.to(f"{self.get_values_units()} {self.get_time_units()}").m

        # Wrapping res
        # Strategy: WrappingStrategyDerivedType(
        #     no_setters_class_suffix='NoSetters',
        #     instance_index_suffix='_instance_index',
        # )
        res_instance_index = fgen_timeseries_w.i_integrate(
            self.instance_index,
            c=c_m,
            name_res=name_res,
        )
        res: TimeseriesNoSetters = TimeseriesNoSetters(res_instance_index)

        return res


@define
class TimeseriesNoSetters(FinalizableWrapperBase):
    """
    Wrapper around the Fortran :class:`Timeseries`

    This wrapper has no setters so can be used for representing objects
    that have no connection to the underlying Fortran
    (i.e. changing their values/attributes
    will have no effect on the underlying Fortran).
    For example, derived type attribute values that are allocatable.

    Timeseries handling container
    """

    @property
    def exposed_attributes(self) -> tuple[str, ...]:
        """
        Attributes exposed by this wrapper
        """
        return (
            "name",
            "values",
            "time",
            "spline",
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
            None,
            None,
        ),
    )
    def from_build_args(
        cls,
        name: str,
        values: Union[
            ValuesBounded,
            ValuesBoundedNoSetters,
        ],
        time: Union[
            TimeAxis,
            TimeAxisNoSetters,
        ],
        spline: OneDimensionalHandlingOption,
    ) -> TimeseriesNoSetters:
        """
        Initialise from build arguments

        This also creates a new connection to a Fortran object.
        The user is responsible for releasing this connection
        using :attr:`~finalize` when it is no longer needed.
        Alternatively a :obj:`~TimeseriesNoSettersContext`
        can be used to handle the finalisation using a context manager.

        Parameters
        ----------
        name
            Name of the timeseries

        values
            values

        time
            Time axis

        spline
            Kind of spline to assume when performing operations

        Returns
        -------
            Built (i.e. linked to Fortran and initialised)
            :obj:`TimeseriesNoSetters`

        See Also
        --------
        :meth:`TimeseriesNoSettersContext.from_build_args`
        """
        # Wrapping values
        # Strategy: WrappingStrategyDerivedType(
        #     no_setters_class_suffix='NoSetters',
        #     instance_index_suffix='_instance_index',
        # )
        values_instance_index = values.instance_index
        # Wrapping time
        # Strategy: WrappingStrategyDerivedType(
        #     no_setters_class_suffix='NoSetters',
        #     instance_index_suffix='_instance_index',
        # )
        time_instance_index = time.instance_index
        # Wrapping spline
        # Strategy: WrappingStrategyEnum(
        #     int_value_suffix='_int_value',
        # )
        spline_int_value = spline.value

        out = cls.from_new_connection()
        execute_finalize_on_fail(
            out,
            fgen_timeseries_w.instance_build,
            name=name,
            values_instance_index=values_instance_index,
            time_instance_index=time_instance_index,
            spline=spline_int_value,
        )

        return out

    @classmethod
    def from_new_connection(cls) -> TimeseriesNoSetters:
        """
        Initialise from a new connection

        The user is responsible for releasing this connection
        using :attr:`~finalize` when it is no longer needed.
        Alternatively a :obj:`~TimeseriesNoSettersContext`
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
        instance_index = fgen_timeseries_w.get_free_instance_number()
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
        fgen_timeseries_w.instance_finalize(self.instance_index)
        self._uninitialise_instance_index()

    # Attribute getters
    @property
    @check_initialised
    @verify_units(
        None,
        (None,),
    )
    def name(self) -> str:
        """
        Name of the timeseries

        Returns
        -------
            Attribute value, retrieved from Fortran.

            The value is a copy of the derived type's data.
            Changes to this value will not be reflected
            in the underlying instance of the derived type.
            To make changes to the underlying instance, use the setter instead.
        """
        # Wrapping name
        # Strategy: WrappingStrategyCharacterDeferredSize(
        #     length_suffix='_length',
        #     length_callable_suffix='_length',
        # )
        call_args = (self.instance_index,)
        name_length = fgen_timeseries_w.iget_name_length(*call_args)
        name_raw = fgen_timeseries_w.iget_name(
            *call_args,
            n=name_length,
        )

        name: str = name_raw.decode()

        return name

    @property
    @check_initialised
    @verify_units(
        None,
        (None,),
    )
    def values(self) -> ValuesBoundedNoSetters:
        """
        values

        Returns
        -------
            Attribute value, retrieved from Fortran.
        """
        # Wrapping values
        # Strategy: WrappingStrategyDerivedType(
        #     no_setters_class_suffix='NoSetters',
        #     instance_index_suffix='_instance_index',
        # )
        values_instance_index = fgen_timeseries_w.iget_values(
            self.instance_index,
        )
        values: ValuesBoundedNoSetters = ValuesBoundedNoSetters(values_instance_index)

        return values

    @property
    @check_initialised
    @verify_units(
        None,
        (None,),
    )
    def time(self) -> TimeAxisNoSetters:
        """
        Time axis

        Returns
        -------
            Attribute value, retrieved from Fortran.
        """
        # Wrapping time
        # Strategy: WrappingStrategyDerivedType(
        #     no_setters_class_suffix='NoSetters',
        #     instance_index_suffix='_instance_index',
        # )
        time_instance_index = fgen_timeseries_w.iget_time(
            self.instance_index,
        )
        time: TimeAxisNoSetters = TimeAxisNoSetters(time_instance_index)

        return time

    @property
    @check_initialised
    @verify_units(
        None,
        (None,),
    )
    def spline(self) -> OneDimensionalHandlingOption:
        """
        Kind of spline to assume when performing operations

        Returns
        -------
            Attribute value, retrieved from Fortran.

            The value is a copy of the derived type's data.
            Changes to this value will not be reflected
            in the underlying instance of the derived type.
            To make changes to the underlying instance, use the setter instead.
        """
        # Wrapping spline
        # Strategy: WrappingStrategyEnum(
        #     int_value_suffix='_int_value',
        # )
        spline_int_value: int = fgen_timeseries_w.iget_spline(
            self.instance_index,
        )

        spline = OneDimensionalHandlingOption(spline_int_value)

        return spline

    # Wrapped methods
    @check_initialised
    @verify_units(
        None,
        (None,),
    )
    def get_time_units(
        self,
    ) -> str:
        """
        Get the time units, without having to chain in Python

        Returns
        -------
            Time units
        """
        # Wrapping tunits
        # Strategy: WrappingStrategyCharacterDeferredSize(
        #     length_suffix='_length',
        #     length_callable_suffix='_length',
        # )
        call_args = (self.instance_index,)
        tunits_length = fgen_timeseries_w.i_get_time_units_length(*call_args)
        tunits_raw = fgen_timeseries_w.i_get_time_units(
            *call_args,
            n=tunits_length,
        )

        tunits: str = tunits_raw.decode()

        return tunits

    @check_initialised
    @verify_units(
        None,
        (None,),
    )
    def get_values_units(
        self,
    ) -> str:
        """
        Get the values units, without having to chain in Python

        Returns
        -------
            values units
        """
        # Wrapping vunits
        # Strategy: WrappingStrategyCharacterDeferredSize(
        #     length_suffix='_length',
        #     length_callable_suffix='_length',
        # )
        call_args = (self.instance_index,)
        vunits_length = fgen_timeseries_w.i_get_values_units_length(*call_args)
        vunits_raw = fgen_timeseries_w.i_get_values_units(
            *call_args,
            n=vunits_length,
        )

        vunits: str = vunits_raw.decode()

        return vunits

    @check_initialised
    @verify_units(
        None,
        (
            None,
            None,
        ),
    )
    def get_value_at_time(
        self,
        time: pint.registry.UnitRegistry.Quantity,
    ) -> pint.registry.UnitRegistry.Quantity:
        """
        Get a value at a specific time

        Parameters
        ----------
        time
            Time for which to get the value

        Returns
        -------
            The value for time t
        """
        # Wrapping time
        # Strategy: WrappingStrategyDefault(
        #     magnitude_suffix='_m',
        # )
        time_m = time.to(self.get_time_units()).m

        # Wrapping value
        # Strategy: WrappingStrategyDefault(
        #     magnitude_suffix='_m',
        # )
        value_raw = fgen_timeseries_w.i_get_value_at_time(
            self.instance_index,
            time=time_m,
        )
        value: pint.registry.UnitRegistry.Quantity = UR.Quantity(value_raw, self.get_values_units())

        return value

    @check_initialised
    @verify_units(
        None,
        (
            None,
            None,
            None,
        ),
    )
    def interpolate(
        self,
        time_axis_new: Union[
            TimeAxis,
            TimeAxisNoSetters,
        ],
        allow_extrapolation: bool,
    ) -> TimeseriesNoSetters:
        """
        Interpolate onto a new time axis

        Parameters
        ----------
        time_axis_new
            New time axis onto which to interpolate

        allow_extrapolation
            Should extrapolation be allowed or not?

        Returns
        -------
            Interpolation
        """
        # Wrapping time_axis_new
        # Strategy: WrappingStrategyDerivedType(
        #     no_setters_class_suffix='NoSetters',
        #     instance_index_suffix='_instance_index',
        # )
        time_axis_new_instance_index = time_axis_new.instance_index

        # Wrapping res
        # Strategy: WrappingStrategyDerivedType(
        #     no_setters_class_suffix='NoSetters',
        #     instance_index_suffix='_instance_index',
        # )
        res_instance_index = fgen_timeseries_w.i_interpolate(
            self.instance_index,
            time_axis_new_instance_index=time_axis_new_instance_index,
            allow_extrapolation=allow_extrapolation,
        )
        res: TimeseriesNoSetters = TimeseriesNoSetters(res_instance_index)

        return res

    @check_initialised
    @verify_units(
        None,
        (
            None,
            None,
        ),
    )
    def differentiate(
        self,
        name_res: str,
    ) -> TimeseriesNoSetters:
        """
        Differentiate

        Parameters
        ----------
        name_res
            Name of the result of the differentiation

        Returns
        -------
            Derivative
        """
        # Wrapping res
        # Strategy: WrappingStrategyDerivedType(
        #     no_setters_class_suffix='NoSetters',
        #     instance_index_suffix='_instance_index',
        # )
        res_instance_index = fgen_timeseries_w.i_differentiate(
            self.instance_index,
            name_res=name_res,
        )
        res: TimeseriesNoSetters = TimeseriesNoSetters(res_instance_index)

        return res

    @check_initialised
    @verify_units(
        None,
        (
            None,
            None,
            None,
        ),
    )
    def integrate(
        self,
        c: pint.registry.UnitRegistry.Quantity,
        name_res: str,
    ) -> TimeseriesNoSetters:
        """
        integrate

        Parameters
        ----------
        c
            Constant of integration

        name_res
            Name of the result of the integration

        Returns
        -------
            Integral
        """
        # Wrapping c
        # Strategy: WrappingStrategyDefault(
        #     magnitude_suffix='_m',
        # )
        c_m = c.to(f"{self.get_values_units()} {self.get_time_units()}").m

        # Wrapping res
        # Strategy: WrappingStrategyDerivedType(
        #     no_setters_class_suffix='NoSetters',
        #     instance_index_suffix='_instance_index',
        # )
        res_instance_index = fgen_timeseries_w.i_integrate(
            self.instance_index,
            c=c_m,
            name_res=name_res,
        )
        res: TimeseriesNoSetters = TimeseriesNoSetters(res_instance_index)

        return res


@define
class TimeseriesContext(FinalizableWrapperBaseContext):
    """
    Context manager for :class:`Timeseries`
    """

    @classmethod
    def from_build_args(
        cls,
        *args: Any,
        **kwargs: Any,
    ) -> TimeseriesContext:
        """
        Docstrings to be handled as part of #223
        """
        return cls(
            Timeseries.from_build_args(*args, **kwargs),
        )


@define
class TimeseriesNoSettersContext(FinalizableWrapperBaseContext):
    """
    Context manager for :class:`TimeseriesNoSetters`
    """

    @classmethod
    def from_build_args(
        cls,
        *args: Any,
        **kwargs: Any,
    ) -> TimeseriesNoSettersContext:
        """
        Docstrings to be handled as part of #223
        """
        return cls(
            TimeseriesNoSetters.from_build_args(*args, **kwargs),
        )
