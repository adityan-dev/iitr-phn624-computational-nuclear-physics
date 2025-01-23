module m_kinds

  use mpi_f08, only : mpi_float, mpi_double, mpi_long_double, mpi_datatype
  use, intrinsic :: iso_fortran_env, only : real32, real64, real128, &
       int8, int16, int32, int64

  implicit none
  private

  public :: sp, dp, qp
  public :: mpi_sp, mpi_dp, mpi_qp
  public :: wp, mpi_wp
  public :: i8, i16, i32, i64
  public :: dint

  integer, parameter :: sp = real32
  type(mpi_datatype), parameter :: mpi_sp = mpi_float
  integer, parameter :: dp = real64
  type(mpi_datatype), parameter :: mpi_dp = mpi_double
  integer, parameter :: qp = real128
  type(mpi_datatype), parameter :: mpi_qp = mpi_long_double

  integer, parameter :: i8 = int8
  integer, parameter :: i16 = int16
  integer, parameter :: i32 = int32
  integer, parameter :: i64 = int64

#ifdef INT8
  integer, parameter :: dint = int8
#elif INT16
  integer, parameter :: dint = int16
#elif INT32
  integer, parameter :: dint = int32
#elif INT64
  integer, parameter :: dint = int64
#else
  integer, parameter :: dint = int64
#endif

#ifdef REAL32
  integer, parameter :: wp = sp
  type(mpi_datatype), parameter :: mpi_wp = mpi_sp
#elif REAL64
  integer, parameter :: wp = dp
  type(mpi_datatype), parameter :: mpi_wp = mpi_dp
#elif REAL128
  integer, parameter :: wp = qp
  type(mpi_datatype), parameter :: mpi_wp = mpi_qp
#else
  integer, parameter :: wp = sp
  type(mpi_datatype), parameter :: mpi_wp = mpi_sp
#endif

end module m_kinds
