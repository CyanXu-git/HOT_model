module types
  use netcdf
  implicit none


  integer, parameter :: char_len=16, char_len_long=32

  type ncvar_attrib
     character(len=char_len) :: short_name
     character(len=char_len_long) :: long_name
     character(len=char_len_long) :: units
     integer :: id        
     integer :: type      
  end type ncvar_attrib


  ! NetCDF types
  integer, parameter :: &
       ncf_float=NF90_FLOAT
  integer, parameter :: ncf_double = NF90_DOUBLE

  real, parameter :: &
       ncf_fillval_float=NF90_FILL_FLOAT
  double precision, parameter :: ncf_fillval_double = NF90_FILL_DOUBLE
      
end module types
