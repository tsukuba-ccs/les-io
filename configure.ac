#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([LESBENCHMARK], [1.5], [les-io@ccs.tsukuba.ac.jp])
AM_INIT_AUTOMAKE([foreign])

AC_PROG_FC([mpif90 mpifort])

# check for C dependencies

## NetCDF-C
AC_MSG_CHECKING([path to NetCDF-C])
AC_ARG_WITH(netcdf,
            [AS_HELP_STRING(
                [--with-netcdf],
                [Specify NetCDF-C path [default=PREFIX]])],
            [
                with_netcdf="${withval}";
                AC_MSG_RESULT([${withval}])
            ],
            [AC_MSG_RESULT([empty (use default)])])

case "x$with_netcdf" in
    xyes)
        # use default value
        netcdf_inc="${PREFIX}/include"
        netcdf_lib="${PREFIX}/lib"
    ;;
    xno)
        AC_MSG_ERROR("NetCDF-C library is required")
    ;;
    *)
        netcdf_inc="${with_netcdf}/include"
        netcdf_lib="${with_netcdf}/lib"
    ;;
esac

SAVED_CFLAGS="${CFLAGS}"
CFLAGS="-I${netcdf_inc} -L${netcdf_lib}"

AC_CHECK_LIB(netcdf, nc_create,
             [AC_CHECK_HEADER(netcdf.h,
                [],
                [AC_MSG_ERROR("NetCDF-C headers is not found.")],
                [AC_INCLUDES_DEFAULT])],
             [AC_MSG_ERROR("NetCDF-C library is not found.")])

AC_CHECK_LIB(netcdf, nc_create_par,
             [],
             [AC_MSG_ERROR("NetCDF-C with parallel support is not found.")])

CFLAGS="${SAVED_CFLAGS}"



# check for Fortran environment
AC_LANG_PUSH(Fortran)

## NetCDF-Fortran
AC_MSG_CHECKING([path to NetCDF-Fortran])
AC_ARG_WITH(netcdff,
            [AS_HELP_STRING(
                [--with-netcdff],
                [Specify NetCDF-Fortran path [default=PREFIX]])],
            [
                with_netcdff="${withval}";
                AC_MSG_RESULT([${withval}])
            ],
            [AC_MSG_RESULT([empty (use default)])])

case "x$with_netcdff" in
    xyes)
        # use default value
        netcdff_inc="${PREFIX}/include"
        netcdff_lib="${PREFIX}/lib"
    ;;
    xno)
        AC_MSG_ERROR("NetCDF-Fortran library is required")
    ;;
    *)
        netcdff_inc="${with_netcdff}/include"
        netcdff_lib="${with_netcdff}/lib"
    ;;
esac

SAVED_FCFLAGS="${FCFLAGS}"
FCFLAGS="-I${netcdff_inc} -L${netcdff_lib}"

AC_CHECK_LIB(netcdff, nf_create,
             [AC_CHECK_HEADER(netcdf.mod,
                [],
                [AC_MSG_ERROR(["NetCDF-Fortran headers is not found."])],
                [AC_INCLUDES_DEFAULT])],
             [AC_MSG_ERROR("NetCDF-Fortran library is not found.")],
             [-lnetcdf])

AC_CHECK_LIB(netcdff, nf_create_par,
             [],
             [AC_MSG_ERROR("NetCDF-Fortran with parallel support is not found.")],
             [-lnetcdf])

FCFLAGS="${SAVED_FCFLAGS}"

AC_LANG_POP(Fortran) # end of Fortran environment checking


AC_CONFIG_FILES([Makefile
                 src/Makefile])
AC_OUTPUT
