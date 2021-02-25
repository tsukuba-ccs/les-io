#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([LESBENCHMARK], [1.5], [les-io@ccs.tsukuba.ac.jp])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_MACRO_DIR([./m4])

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
    x|xyes)
        # use default value
        netcdf_inc="${prefix}/include"
        netcdf_lib="${prefix}/lib"
    ;;
    xno)
        AC_MSG_ERROR("NetCDF-C library is required")
    ;;
    *)
        netcdf_inc="${with_netcdf}/include"
        netcdf_lib="${with_netcdf}/lib"
    ;;
esac

CFLAGS="${CFLAGS} -I${netcdf_inc} -L${netcdf_lib}"

AC_CHECK_LIB(netcdf, nc_create,
             [AC_CHECK_HEADER(netcdf.h,
                [],
                [AC_MSG_ERROR("NetCDF-C headers is not found.")],
                [AC_INCLUDES_DEFAULT])],
             [AC_MSG_ERROR("NetCDF-C library is not found.")])

AC_CHECK_LIB(netcdf, nc_create_par,
             [],
             [AC_MSG_ERROR("NetCDF-C with parallel support is not found.")])

# check for Fortran environment
AC_LANG_PUSH(Fortran)
AX_PROG_FC_MPI([], [], [AC_MSG_ERROR(["Cannot locate MPI implementation for Fortran."])])

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
    x|xyes)
        # use default value
        netcdff_inc="${prefix}/include"
        netcdff_lib="${prefix}/lib"
    ;;
    xno)
        AC_MSG_ERROR("NetCDF-Fortran library is required")
    ;;
    *)
        netcdff_inc="${with_netcdff}/include"
        netcdff_lib="${with_netcdff}/lib"
    ;;
esac

FCFLAGS="${FCFLAGS} -I${netcdff_inc} -L${netcdff_lib}"

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

AC_LANG_POP(Fortran) # end of Fortran environment checking


AC_CONFIG_FILES([Makefile
                 src/Makefile])
AC_OUTPUT
