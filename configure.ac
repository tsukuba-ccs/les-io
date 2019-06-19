#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([LESBENCHMARK], [1.5], [BUG-REPORT-ADDRESS])
AM_INIT_AUTOMAKE([foreign])

AC_PROG_FC([mpif90 mpifort])

# check for C dependencies
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
