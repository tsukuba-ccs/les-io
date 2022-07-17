#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([LESBENCHMARK], [1.5], [les-io@ccs.tsukuba.ac.jp])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_MACRO_DIR([./m4])

AX_PROG_FC_MPI(,, [AC_MSG_FAILURE([No MPI compiler found])])

PKG_CHECK_MODULES([NETCDF], [netcdf])
PKG_CHECK_MODULES([NETCDFF], [netcdf-fortran])

AC_CONFIG_FILES([Makefile src/Makefile])
AC_OUTPUT
