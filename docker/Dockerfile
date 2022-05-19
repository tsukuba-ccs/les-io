FROM ubuntu

ARG USERNAME=foo
ARG HDF5VER=1.13
ARG HDF5=hdf5-$HDF5VER.0
ARG NETCDFVER=4.8.1
ARG NETCDF=netcdf-c-$NETCDFVER
ARG NETCDFFVER=4.5.4
ARG NETCDFF=netcdf-fortran-$NETCDFFVER

RUN apt-get update \
 && apt-get -y upgrade \
 && apt-get -y install git make g++ libopenmpi-dev \
 && apt-get -y install sudo wget pkg-config \
 && cd \
 && wget https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-$HDF5VER/$HDF5/src/$HDF5.tar.gz \
 && tar zxfp $HDF5.tar.gz \
 && cd $HDF5 \
 && CC=mpicc ./configure --enable-parallel --enable-fortran --prefix=/usr/local \
 && make \
 && make install \
 && cd .. \
 && wget https://downloads.unidata.ucar.edu/netcdf-c/$NETCDFVER/$NETCDF.tar.gz \
 && tar zxfp $NETCDF.tar.gz \
 && cd $NETCDF \
 && CC=mpicc H5DIR=/usr/local LIBS=-ldl ./configure --disable-shared --enable-parallel-tests --prefix=/usr/local \
 && make \
 && make install \
 && cd .. \
 && wget https://downloads.unidata.ucar.edu/netcdf-fortran/$NETCDFFVER/$NETCDFF.tar.gz \
 && tar zxfp $NETCDFF.tar.gz \
 && cd $NETCDFF \
 && CC=mpicc FC=mpif90 F77=mpif77 LIBS="$(nc-config --libs)" ./configure --disable-shared --enable-parallel-tests \
 && make \
 && make install

RUN \
  # sshd
  apt-get -y install --no-install-recommends \
    openssh-server \
  # sshd_config
  && printf '%s\n' \
    'PermitRootLogin yes' \
    'PasswordAuthentication yes' \
    'PermitEmptyPasswords yes' \
    'UsePAM no' \
    > /etc/ssh/sshd_config.d/auth.conf \
  # ssh_config
  && printf '%s\n' \
    'Host *' \
    '    StrictHostKeyChecking no' \
    > /etc/ssh/ssh_config.d/ignore-host-key.conf

RUN useradd -m -s /bin/bash $USERNAME \
 && echo "$USERNAME ALL=(ALL:ALL) NOPASSWD: ALL" > /etc/sudoers.d/$USERNAME \
 # delete passwd
 && passwd -d $USERNAME

USER $USERNAME
RUN cd \
 && git clone https://github.com/tsukuba-ccs/les-io.git \
 && cd les-io \
 && autoreconf -i \
 && ./configure \
 && make \
 && sudo make install

COPY --chown=$USERNAME:$USERNAME docker/hosts /home/$USERNAME/hosts
COPY --chown=$USERNAME:$USERNAME docker/namelist.input /home/$USERNAME/namelist.input
COPY --chown=$USERNAME:$USERNAME output /home/$USERNAME/output
