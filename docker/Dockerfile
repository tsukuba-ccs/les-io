FROM ubuntu

ARG USERNAME=foo

RUN apt-get update \
 && apt-get -y upgrade \
 && apt-get -y install git make libopenmpi-dev libnetcdff-dev \
 && apt-get -y install sudo

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

# clean up
RUN apt-get -y autoremove \
 && apt-get -y clean \
 && rm -rf /var/lib/apt/lists/*

RUN useradd -m -s /bin/bash $USERNAME \
 && echo "$USERNAME ALL=(ALL:ALL) NOPASSWD: ALL" > /etc/sudoers.d/$USERNAME \
 # delete passwd
 && passwd -d $USERNAME

USER $USERNAME
RUN cd \
 && git clone https://github.com/tsukuba-ccs/les-io.git \
 && cd les-io \
 && autoreconf -i \
 && ./configure --with-netcdf=/usr --with-netcdff=/usr --prefix=$HOME \
 && make \
 && make install

COPY --chown=$USERNAME:$USERNAME docker/hosts /home/$USERNAME/hosts
COPY --chown=$USERNAME:$USERNAME docker/namelist.input /home/$USERNAME/namelist.input
