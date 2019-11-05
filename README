LES-IO Benchmark
================

LES-IO Benchmark is an I/O benchmark that consists of an I/O kernel of City-LES
code [1] in MPI and netCDF.

LES-IO Benchmark is executed in iprocs x jprocs MPI processes.  The number of
processes, problem sizes, and physics model options can be specified by the
'namelist.input' configuration file.

It generates the following four kinds of three-dimensional meteorological data
in the './output/' directory in the netCDF format;

lesout_average.nc   - average data in several time steps 
lesout_instant.nc   - instant data at a time step
lesout_integral.nc  - integrated data in several time steps
lesrst_restart.nc   - restart data

Installation
------------
See INSTALL.

Configuration
-------------
System configuration and simulation configuration can be specified by the
`namelist.input` file.  It consists of three sections;

`parallel` : parallel execution option
`domain`   : domain size
`physics`  : physics model option

`parallel` section
iprocs : number of processes in the horizontal i direction.
jprocs : number of processes in the horizontal j direction.

Note.
  The total number of MPI processes is iprocs x jprocs.

`domain` section
ids      : the start index of whole domain in i-direction.
ide      : the end index of whole domain in i-direction.
jds      : the start index of whole domain in j-direction.
jde      : the end index of whole domain in j-direction.
kds      : the start index of whole domain in k-direction.
kde      : the end index of whole domain in k-direction.
num_soil : number of soil layers.

Note.
  The domain size is (ide-ids+1) x (jde-jds+1) x (kde-kds+1).
  The (ide-ids+1) and (jde-jds+1) should be larger than iprocs and jprocs,
  respectively.
  num_soil option controls the number of soil layers in the model, which affects
  the size of restart data.  The num_soil is only effective when soil_model is 1
  in physics section.

`physics` section
soil_model : soil model
    0 : no soil model
    1 : use soil model
mp_model : cloud microphysics model
    0 : no cloud microphysics model
    1 : use cloud microphysics model (warm rain model)
    9 : use cloud microphysics model (bin model)

Note.
  When soil_model is 1, the num_soil is effective in domain section.
  When mp_model is 1 or 9, the generated data includes the cloud data.

References
----------
[1] Ikeda, R., H. Kusaka, S. Iizuka, T. Boku, Development of Urban
    Meteorological LES model for thermal environment at city scale, 9th
    International Conference for Urban Climate, Toulouse, France, 2015.

External Link
-------------
- Large Eddy Simulation (LES) Model
  http://www.geoenv.tsukuba.ac.jp/~kusakaken/index.php?id=534
