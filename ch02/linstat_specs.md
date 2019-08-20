# Linstat specifications

- Install pacakges `spdep`, `rgdal` and `sf`

```
linstat> wget https://download.osgeo.org/gdal/2.4.2/gdal-2.4.2.tar.gz
linstat> tar xvf gdal-2.4.2.tar.gz
linstat> mkdir gdal
linstat> cd gdal-2.4.2
linstat> ./configure --prefix=/home/s/sdaza/gdal/
linstat> make -j32
linstat> make install
linstat> setenv LD_LIBRARY_PATH /home/s/sdaza/gdal/lib
linstat> setenv GDAL_DATA /home/s/sdaza/gdal/share/gdal
linstat> R
> install.packages("rgdal", configure.args = "--with-gdal-config=/home/s/sdaza//gdal/bin/gdal-config")
> install.packages("sf", configure.args = "--with-gdal-config=/home/s/sdaza//gdal/bin/gdal-config")
> install.packages("spdep", configure.args = "--with-gdal-config=/home/s/sdaza//gdal/bin/gdal-config")

```

- Install INLA in linux
    - See https://inla.r-inla-download.org/Linux-builds/HOWTO
    - Download the Centos 64bit.tgz files. When this file in unpackaged, it needs to replace the '64bit' directory in the R-INLA package. You can find the location with:

    ```
    INLA:::inla.call.builtin()
    [1] "/home/s/sdaza/R/x86_64-redhat-linux-gnu-library/3.6/INLA/bin/linux/64bit/
    ``