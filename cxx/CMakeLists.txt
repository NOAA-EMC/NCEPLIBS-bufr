# (C) Copyright 2021 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

list(APPEND c_src
     File.cpp
     FortranObject.cpp
     QuerySet.cpp
     ResultSet.cpp)

list(APPEND c_hdr
     FortranObject.h
     File.h
     QuerySet.h
     ResultSet.h)
