# +-----------------------------------------------------------------------------+
# |   Copyright (C) 2011-2015                                                   |
# |   Original by Marcel Loose (loose <at> astron.nl) 2011-2013                 |
# |   Modified by Chris Kerr (chris.kerr <at> mykolab.ch) 2013-2015             |
# |                                                                             |
# |   This program is free software; you can redistribute it and/or modify      |
# |   it under the terms of the GNU General Public License as published by      |
# |   the Free Software Foundation; either version 2 of the License, or         |
# |   (at your option) any later version.                                       |
# |                                                                             |
# |   This program is distributed in the hope that it will be useful,           |
# |   but WITHOUT ANY WARRANTY; without even the implied warranty of            |
# |   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             |
# |   GNU General Public License for more details.                              |
# |                                                                             |
# |   You should have received a copy of the GNU General Public License         |
# |   along with this program; if not, write to the                             |
# |   Free Software Foundation, Inc.,                                           |
# |   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.                 |
# +-----------------------------------------------------------------------------+

## -----------------------------------------------------------------------------
## Macro to generate a Python interface module from one or more Fortran sources
##
## Usage: add_f2py_module(<module-name> SOURCES <src1>..<srcN> DESTINATION <install-dir> ONLY <list>)
##
macro (add_f2py_module _name)

  # Parse arguments.
  set (options USE_MPI)
  set (oneValueArgs DESTINATION)
  set (multiValueArgs SOURCES ONLY LIBRARIES INCLUDEDIRS)
  cmake_parse_arguments(add_f2py_module "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN} )

  if (add_f2py_module_ONLY STREQUAL "")
    set (_only "")
  else ()
    string(REPLACE ";" " " _only "${add_f2py_module_ONLY}")
    set (_only "only: ${_only} :")
  endif()
    

  # Sanity checks.
  if(add_f2py_module_SOURCES MATCHES "^$")
    message(FATAL_ERROR "add_f2py_module: no source files specified")
  endif(add_f2py_module_SOURCES MATCHES "^$")

  # Get the compiler-id and map it to compiler vendor as used by f2py.
  # Currently, we only check for GNU, but this can easily be extended. 
  # Cache the result, so that we only need to check once.
  if(NOT F2PY_FCOMPILER)
    if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
      if(CMAKE_Fortran_COMPILER_SUPPORTS_F90)
        set(_fcompiler "gnu95")
      else(CMAKE_Fortran_COMPILER_SUPPORTS_F90)
        set(_fcompiler "gnu")
      endif(CMAKE_Fortran_COMPILER_SUPPORTS_F90)
    elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
      set(_fcompiler "intelem")
    else(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
      set(_fcompiler "F2PY_FCOMPILER-NOTFOUND")
    endif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(F2PY_FCOMPILER ${_fcompiler} CACHE STRING
      "F2PY: Fortran compiler type by vendor" FORCE)
    if(NOT F2PY_FCOMPILER)
      message(STATUS "[F2PY]: Could not determine Fortran compiler type. "
                     "Troubles ahead!")
    endif(NOT F2PY_FCOMPILER)
  endif(NOT F2PY_FCOMPILER)

  # Set f2py compiler options: compiler vendor and path to Fortran77/90 compiler.
  if(F2PY_FCOMPILER)
    set(_fcompiler_opts "--fcompiler=${F2PY_FCOMPILER}")
    list(APPEND _fcompiler_opts "--f77exec=${CMAKE_Fortran_COMPILER}" "--f77flags='${CMAKE_Fortran_FLAGS}'")
    if(CMAKE_Fortran_COMPILER_SUPPORTS_F90)
      list(APPEND _fcompiler_opts "--f90exec=${CMAKE_Fortran_COMPILER}" "--f90flags='${CMAKE_Fortran_FLAGS}'")
    endif(CMAKE_Fortran_COMPILER_SUPPORTS_F90)
  endif(F2PY_FCOMPILER)

  # Make the source filenames absolute.
  set(_abs_srcs)
  foreach(_src ${add_f2py_module_SOURCES})
    get_filename_component(_abs_src ${_src} ABSOLUTE)
    list(APPEND _abs_srcs ${_abs_src})
  endforeach(_src ${add_f2py_module_SOURCES})

  # Get a list of the include directories.
  # The f2py --include_paths option, used when generating a signature file,
  # needs a colon-separated list. The f2py -I option, used when compiling
  # the sources, must be repeated for every include directory.
  #get_directory_property(_inc_dirs INCLUDE_DIRECTORIES)

  set(_inc_opts)
  set(_lib_opts)
  set(_inc_path_list)
  foreach(_dir ${add_f2py_module_INCLUDEDIRS})
    list(APPEND _inc_opts "-I${_dir}")
    list(APPEND _lib_opts "-L${_dir}")
    list(APPEND _inc_dirs "${_dir}")
  endforeach(_dir)
  string(REPLACE ";" ":" _inc_paths "${_inc_dirs}")

  set(_libs_opts)
  foreach(_lib ${add_f2py_module_LIBRARIES})
     # MAT This is hacky, but so is this whole code
     #     On darwin, esmf is a full path libesmf.a and
     #     not esmf_fullylinked. For now, if libesmf.a
     #     is passed down, replace with esmf
     if (_lib MATCHES "esmf\.a")
        set (_lib esmf)
     endif ()
     list(APPEND _lib_opts "-l${_lib}")
  endforeach(_lib)

  if ( ${add_f2py_module_USE_MPI})
     foreach (lib ${MPI_Fortran_LIBRARIES})
        get_filename_component(lib_dir ${lib} DIRECTORY)
        list(APPEND _lib_opts "-L${lib_dir}")

        get_filename_component(lib_name ${lib} NAME)
        string(REGEX MATCH "lib(.*)(${CMAKE_SHARED_LIBRARY_SUFFIX}|${CMAKE_STATIC_LIBRARY_SUFFIX})" BOBO ${lib_name})
        set(short_lib_name "${CMAKE_MATCH_1}")
        list(APPEND _lib_opts "-l${short_lib_name}")
     endforeach ()
  endif ()

  # Define the command to generate the Fortran to Python interface module. The
  # output will be a shared library that can be imported by python.
  if ( "${add_f2py_module_SOURCES}" MATCHES "^[^;]*\\.pyf;" )
    add_custom_command(OUTPUT "${_name}${F2PY_SUFFIX}"
      COMMAND ${F2PY_EXECUTABLE} --quiet -m ${_name}
              --build-dir "${CMAKE_CURRENT_BINARY_DIR}/f2py-${_name}"
              ${_fcompiler_opts} ${_inc_opts} -c ${_abs_srcs} &> /dev/null
      DEPENDS ${add_f2py_module_SOURCES}
      COMMENT "[F2PY] Building Fortran to Python interface module ${_name}")
  else ( "${add_f2py_module_SOURCES}" MATCHES "^[^;]*\\.pyf;" )
    add_custom_command(OUTPUT "${_name}${F2PY_SUFFIX}"
      COMMAND ${F2PY_EXECUTABLE} --quiet -m ${_name} -h ${_name}.pyf
              --build-dir "${CMAKE_CURRENT_BINARY_DIR}/f2py-${_name}"
              --include-paths ${_inc_paths} --overwrite-signature ${_abs_srcs} &> /dev/null
      COMMAND ${F2PY_EXECUTABLE} --quiet -m ${_name}
              --build-dir "${CMAKE_CURRENT_BINARY_DIR}/f2py-${_name}"
              -c "${CMAKE_CURRENT_BINARY_DIR}/f2py-${_name}/${_name}.pyf"
              ${_fcompiler_opts} ${_inc_opts} ${_lib_opts} ${_abs_srcs} ${_lib_opts} &> /dev/null
      DEPENDS ${add_f2py_module_SOURCES}
      COMMENT "[F2PY] Building Fortran to Python interface module ${_name}")
  endif ( "${add_f2py_module_SOURCES}" MATCHES "^[^;]*\\.pyf;" )
  


  # Add a custom target <name> to trigger the generation of the python module.
  add_custom_target(${_name} ALL DEPENDS "${_name}${F2PY_SUFFIX}")

  if(NOT (add_f2py_module_DESTINATION MATCHES "^$" OR add_f2py_module_DESTINATION MATCHES ";"))
    # Install the python module
    install(PROGRAMS "${CMAKE_CURRENT_BINARY_DIR}/${_name}${F2PY_SUFFIX}"
            DESTINATION ${add_f2py_module_DESTINATION})
  endif(NOT (add_f2py_module_DESTINATION MATCHES "^$" OR add_f2py_module_DESTINATION MATCHES ";"))


endmacro (add_f2py_module)

