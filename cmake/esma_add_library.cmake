# In most cases, GEOS follows a standard process to configure each
# component within the nested hierarchy.  The algorithm below codifies
# this standard process, thereby consiberably simplifying most
# CMakeLists.txt files in the code.

# Eventually the name of subcomponents and subdirectories will coincide.
# Waiting for git so that the rename is easy.


macro (esma_add_library this)

  if (CMAKE_DEBUG)
    message (STATUS "Generating build instructions for component: ${this}")
  endif ()

  set (options EXCLUDE_FROM_ALL)
  set (multiValueArgs
    # esma unique
    SUBCOMPONENTS SUBDIRS NEVER_STUB PRIVATE_DEFINITIONS PUBLIC_DEFINITIONS
    # shared with ecbuild (and not deprecated)
    SOURCES DEPENDS PUBLIC_LIBS
    # deprecated in esma (produces explicit warnings)
    SRCS INCLUDES DEPENDENCIES 
    # deprecated in ecbuild
    PUBLIC_INCLUDES
    )
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  if (ARGS_UNPARSED_ARGUMENTS)
      ecbuild_warn ("Unrecognized keyword arguments passed to esma_add_library: ${ARGS_UNPARSED_ARGUMENTS}")
  endif ()

  # Subdirs must exist and should be configured prior to subcomponents.
  foreach (subdir ${ARGS_SUBDIRS})
    add_subdirectory(${subdir})
  endforeach()

  # Handle deprecated
  if (ARGS_SRCS)
    if (NOT ESMA_ALLOW_DEPRECATED)
      ecbuild_warn("SRCS is a deprecated option for esma_add_library(); use SOURCES instead")
      endif ()
    set (ARGS_SOURCES ${ARGS_SRCS})
  endif ()
  if (ARGS_INCLUDES)
    if (NOT ESMA_ALLOW_DEPRECATED)
      ecbuild_warn("SRCS is a deprecated option for esma_add_library(); use target_include_directories instead")
    endif ()
    set (ARGS_PUBLIC_INCLUDES ${ARGS_INCLUDES})
  endif ()
  if (ARGS_DEPENDENCIES)
    if (NOT ESMA_ALLOW_DEPRECATED)
      ecbuild_warn("DEPENDENCIES is a deprecated option for esma_add_library(); use PUBLIC_LIBS instead")
    endif ()
    set (ARGS_PUBLIC_LIBS ${ARGS_DEPENDENCIES})
  endif ()

  # Configure subcomponents.  These can be stubbed and may have a
  # different name than the directory they reside in.  (Most
  # unfortunate.)
  set (non_stubbed)
  foreach (subdir ${ARGS_SUBCOMPONENTS})

    string (SUBSTRING ${subdir} 0 1 leading_character)
    if (leading_character STREQUAL "@")
      string (SUBSTRING ${subdir} 1 -1 mod_name) # strip leading "@"
    else ()
      set (mod_name ${subdir})
    endif()

    if (NOT rename_${subdir}) # usual case
      set (module_name ${mod_name})
    else ()
      set(module_name ${rename_${mod_name}})
    endif ()

    if (IS_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${subdir})
      add_subdirectory (${subdir})
      list (APPEND non_stubbed ${mod_name})
    else () # make stub and append to sources (in ARGS_SOURCES)
      if (CMAKE_DEBUG)
	message (STATUS  "  ... Creating stub component ${module_name}")
      endif()
      esma_create_stub_component (ARGS_SOURCES ${module_name})
    endif ()

  endforeach ()

  # This library depends on all DEPENDENCIES and _non-stubbed_ subcomponents.
  set (all_dependencies ${ARGS_PUBLIC_LIBS} ${non_stubbed})
  ecbuild_add_library (TARGET ${this}
    SOURCES ${ARGS_SOURCES}
    PUBLIC_LIBS ${all_dependencies}
    PUBLIC_INCLUDES ${ARGS_PUBLIC_INCLUDES}
    )

  set_target_properties(${this} PROPERTIES EXCLUDE_FROM_ALL ${ARGS_EXCLUDE_FROM_ALL})
  set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${esma_include}/${this})

  set (install_dir include/${this})
  # Export target  include directories for other targets
  target_include_directories(${this} PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}> # stubs
# modules and copied *.h, *.inc    
    $<BUILD_INTERFACE:${esma_include}/${this}>
    $<INSTALL_INTERFACE:${install_dir}>
    ) 

  if (ARGS_PUBLIC_INCLUDES)
    target_include_directories(${this} PUBLIC $<BUILD_INTERFACE:${ARGS_PUBLIC_INCLUDES}>)
  endif ()

  if (ARGS_PRIVATE_DEFINITIONS)
    target_compile_definitions(${this} PRIVATE ${ARGS_PRIVATE_DEFINITIONS})
  endif ()
  if (ARGS_PUBLIC_DEFINITIONS)
    target_compile_definitions(${this} PUBLIC ${ARGS_PUBLIC_DEFINITIONS})
  endif ()

  # The following possibly duplicates logic that is already in the ecbuild layer
  install (DIRECTORY  ${esma_include}/${this}/ DESTINATION include/${this})

endmacro ()
