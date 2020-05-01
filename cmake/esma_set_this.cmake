# Defines a macro that establishes "this" for each subdirectory.  The
# default is to take the name from the current (binary) directory, but
# can be overridden with a name.

# The macro also establishes the include_${this} directory
# that can be used by other components that depend on "this".

# Note that the binary directory is used because we might build a
# source directory multiple times with different arguments.

set (esma_include  ${CMAKE_BINARY_DIR}/include CACHE PATH "include directory")
set (esma_etc  ${CMAKE_BINARY_DIR}/etc CACHE PATH "etc directory")
file (MAKE_DIRECTORY ${esma_include})
file (MAKE_DIRECTORY ${esma_etc})


macro (esma_set_this)
  set (options OPTIONAL EXCLUDE_FROM_ALL)
  set (oneValueArgs OVERRIDE)
  set (multiValueArgs)
  cmake_parse_arguments(ARGS "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

  if (ARGS_UNPARSED_ARGUMENTS)
    message (FATAL_ERROR "esma_set_this unparsed arguments: ${ARGS_UNPARSED_ARGUMENTS}")
  endif ()

  # (re) define variable "this"
  if (ARGS_OVERRIDE)
    set (this ${ARGS_OVERRIDE})
  else ()
    get_filename_component (dir "${CMAKE_CURRENT_BINARY_DIR}" NAME)
    string (SUBSTRING ${dir} 0 1 leading_character)
    if (leading_character STREQUAL "@")
      string (SUBSTRING ${dir} 1 -1 this) # strip leading "@"
    else ()
      set (this ${dir})
    endif()
  endif ()
  
  set(include_${this} ${esma_include}/${this})
  file (MAKE_DIRECTORY ${esma_include}/${this})
  file (MAKE_DIRECTORY ${esma_etc}/${this})

  # Control debugging for this subtree

  esma_check_if_debug()

endmacro()


