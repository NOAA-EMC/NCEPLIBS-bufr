# From:
# https://stackoverflow.com/questions/29848176/how-to-make-cmake-use-debug-compilation-mode-for-only-a-subset-of-directories-in
# But modified to cache the debug control flags

# Usage:
# Should be called inside CMakeLists.txt for each directory.
# Set CMake (cached) variable "DEBUG_<dir>" to ON to activate debugging
# in a subdirectory.
# If debugging is already on at this level, the variable is ignored.
# I.e. debugging is sticky going down a tree of directories.

macro (esma_check_if_debug)

  get_filename_component(this_dir_raw ${CMAKE_CURRENT_SOURCE_DIR} NAME)
  string (SUBSTRING ${this_dir_raw} 0 1 leading_character)
  if (leading_character STREQUAL "@")
    string (SUBSTRING ${this_dir_raw} 1 -1 this_dir) # strip leading "@"
  else ()
    set (this_dir ${this_dir_raw})
  endif()
  option (DEBUG_${this_dir} "Activate debugging flags in this directory and its subdirs." OFF)

  if (NOT (CMAKE_BUILD_TYPE MATCHES Debug))
    if (DEBUG_${this_dir})
      file(RELATIVE_PATH dir ${CMAKE_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR})
      message(STATUS "Note: Debugging in directory tree ${dir}")
      set (CMAKE_BUILD_TYPE Debug)
    endif()
  endif()

endmacro()

