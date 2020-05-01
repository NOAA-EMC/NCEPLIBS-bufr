# Only add the directories in alldirs list that actually exist.  GEOS
# can build in multiple configurations, and the build system must be
# able to skip non-existent directiories in the list.

macro (esma_add_subdirectories dirs)
  set (dirs_ ${dirs} ${ARGN})
  foreach (subdirectory ${dirs_})
    if (IS_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${subdirectory})
      add_subdirectory (${subdirectory})
    endif ()
  endforeach()

endmacro (esma_add_subdirectories)

