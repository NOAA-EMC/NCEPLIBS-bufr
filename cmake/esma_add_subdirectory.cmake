macro (esma_add_subdirectory subdirectory)
  if (IS_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${subdirectory})
    add_subdirectory (${subdirectory})
  endif ()
endmacro (esma_add_subdirectory)

