# ESMA allows that some directories are not present - we just want to
# build the ones that exist. 

macro (esma_build_subcomponents)
  foreach (subcomponent ${ARGN})

    if (IS_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${subcomponent})
      add_subdirectory (${subcomponent})
    endif ()

  endforeach ()
endmacro()

macro (esma_include_subcomponents this)
  foreach (subcomponent ${ARGN})

    if (IS_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${subcomponent})
      set (include ${include_${subcomponent}})
      target_include_directories (${this} PUBLIC ${include})
    endif ()

  endforeach ()
endmacro()

macro (esma_link_subcomponents this)
  foreach (subcomponent ${ARGN})

    if (IS_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${subcomponent})
      target_link_libraries (${this} ${subcomponent})
    endif ()

  endforeach ()
endmacro()

macro (esma_config_subcomponents this)
  esma_build_subcomponents("${ARGN}")
  esma_include_subcomponents(${this} "${ARGN}")
  esma_link_subcomponents(${this} "${ARGN}")
endmacro ()
