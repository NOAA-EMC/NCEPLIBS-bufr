macro (esma_create_stub_component srcs module)
  list (APPEND ${srcs} ${module}.F90)
  add_custom_command (
    OUTPUT ${module}.F90
    COMMAND ${MAPL_SOURCE_DIR}/MAPL_Base/mapl_stub.pl ${module}Mod > ${module}.F90
    MAIN_DEPENDENCY ${MAPL_SOURCE_DIR}/MAPL_Base/mapl_stub.pl
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Making component stub for ${module}Mod in ${module}.F90"
    )
  add_custom_target(stub_${module} DEPENDS ${module}.F90)
endmacro ()

