# Generate headers and  resource files for GOCART components

set (acg_flags -v)

macro (new_esma_generate_automatic_code
    target registry headers rcs headers_destination rcs_destination flags)

  set (generator ${esma_include}/MAPL_Base/mapl_acg.pl)
  set (generated_files "${headers};${rcs}")

  add_custom_command (
    OUTPUT ${generated_files}
    COMMAND ${generator} ${acg_flags} ${flags} ${CMAKE_CURRENT_SOURCE_DIR}/${registry}
    COMMAND ${CMAKE_COMMAND} -E copy ${headers} ${headers_destination}
    COMMAND ${CMAKE_COMMAND} -E copy ${rcs} ${rcs_destination}
    COMMAND touch foo
    MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${registry}
    DEPENDS ${generator}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Generating automated code for ${registry}"
    )
  add_custom_target (phony_${target} DEPENDS ${generated_files})
  add_dependencies (${target} phony_${target})
  install(FILES ${rcs_destination}/${rcs} DESTINATION etc)

endmacro ()

macro (esma_generate_gocart_code target flags)
  string (REPLACE "_GridComp" "" name ${target})

  set (automatic_headers
    ${name}_ExportSpec___.h
    ${name}_GetPointer___.h
    )
  set (automatic_rc
    ${name}_History___.rc
    )

  set (registry ${name}_Registry.rc)

  new_esma_generate_automatic_code (
    ${target} ${registry}
    "${automatic_headers}" "${automatic_rc}"
    ${esma_include}/GEOSchem_GridComp ${esma_etc}
    ${flags}
  )
  
endmacro ()

macro (esma_generate_gmi_code target type)
  set (name GMICHEM)

  set (automatic_headers
    ${name}_DeclarePointer___.h
    ${name}_GetPointer___.h
    ${name}_ExportSpec___.h
    )

  set (generator ${CMAKE_SOURCE_DIR}/GMAO_Shared/MAPL_Base/mapl_acg.pl)

  add_custom_command (
    #    TARGET ${this}
    OUTPUT ${name}_GetPointer___.h ${name}_ExportSpec___.h ${name}_DeclarePointer___.h
    COMMAND ${generator} ${acg_flags} -N ${name} ${type}_Registry___.rc
    COMMAND ${CMAKE_COMMAND} -E copy ${name}_GetPointer___.h ${esma_include}/${this}/${type}_DeclarePointer___.h
    COMMAND ${CMAKE_COMMAND} -E copy ${name}_ExportSpec___.h ${esma_include}/${this}/${type}_ExportSpec___.h
    COMMAND ${CMAKE_COMMAND} -E copy ${name}_DeclarePointer___.h ${esma_include}/${this}/${type}_DeclarePointer___.h
    MAIN_DEPENDENCY ${type}_Registry___.rc
    DEPENDS ${generator}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Generating automated GMI code for ${type}"
    )
#  add_custom_target (phony_${target}_${type} DEPENDS ${automatic_headers})
#  add_dependencies (${target} phony_${target}_${type})

  
endmacro ()


macro (esma_generate_automatic_code this name destination flags)
  set (registry ${CMAKE_CURRENT_SOURCE_DIR}/${name}_Registry.rc)

  add_custom_command (
    OUTPUT ${name}_ExportSpec___.h ${name}_GetPointer___.h ${name}_History___.rc
    COMMAND ${CMAKE_SOURCE_DIR}/GMAO_Shared/MAPL_Base/mapl_acg.pl ${acg_flags} ${flags} ${registry}
    COMMAND ${CMAKE_COMMAND} -E copy ${name}_ExportSpec___.h ${include_GEOSchem_GridComp}
    COMMAND ${CMAKE_COMMAND} -E copy ${name}_ImportSpec___.h ${include_GEOSchem_GridComp}
    COMMAND ${CMAKE_COMMAND} -E copy ${name}_GetPointer___.h ${include_GEOSchem_GridComp}
    MAIN_DEPENDENCY ${registry}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Generating automated code for ${registry}"
    )
  add_custom_target (fake_target DEPENDS ${name}_ExportSpec___.h ${name}_GetPointer___.h ${name}_History___.rc
    COMMENT "Checking if regeneration is required")
  add_dependencies (${this} fake_target)
endmacro ()
