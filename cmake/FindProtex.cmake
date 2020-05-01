set (protex_exe ${CMAKE_SOURCE_DIRECTORY}/config/protex)
set (protex_flags -g -b -f)

  get_filename_component (base ${src} NAME_WE)
  add_custom_command (OUTPUT ${base}.tex
    COMMAND ${protex_exe} ${protex_flags} -f ${src} > ${base}.tex
    DEPENDS ${src}
    COMMENT "[protex] Building documentatino for ${src}"
    )
endfunction (protex src)
