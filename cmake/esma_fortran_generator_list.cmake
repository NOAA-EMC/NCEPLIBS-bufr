# CMake target_compile_options () apparently does not like receiving 2 options at once

macro (esma_fortran_generator_list target multioption)
  string (REPLACE " " ";" flags ${multioption})
  foreach(item ${flags})
    target_compile_options (${target} PRIVATE $<$<COMPILE_LANGUAGE:Fortran>:${item}>)
  endforeach ()
endmacro ()
