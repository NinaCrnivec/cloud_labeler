message(STATUS "Creating python interfaces with ${F2PY_PATH}")

set(DEPENDENCY_LIST cloud_label)

list(APPEND wrappers
  "py_cloud_label.f90"
  )

foreach ( dep ${DEPENDENCY_LIST} )
  set(lib_line "${lib_line} -l${dep}")
endforeach()
foreach ( dep ${DEPENDENCY_LIST} )
  set(lib_line "${lib_line} -l${dep}")
endforeach()
message(STATUS "Linker line: ${lib_line}")

get_property(include_dirs DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} PROPERTY INCLUDE_DIRECTORIES)
foreach ( inc_dir ${include_dirs} )
  set(inc_line "${inc_line} -I${inc_dir}")
endforeach()
message(STATUS "Include line: ${inc_line}")

foreach ( wrapper ${wrappers} )
  get_filename_component(basename "${wrapper}" NAME_WE )
  get_filename_component(wrappername "${wrapper}" NAME )

  get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)
  message(STATUS "Fortran Compiler Name: ${Fortran_COMPILER_NAME}")

  add_custom_command(OUTPUT ${basename}.so
    COMMAND CC=${CMAKE_C_COMPILER}
    ${F2PY_PATH}
    --f77exec=${CMAKE_Fortran_COMPILER}
    --f90exec=${CMAKE_Fortran_COMPILER}
    -m ${basename}
    -c ${CMAKE_CURRENT_SOURCE_DIR}/${wrapper}
    -L${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    ${lib_line}
    ${inc_line}
    -I${CMAKE_Fortran_MODULE_DIRECTORY}
    DEPENDS ${DEPENDENCY_LIST} ${CMAKE_CURRENT_SOURCE_DIR}/${wrapper}
    COMMENT "Building f2py target"
    VERBATIM)

  add_custom_target(do_python_interface_${basename} DEPENDS ${basename}.so)
  add_dependencies(python_interface do_python_interface_${basename})
endforeach ()
