cmake_minimum_required(VERSION 3.30)

project(phn624 Fortran)
enable_language(Fortran)

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DDEBUG_MODE -O3 -g -cpp -DREAL64 -DINT64 -fcheck=all -fbackslash -fwhole-file -pedantic -fimplicit-none -fbacktrace -ffpe-trap=zero,overflow,underflow -march=native -Wline-truncation -Wall -Wcharacter-truncation -ftrapv")

find_package(MPI REQUIRED)
add_definitions(${MPI_Fortran_COMPILE_FLAGS})
include_directories(${MPI_Fortran_INCLUDE_PATH})
link_directories(${MPI_Fortran_LIBRARIES})

file(GLOB_RECURSE SRC CONFIGURE_DEPENDS
  ${PROJECT_SOURCE_DIR}/src/*.F90
  ${PROJECT_SOURCE_DIR}/src/**/*.F90)
add_library(CORE ${SRC})

file(GLOB units ${PROJECT_SOURCE_DIR}/basic/*/*/)
foreach(unit ${units})
  foreach(prog ${unit})
    string(REPLACE "/" "_" output ${prog}/out)
    add_executable(${output} ${prog}/main.F90)
    target_link_libraries(${output} CORE ${MPI_Fortran_LIBRARIES})
    set_target_properties(${output} PROPERTIES OUTPUT_NAME "out")
    set_target_properties(${output} PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${prog})
  endforeach()
endforeach()
