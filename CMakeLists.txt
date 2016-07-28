project (building-lisp)

cmake_minimum_required (VERSION 3.6.0)

set (LISP_VERSION_MAJOR 0)
set (LISP_VERSION_MINOR 1)
set (LISP_VERSION_PATCH 0)

set (CMAKE_BUILD_TYPE Debug)

configure_file(
  "${PROJECT_SOURCE_DIR}/lisp_config.h.in"
  "${PROJECT_SOURCE_DIR}/lisp_config.h"
)

add_executable(lisp lisp.c)

# Add the binary dir so that we file the generated `lisp_config.h` file.
include_directories("${PROJECT_BINARY_DIR}")

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")

find_package(Readline REQUIRED)
include_directories("${READLINE_INCLUDE_DIR}")
target_link_libraries(lisp ${READLINE_LIBRARY})

install(TARGETS lisp DESTINATION bin)
