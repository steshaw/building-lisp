project (building-lisp)

cmake_minimum_required (VERSION 3.6.0)

set (LISP_VERSION_MAJOR 0)
set (LISP_VERSION_MINOR 1)
set (LISP_VERSION_PATCH 0)

configure_file(
  "${PROJECT_SOURCE_DIR}/lisp_config.h.in"
  "${PROJECT_SOURCE_DIR}/lisp_config.h"
)

# Add the binary dir so that we file the generated `lisp_config.h` file.
include_directories("${PROJECT_BINARY_DIR}")

add_executable(lisp lisp.c)

install(TARGETS lisp DESTINATION bin)
