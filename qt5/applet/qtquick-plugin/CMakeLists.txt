# AlmaLinux 8: 3.26
# AlmaLinux 9: 3.26
# Debian bookworm: 3.25
# Ubuntu 22.04: 3.22
# Ubuntu 24.04: 3.28
cmake_minimum_required(VERSION 3.22)

set(CMAKE_AUTOMOC ON)
find_package(Qt5 REQUIRED COMPONENTS Quick Core Qml)

add_library(
  uim-qtquick-plugin
  SHARED

  plugin.h
  uim-socket.h

  plugin.cpp
  uim-socket.cpp

  # It's not important to include 'qmldir' or 'plugin.qmltypes' here,
  # but it will make IDEs (such as Qt Creator) to include them in project tree
  qmldir
  )

include_directories(
  ${CMAKE_BINARY_DIR}/../../uim
  ${CMAKE_SOURCE_DIR}/../..
  ${CMAKE_SOURCE_DIR}/../../uim
  ${CMAKE_SOURCE_DIR}/../../replace)

target_compile_features(
  uim-qtquick-plugin
  PRIVATE
  cxx_auto_type
  cxx_generic_lambdas
  )

target_compile_options(
  uim-qtquick-plugin
  PRIVATE
  -Wall
  -Wextra
  -Wpedantic
  )

target_link_libraries(
  uim-qtquick-plugin
  PRIVATE
  ${CMAKE_BINARY_DIR}/../../uim/.libs/libuim.so
  ${CMAKE_BINARY_DIR}/../../uim/.libs/libuim-scm.so
  Qt5::Core
  Qt5::Quick
  Qt5::Qml
  )

install(
  TARGETS uim-qtquick-plugin
  DESTINATION ${QML_INSTALL_DIR}/com/github/uim
  )

install(
  FILES qmldir
  DESTINATION ${QML_INSTALL_DIR}/com/github/uim
  )
