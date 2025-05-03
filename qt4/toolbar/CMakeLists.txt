# AlmaLinux 8: 3.26
# AlmaLinux 9: 3.26
# Debian bookworm: 3.25
# Ubuntu 22.04: 3.22
# Ubuntu 24.04: 3.28
cmake_minimum_required(VERSION 3.22)

project(plasma-uim)
set(CMAKE_VERBOSE_MAKEFILE on)

find_package(KDE4 REQUIRED)
include(KDE4Defaults)

add_definitions(${QT_DEFINITIONS} ${KDE4_DEFINITIONS} -DPLASMA_APPLET_UIM)
include_directories(${KDE4_INCLUDES}
        ${CMAKE_SOURCE_DIR}/../.. ${CMAKE_SOURCE_DIR}/../../uim
        ${CMAKE_SOURCE_DIR}/../../replace ${CMAKE_SOURCE_DIR}/..
        ${CMAKE_BINARY_DIR}/../../../uim)

set(uim_SRCS
        common-quimhelpertoolbar.cpp
        common-uimstateindicator.cpp
        applet-kde4.cpp)

# Remove "-Wconversion" from CXXFLAGS to avoid lots of warnings
# from Qt4 header files
string(REGEX REPLACE "-Wconversion" ""
        CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} $ENV{CPPFLAGS}")
 
kde4_add_plugin(plasma_applet_uim ${uim_SRCS})
target_link_libraries(plasma_applet_uim ${KDE4_PLASMA_LIBS} ${KDE4_KDEUI_LIBS}
        ${CMAKE_BINARY_DIR}/../../../uim/.libs/libuim.so
        ${CMAKE_BINARY_DIR}/../../../uim/.libs/libuim-scm.so
        -luim -luim-scm)
 
install(TARGETS plasma_applet_uim DESTINATION ${PLUGIN_INSTALL_DIR})
install(FILES ${CMAKE_BINARY_DIR}/../plasma-applet-uim.desktop DESTINATION ${SERVICES_INSTALL_DIR})
