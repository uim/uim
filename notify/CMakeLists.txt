# AlmaLinux 8: 3.26
# AlmaLinux 9: 3.26
# Debian bookworm: 3.25
# Ubuntu 22.04: 3.22
# Ubuntu 24.04: 3.28
cmake_minimum_required(VERSION 3.22)

project(knotify4)

find_package(KDE4 REQUIRED)

include_directories(${KDE4_INCLUDES} ${CMAKE_SOURCE_DIR}/..
        ${CMAKE_SOURCE_DIR}/../uim ${CMAKE_SOURCE_DIR}/../replace
        ${CMAKE_SOURCE_DIR}/../qt4 ${CMAKE_BINARY_DIR}/../../uim)

set(uimnotify_knotify4_SRCS
        uim-knotify4.cpp)
 
kde4_add_library(uimnotify-knotify4 SHARED ${uimnotify_knotify4_SRCS})
target_link_libraries(uimnotify-knotify4 ${KDE4_KDEUI_LIBS})
install(TARGETS uimnotify-knotify4 DESTINATION ${UIMNOTIFY_PLUGINDIR})
