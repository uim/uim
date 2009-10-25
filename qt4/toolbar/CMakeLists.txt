project(plasma-uim)

find_package(KDE4 REQUIRED)
include(KDE4Defaults)

add_definitions(${QT_DEFINITIONS} ${KDE4_DEFINITIONS})
include_directories(${KDE4_INCLUDES}
        ${CMAKE_SOURCE_DIR}/../.. ${CMAKE_SOURCE_DIR}/../../uim
        ${CMAKE_SOURCE_DIR}/../../replace ${CMAKE_SOURCE_DIR}/..)

set(uim_SRCS
        common-quimhelpertoolbar.cpp
        common-uimstateindicator.cpp
        applet-kde4.cpp)
 
kde4_add_plugin(plasma_applet_uim ${uim_SRCS})
target_link_libraries(plasma_applet_uim ${KDE4_PLASMA_LIBS} ${KDE4_KDEUI_LIBS}
        ${CMAKE_SOURCE_DIR}/../../uim/.libs/libuim.so
        ${CMAKE_SOURCE_DIR}/../../uim/.libs/libuim-scm.so
        -luim -luim-scm)
 
install(TARGETS plasma_applet_uim DESTINATION ${PLUGIN_INSTALL_DIR})
install(FILES plasma-applet-uim.desktop DESTINATION ${SERVICES_INSTALL_DIR})
