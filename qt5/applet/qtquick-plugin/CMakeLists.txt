set(CMAKE_AUTOMOC ON)
find_package(Qt5 REQUIRED COMPONENTS Quick Core Qml)

add_library(
    uim-qtquick-plugin
    SHARED

    ${CMAKE_CURRENT_LIST_DIR}/plugin.h
    ${CMAKE_CURRENT_LIST_DIR}/uim-status.h

    ${CMAKE_CURRENT_LIST_DIR}/plugin.cpp
    ${CMAKE_CURRENT_LIST_DIR}/uim-status.cpp

    # It's not important to include 'qmldir' or 'plugin.qmltypes' here,
    # but it will make IDEs (such as Qt Creator) to include them in project tree
    ${CMAKE_CURRENT_LIST_DIR}/qmldir
)

include_directories(
    ${CMAKE_SOURCE_DIR}/../.. ${CMAKE_SOURCE_DIR}/../../uim
    ${CMAKE_SOURCE_DIR}/../../replace ${CMAKE_SOURCE_DIR}/..
    ${CMAKE_BINARY_DIR}/../../../uim
)

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
        ${CMAKE_BINARY_DIR}/../../../uim/.libs/libuim.so
        ${CMAKE_BINARY_DIR}/../../../uim/.libs/libuim-scm.so
        -luim -luim-scm
        Qt5::Core
        Qt5::Quick
        Qt5::Qml
)

install(
    TARGETS 
        uim-qtquick-plugin
    DESTINATION 
        ${QML_INSTALL_DIR}/org/kde/private/uim
)
