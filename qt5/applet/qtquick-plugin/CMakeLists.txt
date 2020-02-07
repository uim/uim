set(CMAKE_AUTOMOC ON)
find_package(Qt5 REQUIRED COMPONENTS Quick Core Qml)

add_library(
    uim-qtquick-plugin
    SHARED

    ${CMAKE_CURRENT_LIST_DIR}/plugin.cpp
    ${CMAKE_CURRENT_LIST_DIR}/uim-status.cpp
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

install(
    FILES
        qmldir
    DESTINATION 
        ${QML_INSTALL_DIR}/org/kde/private/uim
)