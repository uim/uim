# AlmaLinux 8: 3.26
# AlmaLinux 9: 3.26
# Debian bookworm: 3.25
# Ubuntu 22.04: 3.22
# Ubuntu 24.04: 3.28
cmake_minimum_required(VERSION 3.22)

project(uim-applet)

# use Extra CMake Modules (ECM) for common functionality
find_package(ECM REQUIRED NO_MODULE)
# needed by find_package(KF5Plasma) below.
list(APPEND CMAKE_MODULE_PATH ${ECM_MODULE_PATH} ${ECM_KDE_MODULE_DIR})

# Locate plasma_install_package macro
find_package(KF5Plasma REQUIRED)

# Add installation target ("make install")
plasma_install_package(plasmoid com.github.uim.status)

add_subdirectory(qtquick-plugin)
