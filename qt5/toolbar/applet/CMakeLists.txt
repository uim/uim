# set minimum CMake version (req'd for CMake 3.0 or later)
cmake_minimum_required(VERSION 2.8.12)

# use Extra CMake Modules (ECM) for common functionality
find_package(ECM REQUIRED NO_MODULE)
# needed by find_package(KF5Plasma) below.
set(CMAKE_MODULE_PATH ${ECM_MODULE_PATH} ${ECM_KDE_MODULE_DIR} ${CMAKE_MODULE_PATH})

# Locate plasma_install_package macro
find_package(KF5Plasma REQUIRED)

# Add installation target ("make install")
plasma_install_package(. org.uim.status)

add_subdirectory(uim-plugin)
