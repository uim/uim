project(knotify4)

find_package(KDE4 REQUIRED)

include_directories(${KDE4_INCLUDES} ${CMAKE_SOURCE_DIR}/../uim
        ${CMAKE_SOURCE_DIR}/../replace)

set(uimnotify_knotify4_SRCS
        uim-knotify4.cpp)
 
kde4_add_library(uimnotify-knotify4 SHARED ${uimnotify_knotify4_SRCS})
target_link_libraries(uimnotify-knotify4 ${KDE4_KDEUI_LIBS})
install(TARGETS uimnotify-knotify4 DESTINATION ${UIMNOTIFY_PLUGINDIR})
