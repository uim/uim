INCLUDES = $(all_includes)
METASOURCES = AUTO

kde_module_LTLIBRARIES = kcm_uimqtconfig.la

noinst_HEADERS = kcmuimqtconfigmodule.h

kcm_uimqtconfig_la_LDFLAGS = -module -no-undefined $(KDE_PLUGIN) $(all_libraries)
kcm_uimqtconfig_la_SOURCES = kcmuimqtconfigmodule.cpp
kcm_uimqtconfig_la_LIBADD = $(LIB_KUTILS)

xdg_apps_DATA = uimqtconfig.desktop
