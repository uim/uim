INCLUDES = $(all_includes)
METASOURCES = AUTO

kde_module_LTLIBRARIES = kcm_uimconfig.la

noinst_HEADERS = kcmuimconfigmodule.h

kcm_uimconfig_la_LDFLAGS = -module -no-undefined $(KDE_PLUGIN) $(all_libraries)
kcm_uimconfig_la_SOURCES = kcmuimconfigmodule.cpp
kcm_uimconfig_la_LIBADD = $(LIB_KUTILS)

xdg_apps_DATA = uimconfig.desktop
