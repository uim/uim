INCLUDES = $(all_includes)

noinst_HEADERS = quimhelpertoolbar.h uimstateindicator.h
METASOURCES = AUTO

# convinience
noinst_LTLIBRARIES = libcommon.la
libcommon_la_SOURCES = quimhelpertoolbar.cpp uimstateindicator.cpp
libcommon_la_LIBADD = $(LIB_UIM)

if KASUMI
libcommon_la_CPPFLAGS = -DUSE_KASUMI
endif
