# set the include path for X, qt and KDE
INCLUDES = $(all_includes)

# these are the headers for your project
noinst_HEADERS = candidatewindow.h qhelpermanager.h quiminputcontext.h quiminputcontext_with_slave.h

# let automoc handle all of the meta source files (moc)
METASOURCES = AUTO

#messages: rc.cpp
#	$(XGETTEXT) *.cpp -o $(podir)/qt_immodule.pot

#KDE_ICON = qt_immodule

#########################################################################
# APPLICATION SECTION
#########################################################################
# this is the program that gets installed.  it's name is used for all
# of the other Makefile.am variables

pkglib_LTLIBRARIES =  libquiminputcontextplugin.la
pkglibdir=${INPUTMETHODSDIR}

# the application source, library search path, and link libraries
libquiminputcontextplugin_la_SOURCES = plugin.cpp candidatewindow.cpp qhelpermanager.cpp quiminputcontext.cpp quiminputcontext_with_slave.cpp
libquiminputcontextplugin_la_LDFLAGS = $(all_libraries)
libquiminputcontextplugin_la_LIBADD = -luim

#KDE_OPTIONS = qtonly

MAINTAINERCLEANFILES = Makefile.in
