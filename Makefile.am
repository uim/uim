AUTOMAKE_OPTIONS = foreign
ACLOCAL_AMFLAGS = -I m4

SUBDIRS = m4 doc replace sigscheme uim scm test test2 \
          gtk2 gtk3 qt3 notify
SUBDIRS += qt4
SUBDIRS += xim fep emacs po pixmaps examples tables byeoru-data

EXTRA_DIST = RELNOTE ChangeLog.old autogen.sh make-dist.sh \
	uim.pc.in uim.desktop uim.spec.in uim.spec \
	intltool-extract.in intltool-merge.in intltool-update.in

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = uim.pc

desktopdir = $(datadir)/applications
desktop_in_files = uim.desktop.in
desktop_DATA = $(desktop_in_files:.desktop.in=.desktop)
@INTLTOOL_DESKTOP_RULE@

# $(distdir) does work as a part of $(RELEASE_URL) when configured as a
# subpackage.
DIST_NAME = $(PACKAGE)-$(VERSION)$(RELEASE_SUFFIX)
UIM_REPOSITORY = http://uim.googlecode.com/svn
TAGS_REPOSITORY = $(UIM_REPOSITORY)/tags
#RELEASE_URL     = $(UIM_REPOSITORY)/trunk
RELEASE_URL     = $(TAGS_REPOSITORY)/$(DIST_NAME)
DIST_SUM_LIST = $(DIST_NAME).sum

DISTCLEANFILES = uim.pc uim.desktop \
		 intltool-extract intltool-merge intltool-update \
		 gnuify-changelog.pl $(DIST_SUM_LIST)

MAINTAINERCLEANFILES = \
    aclocal.m4 ltmain.sh depcomp missing install-sh \
    intltool-extract.in intltool-merge.in intltool-update.in

.PHONY: FORCE sum

FORCE:

ChangeLog: FORCE
	svn export $(UIM_REPOSITORY)/trunk/tools/gnuify-changelog.pl
	svn log $(RELEASE_URL) | ./gnuify-changelog.pl > $@
	@if test -s $@; then                                    \
	    echo "ChangeLog updated successfully.";             \
	else                                                    \
	    echo "ChangeLog: Failed to update ChangeLog.";      \
	    $(RM) -f $@;                                        \
	    false;                                              \
	fi

sum:
	$(MD5) $(DIST_ARCHIVES) >$(DIST_SUM_LIST)
	$(SHA1) $(DIST_ARCHIVES) >>$(DIST_SUM_LIST)

distclean-local:
	if test '$(top_srcdir)' != '$(top_builddir)'; then      \
	    rm -f ChangeLog;                                    \
	fi
