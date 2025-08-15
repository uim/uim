ACLOCAL_AMFLAGS = -I m4.generated -I m4

SUBDIRS = m4 doc replace sigscheme uim scm test test2 \
          gtk2 gtk3 qt3 qt4 qt5 qt6 notify
SUBDIRS += xim fep emacs pixmaps examples tables byeoru-data data
if USE_NLS
  SUBDIRS += po
endif

EXTRA_DIST = RELNOTE Rakefile autogen.sh \
	uim.pc.in uim.desktop \
	intltool-extract.in intltool-merge.in intltool-update.in

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = uim.pc

desktopdir = $(datadir)/applications
desktop_in_files = uim.desktop.in
desktop_DATA = $(desktop_in_files:.desktop.in=.desktop)
if USE_NLS
  @INTLTOOL_DESKTOP_RULE@
else
  %.desktop: %.desktop.in ; cp $< $@
endif

DIST_NAME = $(PACKAGE)-$(VERSION)
#RELEASE_TAG     = master
RELEASE_TAG     = $(DIST_NAME)
DIST_SUM_LIST = $(DIST_NAME).sum

DISTCLEANFILES = uim.pc uim.desktop \
		 intltool-extract intltool-merge intltool-update \
		 $(DIST_SUM_LIST)

MAINTAINERCLEANFILES = \
    aclocal.m4 ltmain.sh depcomp missing install-sh \
    intltool-extract.in intltool-merge.in intltool-update.in

.PHONY: FORCE sum

FORCE:

sum:
	$(MD5) $(DIST_ARCHIVES) | $(SED) 's/^/MD5:  /' >$(DIST_SUM_LIST)
	$(SHA1) $(DIST_ARCHIVES) | $(SED) 's/^/SHA1: /' >>$(DIST_SUM_LIST)
	$(SHA256) $(DIST_ARCHIVES) | $(SED) 's/^/SHA256: /' >>$(DIST_SUM_LIST)

tag:
	cd "$(srcdir)" && git tag -a -m "$(VERSION) has been released!!!" $(VERSION)
	cd "$(srcdir)" && git push --tags
