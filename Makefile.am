ACLOCAL_AMFLAGS = -I m4.generated -I m4

SUBDIRS = m4 doc replace sigscheme uim scm test test2 \
          gtk3 gtk4 qt3 tqt qt4 qt5 qt6 notify
SUBDIRS += xim wayland fep emacs pixmaps examples tables byeoru-data data
if MOZC
  SUBDIRS += mozc
endif
if USE_NLS
  SUBDIRS += po
endif

EXTRA_DIST = RELNOTE Rakefile autogen.sh setup.sh \
	uim.pc.in uim.desktop

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = uim.pc

desktopdir = $(datadir)/applications
desktop_in_files = uim.desktop.in
desktop_DATA = $(desktop_in_files:.desktop.in=.desktop)
if USE_NLS
  po_files = $(wildcard $(top_srcdir)/po/*.po)
  %.desktop: %.desktop.in $(po_files)
	$(MSGFMT) --desktop --template=$< -d $(top_srcdir)/po -o $@
else
  %.desktop: %.desktop.in ; cp $< $@
endif

DIST_NAME = $(PACKAGE)-$(VERSION)
#RELEASE_TAG     = master
RELEASE_TAG     = $(DIST_NAME)
DIST_SUM_LIST = $(DIST_NAME).sum

DISTCLEANFILES = uim.pc uim.desktop \
		 $(DIST_SUM_LIST)

MAINTAINERCLEANFILES = \
    aclocal.m4 ltmain.sh depcomp missing install-sh

.PHONY: FORCE sum

FORCE:

sum:
	$(MD5) $(DIST_ARCHIVES) | $(SED) 's/^/MD5:  /' >$(DIST_SUM_LIST)
	$(SHA1) $(DIST_ARCHIVES) | $(SED) 's/^/SHA1: /' >>$(DIST_SUM_LIST)
	$(SHA256) $(DIST_ARCHIVES) | $(SED) 's/^/SHA256: /' >>$(DIST_SUM_LIST)

tag:
	cd "$(srcdir)" && git tag -a -m "$(VERSION) has been released!!!" $(VERSION)
	cd "$(srcdir)" && git push --tags
