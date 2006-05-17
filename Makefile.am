AUTOMAKE_OPTIONS = foreign
SUBDIRS = m4 doc replace uim scm gtk qt xim helper po test fep emacs \
	examples pixmaps
EXTRA_DIST = RELNOTE uim.spec.in \
	intltool-extract.in intltool-merge.in intltool-update.in \
	uim.pc.in ChangeLog.old uim.desktop autogen.sh

DISTCLEANFILES = uim.pc intltool-extract intltool-merge intltool-update po/.intltool-merge-cache

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = uim.pc

desktopdir = $(datadir)/applications
desktop_in_files = uim.desktop.in
desktop_DATA = $(desktop_in_files:.desktop.in=.desktop)
@INTLTOOL_DESKTOP_RULE@

ACLOCAL_AMFLAGS = -I m4

ChangeLog: FORCE
	@cd $(top_srcdir); \
	svn log | $(top_srcdir)/tools/gnuify-changelog.pl > ChangeLog.tmpl; \
	if test -s ChangeLog.tmpl; then \
	    mv ChangeLog.tmpl ChangeLog; \
	    echo "ChangeLog updated successfully."; \
	else \
	    echo "ChangeLog: Failed to update ChangeLog."; \
	    $(RM) -f ChangeLog.tmpl; \
	fi

.PHONY: FORCE releasetest-all-flag-enabled releasetest-all-flag-disabled each-releasetest releasetest

FORCE:

releasetest-all-flag-enabled: RELEASETEST_CONFIGURE_FLAGS= --enable-debug \
	--enable-fep --enable-emacs --enable-dict \
	--with-m17nlib --with-canna --with-gtk2 --with-gnome2 --with-qt \
	--with-qt-immodule --with-libedit
releasetest-all-flag-enabled: each-releasetest

releasetest-all-flag-disabled: RELEASETEST_CONFIGURE_FLAGS= --disable-debug \
	--disable-fep --disable-sumika --disable-emacs --disable-dict \
	--without-m17nlib --without-canna --without-gtk2  --without-gnome2 \
	--without-qt --without-qt-immodule --without-libedit
releasetest-all-flag-disabled: each-releasetest

each-releasetest:
	$(am__remove_distdir)
	GZIP=$(GZIP_ENV) gunzip -c $(distdir).tar.gz | $(AMTAR) xf -
	chmod -R a-w $(distdir); chmod a+w $(distdir)
	mkdir $(distdir)/_build
	mkdir $(distdir)/_inst
	chmod a-w $(distdir)
	dc_install_base=`$(am__cd) $(distdir)/_inst && pwd | sed -e 's,^[^:\\/]:[\\/],/,'` \
	  && dc_destdir="$${TMPDIR-/tmp}/am-dc-$$$$/" \
	  && cd $(distdir)/_build \
	  && ../configure --srcdir=.. --prefix="$$dc_install_base" \
	    $(RELEASETEST_CONFIGURE_FLAGS) \
	  && $(MAKE) $(AM_MAKEFLAGS) \
	  && $(MAKE) $(AM_MAKEFLAGS) dvi \
	  && $(MAKE) $(AM_MAKEFLAGS) check \
	  && $(MAKE) $(AM_MAKEFLAGS) install \
	  && $(MAKE) $(AM_MAKEFLAGS) installcheck \
	  && $(MAKE) $(AM_MAKEFLAGS) uninstall \
	  && $(MAKE) $(AM_MAKEFLAGS) distuninstallcheck_dir="$$dc_install_base" \
	        distuninstallcheck \
	  && chmod -R a-w "$$dc_install_base" \
	  && ({ \
	       (cd ../.. && umask 077 && mkdir "$$dc_destdir") \
	       && $(MAKE) $(AM_MAKEFLAGS) DESTDIR="$$dc_destdir" install \
	       && $(MAKE) $(AM_MAKEFLAGS) DESTDIR="$$dc_destdir" uninstall \
	       && $(MAKE) $(AM_MAKEFLAGS) DESTDIR="$$dc_destdir" \
	            distuninstallcheck_dir="$$dc_destdir" distuninstallcheck; \
	      } || { rm -rf "$$dc_destdir"; exit 1; }) \
	  && rm -rf "$$dc_destdir" \
	  && rm -rf $(DIST_ARCHIVES) \
	  && $(MAKE) $(AM_MAKEFLAGS) distcleancheck

releasetest: dist
	$(MAKE) releasetest-all-flag-enabled
	$(am__remove_distdir)
	$(MAKE) releasetest-all-flag-disabled
	$(am__remove_distdir)
	@(echo "$(distdir) archives ready for distribution: "; \
	  list='$(DIST_ARCHIVES)'; for i in $$list; do echo $$i; done) | \
	  sed -e '1{h;s/./=/g;p;x;}' -e '$${p;x;}'

dist-hook: uim.spec
	cp uim.spec $(distdir)
