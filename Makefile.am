AUTOMAKE_OPTIONS = foreign
SUBDIRS = m4 doc scm uim xim helper po gtk tables test fep examples pixmaps qt
EXTRA_DIST = README.ja INSTALL.ja test.sh.in uim.spec.in \
	intltool-extract.in intltool-merge.in intltool-update.in \
	uim.pc.in

DISTCLEANFILES = uim.pc intltool-extract\
                 intltool-merge intltool-update po/.intltool-merge-cache

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = uim.pc

desktopdir = $(datadir)/applications
desktop_DATA = uim.desktop


noinst_PROGRAMS = $(gtk_entry) $(qedittest)

if GTK2
gtk_entry = gtk-entry
gtk_entry_SOURCES = entry.c
gtk_entry_LDADD = @GTK2_LIBS@
gtk_entry_CFLAGS = @GTK2_CFLAGS@
else
endif

if QT
qedittest = qedittest
qedittest_SOURCES = qedittest.cpp
qedittest_CXXFLAGS  = $(UIM_QT_CXXFLAGS)
qedittest_LDFLAGS = $(UIM_QT_LDFLAGS)
endif

ACLOCAL_AMFLAGS = -I m4

ChangeLog:
	@cd $(top_srcdir); \
	svn log | ./gnuify-changelog.pl > ChangeLog.tmpl; \
	if test -s ChangeLog.tmpl; then \
	    mv ChangeLog.tmpl ChangeLog; \
	    echo "ChangeLog updated successfully."; \
	else \
	    echo "ChangeLog: Failed to update ChangeLog."; \
	    $(RM) -f ChangeLog.tmpl; \
	fi

.PHONY: ChangeLog releasetest-all-flag-enabled releasetest-all-flag-disabled each-releasetest releasetest

releasetest-all-flag-enabled: RELEASETEST_CONFIGURE_FLAGS= --enable-debug \
	 --enable-fep --enable-dict --with-m17nlib --with-canna \
	 --with-gtk2 --with-gnome2
releasetest-all-flag-enabled: each-releasetest

releasetest-all-flag-disabled: RELEASETEST_CONFIGURE_FLAGS= --disable-debug \
	--disable-fep --disable-sumika	--disable-dict --without-m17nlib \
	--without-canna --without-gtk2  --without-gnome2
releasetest-all-flag-disabled: each-releasetest

each-releasetest:
	case '$(DIST_ARCHIVES)' in \
	*.tar.gz*) \
	  GZIP=$(GZIP_ENV) gunzip -c $(distdir).tar.gz | $(am__untar) ;;\
	*.tar.bz2*) \
	  bunzip2 -c $(distdir).tar.bz2 | $(am__untar) ;;\
	esac
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
