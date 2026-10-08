# Message catalogs, built with the plain gettext tools.
#
# gettext's own po/Makefile.in.in insists on writing $(DOMAIN).pot and
# the .gmo files into the source directory.  This replacement keeps
# every generated file in the build directory, so that a read-only
# source tree (as in CI and "make distcheck") can be built.

DOMAIN = $(PACKAGE)
LINGUAS = $(shell sed -e '/^\#/d' $(srcdir)/LINGUAS)
GMOFILES = $(LINGUAS:%=%.gmo)

POTFILES = $(shell sed -e '/^\#/d' -e 's|^|$(top_srcdir)/|' \
	$(srcdir)/POTFILES.in)
EUCJP_POTFILES = $(shell sed -e '/^\#/d' -e 's|^|$(top_srcdir)/|' \
	$(srcdir)/POTFILES.eucjp.in)

EXTRA_DIST = LINGUAS POTFILES.in POTFILES.eucjp.in \
	meson.build update-po.sh \
	its/uim-gmenu.its its/uim-gmenu.loc \
	$(LINGUAS:%=%.po)
CLEANFILES = $(GMOFILES) $(DOMAIN).pot eucjp.pot
SUFFIXES = .po .gmo

all-local: $(GMOFILES)

.po.gmo:
	$(MSGFMT) -c --statistics --verbose -o $@ $<

# See update-po.sh for how uim.pot is generated.
UPDATE_PO = $(SHELL) $(srcdir)/update-po.sh
UPDATE_PO_ARGS = '$(abs_top_srcdir)' '$(abs_builddir)' '$(VERSION)' \
	'$(XGETTEXT)' '$(MSGMERGE)' '$(MSGATTRIB)'

$(DOMAIN).pot: $(srcdir)/update-po.sh \
	$(srcdir)/POTFILES.in $(POTFILES) \
	$(srcdir)/POTFILES.eucjp.in $(EUCJP_POTFILES)
	$(UPDATE_PO) pot $(UPDATE_PO_ARGS)

# Merge the current sources into the translations.  This is the only
# rule that writes into the source directory; run it explicitly.
update-po:
	$(UPDATE_PO) update-po $(UPDATE_PO_ARGS)
.PHONY: update-po

install-data-local: $(GMOFILES)
	@for lang in $(LINGUAS); do \
	  dir="$(DESTDIR)$(localedir)/$$lang/LC_MESSAGES"; \
	  echo " $(MKDIR_P) '$$dir'"; \
	  $(MKDIR_P) "$$dir" || exit 1; \
	  echo " $(INSTALL_DATA) $$lang.gmo '$$dir/$(DOMAIN).mo'"; \
	  $(INSTALL_DATA) $$lang.gmo "$$dir/$(DOMAIN).mo" || exit 1; \
	done

uninstall-local:
	@for lang in $(LINGUAS); do \
	  rm -f "$(DESTDIR)$(localedir)/$$lang/LC_MESSAGES/$(DOMAIN).mo"; \
	done
