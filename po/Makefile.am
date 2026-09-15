# Message catalogs, built with the plain gettext tools.
#
# gettext's own po/Makefile.in.in insists on writing $(DOMAIN).pot and
# the .gmo files into the source directory.  This replacement keeps
# every generated file in the build directory, so that a read-only
# source tree (as in CI and "make distcheck") can be built.

DOMAIN = $(PACKAGE)
LINGUAS = $(shell sed -e '/^\#/d' $(srcdir)/LINGUAS)
GMOFILES = $(LINGUAS:%=%.gmo)

# --keyword=Description: an extra key to extract from
# gtk3/toolbar/UimApplet.panel-applet.desktop.in.in.
XGETTEXT_KEYWORDS = --keyword=_ --keyword=N_ --keyword=NC_:1c,2 \
	--keyword=Description
# GETTEXTDATADIRS lets xgettext find its/*.loc, which describe how to
# extract strings from our GMenu XML files.
XGETTEXT_RUN = GETTEXTDATADIRS=$(srcdir) $(XGETTEXT) \
	--default-domain=$(DOMAIN) --add-comments=TRANSLATORS: \
	$(XGETTEXT_KEYWORDS) \
	--package-name=$(PACKAGE) --package-version=$(VERSION) \
	--copyright-holder='uim Developers' \
	--msgid-bugs-address='uim-en@googlegroups.com'

POTFILES = $(shell sed -e '/^\#/d' -e 's|^|$(top_srcdir)/|' \
	$(srcdir)/POTFILES.in)
EUCJP_POTFILES = $(shell sed -e '/^\#/d' -e 's|^|$(top_srcdir)/|' \
	$(srcdir)/POTFILES.eucjp.in)

EXTRA_DIST = LINGUAS POTFILES.in POTFILES.eucjp.in \
	its/uim-gmenu.its its/uim-gmenu.loc \
	$(LINGUAS:%=%.po)
CLEANFILES = $(GMOFILES) $(DOMAIN).pot eucjp.pot
SUFFIXES = .po .gmo

all-local: $(GMOFILES)

.po.gmo:
	$(MSGFMT) -c --statistics --verbose -o $@ $<

# xgettext accepts a single --from-code per run, but some scm/*.scm
# files are still EUC-JP.  Those are listed in POTFILES.eucjp.in and
# extracted separately into eucjp.pot, which is then fed into the main
# UTF-8 run as an additional input.  Remove all of this once every
# source file is UTF-8.
eucjp.pot: $(srcdir)/POTFILES.eucjp.in $(EUCJP_POTFILES)
	$(XGETTEXT_RUN) --from-code=EUC-JP --directory=$(top_srcdir) \
	  --files-from=$(srcdir)/POTFILES.eucjp.in --output=$@

$(DOMAIN).pot: $(srcdir)/POTFILES.in $(POTFILES) eucjp.pot
	$(XGETTEXT_RUN) --from-code=UTF-8 --directory=$(top_srcdir) \
	  --files-from=$(srcdir)/POTFILES.in $(abs_builddir)/eucjp.pot \
	  --output=$@

# Merge the current sources into the translations.  This is the only
# rule that writes into the source directory; run it explicitly.
update-po: $(DOMAIN).pot
	@for lang in $(LINGUAS); do \
	  echo "$(MSGMERGE) --update --backup=none $(srcdir)/$$lang.po $(DOMAIN).pot"; \
	  $(MSGMERGE) --update --backup=none $(srcdir)/$$lang.po $(DOMAIN).pot \
	    || exit 1; \
	done
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
