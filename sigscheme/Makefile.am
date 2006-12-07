SUBDIRS = doc m4 tools include src lib test test-c2 bench

UIM_REPOSITORY = http://anonsvn.freedesktop.org/svn/uim
SSCM_REPOSITORY = $(UIM_REPOSITORY)/branches/r5rs/sigscheme
TAGS_REPOSITORY = $(UIM_REPOSITORY)/tags
DIST_SUM_LIST = $(PACKAGE)-$(VERSION).md5

EXTRA_DIST = \
        TODO QALog autogen.sh \
        compare-scm.sh runbench.sh runtest.sh runtest-tail-rec.sh

DISTCLEANFILES = tools/gnuify-changelog.pl $(DIST_SUM_LIST)

.PHONY: FORCE sum
FORCE:

ChangeLog: FORCE
	svn export $(UIM_REPOSITORY)/trunk/tools/gnuify-changelog.pl \
	    tools/gnuify-changelog.pl
	svn log | tools/gnuify-changelog.pl > $@
	@test -s $@ && echo 'ChangeLog updated successfully.'

sum: FORCE
	$(MD5) $(DIST_ARCHIVES) >$(DIST_SUM_LIST)

# To generate ChangeLog for a release properly, a source tree for official
# release must not be obtained by 'svn export' but 'svn co'. See
# doc/release.txt for further information.
dist-hook:
	svn info | $(EGREP) -q "^URL: .+/svn/uim/tags/sigscheme-" \
	    || (echo "error: not an official release. checkout tagged one"; exit 1)

# for 'svn log'
distcheck-hook:
	cp -Rp $(top_srcdir)/.svn $(distdir)/_build
	touch $(distdir)/_build/.svn/prepared-for-distcheck

distclean-local:
	if test '$(top_srcdir)' != '$(top_builddir)'; then  \
	    rm -f ChangeLog;                                    \
	fi
	if test -f .svn/prepared-for-distcheck; then    \
	    rm -rf .svn;                                \
	fi
