SUBDIRS = doc m4 tools misc src test test-c bench experimental
EXTRA_DIST = \
        TODO autogen.sh \
        compare-scm.sh runbench.sh runtest.sh runtest-tail-rec.sh

# FIXME: move to appropriate directory
EXTRA_DIST += slib.scm

UIM_REPOSITORY = http://anonsvn.freedesktop.org/svn/uim
SSCM_REPOSITORY = $(UIM_REPOSITORY)/sigscheme

ChangeLog: FORCE
	svn export $(UIM_REPOSITORY)/trunk/tools/gnuify-changelog.pl \
	    $(top_srcdir)/tools/gnuify-changelog.pl
	svn up
	svn log | $(top_srcdir)/tools/gnuify-changelog.pl > $@
	@test -s $@ && echo 'ChangeLog updated successfully.'

.PHONY: FORCE

FORCE:
