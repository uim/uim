TXTS = design.txt spec.txt style.txt test-c.txt global-obj.txt \
        gc-protection.txt release.txt
ASCIIDOC_HTMLS = \
        design.html spec.html style.html test-c.html global-obj.html \
        gc-protection.html release.html

EXTRA_DIST = TODO

dist_doc_DATA  = $(TXTS)
dist_html_DATA = $(ASCIIDOC_HTMLS)

MAINTAINERCLEANFILES = $(ASCIIDOC_HTMLS)

.PHONY: clean-html

# xhtml11 backend is supported from AsciiDoc 7.0.0
.txt.html:
	if test -n "$(ASCIIDOC)"; then          \
	    $(ASCIIDOC) -b xhtml11 -n -o $@ $<; \
	fi

html: $(ASCIIDOC_HTMLS)
clean-html:
	rm -f $(ASCIIDOC_HTMLS)

dist-hook:
	@test -n "$(ASCIIDOC)"                                          \
	    || (echo 'asciidoc(1) not found: install AsciiDoc'; exit 1)
