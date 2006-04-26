TXTS = design.txt spec.txt style.txt test-c.txt global-obj.txt \
        gc-protection.txt
ASCIIDOC_HTMLS = design.html spec.html global-obj.html gc-protection.html

BUILT_DIST = $(ASCIIDOC_HTMLS)
EXTRA_DIST = $(BUILT_DIST) TODO $(TXTS)

all: html

# xhtml11 backend is supported from AsciiDoc 7.0.0
.txt.html:
	if test -n "$(ASCIIDOC)"; then          \
	    $(ASCIIDOC) -b xhtml11 -n -o $@ $<; \
	fi

html: $(ASCIIDOC_HTMLS)

dist-hook:
	@test -n "$(ASCIIDOC)"                                          \
	    || (echo 'asciidoc(1) not found: install AsciiDoc'; exit 1)
