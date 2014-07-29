all: format tags

.PHONY: all test clean

SOURCES=$(shell find src -name '*.hs' -type f) \
	$(shell find tests -name '*.hs' -type f)

HOTHASKTAGS=$(shell which hothasktags 2>/dev/null)
CTAGS=$(if $(HOTHASKTAGS),$(HOTHASKTAGS),/bin/false)

tags: $(SOURCES)
	@if [ "$(HOTHASKTAGS)" ] ; then /bin/echo -e "CTAGS\ttags" ; fi
	@$(CTAGS) $^ > tags $(REDIRECT)

STYLISH=$(shell which stylish-haskell 2>/dev/null)

# Note that the config file *MUST* be the first dependency!
format: .stylish-haskell.yml $(SOURCES)
	@if [ ! -z "$(STYLISH)" ] ; then /bin/echo -e "FORMAT" ; fi
	@$(STYLISH) -c $< -i $(filter-out $<,$^)

clean:
	@/bin/echo -e "CLEAN"
	@cabal clean >/dev/null
	@rm -f tags

test:
	cabal test
