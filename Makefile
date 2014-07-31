SOURCES=$(shell find src tests -name '*.hs' -type f)

HOTHASKTAGS=$(shell which hothasktags 2>/dev/null)
CTAGS=$(if $(HOTHASKTAGS),$(HOTHASKTAGS),/bin/false)

STYLISHHASKELL=$(shell which stylish-haskell 2>/dev/null)
STYLISH=$(if $(STYLISHHASKELL),$(STYLISHHASKELL),/bin/false)

all: format tags

.PHONY: all test clean

tags: $(SOURCES)
	@if [ "$(HOTHASKTAGS)" ] ; then /bin/echo -e "CTAGS\ttags" ; fi
	@$(CTAGS) $^ > tags $(REDIRECT)

# Note that the config file *MUST* be the first dependency!
format: .stylish-haskell.yaml $(SOURCES)
	@if [ ! -z "$(STYLISH)" ] ; then /bin/echo -e "FORMAT" ; fi
	@$(STYLISH) -c $< -i $(filter-out $<,$^)

clean:
	@/bin/echo -e "CLEAN"
	@cabal clean >/dev/null
	@rm -f tags

test:
	@/bin/echo -e "TEST"
	cabal test
