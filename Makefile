SOURCES=$(shell find src lib test bench -name '*.hs' -type f)

HOTHASKTAGS=$(shell which hothasktags 2>/dev/null)
CTAGS=$(if $(HOTHASKTAGS),$(HOTHASKTAGS),/bin/false)

STYLISHHASKELL=$(shell which stylish-haskell 2>/dev/null)
STYLISH=$(if $(STYLISHHASKELL),$(STYLISHHASKELL),/bin/false)

HLINT=$(shell which hlint 2>/dev/null)
LINT=$(if $(HLINT),$(HLINT),/bin/false)

.PHONY: all format lint

all: lint format tags

lint: HLint.hs $(SOURCES)
	@$(LINT) -h $< $(filter-out $<,$^)

format: .stylish-haskell.yaml $(SOURCES)
	@$(STYLISH) -c $< -i $(filter-out $<,$^)

tags: $(SOURCES)
	@if [ "$(HOTHASKTAGS)" ] ; then /bin/echo -e "CTAGS\ttags" ; fi
	@$(CTAGS) $^ > tags $(REDIRECT)
