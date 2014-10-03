MARKDOWNS=$(wildcard */*.md)
DOTS=$(wildcard */*.dot)
PDFS=$(MARKDOWNS:md=pdf) $(DOTS:dot=pdf)
SVGS=$(DOTS:dot=svg)

all: $(PDFS) $(SVGS)

clean:
	rm -f $(PDFS) $(SVGS)

%.pdf: %.md
	pandoc -V geometry:margin=1in -V papersize:"a4paper" $< -o $@

%.pdf: %.dot
	dot -Tpdf -o $@ $<

%.svg: %.dot
	dot -Tsvg -o $@ $<
