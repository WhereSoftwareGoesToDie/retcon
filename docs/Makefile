MARKDOWNS=$(wildcard */*.md)
PDFS=$(MARKDOWNS:md=pdf)

all: $(PDFS)
clean:
	rm -f $(PDFS)

%.pdf: %.md
	pandoc -V geometry:margin=1in -V papersize:"a4paper" $< -o $@
