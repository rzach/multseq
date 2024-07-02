EXDIR = ../examples

EXPL := $(shell find $(EXDIR) -name ex_*.pl)
EXPDF := $(EXPL:.pl=.pdf)
MSVER := $(shell git describe)

.FORCE:
.PHONY: .FORCE

all: index.html examples manual

examples: $(EXPDF)

manual: ../doc/multseq.pdf ../doc/multseq.html

../doc/multseq.tex: ../doc/multseq.md .FORCE
	pandoc --template=$< </dev/null -V ms_version="$(MSVER)" --metadata pagetitle="MUltseq $(MSVER) Manual" | pandoc --standalone --listings -H latex-listings.tex --toc -N --toc-depth=2 --output=$@

../doc/multseq.html: ../doc/multseq.md
	pandoc --template=$< </dev/null -V ms_version="$(MSVER)" --metadata pagetitle="MUltseq $(MSVER) Manual" | pandoc --standalone --toc -N --toc-depth=1 -c https://www.logic.at/user/themes/logic/assets/css/main.css -B before.html -A after.html -H header.html --output=$@

../doc/multseq.pdf: ../doc/multseq.tex
	pdflatex -output-directory $(@D) $< 
	pdflatex -output-directory $(@D) $< 

$(EXDIR)/%.tex: $(EXDIR)/%.pl ../multseq/*.pl
	cd $(EXDIR); \
	prolog -s "$(notdir $*)" -g "halt."

$(EXDIR)/%.pdf: $(EXDIR)/%.tex
	cd $(EXDIR); \
	pdflatex -interaction=nonstopmode $* || true

index.html: index.md website.md ../doc/multseq.bib .FORCE
	pandoc --template=index.md  -V ms_version="$(MSVER)" \
	--metadata pagetitle="website" website.md | \
	pandoc -s --citeproc --bibliography=../doc/multseq.bib --metadata link-citations -c https://www.logic.at/user/themes/logic/assets/css/main.css -B before.html -A after.html -H header.html --output=index.html

update:
	scp -o PubkeyAuthentication=false index.html ../examples/ex_*.pdf ../doc/multseq.html ../doc/multseq.pdf rick.logic.at:web/www/multseq/