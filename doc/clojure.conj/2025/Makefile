# The variable LATEX_OPTS was suggested by chatgpt.
# to include the -pvc option, run as follows
# make LATEX_OPTS=-pvc
LATEX_OPTS ?=
LATEXMK := latexmk --pdf $(LATEX_OPTS)

DOT_DIR := ./dots
DOTS := $(wildcard $(DOT_DIR)/*.dot)
GEN_DOT_PDFS := $(DOTS:.dot=.pdf)

GNU_DIR := ./gnu
GNUS := $(wildcard $(GNU_DIR)/*.gnu)
GEN_GNU_PDFS := $(GNUS:.gnu=.pdf)

newton.2025.clojureconj.slides.pdf : newton.2025.clojureconj.slides.ltx includes/*.tex $(GEN_DOT_PDFS) $(GEN_GNU_PDFS)
	TEXINPUTS="$(TEXINPUTS):.:./includes:./gnu:./dots" \
	${LATEXMK} ${FLAGS} newton.2025.clojureconj.slides.ltx

$(DOT_DIR)/%.pdf: $(DOT_DIR)/%.dot
	dot -Tpdf $< -o $@

$(GNU_DIR)/%.pdf: $(GNU_DIR)/%.gnu
	LC_CTYPE=en_US.UTF-8 gnuplot -e "set terminal pdf ; set output '$@' ; load '$<'"

clean:
	rm -rf newton.2025.clojureconj.slides.pdf *.sta *.log *.fls *.aux *.fdb_latexmk *.snm *.out *.nav *.toc

WEBPAGE_REPO := git@gitlab.lre.epita.fr:lre/lre-web/members/jim-newton.git

webpage: newton.2025.clojureconj.slides.pdf
	tmp=$$(mktemp -d) && \
	git clone --depth 1 --quiet $(WEBPAGE_REPO) $$tmp && \
	cp newton.2025.clojureconj.slides.pdf $$tmp/ && \
	cd $$tmp && \
	git add newton.2025.clojureconj.slides.pdf && \
	(git diff --cached --quiet || git commit --quiet -m "Update newton.2025.clojureconj.slides.pdf") && \
	git push --quiet ; \
	status=$$? ; rm -rf $$tmp ; exit $$status
