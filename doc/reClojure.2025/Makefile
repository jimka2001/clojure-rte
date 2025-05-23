
all: pdf

SRC_DIR  := ${PWD}

ARTICLE = reclojure.2025

# Text dependencies:
LTX_FILES := $(wildcard $(SRC_DIR)/*.ltx)
TEX_FILES := $(wildcard $(SRC_DIR)/*.tex)
CLS_FILES := $(wildcard $(SRC_DIR)/*.cls)
STY_FILES := $(wildcard $(SRC_DIR)/*.sty)
DOT_DIR   := $(SRC_DIR)/${ARTICLE}.dot
DOT_FILES := $(wildcard $(DOT_DIR)/*.dot)
IMAGE_DIR := $(SRC_DIR)/${ARTICLE}.images
INCLUDES_DIR := ${SRC_DIR}/${ARTICLE}.includes
PDF_FILES := $(wildcard $(IMAGE_DIR)/*.pdf)
PNG_FILES := $(wildcard $(IMAGE_DIR)/*.png)

%LATEXMK := latexmk --pdf -pvc
LATEXMK := latexmk --pdf

CLEAN_FILES += *.aux *.toc *.lof *.lot *.lox *.log	\
	       *.cb *.cb2 *.fls *.fdb_latexmk		\
	       *.bbl *.blg *.sta \
	       *.vrb *.sav *.nav *.out *.snm

DEPENDENCIES += $(CLS_FILES) $(STY_FILES)              \
		$(PDF_FILES) $(PNG_FILES)

pdf: dots ${ARTICLE}.slides.pdf ${ARTICLE}.demo.pdf 

install: ${ARTICLE}.slides.pdf ${ARTICLE}.demo.pdf
	cp ${ARTICLE}.slides.pdf ${ARTICLE}.demo.pdf ..

dots:
	cd ${DOT_DIR} ; make

deps: $(DEPENDENCIES)

${ARTICLE}.slides.pdf: dots $(DEPENDENCIES) ${SRC_DIR}/${ARTICLE}.slides.ltx ${INCLUDES_DIR}/* 
	TEXINPUTS=$(TEXINPUTS):${SRC_DIR}:${INCLUDES_DIR}:${IMAGE_DIR} \
	  $(LATEXMK) $(SRC_DIR)/${ARTICLE}.slides.ltx

${ARTICLE}.demo.pdf: dots $(DEPENDENCIES) ${SRC_DIR}/${ARTICLE}.demo.ltx ${INCLUDES_DIR}/* 
	TEXINPUTS=$(TEXINPUTS):${INCLUDES_DIR}:${IMAGE_DIR} \
	  $(LATEXMK) $(SRC_DIR)/${ARTICLE}.demo.ltx

clean:
	rm -fv $(CLEAN_FILES) $(EXTRA_CLEAN_FILES) ${ARTICLE}.slides.pdf ${ARTICLE}.pdf

$(IMAGE_DIR)/%.pdf: $(DOT_DIR)/%.dot
	dot "$<" -Tpdf -o "$@"

.PHONY: all pdf view pdfview clean distclean
.SUFFIXES: .ltx .tex .fig .dia .svg .jpg .eps .pdf .png

### pdf.mak ends here
