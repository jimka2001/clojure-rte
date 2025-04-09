
all: pdf

SRC_DIR  := ${PWD}



ifeq "$(origin EXTENSION)" "undefined"
EXTENSION := ltx
endif



# Text dependencies:
LTX_FILES := $(wildcard $(SRC_DIR)/*.ltx)
TEX_FILES := $(wildcard $(SRC_DIR)/*.tex)
CLS_FILES := $(wildcard $(SRC_DIR)/*.cls)
STY_FILES := $(wildcard $(SRC_DIR)/*.sty)
DOT_DIR   := $(SRC_DIR)/../../dot
DOT_FILES := $(wildcard $(DOT_DIR)/*.dot)
IMAGE_DIR := $(SRC_DIR)/../../images
GNU_IMAGE_DIR := $(SRC_DIR)/../../gnu-images
GNUPLOT_DIR := $(SRC_DIR)/../../gnuplot
INCLUDES_DIR := ${SRC_DIR}/../../includes
GNU_FILES := $(wildcard $(GNUPLOT_DIR)/*.gnu)
PDF_FILES := $(wildcard $(IMAGE_DIR)/*.pdf)
PNG_FILES := $(wildcard $(IMAGE_DIR)/*.png)
AUTOGEN_DIR  := $(SRC_DIR)/../../autogen
DAT_FILES    := $(wildcard $(AUTOGEN_DIR)/*.dat)
LTXDAT_FILES := $(wildcard $(AUTOGEN_DIR)/*.ltxdat)
# Generated graphics files:
GEN_PDF_FILES := $(patsubst %, $(IMAGE_DIR)/%, $(patsubst %.dot, %.pdf, $(notdir $(DOT_FILES))))
GEN_PNG_FILES := $(patsubst %, $(IMAGE_DIR)/%, $(patsubst %.gnu, %.png, $(notdir $(GNU_FILES))))
GEN_GNU_PDF_FILES := $(patsubst %, $(GNU_IMAGE_DIR)/%, $(patsubst %.gnu, %.pdf, $(notdir $(GNU_FILES))))

LATEXMK := latexmk --pdf -pvc
%LATEXMK := latexmk --pdf

CLEAN_FILES += *.aux *.toc *.lof *.lot *.lox *.log	\
	       *.cb *.cb2 *.fls *.fdb_latexmk		\
	       *.bbl *.blg *.sta \
	       *.vrb *.sav *.nav *.out *.snm

DEPENDENCIES += $(CLS_FILES) $(STY_FILES)              \
		$(PDF_FILES) $(GEN_PDF_FILES) $(GEN_GNU_PDF_FILES) $(GEN_PNG_FILES) $(PNG_FILES)

ARTICLE = reclojure.2025

pdf: ${ARTICLE}.slides.pdf

deps: $(DEPENDENCIES)

${ARTICLE}.slides.pdf: $(DEPENDENCIES) ${SRC_DIR}/${ARTICLE}.slides.ltx ${SRC_DIR}/${ARTICLE}.slides.includes/* 
	(cd ${SRC_DIR}/${ARTICLE}.slides.dot ; make)
	TEXINPUTS=$(TEXINPUTS):${SRC_DIR}/../../autogen:${SRC_DIR}/${ARTICLE}.slides.includes:${SRC_DIR}/${ARTICLE}.slides.images:$(IMAGE_DIR): \
	  $(LATEXMK) $(SRC_DIR)/${ARTICLE}.slides.ltx

google-drive: ${ARTICLE}.pdf ${ARTICLE}.slides.pdf
	([ -d ~/Google\ Drive/R+I/. ] && cp -f ${ARTICLE}.pdf ~/Google\ Drive/R+I/.) || true
	([ -d ~/Google\ Drive/R+I/. ] && cp -f ${ARTICLE}.slides.pdf ~/Google\ Drive/R+I/.) || true

clean:
	rm -fv $(CLEAN_FILES) $(EXTRA_CLEAN_FILES) ${ARTICLE}.slides.pdf ${ARTICLE}.pdf

$(IMAGE_DIR)/%.pdf: $(DOT_DIR)/%.dot
	dot "$<" -Tpdf -o "$@"

$(IMAGE_DIR)/%.png: $(GNUPLOT_DIR)/%.gnu
	gnuplot  -e "set terminal png" "$<" > "$@"

$(GNU_IMAGE_DIR)/%.pdf: $(GNUPLOT_DIR)/%.gnu
	gnuplot  -e "set terminal pdf" "$<" > "$@"

.PHONY: all pdf view pdfview clean distclean
.SUFFIXES: .ltx .tex .fig .dia .svg .jpg .eps .pdf .png

### pdf.mak ends here
