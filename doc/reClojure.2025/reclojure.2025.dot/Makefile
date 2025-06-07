DOT_DIR   := .
DOT_FILES := $(wildcard $(DOT_DIR)/*.dot)
IMAGE_DIR := ../reclojure.2025.images
PDF_FILES := $(wildcard $(IMAGE_DIR)/*.pdf)

GEN_PDF_FILES := $(patsubst %, $(IMAGE_DIR)/%, $(patsubst %.dot, %.pdf, $(notdir $(DOT_FILES))))

all: ${GEN_PDF_FILES}


$(IMAGE_DIR)/%.pdf: $(DOT_DIR)/%.dot
	dot "$<" -Tpdf -o "$@"
