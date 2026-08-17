.DEFAULT_GOAL := all

RSCRIPT ?= Rscript
QUARTO ?= quarto

RSCRIPT_FILE := code/so_prediction_rev.R
QUARTO_FILE := reviewer_comments.qmd
UNWANTED_PLOT := Rplots.pdf

.PHONY: all analysis report clean

all: report

analysis:
	@trap '$(RM) $(UNWANTED_PLOT)' EXIT; \
	$(RSCRIPT) $(RSCRIPT_FILE)

report: analysis
	@trap '$(RM) $(UNWANTED_PLOT)' EXIT; \
	$(QUARTO) render $(QUARTO_FILE)

clean:
	$(RM) $(UNWANTED_PLOT)
