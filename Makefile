.DEFAULT_GOAL := all
.ONESHELL:
SHELL := /bin/bash
.SHELLFLAGS := -ec

RSCRIPT ?= Rscript
QUARTO ?= quarto

RSCRIPT_FILE := code/so_prediction_rev.R
QUARTO_FILE  := revision/reviewer_comments.qmd
UNWANTED_PLOT := Rplots.pdf
VO2MAX_MODES := none normal forced

.PHONY: all analysis report report-only clean purge

all: analysis report

analysis:
	@trap '$(RM) $(UNWANTED_PLOT)' EXIT
	for mode in $(VO2MAX_MODES); do \
		$(RSCRIPT) $(RSCRIPT_FILE) --vo2max=$$mode; \
	done

report: analysis report-only

report-only:
	@trap '$(RM) $(UNWANTED_PLOT)' EXIT
	$(QUARTO) render $(QUARTO_FILE)

clean:
	$(RM) $(UNWANTED_PLOT)
	$(RM) -rf tmp/
	$(RM) -rf /dev/shm/2026_st_brasier_heatpub

purge: clean
	$(RM) -rf output
	$(RM) $(QUARTO_FILE:.qmd=.html)
