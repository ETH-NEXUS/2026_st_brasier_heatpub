.DEFAULT_GOAL := all
.ONESHELL:
SHELL := /bin/bash
.SHELLFLAGS := -ec

RSCRIPT ?= Rscript
QUARTO ?= quarto

PREDICTION_SCRIPT := code/so_prediction_rev.R
REVISION_REPORT := revision/reviewer_comments.qmd
UNWANTED_PLOT := Rplots.pdf
VO2MAX_MODES := none normal forced

.PHONY: all setup prepare descriptives figures prediction validate \
	revision-report clean purge

all: validate

# Install the package versions of renv.lock in a library specific to the
# project. Run once; not a dependency of the other targets. The restore must
# run in a new R session, which loads the project library through .Rprofile.
setup:
	$(RSCRIPT) -e 'renv::activate()'
	$(RSCRIPT) -e 'renv::restore(prompt = FALSE)'

prepare:
	@$(RSCRIPT) code/prepare_data.R

descriptives: prepare
	@$(RSCRIPT) code/descriptives.R

figures: prepare
	@trap '$(RM) $(UNWANTED_PLOT)' EXIT
	$(RSCRIPT) code/participant_flow.R
	$(RSCRIPT) code/hr_slope_vo2max.R
	$(RSCRIPT) code/supplement_timeseries.R

prediction: prepare
	@trap '$(RM) $(UNWANTED_PLOT)' EXIT
	for mode in $(VO2MAX_MODES); do \
		$(RSCRIPT) $(PREDICTION_SCRIPT) --vo2max=$$mode; \
	done

validate: descriptives figures prediction
	@$(RSCRIPT) code/validate_article.R

# Historical record of the revision. Not part of the reproduction.
revision-report:
	@trap '$(RM) $(UNWANTED_PLOT)' EXIT
	$(QUARTO) render $(REVISION_REPORT)

clean:
	$(RM) $(UNWANTED_PLOT)
	$(RM) -rf tmp/

purge: clean
	$(RM) -rf output
	$(RM) $(REVISION_REPORT:.qmd=.html)
