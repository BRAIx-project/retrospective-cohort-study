Rscript := /opt/R/4.2.1/bin/Rscript


setup: setup-dev setup-test

setup-test:
	$(Rscript) setup.R --TEST=TRUE

setup-dev:
	$(Rscript) setup.R --TEST=FALSE


setup-threshold: replacement-threshold bandpass-threshold

replacement-threshold: reader-perf output/log/dev_reader_weighted_mean.rds
	$(Rscript) scripts/find_threshold_replacement.R

bandpass-threshold: reader-perf 
	$(Rscript) scripts/find_threshold_bandpass.R

reader-perf: output/model_pred-dev output/reader_perf-dev
	$(Rscript) scripts/compute_reader_performance.R


run-simulation: output/model_pred output/reader_perf output/accession_df output/log/bandpass_threshold.rds output/log/replacement_threshold.rds
	$(Rscript) scripts/run_simulation.R
