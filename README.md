# GlobalViolence

Joint work with Tim Riffe, Jose Manuel Aburto, and Vanessa di Lego
Presently in initial stages

To get started. Go to the R folder.

1) `DownloadData.R` should be stepped through one piece at a time. You may need ca 8+Gb of RAM for this to run without further chunking down of larger files. This will populate `Data/Inputs`

2) `DataPrep_XYZ.R` does some more harmonization for each of the sources, where `XYZ` stands for the source acronym. Of these, only `GBD` has been worked out so far. Even this still has remaining steps.

3) `DataPrep_Graduate.R` splits data into single ages, presently using the PCLM method from `ungroup`. Presently only set up for GBD. This has been modified from earlier versions and should be rerun if necessary.

4) `DataPrep_Closeout.R` uses the `MortalityLaws` package to fit gamma Gompertz models to ages 65-90, replacing all-cause mortality in ages 65+ with fitted rates. 

5) `Uncertainty.R` calculates several lifetable quantities, presently only for GBD data. This uses `DemoDecomp`, as well as some other packages that may need to be installed from github. This populates `Data/Results`, used variously elsewhere.

6) `Comparison.R` decomposes differences in a selection of lifetable quantities between GPI high-violence countries and a synthetic low-violence standard. This uses `DemoDecomp`, as well as some other packages that may need to be installed from github. `Relationships.R` can also be run at this stage, independently.



