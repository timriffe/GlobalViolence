# GlobalViolence

Joint work with Jose Manuel Aburto, Vanessa di Lego, Orsola Torrisi, Ridhi Kashyap, and Tim Riffe
Presently in initial stages

To get started. Go to the R folder. Scripts are intended to be run in a sequence

- `00_Install_Packages.R` takes care of package installation for dependencies. This can be sourced.

- `01_Functions.R` loads some custom functions. Other custom functions are defined in the scripts where they are used. This can be sourced

- `02_DownloadData.R` should be stepped through **interactively**, and you will need to interact with the GBD web app, following instructions in the code. You may need ca 8+Gb of RAM for this to run without further chunking down of larger files. This will populate `Data/Inputs`.

- `03_DataPrep_GBD.R` does some more harmonization for the GBD data.

- `04_DataPrep_Graduate.R` splits data into single ages, presently using the PCLM method from `ungroup`. 

- `05_DataPrep_Closeout.R` uses the `MortalityLaws` package to fit gamma Gompertz models to ages 65-90, replacing all-cause mortality in ages 65+ with fitted rates. 

- `06_Uncertainty.R` calculates several lifetable quantities. This uses `DemoDecomp`, as well as some other packages that may need to be installed from github. This populates `Data/Results`, used variously elsewhere. This step may take a long time to execute

- `07_Decomposition.R` decomposes differences in a selection of lifetable quantities between GPI high-violence countries and a synthetic low-violence standard. This uses `DemoDecomp`, as well as some other packages that may need to be installed from github. `Relationships.R` can also be run at this stage, independently.


