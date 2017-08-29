To plot results using diagnostics_USA.R, follow these steps:

1. Run the model verification queries (batch_queries_USA.xml) on the output of at least one scenario.
2. Place resulting csv file in the output\gcam_diagnostics\gcam_data folder (or a sub-folder).
3. Open the diagnostics.R script. If R was not already open, this will by default set the working directory to the correct path. 
   Otherwise, set the working directory to output\gcam_diagnostics using the setwd() function or select from a drop-down menu.
4. Ensure that the script is reading the correct data file (csv batch file) from the correct locations (line 56).
5. Stipulate which run from the batch file is the "base scenario" (BASE_SCENARIO_NAME, line 57). 
6. Set the OUTPUT_DIR where the figures will be placed (line 79). By default this is "figures".
7. Rename (shorten) the scenario names if you'd like (lines 131-133, 281-283, 308-310, 338-340, 366-368, depending on which results are plotted).
8. Comment out lines for results that do not need to be plotted.  
	a. Running the full diagnostic tool will produce 301 charts.
9. Source the script (source("diagnostics_USA.R"). The charts will be printed into the designated output folder in the working directory.  
	a. The tool will continue to print into this folder, so it's helpful to move or delete old charts before re-running the script.
	b. Alternatively, a new destination folder can be specified (line 79).
