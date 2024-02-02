
DONE:
- [X] Swap Survival Curve / Death Distribution plots. They're wrongly exchanged.
- [X] Example file should just be age, deaths and exposures.
- [X] Add a pre-loaded button to upload an already existing dataset.
- [X] Add an "info" button to explain that exposures is actually population
- [X] When "Show Advanced Options" is shown, switch the text to "Hide".
- [X] Make sure plotly legend has correct spelling in all diagnostics
- [X] Round the empirical mx numbers in the tool tip to 8 decimals.
- [X] Clean the text in the tool tip.
- [X] Change the label of the slider "Ages to include in model fit".
- [X] Put a lower bound of open age group at 70
- [X] Make sure there's a button "reset" button to reset the default values of the app.
- [X] Check error when open age group is lower than age extrapolation mortality. Can't reproduce the error.
- [X] Add table of diagnostics in results. Should render latex formula
- [X] Fix diagnostics text fitting
- [X] Add conditional text for diagnostics table for single ages
- [X] Show summary life table statistics
- [X] Add download all button

Tim DONE:

- [X] In Diagnostics plots Empirical Mx, patrick was saying he wante to see the age group in the tool tip legend instead of the single number. I think Tim said he would provide this as a column.
- [X] Tim needs to add sex and other groups in the final CSV file that is downloaded. Coordinate with them.
- [X] Add label of grey lines in "Death Distributons" + "Surival Curve".
- [X] Add legend to what the dotted vertical line means in the plot "Mortality Rate Comparison".
- [X] Add legend to what the red line means in the plot "Mortality Rate Comparison".
- [X] Tim should provide text for what to put in "Ages to include in model fit".
- [X] Tim should provide some text explaining what the numbers mean in the diagnostics table
- [X] Tim should provide some text explaining the color cells according to low / mid / high values for the indicators.
- [X] How do we deal with 0 in the diagnostics plot of deaths?
- [X] Implement data download correctly
- [X] Add three tabs of plots in results. Titles for the tabs: Mortality Rates, Survival Curve and Death Distribution.
- [X] On the welcome page where we show the desired data, we should add one more column "Sex". This is a new development we have decided on it now.



TODO:
- [X] Fix footer on different screen sizes. Not at the bottom of the screen in Patrick's screen.
- [X] Black line on diagnostics changes to read sometimes
- [X] If you alter the sample csv to have a final age of 75+ then Extrap. Jump-off Age should be the same?
- [X] You should also download the entire life table when clicking on Download All.
- [X] Can you add the entire life table as a new tab on the analysis tabs?
- [ ] Add subtitles to plotly
- [X] Provide the age label in the analysis plots as well
- [X] Use the more informative message from the error checks
- [ ] Add a download plot / data from the diagnostics tab
- [ ] Add smoothing UI page
- [ ] Online Demographic Analysis (rename the URL to this)
- [ ] Patrick said one idea to pursue is for the diagnostics table to perform diagnostics on each of the plots in a way that everytime you switch between tabs, the table is updated with new diagnostics.
- [ ] Apps time out very quickly.

Version 2.0:

- [ ] Do we have tools to read in non-csv files? Can we doctor our read_data() function, which is more flexible in this regard? Or ar ewe really bound to csv?
- [ ] You should be able to provide sex in the CSV or not and it should be smart enough to use it if it's provided.
- [ ] Add all possible groups specified in the analysis (male, country, groups, etc..) in the subtitle of the analysis results.
- [ ] Report document generated on the fly
- [ ] I also think that the desired data should show 2 examples. For "The best" we should add Sex and nMx columns and "Minimum", the latter having only Deaths, Exposures, Age, and Sex
- [ ] Add log label on Empirical Mx diagnostics plot. Update: Currently not possible, see commented code inside nmx plot in diagnostics.
- [ ] When user provides sex in the excel file, the input widget for gender should take the values from there.
- [ ] Remove sex argument if sex is not provided
