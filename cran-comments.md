## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
The note states, " Adding so many packages to the search path is excessive and importing
  selectively is preferable."
Each package is used every time the function is run. Removing any package will reduce the accuracy of the results and/or the results returned to the user.


* Added barchart to provide results of the Komogrov-Smirnoff test.
The test determines if two samples came from the same population. The two samples tested are the predictions for each of the 40 models, and the true values.
The null hypothesis for this test is the two samples are from the same popluation.
For example, this will test if the Ensemble BayesRNN predictions, and the true holdout values come from the same population. Same for each of the 40 models.
Two dotted blue lines are given to guide the user as a starting point, but the user may select any p-values they wish: p = 0.10 and p = 0.05.
If the p-value for a model (such as SVM) is below the desired value, the test indicates there is sufficient evidence to reject the null hypothesis. Otherwise there is not sufficient evidence to reject the null hypothesis.

* Added a small function to remove NAs from the ensemble.
The ensemble virtually never has NAs, but this is just in case.

* Please reduce the length of the title to less than 65 characters.
Done

* The Description field is intended to be a (one paragraph) description of what the package does and why it may be useful.
* Please add more details about the package functionality and implemented methods in your Description text.
Done, thank you for the note, the new description has more the motivation for the ensemble, and how it can be used to solve real problems

*Please add small executable examples in your Rd-files to illustrate the use of the exported function but also enable automatic testing.
All my attempts to create an example that runs in five seconds or less have failed. This package totally automates the entire analytics process for
numeric data, and takes more than five seconds even with the smallest data set.

* You write information messages to the console that cannot be easily suppressed.
Thank you for the note, changed all print commands to message.

* Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()).
* This is not allowed by CRAN policies. Please omit any default path in writing functions. In your examples/vignettes/tests you can write to tempdir().
This took a very large amount of work, but is fixed. Nothing writes to the user's home filespace now, it all writes to tempdir1.

* 0.9.2 updated the vignette so the code runs correctly, and corrected /R/NumericEnsembles.R code, so that set.seed runs correctly. (there was a line of code that was randomizing the data, that has been fixed).

* 0.10.0, updated to show that NumericEnsembles runs 32 models now (used to run 40). The package runs in much less time than previously. Therefore the Vignette was updated to show the greatly improved speed.
