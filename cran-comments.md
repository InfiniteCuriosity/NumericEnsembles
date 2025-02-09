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
