% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{New_Boston}
\alias{New_Boston}
\title{NewBoston—These are only the five rows c(1:3, 210:211) from Boston Housing data set. This can be used as new data, and the Boston_housing data set as the original. The numeric function will return predictions on the new data.}
\format{
An object of class \code{data.frame} with 5 rows and 14 columns.
}
\source{
\url{https://www.law.berkeley.edu/files/Hedonic.PDF}
}
\usage{
New_Boston
}
\description{
This is the first five rows of the Boston housing data set, which have been removed from the Boston data set included here. It is otherwise identical to the Boston data set.

\describe{
\item{crim}{Crime rate by town. Original data in 1970 FBI data}
\item{zn}{Proportion of a town's residential land zoned for lots greater than 25,000 square feet}
\item{indus}{Proportional non-retail business per town}
\item{chas}{Captures the amenities of a riverside location and thus should be positive}
\item{nox}{Nitrogen oxygen concentrations in part per hundred million}
\item{rm}{Average number of rooms in owner units}
\item{age}{Proportion of owner units built prior to 1940}
\item{dis}{Weighted distances to five employment centers in the Boston region}
\item{rad}{Index of accessibility to radial highways}
\item{tax}{Full property value tax rate ($/$10,000)}
\item{ptratio}{Pupil-teacher ratio by town school district}
\item{black}{Black proportion of population}
\item{lstat}{Proportion of population that is lower status (proportion of adults without some high school education and proportion of male workers classified as laborers)}
\item{medv}{Median value of owner occupied homes, from the 1970 United States census}
}
}
\keyword{datasets}
