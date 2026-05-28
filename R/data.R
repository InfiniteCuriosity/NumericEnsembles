#' Concrete - This is the strength of concrete daa set originally posted on UCI
#'
#' @description
#' Concrete is the most important material in civil engineering. The concrete compressive strength is a highly nonlinear function of age and ingredients.
#'
#' @format Concrete
#' A data frame with 1030 rows and 9 columns:
#' \describe{
#'    \item{Cement}{ quantitative -- kg in a m3 mixture -- Input Variable}
#'    \item{Blast_Furnace_Slag}{quantitative -- kg in a m3 mixture -- Input Variable}
#'    \item{Fly_Ash}{ quantitative  -- kg in a m3 mixture -- Input Variable}
#'    \item{Water}{quantitative  -- kg in a m3 mixture -- Input Variable}
#'    \item{Superplasticizer}{quantitative -- kg in a m3 mixture -- Input Variable}
#'    \item{Coarse_Aggregate}{quantitative -- kg in a m3 mixture -- Input Variable}
#'    \item{Fine_Aggregate}{quantitative  -- kg in a m3 mixture -- Input Variable}
#'    \item{Age}{Day (1~365) -- Input Variable}
#'    \item{Strength}{quantitative -- MPa -- Output Variable}
#' }
#' @source https://archive.ics.uci.edu/dataset/165/concrete+compressive+strength
"Concrete"

#' Insurance - The data is from UCI
#'
#' @description
#' This dataset contains detailed information about insurance customers, including their age, sex, body mass index (BMI), number of children, smoking status and region. Having access to such valuable insights allows analysts to get a better view into customer behaviour and the factors that contribute to their insurance charges.

#' @format Insurance
#' A data frame with 1338 rows and 7 columns
#' Credit to Bob Wakefield
#' \describe{
#'    \item{Age}{The age of the customer. (Integer)}
#'    \item{Children}{The number of children the customer has. (Integer)}
#'    \item{Smoker}{Whether or not the customer is a smoker. (Boolean)}
#'    \item{Region}{The region the customer lives in. (String)}
#'    \item{Charges}{The insurance charges for the customer. (Float)}
#' }
#' @source https://www.kaggle.com/datasets/thedevastator/prediction-of-insurance-charges-using-age-gender
"Insurance"
