#'
#' Simulated Reputation data
#'
#' Simulated data to be used for examples with the ReputationAnalyses package.
#' The data were simulate based on results from Costello & Srivastava (in prep).
#' We used the mvtnorm package to simulate a dataset based on the cariance-covariance matrix
#' and vector of means for variables across the 2 reputation studies. At present, it includes
#' ratings on Agreeableness and P2 self-reported perspective taking.
#'
#' The data, being simulated, should be treated solely as example data.
#'
#' @docType data
#'
#' @usage data(rep_sim_data)
#'
#' @format A dataframe with 300 rows and 13 variables, simulated
#'  to be similar to data used in Costello & Srivastava (in prep):
#' \describe{
#'   \item{sess_id}{session IDs, identifier marking sessions}
#'   \item{C_C_agreeableness}{target self-report (C(C)) on agreeableness}
#'   \item{A_A_agreeableness}{target self-report (A(A)) on agreeableness}
#'   \item{A_C_agreeableness}{P1 (A(C)) report on agreeableness}
#'   \item{C_A_agreeableness}{P1 (C(A)) report on agreeableness}
#'   \item{B_C_agreeableness}{P2 (B(C)) report on agreeableness}
#'   \item{D_A_agreeableness}{P2 (D(A)) report on agreeableness}
#'   \item{A_B_C_agree_meta}{P1 meta-perception (A(B(C)) on agreeableness}
#'   \item{C_D_A_agree_meta}{P1 meta-perception (C(D(A)) on agreeableness}
#'   \item{B_A_C_agree_meta}{P2 meta-perception (B(A(C)) on agreeableness}
#'   \item{D_C_A_agree_meta}{P2 meta-perception (D(C(A)) on agreeableness}
#'   \item{B_iri_perspective}{P2 self-reported perspective taking}
#'   \item{D_iri_perspective}{P2 self-reported perspective taking}
#' }
#'
#' @keywords datasets
"rep_sim_data"
