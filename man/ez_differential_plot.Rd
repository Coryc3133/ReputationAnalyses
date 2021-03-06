% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ez_plots.R
\name{ez_differential_plot}
\alias{ez_differential_plot}
\title{Easily Plot Differential Results}
\usage{
ez_differential_plot(rep_model, what = "main")
}
\arguments{
\item{rep_model}{The results from one of the ReputationModelR
Models (e.g., rep_analyses_auto).}

\item{what}{The parameters you want in the plot Current options are
main and all. If what = "main", then just the 'main' model parameters are provided.
This will include, when avaiable, hearsay consensus, hearsay accuracy,
direct accuracy (P1-P1 agreement), P1 Meta-Accuracy, and P2 Meta-Accuracy.}
}
\value{
The function returns an object of class \code{\link[ggplot2:ggplot2-package]{ggplot}}.
}
\description{
This takes output from one of the reputation models
(e.g., rep_analyses_auto) and returns a plot of differential
(correlational) parameters. It works with any combination of P1-,
P2-, and 3rd person Meta-perceptions. it returns a `ggplot()` object, and
can thus be edited with ggplot2 functions.
}
\details{
The function can handle up to n exchangeable triads.
}
\examples{
data("rep_sim_data")
     # Consensus only Model
          agree_rep_consensus <- rep_analyses_auto(data = rep_sim_data,
                        p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
                        p2_reports = c("B_C_agreeableness", "D_A_agreeableness"))

          ez_differential_plot(agree_rep_consensus, what = "main")

          ez_differential_plot(agree_rep_consensus, what = "all")

       # Consensus & Accuracy

          agree_rep_con_acc <- rep_analyses_auto(data = rep_sim_data,
                       p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
                       p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
                       target_self = c("C_C_agreeableness", "A_A_agreeableness"))

          ez_differential_plot(agree_rep_con_acc, what = "main")

          ez_differential_plot(agree_rep_con_acc, what = "all")

      # Consensus, Accuracy, 3rd  Person Meta

         agree_rep_all <- rep_analyses_auto(data = rep_sim_data,
                       p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
                       p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
                       target_self = c("C_C_agreeableness", "A_A_agreeableness"),
                       p1_meta = c("A_B_C_agree_meta", "C_D_A_agree_meta"),
                       p2_meta = c("B_A_C_agree_meta", "D_C_A_agree_meta"))

         ez_differential_plot(agree_rep_all, what = "main")

          ez_differential_plot(agree_rep_all, what = "all")

}
\seealso{
\code{\link[ggplot2]{ggplot}}
}
