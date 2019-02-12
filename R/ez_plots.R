
#' Easily Plot Differential Results
#'
#' This takes output from one of the reputational analysis models
#' (e.g., rep_analyses_auto) and returns a plot of differential
#' (correlational) parameters. It works with any combination of P1-,
#' P2-, and 3rd person Meta-perceptions. it returns a `ggplot()` object, and
#' can thus be edited with ggplot2 functions.
#'
#' The function can handle up to n exchangeable triads.
#' @param rep_model The results from one of the ReputationAnalyses
#' Models (e.g., rep_analyses_auto).
#' @param what The parameters you want in the plot Current options are
#' main and all. If what = "main", then just the 'main' model parameters are provided.
#' This will include, when avaiable, hearsay consensus, hearsay accuracy,
#' direct accuracy (P1-P1 agreement), P1 Meta-Accuracy, and P2 Meta-Accuracy.
#' @import tidyverse
#' @export
#' @examples data("rep_sim_data")
#'      # Consensus only Model
#'           agree_rep_consensus <- rep_analyses_auto(data = rep_sim_data,
#'                         p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                         p2_reports = c("B_C_agreeableness", "D_A_agreeableness"))
#'
#'           ez_differential_plot(agree_rep_consensus, what = "main")
#'
#'           ez_differential_plot(agree_rep_consensus, what = "all")
#'
#'        # Consensus & Accuracy
#'
#'           agree_rep_con_acc <- rep_analyses_auto(data = rep_sim_data,
#'                        p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                        p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
#'                        target_self = c("C_C_agreeableness", "A_A_agreeableness"))
#'
#'           ez_differential_plot(agree_rep_con_acc, what = "main")
#'
#'           ez_differential_plot(agree_rep_con_acc, what = "all")
#'
#'       # Consensus, Accuracy, 3rd  Person Meta
#'
#'          agree_rep_all <- rep_analyses_auto(data = rep_sim_data,
#'                        p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                        p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
#'                        target_self = c("C_C_agreeableness", "A_A_agreeableness"),
#'                        p1_meta = c("A_B_C_agree_meta", "C_D_A_agree_meta"),
#'                        p2_meta = c("B_A_C_agree_meta", "D_C_A_agree_meta"))
#'
#'          ez_differential_plot(agree_rep_all, what = "main")
#'
#'           ez_differential_plot(agree_rep_all, what = "all")
#'
#' @return The function returns an object of class \code{\link[ggplot2::ggplot()]{ggplot}}.
ez_differential_plot <- function(agree_rep_model, what = "main"){
  ez_tables(agree_rep_model, what = what)$differential_table %>%
    dplyr::mutate(parameter = stringr::str_to_title(parameter),
                  parameter = forcats::fct_relevel(parameter,
                                                   "P2 Meta-Accuracy",
                                                   "P1 Meta-Accuracy",
                                                   "Direct Accuracy",
                                                   "Hearsay Accuracy",
                                                   "Hearsay Consensus")) %>%
    ggplot2::ggplot(ggplot2::aes(x = parameter, y = r)) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = ci_lower, ymax = ci_upper)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = .5) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Differential Reputation Results",
                  x = NULL)}

