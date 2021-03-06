
#' Easily Plot Differential Results
#'
#' This takes output from one of the reputation models
#' (e.g., rep_analyses_auto) and returns a plot of differential
#' (correlational) parameters. It works with any combination of P1-,
#' P2-, and 3rd person Meta-perceptions. it returns a `ggplot()` object, and
#' can thus be edited with ggplot2 functions.
#'
#' The function can handle up to n exchangeable triads.
#' @param rep_model The results from one of the ReputationModelR
#' Models (e.g., rep_analyses_auto).
#' @param what The parameters you want in the plot Current options are
#' main and all. If what = "main", then just the 'main' model parameters are provided.
#' This will include, when avaiable, hearsay consensus, hearsay accuracy,
#' direct accuracy (P1-P1 agreement), P1 Meta-Accuracy, and P2 Meta-Accuracy.

#' @export
#' @import magrittr stringr lavaan
#' @seealso \code{\link[ggplot2]{ggplot}}
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
#' @return The function returns an object of class \code{\link[ggplot2:ggplot2-package]{ggplot}}.
ez_differential_plot <- function(rep_model, what = "main"){
  param_tbl <- ez_tables(rep_model, what = what)$differential_table
    if(length(unique(param_tbl$parameter)) == 1){
      param_tbl <- param_tbl %>%
        dplyr::mutate(parameter = stringr::str_to_title(.data$parameter),
                      parameter = forcats::fct_relevel(.data$parameter,
                                                       "Hearsay Consensus"))
    }
  if(length(unique(param_tbl$parameter)) == 2){
    param_tbl <- param_tbl %>%
      dplyr::mutate(parameter = stringr::str_to_title(.data$parameter),
                    parameter = forcats::fct_relevel(.data$parameter,
                                                     "Direct Accuracy",
                                                     "Hearsay Accuracy",
                                                     "Hearsay Consensus"))
  }
  if(length(unique(param_tbl$parameter)) == 5){
  param_tbl <- param_tbl %>%
    dplyr::mutate(parameter = stringr::str_to_title(.data$parameter),
                  parameter = forcats::fct_relevel(.data$parameter,
                                                   "P2 Meta-Accuracy",
                                                   "P1 Meta-Accuracy",
                                                   "Direct Accuracy",
                                                   "Hearsay Accuracy",
                                                   "Hearsay Consensus"))
  }

  if(!("group_label" %in% colnames(ez_tables(rep_model)$differential_table))){
    ggplot2::ggplot(param_tbl, ggplot2::aes(x = .data$parameter, y = .data$r)) +
      ggplot2::geom_pointrange(ggplot2::aes(ymin = .data$ci_lower, ymax = .data$ci_upper)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = .5) +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Differential Reputation Results",
                    x = NULL)
  }
  else{
    ggplot2::ggplot(param_tbl, ggplot2::aes(x = .data$parameter, y = .data$r, shape = forcats::fct_rev(.data$group_label))) +
      ggplot2::geom_pointrange(ggplot2::aes(ymin = .data$ci_lower, ymax = .data$ci_upper),
                               position = ggplot2::position_dodge(width = .5)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = .5) +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Differential Reputation Results with Group Moderator",
                    x = NULL,
                    shape = "group") +
      ggplot2::guides(shape = ggplot2::guide_legend(reverse = TRUE))
  }
    }

