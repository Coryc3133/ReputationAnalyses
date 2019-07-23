#' Easily Table Differential Results
#'
#' This takes output from one of the ReputationModelR model functions
#' (e.g., rep_analyses_auto) and returns a tibble of differential
#' (correlational) parameters. It works with any combination of P1-,
#' P2-, and 3rd person Meta-perceptions.
#'
#' The function can handle up to n exchangeable triads.
#' @param rep_model The results from one of the ReputationModelR
#' Models (e.g., rep_analyses_auto).
#' @param what The parameters you want in the table. Current options are
#' main and all. If what = "main", then just the 'main' model parameters are provided.
#' This will include, when avaiable, hearsay consensus, hearsay accuracy,
#' direct accuracy (P1-P1 agreement), P1 Meta-Accuracy, and P2 Meta-Accuracy.

#' @export
#' @import magrittr stringr lavaan
#' @importFrom rlang .data
#' @examples data("rep_sim_data")
#'      # Consensus only Model
#'           agree_rep_consensus <- rep_analyses_auto(data = rep_sim_data,
#'                         p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                         p2_reports = c("B_C_agreeableness", "D_A_agreeableness"))
#'
#'           ez_differential_table(agree_rep_consensus, what = "main")
#'
#'           ez_differential_table(agree_rep_consensus, what = "all")
#'
#'        # Consensus & Accuracy
#'
#'           agree_rep_con_acc <- rep_analyses_auto(data = rep_sim_data,
#'                        p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                        p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
#'                        target_self = c("C_C_agreeableness", "A_A_agreeableness"))
#'
#'           ez_differential_table(agree_rep_con_acc, what = "main")
#'
#'           ez_differential_table(agree_rep_con_acc, what = "all")
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
#'          ez_differential_table(agree_rep_all, what = "main")
#'
#'           ez_differential_table(agree_rep_all, what = "all")
#'
#' @return The function returns an object of class \code{\link[tibble:tbl_df-class]{tbl_df}}.

ez_differential_table <- function(rep_model,
                                  what = "main"){
    # First save out labels
    # we need these to remove the repeats (from equality constraints)
    labels <-
      rep_model@ParTable %>%
      tibble::as_tibble() %>%
      dplyr::select(.data$lhs, .data$op, .data$rhs, .data$label)

    # Main parameters only
    if(what == "main"){
      rep_parameter_table <- rep_model %>%
        standardizedsolution() %>%
        tibble::as_tibble() %>%
        dplyr::full_join(labels) %>%
        dplyr::distinct(.data$label, .keep_all = TRUE) %>%
        dplyr::filter(.data$label == "hc" |
                      .data$label == "ha" |
                      .data$label == "da" |
                      .data$label == "p1ma"|
                      .data$label == "p2ma") %>%
        # give them their substantive labels
        dplyr::mutate(parameter = ifelse(.data$label == "ha", "hearsay accuracy",
                                  ifelse(.data$label == "hc", "hearsay consensus",
                                  ifelse(.data$label == "da", "direct accuracy",
                                  ifelse(.data$label == "p1ma", "P1 Meta-Accuracy",
                                  ifelse(.data$label == "p2ma", "P2 Meta-Accuracy", NA)))))) %>%
        dplyr::select(.data$parameter, .data$est.std,
                      .data$ci.lower, .data$ci.upper, .data$pvalue) %>%
        dplyr::rename(r = .data$est.std,
                      ci_lower = .data$ci.lower,
                      ci_upper = .data$ci.upper)}
    if(what == "all"){
      rep_parameter_table <- rep_model %>%
        standardizedsolution() %>%
        tibble::as_tibble() %>%
        dplyr::full_join(labels) %>%
        dplyr::distinct(.data$label, .keep_all = TRUE) %>%
        dplyr::filter(.data$label == "ha"|
                      .data$label == "hc"|
                      .data$label == "da"|
                      .data$label == "p1ma"|
                      .data$label == "p2ma"|
                      .data$label == "as_ac1"|
                      .data$label == "as_con1"|
                      .data$label == "mp_rec"|
                      .data$label == "as_ac2"|
                      .data$label == "as_con2"|
                      .data$label == "rec"|
                      .data$label == "h"|
                      .data$label == "m"|
                      .data$label == "tru_sim"|
                      .data$label == "as_sim_3p"|
                      .data$label == "as_sim_1p"|
                      .data$label == "as_sim_p1m"|
                      .data$label == "ukp1m1"|
                      .data$label == "p1meta_sim"|
                      .data$label == "ukp2m1"|
                      .data$label == "ukp2m3"|
                      .data$label == "p2meta_sim"|
                      .data$label == "ukm1") %>%
        # give them their substantive labels
        dplyr::mutate(parameter = ifelse(.data$label == "ha", "hearsay accuracy",
                                  ifelse(.data$label == "hc", "hearsay consensus",
                                  ifelse(.data$label == "da", "direct accuracy",
                                  ifelse(.data$label == "p1ma", "P1 Meta-Accuracy",
                                  ifelse(.data$label == "p2ma", "P2 Meta-Accuracy",
                                  ifelse(.data$label == "as_ac1", "P1 Assumed Accuracy",
                                  ifelse(.data$label == "as_con1", "P1 Assumed Consensus",
                                  ifelse(.data$label == "mp_rec", "P1-P2 Meta-Perception Reciprocity",
                                  ifelse(.data$label == "as_ac2", "P2 Assumed Accuracy",
                                  ifelse(.data$label == "as_con2", "P2 Assumed Consensus",
                                  ifelse(.data$label == "rec", "direct reciprocity",
                                  ifelse(.data$label == "h", "hearsay reciprocity",
                                  ifelse(.data$label == "m", "P2(T) <-> opposite P1(T)",
                                  ifelse(.data$label == "tru_sim", "True Target Similarity",
                                  ifelse(.data$label == "as_sim_3p", "Third-Person Assumed Similarity",
                                  ifelse(.data$label == "as_sim_1p", "First-Person Assumed Similarity",
                                  ifelse(.data$label == "as_sim_p1m", "P1 Meta- Assumed Similarity",
                                  ifelse(.data$label == "ukp1m1", "P1 Meta <-> opposite P1-Report",
                                  ifelse(.data$label == "p1meta_sim", "P1 meta-similarity",
                                  ifelse(.data$label == "ukp2m1", "P2 Meta <-> opposite target self-report",
                                  ifelse(.data$label == "ukp2m3", "P2 meta <-> opposite P1-report",
                                  ifelse(.data$label == "p2meta_sim", "P2 Meta-similarity",
                                  ifelse(.data$label == "ukm1", "P1 Meta <-> opposite P2 Meta",
                                         NA)))))))))))))))))))))))) %>%
        dplyr::select(.data$parameter, .data$est.std,
                      .data$ci.lower, .data$ci.upper, .data$pvalue) %>%
        dplyr::rename(r = .data$est.std,
                      ci_lower = .data$ci.lower,
                      ci_upper = .data$ci.upper)}
  return(rep_parameter_table)}

#' Easily Table Differential Results from Group Moderated Models
#'
#' This takes output from one of the group moderated reputation models
#' (e.g., rep_analyses_auto) and returns a tibble of differential
#' (correlational) parameters. It works with any of the group moderated models.
#' @param rep_model The results from one of the ReputationModelR
#' group Models (e.g., rep_auto_group_mod). The model can have between-group equality
#' constraints; those are marked by missing values (NA) in the group_label column.
#' @param what The parameters you want in the table. Current options are
#' main and all. If what = "main", then just the 'main' model parameters are provided.
#' This will include, when avaiable, hearsay consensus, hearsay accuracy,
#' direct accuracy (P1-P1 agreement), P1 Meta-Accuracy, and P2 Meta-Accuracy.
#' @export
#' @import magrittr stringr lavaan
#' @examples data("rep_sim_data")
#'      # Consensus only Model
#'           agree_rep_consensus_grpmod <- rep_auto_group_mod(data = rep_sim_data,
#'                                                            p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                                                            p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
#'                                                            group_mod = "study",
#'                                                            groups_eql = "all",
#'                                                            params_eql = "all")
#'
#'           ez_differential_group_table(agree_rep_consensus_grpmod, what = "main")
#'
#'           ez_differential_group_table(agree_rep_consensus_grpmod, what = "all")
#'
#'        # Consensus & Accuracy
#'
#'           agree_rep_con_acc_grpmod <- rep_auto_group_mod(data = rep_sim_data,
#'                                                          p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                                                          p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
#'                                                          target_self = c("C_C_agreeableness", "A_A_agreeableness"),
#'                                                          group_mod = "study", groups_eql = "all",params_eql = "all")
#'
#'           ez_differential_group_table(agree_rep_con_acc_grpmod, what = "main")
#'
#'           ez_differential_group_table(agree_rep_con_acc_grpmod, what = "all")
#'
#'       # Consensus, Accuracy, 3rd  Person Meta
#'
#' agree_full_3pmeta_grpmod <- rep_auto_group_mod(data = rep_sim_data,
#'                                                       p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                                                       p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
#'                                                       target_self = c("C_C_agreeableness", "A_A_agreeableness"),
#'                                                       p1_meta = c("A_B_C_agree_meta", "C_D_A_agree_meta"),
#'                                                       p2_meta = c("B_A_C_agree_meta", "D_C_A_agree_meta"),
#'                                                       group_mod = "study",
#'                                                       groups_eql = "all",
#'                                                       params_eql = "all")
#'
#'          ez_differential_group_table(agree_full_3pmeta_grpmod, what = "main")
#'
#'           ez_differential_group_table(agree_full_3pmeta_grpmod, what = "all")
#' @return The function returns an object of class \code{\link[tibble:tbl_df-class]{tbl_df}}.



ez_differential_group_table <- function(rep_model,
                                  what = "main"){
  # First save out labels
  # we need these to remove the repeats (from equality constraints)
  labels <- rep_model@ParTable %>%
    tibble::as_tibble() %>%
    dplyr::select(.data$lhs, .data$op, .data$rhs, .data$group, .data$label) %>%
    tidyr::separate(.data$label, c("group_label", "param_label"),
                    extra = "merge", fill = "left") %>%
    dplyr::filter(.data$group_label != "")

  # Main parameters only
  if(what == "main"){
    rep_parameter_table <- rep_model %>%
      standardizedsolution() %>%
      tibble::as_tibble() %>%
      dplyr::full_join(labels) %>%
      dplyr::distinct(.data$group_label, .data$param_label, .keep_all = TRUE) %>%
      dplyr::filter(.data$param_label == "hc" |
                    .data$param_label == "ha" |
                    .data$param_label == "da" |
                    .data$param_label == "p1ma"|
                    .data$param_label == "p2ma") %>%
      # give them their substantive labels
      dplyr::mutate(parameter = ifelse(.data$param_label == "ha", "hearsay accuracy",
                                      ifelse(.data$param_label == "hc", "hearsay consensus",
                                      ifelse(.data$param_label == "da", "direct accuracy",
                                      ifelse(.data$param_label == "p1ma", "P1 Meta-Accuracy",
                                      ifelse(.data$param_label == "p2ma", "P2 Meta-Accuracy", NA)))))) %>%
      dplyr::select(.data$group_label, .data$parameter, .data$est.std,
                    .data$ci.lower, .data$ci.upper, .data$pvalue) %>%
      dplyr::rename(r = .data$est.std,
                    ci_lower = .data$ci.lower,
                    ci_upper = .data$ci.upper)}
  if(what == "all"){
    rep_parameter_table <- rep_model %>%
      standardizedsolution() %>%
      tibble::as_tibble() %>%
      dplyr::full_join(labels) %>%
      dplyr::distinct(.data$label, .keep_all = TRUE) %>%
      dplyr::filter(.data$param_label == "ha"|
                    .data$param_label == "hc"|
                    .data$param_label == "da"|
                    .data$param_label == "p1ma"|
                    .data$param_label == "p2ma"|
                    .data$param_label == "as_ac1"|
                    .data$param_label == "as_con1"|
                    .data$param_label == "mp_rec"|
                    .data$param_label == "as_ac2"|
                    .data$param_label == "as_con2"|
                    .data$param_label == "rec"|
                    .data$param_label == "h"|
                    .data$param_label == "m"|
                    .data$param_label == "tru_sim"|
                    .data$param_label == "as_sim_3p"|
                    .data$param_label == "as_sim_1p"|
                    .data$param_label == "as_sim_p1m"|
                    .data$param_label == "ukp1m1"|
                    .data$param_label == "p1meta_sim"|
                    .data$param_label == "ukp2m1"|
                    .data$param_label == "ukp2m3"|
                    .data$param_label == "p2meta_sim"|
                    .data$param_label == "ukm1") %>%
      # give them their substantive labels
      dplyr::mutate(parameter = ifelse(.data$param_label == "ha", "hearsay accuracy",
                                ifelse(.data$param_label == "hc", "hearsay consensus",
                                ifelse(.data$param_label == "da", "direct accuracy",
                                ifelse(.data$param_label == "p1ma", "P1 Meta-Accuracy",
                                ifelse(.data$param_label == "p2ma", "P2 Meta-Accuracy",
                                ifelse(.data$param_label == "as_ac1", "P1 Assumed Accuracy",
                                ifelse(.data$param_label == "as_con1", "P1 Assumed Consensus",
                                ifelse(.data$param_label == "mp_rec", "P1-P2 Meta-Perception Reciprocity",
                                ifelse(.data$param_label == "as_ac2", "P2 Assumed Accuracy",
                                ifelse(.data$param_label == "as_con2", "P2 Assumed Consensus",
                                ifelse(.data$param_label == "rec", "direct reciprocity",
                                ifelse(.data$param_label == "h", "hearsay reciprocity",
                                ifelse(.data$param_label == "m", "P2(T) <-> opposite P1(T)",
                                ifelse(.data$param_label == "tru_sim", "True Target Similarity",
                                ifelse(.data$param_label == "as_sim_3p", "Third-Person Assumed Similarity",
                                ifelse(.data$param_label == "as_sim_1p", "First-Person Assumed Similarity",
                                ifelse(.data$param_label == "as_sim_p1m", "P1 Meta- Assumed Similarity",
                                ifelse(.data$param_label == "ukp1m1", "P1 Meta <-> opposite P1-Report",
                                ifelse(.data$param_label == "p1meta_sim", "P1 meta-similarity",
                                ifelse(.data$param_label == "ukp2m1", "P2 Meta <-> opposite target self-report",
                                ifelse(.data$param_label == "ukp2m3", "P2 meta <-> opposite P1-report",
                                ifelse(.data$param_label == "p2meta_sim", "P2 Meta-similarity",
                                ifelse(.data$param_label == "ukm1", "P1 Meta <-> opposite P2 Meta",
                                       NA)))))))))))))))))))))))) %>%
      dplyr::select(.data$group_label, .data$parameter, .data$est.std,
                    .data$ci.lower, .data$ci.upper, .data$pvalue) %>%
      dplyr::rename(r = .data$est.std,
                    ci_lower = .data$ci.lower,
                    ci_upper = .data$ci.upper)
  }
  # the function above will only get parameters without equality constraints
  # which sort of makes sense for this (it only gives you the results of group moderated analyses).
  # However, you might have a case in which some (but not all) groups or parameters are equal, and you
  # want to table them all together. This adds the equality constrained rows
  if(nrow(rep_parameter_table) == 0){
    rep_parameter_table <- ez_differential_table(rep_model = rep_model,
                                                  what = what)
  }
  else{
    rep_parameter_table_eqls <- NULL
    if(max(rep_model@ParTable[["group"]]) != nrow(unique(rep_parameter_table["group_label"]))){
      rep_parameter_table_eqls <- ez_differential_table(rep_model = rep_model,
                                                    what = what)
    }
    if(!is.null(rep_parameter_table_eqls) && nrow(rep_parameter_table_eqls) > 0){
      rep_parameter_table <- dplyr::full_join(rep_parameter_table, rep_parameter_table_eqls)
      rep_parameter_table <- dplyr::mutate(rep_parameter_table, group_label = ifelse(is.na(.data$group_label), "eql", .data$group_label))
      message("The Model you provided had some between-group equality constraints.
              Those pooled estimates are in the rows where group is marked eql")
    }
  }
  return(rep_parameter_table)}

#' Easily Table Individual Difference Moderator Results
#'
#' This takes output from one of the individual-level moderated reputation models
#' (e.g., rep_auto_id_mods) and returns a tibble of model
#' (regression) parameters. It works with any of the Individual-level moderated models.
#' @param rep_model The results from one of the ReputationModelR
#' individual-level moderator Models (e.g., rep_auto_id_mods).

#' @export
#' @import magrittr stringr lavaan
#' @examples data("rep_sim_data")
#' moderator_data <- rep_sim_data %>%
#' dplyr::mutate(B_C_agreeableness_cent = scale(B_C_agreeableness, scale = FALSE),
#'        D_A_agreeableness_cent = scale(D_A_agreeableness, scale = FALSE),
#'        B_iri_perspective_cent = scale(B_iri_perspective, scale = FALSE),
#'        D_iri_perspective_cent = scale(D_iri_perspective, scale = FALSE),
#'        B_ptXagree_interaction = B_C_agreeableness_cent*B_iri_perspective_cent,
#'        D_ptXagree_interaction = D_A_agreeableness_cent*D_iri_perspective_cent)
#'
#' # Example for hearsay accuracy
#' agree_ha_p2ptmod_model <- rep_auto_id_mods(data = moderator_data,
#'                  target_self = c("C_C_agreeableness", "A_A_agreeableness"),
#'                  p2_reports = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                  id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                  interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"))
#'  ez_id_mod_table(agree_ha_p2ptmod_model)
#'
#'
#' @return The function returns an object of class \code{\link[tibble:tbl_df-class]{tbl_df}}.

ez_id_mod_table <- function(rep_model){
  # First save out labels
  # we need these to remove the repeats (from equality constraints)
  labels <- rep_model@ParTable %>%
    tibble::as_tibble() %>%
    dplyr::select(.data$lhs, .data$op, .data$rhs, .data$label)

  rep_parameter_table <- rep_model %>%
      standardizedsolution() %>%
      tibble::as_tibble() %>%
      dplyr::full_join(labels) %>%
      dplyr::distinct(.data$label, .keep_all = TRUE) %>%
      dplyr::filter(.data$label == "hc_me" |
                    .data$label == "ha_me" |
                    .data$label == "mod_me" |
                    .data$label == "interaction") %>%
      # give them their substantive labels
      dplyr::mutate(parameter = ifelse(.data$label == "hc_me", "hearsay consensus (main effect)",
                                ifelse(.data$label == "ha_me", "hearsay accuracy (main effect)",
                                ifelse(.data$label == "mod_me", "moderator (main effect)",
                                ifelse(.data$label == "interaction", "interaction effect",NA))))) %>%
      dplyr::select(.data$parameter, .data$est.std,
                    .data$ci.lower, .data$ci.upper, .data$pvalue) %>%
      dplyr::rename(beta = .data$est.std,
                    ci_lower = .data$ci.lower,
                    ci_upper = .data$ci.upper)
  return(rep_parameter_table)}

#' Easily Table Model with Group and Individual Difference Moderators
#'
#' This takes output from one of the group- and individual-level moderated reputation models
#' (e.g., rep_accuracy_group_id_mods) and returns a tibble of model
#' (regression) parameters. It works with any of the Group- and Individual-level moderated models.
#' @param rep_model The results from one of the ReputationModelR
#' group- and individual-level moderator Models (e.g., rep_accuracy_group_id_mods).

#' @export
#' @examples data("rep_sim_data")
#' moderator_data <- rep_sim_data %>%
#' dplyr::mutate(B_C_agreeableness_cent = scale(B_C_agreeableness, scale = FALSE),
#'        D_A_agreeableness_cent = scale(D_A_agreeableness, scale = FALSE),
#'        B_iri_perspective_cent = scale(B_iri_perspective, scale = FALSE),
#'        D_iri_perspective_cent = scale(D_iri_perspective, scale = FALSE),
#'        B_ptXagree_interaction = B_C_agreeableness_cent*B_iri_perspective_cent,
#'        D_ptXagree_interaction = D_A_agreeableness_cent*D_iri_perspective_cent)
#'
#' # Example for hearsay accuracy with no equality constraints
#' agree_pt_mod_fit <- rep_accuracy_group_id_mods(moderator_data,
#'                                           target_self = c("C_C_agreeableness", "A_A_agreeableness"),
#'                                           p2_reports = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                           id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                           interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"),
#'                                           group_mod = "study")
#'  ez_group_id_mod_table(agree_pt_mod_fit)
#'
#'  # Example for hearsay accuracy with all parameters equal across all groups
#'  agree_pt_mod_fit_alleq <- rep_accuracy_group_id_mods(moderator_data,
#'                                           target_self = c("C_C_agreeableness", "A_A_agreeableness"),
#'                                           p2_reports = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                           id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                           interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"),
#'                                           group_mod = "study", groups_eql = "all", params_eql = "all")
#'  ez_group_id_mod_table(agree_pt_mod_fit_alleq)
#'
#'  # Example with some equality constraints
#'
#'  agree_pt_mod_fit_someeql <-  rep_accuracy_group_id_mods(moderator_data,
#'                                                          target_self = c("C_C_agreeableness", "A_A_agreeableness"),
#'                                                          p2_reports = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                                          id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                                          interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"),
#'                                                          group_mod = "group_var", groups_eql = c(1, 4), params_eql = "all")
#'
#' ez_group_id_mod_table(agree_pt_mod_fit_someeql)
#' @return The function returns an object of class \code{\link[tibble:tbl_df-class]{tbl_df}}.

ez_group_id_mod_table <- function(rep_model){
  # First save out labels
  # we need these to remove the repeats (from equality constraints)
  labels <- rep_model@ParTable%>%
    tibble::as_tibble() %>%
    dplyr::select(.data$lhs, .data$op, .data$rhs, .data$group, .data$label) %>%
    tidyr::separate(.data$label, c("group_label", "param_label"), extra = "merge", fill = "left") %>%
    dplyr::filter(.data$group_label != "")

  rep_parameter_table <- rep_model %>%
    standardizedsolution() %>%
    tibble::as_tibble() %>%
    dplyr::full_join(labels) %>%
    dplyr::filter(.data$lhs != .data$rhs &
                    .data$op != "~1") %>%
    dplyr::distinct(.data$group_label, .data$param_label, .keep_all = TRUE) %>%
    dplyr::filter(.data$param_label == "hc_me" |
                  .data$param_label == "ha_me" |
                  .data$param_label == "mod_me" |
                  .data$param_label == "interaction") %>%
    # give them their substantive labels
    dplyr::mutate(parameter = ifelse(.data$param_label == "hc_me", "hearsay consensus (main effect)",
                              ifelse(.data$param_label == "ha_me", "hearsay accuracy (main effect)",
                              ifelse(.data$param_label == "mod_me", "moderator (main effect)",
                              ifelse(.data$param_label == "interaction", "interaction effect",NA))))) %>%
    dplyr::select(.data$group_label, .data$parameter, .data$est.std,
                  .data$ci.lower, .data$ci.upper, .data$pvalue) %>%
    dplyr::rename(beta = .data$est.std,
                  ci_lower = .data$ci.lower,
                  ci_upper = .data$ci.upper)

  # the function above will only get parameters without equality constraints
  # which sort of makes sense for this (it only gives you the results of group moderated analyses).
  # However, you might have a case in which some (but not all) groups or parameters are equal, and you
  # want to table them all together. This adds the equality constrained rows
  if(nrow(rep_parameter_table) == 0){
    rep_parameter_table <- ez_id_mod_table(rep_model = rep_model)
  }
  else{
    rep_parameter_table_eqls <- NULL
    if(max(rep_model@ParTable[["group"]]) != nrow(unique(rep_parameter_table["group_label"]))){
      rep_parameter_table_eqls <-  ez_id_mod_table(rep_model = rep_model)
    }
    if(!is.null(rep_parameter_table_eqls) && nrow(rep_parameter_table_eqls) > 0){
      rep_parameter_table <- dplyr::full_join(rep_parameter_table, rep_parameter_table_eqls)
      rep_parameter_table <- dplyr::mutate(rep_parameter_table, group_label = ifelse(is.na(.data$group_label), "eql", .data$group_label))
      message("The Model you provided had some between-group equality constraints.
              Those pooled estimates are in the rows where group is marked eql")
    }
  }
  return(rep_parameter_table)}

#' Easily Table Elevation Results
#'
#' This takes output from one of the reputational analysis models
#' (e.g., rep_analyses_auto) and returns a list of two tibbles corresponding to
#' elevation (mean-differences) estimates and the relevant pooled Means and SDs respectively.
#' The elevation table contains the raw difference, 95% CI around the raw difference,
#' z scores, p values, and a cohen's d (difference / pooled SD). It has only 1 parameter.
#'
#' @param rep_model The results from one of the ReputationModelR
#' Models (e.g., rep_analyses_auto).
#' @import magrittr stringr lavaan
#' @export
#' @examples data("rep_sim_data")
#'
#'        # Consensus only Model
#'           agree_rep_consensus <- rep_analyses_auto(data = rep_sim_data,
#'                         p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                         p2_reports = c("B_C_agreeableness", "D_A_agreeableness"))
#'
#'           ez_elevation_table(agree_rep_consensus)
#'
#'        # Consensus & Accuracy
#'
#'           agree_rep_con_acc <- rep_analyses_auto(data = rep_sim_data,
#'                        p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                        p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
#'                        target_self = c("C_C_agreeableness", "A_A_agreeableness"))
#'
#'           ez_elevation_table(agree_rep_con_acc)
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
#'          ez_elevation_table(agree_rep_all)
#'
#' @return The function returns a list of 2 objects of class \code{\link[tibble:tbl_df-class]{tbl_df}}.

ez_elevation_table <- function(rep_model){
  # dealing with global binding issue
  label <- cohen_d <- variable <- value <- NULL

  rep_elevation_table <- rep_model %>%
    parameterestimates() %>%
    tibble::as_tibble() %>%
    dplyr::distinct(.data$label, .keep_all = TRUE) %>%
    dplyr::filter(.data$label == "p1_p2_rel_el"|
                  .data$label == "self_p2_rel_el"|
                  .data$label == "self_p1_rel_el"|
                  .data$label == "p1_meta_rel_el"|
                  .data$label == "p2_meta_rel_el"|
                  # and get the variances for calculating a
                  # cohen's d
                  stringr::str_detect(.data$label, "v_") |
                  stringr::str_detect(.data$label, "int_")) %>%
    dplyr::select(.data$label, .data$est) %>%
    tidyr::spread(.data$label, .data$est)
  # calculate d's
  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "p1_p2_rel_el"))) > 0){
    rep_elevation_table <- dplyr::mutate(rep_elevation_table,
                                         p1_p2_std_d = .data$p1_p2_rel_el / sqrt((.data$v_p1 + .data$v_p2) / 2))}

  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "self_p2_rel_el"))) > 0){
    rep_elevation_table <- dplyr::mutate(rep_elevation_table,
                                         self_p2_std_d = .data$self_p2_rel_el / sqrt((.data$v_self + .data$v_p2) / 2))}

  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "self_p1_rel_el"))) > 0){
    rep_elevation_table <- dplyr::mutate(rep_elevation_table,
                                         self_p1_std_d = .data$self_p1_rel_el / sqrt((.data$v_self + .data$v_p1) / 2))}

  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "p1_meta_rel_el"))) > 0){
    rep_elevation_table <- dplyr::mutate(rep_elevation_table,
                                         p1_meta_std_d = .data$p1_meta_rel_el / sqrt((.data$v_mp1 + .data$v_p2) / 2))}

  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "p2_meta_rel_el"))) > 0){
    rep_elevation_table <- dplyr::mutate(rep_elevation_table,
                                         p2_meta_std_d = .data$p2_meta_rel_el / sqrt((.data$v_mp2 + .data$v_p1) / 2))}

  rep_elevation_table <- rep_elevation_table %>%
    dplyr::select(dplyr::ends_with("std_d")) %>%
    tidyr::gather(label, cohen_d) %>%
    dplyr::mutate(label = stringr::str_replace_all(.data$label, "std_d", "rel_el")) %>%
    dplyr::left_join(parameterestimates(rep_model)) %>%
    # give them their substantive labels
    dplyr::mutate(parameter = ifelse(.data$label == "p1_p2_rel_el", "P1-P2 Relative Elevation",
                              ifelse(.data$label == "self_p2_rel_el", "Self-P2 Relative Elevation",
                              ifelse(.data$label == "self_p1_rel_el", "Self-P1 Relative Elevation",
                              ifelse(.data$label == "p1_meta_rel_el", "P1 Meta-Elevation",
                              ifelse(.data$label == "p2_meta_rel_el", "P2 Meta-Elevation", NA)))))) %>%
    dplyr::select(.data$parameter, .data$est,
                  .data$ci.lower, .data$ci.upper,
                  .data$cohen_d, .data$z, .data$pvalue) %>%
    dplyr::rename(raw_diff =.data$est,
                  ci_lower = .data$ci.lower,
                  ci_upper = .data$ci.upper) %>%
    dplyr::distinct(.data$parameter, .keep_all = TRUE)

  rep_descrips <- rep_model %>%
    parameterestimates() %>%
    dplyr::distinct(.data$label, .keep_all = TRUE) %>%
    dplyr::select(.data$label, .data$est) %>%
    tidyr::spread(.data$label, .data$est)

  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_self"))) > 0){
    rep_descrips <- dplyr::mutate(rep_descrips,
                                  self_sd = sqrt(.data$v_self))}

  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_p1"))) > 0){
    rep_descrips <- dplyr::mutate(rep_descrips,
                                  p1_sd = sqrt(.data$v_p1))}

  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_p2"))) > 0){
    rep_descrips <- dplyr::mutate(rep_descrips,
                                  p2_sd = sqrt(.data$v_p2))}

  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_mp1"))) > 0){
    rep_descrips <- dplyr::mutate(rep_descrips,
                                  mp1_sd = sqrt(.data$v_mp1))}

  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_mp2"))) > 0){
    rep_descrips <- dplyr::mutate(rep_descrips,
                                  mp2_sd = sqrt(.data$v_mp2))}

  rep_descrips <- rep_descrips %>%
    dplyr::select(dplyr::starts_with("int"), dplyr::ends_with("sd"))
  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "p1_p2_rel_el"))) > 0){
    rep_descrips <- dplyr::rename(rep_descrips,
                                  p1_t_mean = .data$int_p1,
                                  p1_t_sd = .data$p1_sd,
                                  p2_t_mean = .data$int_p2,
                                  p2_t_sd = .data$p2_sd)}

  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_self"))) > 0){
    rep_descrips <- dplyr::rename(rep_descrips,
                                  t_t_mean  = .data$int_self,
                                  t_t_sd    = .data$self_sd)}

  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_mp1"))) > 0){
    rep_descrips <- dplyr::rename(rep_descrips,
                                  p1_p2t_mean = .data$int_mp1,
                                  p1_p2t_sd = .data$mp1_sd)}
  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_mp2"))) > 0){
    rep_descrips <- dplyr::rename(rep_descrips,
                                  p2_p1t_mean = .data$int_mp2,
                                  p2_p1t_sd = .data$mp2_sd)}

  rep_descrips <- rep_descrips %>%
    tidyr::gather(variable, value) %>%
    tidyr::separate(.data$variable, c("perceiver", "target", "stat"), extra = "merge") %>%
    dplyr::mutate(target = stringr::str_replace(.data$target, "p1t", "p1_t"),
                  target = stringr::str_replace(.data$target, "p2t", "p2_t")) %>%
    tidyr::spread(.data$stat, .data$value)


  return(list(elevation_table = rep_elevation_table,
              pooled_means_sd = rep_descrips))}

#' Easily Table Elevation Results from Group Moderated Models
#'
#' This takes output from one of the reputation group moderated models
#' (e.g., rep_auto_group_mod) and returns a list of two tibbles corresponding to
#' elevation (mean-differences) estimates and the relevant pooled Means and SDs respectively.
#' The elevation table contains the raw difference, 95% CI around the raw difference,
#' z scores, p values, and a cohen's d (difference / pooled SD). It has only 1 parameter.
#'
#' @param rep_model The results from one of the ReputationModelR group moderated models
#' Models (e.g., rep_auto_group_mod).
#' @import magrittr stringr lavaan
#' @export
#' @examples data("rep_sim_data")
#'
#'        # Consensus only Model
#'           agree_rep_consensus_grpmod <- rep_auto_group_mod(data = rep_sim_data,
#'                                                            p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                                                            p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
#'                                                            group_mod = "study",
#'                                                            groups_eql = "all",
#'                                                            params_eql = "all")
#'
#'           ez_elevation_group_table(agree_rep_consensus_grpmod)
#'
#'           ez_elevation_group_table(agree_rep_consensus_grpmod)
#'
#'        # Consensus & Accuracy
#'
#'           agree_rep_con_acc_grpmod <- rep_auto_group_mod(data = rep_sim_data,
#'                                                          p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                                                          p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
#'                                                          target_self = c("C_C_agreeableness", "A_A_agreeableness"),
#'                                                          group_mod = "study", groups_eql = "all",params_eql = "all")
#'
#'           ez_elevation_group_table(agree_rep_con_acc_grpmod)
#'
#'           ez_elevation_group_table(agree_rep_con_acc_grpmod)
#'
#'       # Consensus, Accuracy, 3rd  Person Meta
#'
#' agree_full_3pmeta_grpmod <- rep_auto_group_mod(data = rep_sim_data,
#'                                                       p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                                                       p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
#'                                                       target_self = c("C_C_agreeableness", "A_A_agreeableness"),
#'                                                       p1_meta = c("A_B_C_agree_meta", "C_D_A_agree_meta"),
#'                                                       p2_meta = c("B_A_C_agree_meta", "D_C_A_agree_meta"),
#'                                                       group_mod = "study",
#'                                                       groups_eql = "all",
#'                                                       params_eql = "all")
#'
#'          ez_elevation_group_table(agree_full_3pmeta_grpmod)
#'
#'           ez_elevation_group_table(agree_full_3pmeta_grpmod)
#'
#' @return The function returns a list of 2 objects of class \code{\link[tibble:tbl_df-class]{tbl_df}}.

ez_elevation_group_table <- function(rep_model){
  # dealing with binding issue
  label <- cohen_d <- variable <- value <- parameter <- NULL

  rep_elevation_table <- rep_model %>%
    parameterestimates() %>%
    tibble::as_tibble() %>%
    tidyr::separate(.data$label, c("group_label", "parameter"),
                    extra = "merge", fill = "left") %>%
    dplyr::distinct(.data$group_label, .data$parameter, .keep_all = TRUE) %>%
    dplyr::filter(.data$parameter == "p1_p2_rel_el"|
                  .data$parameter == "self_p2_rel_el"|
                  .data$parameter == "self_p1_rel_el"|
                  .data$parameter == "p1_meta_rel_el"|
                  .data$parameter == "p2_meta_rel_el"|
                    # and get the variances for calculating a
                    # cohen's d
                    stringr::str_detect(.data$parameter, "v_") |
                    stringr::str_detect(.data$parameter, "int_")) %>%
    dplyr::select(.data$group_label, .data$parameter, .data$est) %>%
    tidyr::spread(.data$parameter, .data$est)
  # When all parameters are constrained to be equal, the above
  # will produce an empty tibble. In that case, we just want to
  # run the non-group elevation table function.
  if(nrow(rep_elevation_table) == 0){
    rep_elevation_table <- ez_elevation_table(rep_model = rep_model)[[1]]
    rep_descrips <- ez_elevation_table(rep_model = rep_model)[[2]]
    # provide a label of 'eql' for group_labs if there is a group variable
    if(max(rep_model@ParTable[["group"]]) > 1){
      rep_elevation_table <- rep_elevation_table %>%
        dplyr::mutate(group_label = "eql") %>%
        dplyr::select(.data$group_label, dplyr::everything())

      rep_descrips <- rep_descrips %>%
        dplyr::mutate(group_label = "eql") %>%
        dplyr::select(.data$group_label, dplyr::everything())
    }
    else{
      rep_elevation_table <- rep_elevation_table %>%
        dplyr::mutate(group_label = "No Group Moderator in Model") %>%
        dplyr::select(.data$group_label, dplyr::everything())

      rep_descrips <- rep_descrips %>%
        dplyr::mutate(group_label = "No Group Moderator in Model") %>%
        dplyr::select(.data$group_label, dplyr::everything())
    }
  }
  else{
    # calculate d's
    if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "p1_p2_rel_el"))) > 0){
      rep_elevation_table <- dplyr::mutate(rep_elevation_table,
                                           p1_p2_std_d = .data$p1_p2_rel_el / sqrt((.data$v_p1 + .data$v_p2) / 2))}

    if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "self_p2_rel_el"))) > 0){
      rep_elevation_table <- dplyr::mutate(rep_elevation_table,
                                           self_p2_std_d = .data$self_p2_rel_el / sqrt((.data$v_self + .data$v_p2) / 2))}

    if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "self_p1_rel_el"))) > 0){
      rep_elevation_table <- dplyr::mutate(rep_elevation_table,
                                           self_p1_std_d = .data$self_p1_rel_el / sqrt((.data$v_self + .data$v_p1) / 2))}

    if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "p1_meta_rel_el"))) > 0){
      rep_elevation_table <- dplyr::mutate(rep_elevation_table,
                                           p1_meta_std_d = .data$p1_meta_rel_el / sqrt((.data$v_mp1 + .data$v_p2) / 2))}

    if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "p2_meta_rel_el"))) > 0){
      rep_elevation_table <- dplyr::mutate(rep_elevation_table,
                                           p2_meta_std_d = .data$p2_meta_rel_el / sqrt((.data$v_mp2 + .data$v_p1) / 2))}
    rel_el_only <- rep_model %>%
      parameterestimates() %>%
      tibble::as_tibble() %>%
      tidyr::separate(.data$label, c("group_label", "parameter"),
                      extra = "merge", fill = "left") %>%
      dplyr::distinct(.data$group_label, .data$parameter, .keep_all = TRUE) %>%
      dplyr::filter(.data$parameter == "p1_p2_rel_el"|
                    .data$parameter == "self_p2_rel_el"|
                    .data$parameter == "self_p1_rel_el"|
                    .data$parameter == "p1_meta_rel_el"|
                    .data$parameter == "p2_meta_rel_el") %>%
      dplyr::select(.data$group_label, .data$parameter, .data$est,
                    .data$se, .data$z, .data$pvalue, .data$ci.lower, .data$ci.upper)

    rep_elevation_table <-
      rep_elevation_table %>%
      dplyr::select(.data$group_label, dplyr::ends_with("std_d")) %>%
      tidyr::gather(parameter, cohen_d, -.data$group_label) %>%
      dplyr::mutate(parameter = stringr::str_replace_all(.data$parameter, "std_d", "rel_el")) %>%
      dplyr::left_join(rel_el_only) %>%
      # give them their substantive labels
      dplyr::mutate(parameter = ifelse(.data$parameter == "p1_p2_rel_el", "P1-P2 Relative Elevation",
                                ifelse(.data$parameter == "self_p2_rel_el", "Self-P2 Relative Elevation",
                                ifelse(.data$parameter == "self_p1_rel_el", "Self-P1 Relative Elevation",
                                ifelse(.data$parameter == "p1_meta_rel_el", "P1 Meta-Elevation",
                                ifelse(.data$parameter == "p2_meta_rel_el", "P2 Meta-Elevation", NA)))))) %>%
      dplyr::select(.data$group_label, .data$parameter,
                    .data$est, .data$ci.lower, .data$ci.upper,
                    .data$cohen_d, .data$z, .data$pvalue) %>%
      dplyr::rename(raw_diff = .data$est,
                    ci_lower = .data$ci.lower,
                    ci_upper = .data$ci.upper)

    rep_elevation_table_eqls <- NULL
    if(max(rep_model@ParTable[["group"]]) != nrow(unique(rep_elevation_table["group_label"]))){
      rep_elevation_table_eqls <- ez_elevation_table(rep_model = rep_model)$elevation_table
    }
    if(!is.null(rep_elevation_table_eqls) && nrow(rep_elevation_table_eqls) > 0){
      rep_elevation_table <- dplyr::full_join(rep_elevation_table, rep_elevation_table_eqls)
      rep_elevation_table <- dplyr::mutate(rep_elevation_table, group_label = ifelse(is.na(.data$group_label), "eql", .data$group_label))
      message("The Model you provided had some between-group equality constraints.
              Those pooled estimates are in the rows where group is marked eql")
    }
    rep_descrips <- rep_model %>%
      parameterestimates() %>%
      tibble::as_tibble() %>%
      dplyr::filter(str_detect(.data$label, "int") | str_detect(.data$label, "v")) %>%
      dplyr::mutate(label = ifelse(str_detect(.data$label, "^int"),
                                   str_replace_all(.data$label,
                                                   .data$label,
                                                   paste("eql", .data$label, sep = "_")),
                            ifelse(str_detect(.data$label, "^v"),
                                   str_replace_all(.data$label,
                                                   .data$label,
                                                   paste("eql", .data$label, sep = "_")),
                                   .data$label))) %>%
      tidyr::separate(.data$label, c("group_label", "parameter"),
                      extra = "merge", fill = "left") %>%
      dplyr::distinct(.data$group_label, .data$parameter, .keep_all = TRUE) %>%
      dplyr::select(.data$group_label, .data$parameter, .data$est) %>%
      tidyr::spread(.data$parameter, .data$est)

    if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_self"))) > 0){
      rep_descrips <- dplyr::mutate(rep_descrips,
                                    self_sd = sqrt(.data$v_self))}

    if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_p1"))) > 0){
      rep_descrips <- dplyr::mutate(rep_descrips,
                                    p1_sd = sqrt(.data$v_p1))}

    if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_p2"))) > 0){
      rep_descrips <- dplyr::mutate(rep_descrips,
                                    p2_sd = sqrt(.data$v_p2))}

    if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_mp1"))) > 0){
      rep_descrips <- dplyr::mutate(rep_descrips,
                                    mp1_sd = sqrt(.data$v_mp1))}

    if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_mp2"))) > 0){
      rep_descrips <- dplyr::mutate(rep_descrips,
                                    mp2_sd = sqrt(.data$v_mp2))}

    rep_descrips <- rep_descrips %>%
      dplyr::select(.data$group_label,
                    dplyr::starts_with("int"),
                    dplyr::ends_with("sd"))

    if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "p1_p2_rel_el"))) > 0){
      rep_descrips <- dplyr::rename(rep_descrips,
                                    p1_t_mean = .data$int_p1,
                                    p1_t_sd = .data$p1_sd,
                                    p2_t_mean = .data$int_p2,
                                    p2_t_sd = .data$p2_sd)}

    if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_self"))) > 0){
      rep_descrips <- dplyr::rename(rep_descrips,
                                    t_t_mean  = .data$int_self,
                                    t_t_sd    = .data$self_sd)}

    if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_mp1"))) > 0){
      rep_descrips <- dplyr::rename(rep_descrips,
                                    p1_p2t_mean = .data$int_mp1,
                                    p1_p2t_sd = .data$mp1_sd)}
    if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_mp2"))) > 0){
      rep_descrips <- dplyr::rename(rep_descrips,
                                    p2_p1t_mean = .data$int_mp2,
                                    p2_p1t_sd = .data$mp2_sd)}

    rep_descrips <- rep_descrips %>%
      tidyr::gather(variable, value, -.data$group_label) %>%
      tidyr::separate(.data$variable,
                      c("perceiver", "target", "stat"), extra = "merge") %>%
      dplyr::mutate(target = stringr::str_replace(.data$target, "p1t", "p1_t"),
                    target = stringr::str_replace(.data$target, "p2t", "p2_t")) %>%
      tidyr::spread(.data$stat, .data$value)
  }


  return(list(elevation_table = dplyr::arrange(rep_elevation_table, .data$group_label),
              pooled_means_sd = rep_descrips))}

#' Easy Differential & Elevation Tables
#'
#' This takes output from one of the reputational analysis models
#' (e.g., rep_analyses_auto) and returns a list of length three. The first element of the list is a
#'  a tibble of differential (correlational) parameters.
#'  The second element is a table of elevation results. The third element is a tibble containing
#'  the table of descriptives (means and SDs, pooling across exchangeable roles).
#'
#' @param rep_model The results from one of the ReputationModelR
#' Models (e.g., rep_analyses_auto).
#' @param what The parameters you want in the differential table. Current options are
#' main and all. If what = "main", then just the 'main' model parameters are provided.
#' This will include, when avaiable, hearsay consensus, hearsay accuracy,
#' direct accuracy (P1-P1 agreement), P1 Meta-Accuracy, and P2 Meta-Accuracy.
#'
#' @import magrittr stringr lavaan
#' @export
#' @examples data("rep_sim_data")
#'
#'        # Consensus only Model
#'           agree_rep_consensus <- rep_analyses_auto(data = rep_sim_data,
#'                         p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                         p2_reports = c("B_C_agreeableness", "D_A_agreeableness"))
#'
#'           ez_tables(agree_rep_consensus, what = "main")
#'           ez_tables(agree_rep_consensus, what = "all")
#'
#'        # Consensus & Accuracy
#'
#'           agree_rep_con_acc <- rep_analyses_auto(data = rep_sim_data,
#'                        p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                        p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
#'                        target_self = c("C_C_agreeableness", "A_A_agreeableness"))
#'
#'           ez_tables(agree_rep_con_acc, what = "main")
#'           ez_tables(agree_rep_con_acc, what = "all")
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
#'          ez_tables(agree_rep_all, what = "main")
#'          ez_tables(agree_rep_all, what = "all")
#'
#' @return The function returns a list of 2 objects of class \code{\link[tibble:tbl_df-class]{tbl_df}}.

ez_tables <- function(rep_model, what = "main"){
  if(max(rep_model@ParTable[["group"]]) > 1){
    differential_table <- ez_differential_group_table(rep_model = rep_model, what = what)
    elevation_table <- ez_elevation_group_table(rep_model = rep_model)[[1]]
    pooled_means_sd <- ez_elevation_group_table(rep_model = rep_model)[[2]]
  }
  else{
    differential_table <- ez_differential_table(rep_model = rep_model, what = what)
    elevation_table <- ez_elevation_table(rep_model = rep_model)[[1]]
    pooled_means_sd <- ez_elevation_table(rep_model = rep_model)[[2]]
  }
  return(list(differential_table = differential_table,
              elevation_table = elevation_table,
              pooled_means_sd = pooled_means_sd))
}
