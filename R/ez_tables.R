#' Easily Table Differential Results
#'
#' This takes output from one of the reputational analysis models
#' (e.g., rep_analyses_auto) and returns a tibble of differential
#' (correlational) parameters. It works with any combination of P1-,
#' P2-, and 3rd person Meta-perceptions.
#'
#' The function can handle up to n exchangeable triads.
#' @param rep_model The results from one of the ReputationAnalyses
#' Models (e.g., rep_analyses_auto).
#' @param what The parameters you want in the table. Current options are
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
#' @return The function returns an object of class \code{\link[tibble::tibble()]{tibble}}.

ez_differential_table <- function(rep_model,
                                  what = "main"){
  # First save out labels
  # we need these to remove the repeats (from equality constraints)
  labels <-
    rep_model@ParTable %>%
    tibble::as_tibble() %>%
    dplyr::select(lhs, op, rhs, label)

  # Main parameters only
  if(what == "main"){
    rep_parameter_table <- rep_model %>%
      standardizedsolution() %>%
      tibble::as_tibble() %>%
      dplyr::full_join(labels) %>%
      dplyr::distinct(label, .keep_all = TRUE) %>%
      dplyr::filter(label == "hc" |
                      label == "ha" |
                      label == "da" |
                      label == "p1ma"|
                      label == "p2ma") %>%
      # give them their substantive labels
      dplyr::mutate(parameter = ifelse(label == "ha", "hearsay accuracy",
                                       ifelse(label == "hc", "hearsay consensus",
                                              ifelse(label == "da", "direct accuracy",
                                                     ifelse(label == "p1ma", "P1 Meta-Accuracy",
                                                            ifelse(label == "p2ma", "P2 Meta-Accuracy", NA)))))) %>%
      dplyr::select(parameter, est.std, ci.lower, ci.upper, pvalue) %>%
      dplyr::rename(r = est.std,
                    ci_lower = ci.lower,
                    ci_upper = ci.upper)}
  if(what == "all"){
    rep_parameter_table <- rep_model %>%
      standardizedsolution() %>%
      tibble::as_tibble() %>%
      dplyr::full_join(labels) %>%
      dplyr::distinct(label, .keep_all = TRUE) %>%
      dplyr::filter(label == "ha"|
                      label == "hc"|
                      label == "da"|
                      label == "p1ma"|
                      label == "p2ma"|
                      label == "as_ac1"|
                      label == "as_con1"|
                      label == "mp_rec"|
                      label == "as_ac2"|
                      label == "as_con2"|
                      label == "rec"|
                      label == "h"|
                      label == "m"|
                      label == "tru_sim"|
                      label == "as_sim_3p"|
                      label == "as_sim_1p"|
                      label == "as_sim_p1m"|
                      label == "ukp1m1"|
                      label == "p1meta_sim"|
                      label == "ukp2m1"|
                      label == "ukp2m3"|
                      label == "p2meta_sim"|
                      label == "ukm1") %>%
      # give them their substantive labels
      dplyr::mutate(parameter = ifelse(label == "ha", "hearsay accuracy",
                                       ifelse(label == "hc", "hearsay consensus",
                                              ifelse(label == "da", "direct accuracy",
                                                     ifelse(label == "p1ma", "P1 Meta-Accuracy",
                                                            ifelse(label == "p2ma", "P2 Meta-Accuracy",
                                                                   ifelse(label == "as_ac1", "P1 Assumed Accuracy",
                                                                          ifelse(label == "as_con1", "P1 Assumed Consensus",
                                                                                 ifelse(label == "mp_rec", "P1-P2 Meta-Perception Reciprocity",
                                                                                        ifelse(label == "as_ac2", "P2 Assumed Accuracy",
                                                                                               ifelse(label == "as_con2", "P2 Assumed Consensus",
                                                                                                      ifelse(label == "rec", "direct reciprocity",
                                                                                                             ifelse(label == "h", "hearsay reciprocity",
                                                                                                                    ifelse(label == "m", "P2(T) <-> opposite P1(T)",
                                                                                                                           ifelse(label == "tru_sim", "True Target Similarity",
                                                                                                                                  ifelse(label == "as_sim_3p", "Third-Person Assumed Similarity",
                                                                                                                                         ifelse(label == "as_sim_1p", "First-Person Assumed Similarity",
                                                                                                                                                ifelse(label == "as_sim_p1m", "P1 Meta- Assumed Similarity",
                                                                                                                                                       ifelse(label == "ukp1m1", "P1 Meta <-> opposite P1-Report",
                                                                                                                                                              ifelse(label == "p1meta_sim", "P1 meta-similarity",
                                                                                                                                                                     ifelse(label == "ukp2m1", "P2 Meta <-> opposite target self-report",
                                                                                                                                                                            ifelse(label == "ukp2m3", "P2 meta <-> opposite P1-report",
                                                                                                                                                                                   ifelse(label == "p2meta_sim", "P2 Meta-similarity",
                                                                                                                                                                                          ifelse(label == "ukm1", "P1 Meta <-> opposite P2 Meta", NA)))))))))))))))))))))))) %>%
      dplyr::select(parameter, est.std, ci.lower, ci.upper, pvalue) %>%
      dplyr::rename(r = est.std,
                    ci_lower = ci.lower,
                    ci_upper = ci.upper)}
  return(rep_parameter_table)}

#' Easily Table Differential Results from Group Moderated Models
#'
#' This takes output from one of the group moderated reputation models
#' (e.g., rep_analyses_auto) and returns a tibble of differential
#' (correlational) parameters. It works with any of the group moderated models.
#' @param rep_model The results from one of the ReputationAnalyses
#' group Models (e.g., rep_auto_group_mod).
#' @param what The parameters you want in the table. Current options are
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
#' @return The function returns an object of class \code{\link[tibble::tibble()]{tibble}}.

ez_differential_group_table <- function(rep_model,
                                  what = "main"){
  # First save out labels
  # we need these to remove the repeats (from equality constraints)
  labels <- rep_model@ParTable %>%
    tibble::as_tibble() %>%
    dplyr::select(lhs, op, rhs, group, label) %>%
    tidyr::separate(label, c("group_label", "param_label"), extra = "merge") %>%
    dplyr::filter(group_label != "")

  # Main parameters only
  if(what == "main"){
    rep_parameter_table <- rep_model %>%
      standardizedsolution() %>%
      tibble::as_tibble() %>%
      dplyr::full_join(labels) %>%
      dplyr::distinct(group_label, param_label, .keep_all = TRUE) %>%
      dplyr::filter(param_label == "hc" |
                      param_label == "ha" |
                      param_label == "da" |
                      param_label == "p1ma"|
                      param_label == "p2ma") %>%
      # give them their substantive labels
      dplyr::mutate(parameter = ifelse(param_label == "ha", "hearsay accuracy",
                                       ifelse(param_label == "hc", "hearsay consensus",
                                              ifelse(param_label == "da", "direct accuracy",
                                                     ifelse(param_label == "p1ma", "P1 Meta-Accuracy",
                                                            ifelse(param_label == "p2ma", "P2 Meta-Accuracy", NA)))))) %>%
      dplyr::select(group_label, parameter, est.std, ci.lower, ci.upper, pvalue) %>%
      dplyr::rename(r = est.std,
                    ci_lower = ci.lower,
                    ci_upper = ci.upper)}
  if(what == "all"){
    rep_parameter_table <- rep_model %>%
      standardizedsolution() %>%
      tibble::as_tibble() %>%
      dplyr::full_join(labels) %>%
      dplyr::distinct(label, .keep_all = TRUE) %>%
      dplyr::filter(param_label == "ha"|
                      param_label == "hc"|
                      param_label == "da"|
                      param_label == "p1ma"|
                      param_label == "p2ma"|
                      param_label == "as_ac1"|
                      param_label == "as_con1"|
                      param_label == "mp_rec"|
                      param_label == "as_ac2"|
                      param_label == "as_con2"|
                      param_label == "rec"|
                      param_label == "h"|
                      param_label == "m"|
                      param_label == "tru_sim"|
                      param_label == "as_sim_3p"|
                      param_label == "as_sim_1p"|
                      param_label == "as_sim_p1m"|
                      param_label == "ukp1m1"|
                      param_label == "p1meta_sim"|
                      param_label == "ukp2m1"|
                      param_label == "ukp2m3"|
                      param_label == "p2meta_sim"|
                      param_label == "ukm1") %>%
      # give them their substantive labels
      dplyr::mutate(parameter = ifelse(param_label == "ha", "hearsay accuracy",
                                       ifelse(param_label == "hc", "hearsay consensus",
                                              ifelse(param_label == "da", "direct accuracy",
                                                     ifelse(param_label == "p1ma", "P1 Meta-Accuracy",
                                                            ifelse(param_label == "p2ma", "P2 Meta-Accuracy",
                                                                   ifelse(param_label == "as_ac1", "P1 Assumed Accuracy",
                                                                          ifelse(param_label == "as_con1", "P1 Assumed Consensus",
                                                                                 ifelse(param_label == "mp_rec", "P1-P2 Meta-Perception Reciprocity",
                                                                                        ifelse(param_label == "as_ac2", "P2 Assumed Accuracy",
                                                                                               ifelse(param_label == "as_con2", "P2 Assumed Consensus",
                                                                                                      ifelse(param_label == "rec", "direct reciprocity",
                                                                                                             ifelse(param_label == "h", "hearsay reciprocity",
                                                                                                                    ifelse(param_label == "m", "P2(T) <-> opposite P1(T)",
                                                                                                                           ifelse(param_label == "tru_sim", "True Target Similarity",
                                                                                                                                  ifelse(param_label == "as_sim_3p", "Third-Person Assumed Similarity",
                                                                                                                                         ifelse(param_label == "as_sim_1p", "First-Person Assumed Similarity",
                                                                                                                                                ifelse(param_label == "as_sim_p1m", "P1 Meta- Assumed Similarity",
                                                                                                                                                       ifelse(param_label == "ukp1m1", "P1 Meta <-> opposite P1-Report",
                                                                                                                                                              ifelse(param_label == "p1meta_sim", "P1 meta-similarity",
                                                                                                                                                                     ifelse(param_label == "ukp2m1", "P2 Meta <-> opposite target self-report",
                                                                                                                                                                            ifelse(param_label == "ukp2m3", "P2 meta <-> opposite P1-report",
                                                                                                                                                                                   ifelse(param_label == "p2meta_sim", "P2 Meta-similarity",
                                                                                                                                                                                          ifelse(param_label == "ukm1", "P1 Meta <-> opposite P2 Meta", NA)))))))))))))))))))))))) %>%
      dplyr::select(group_label, parameter, est.std, ci.lower, ci.upper, pvalue) %>%
      dplyr::rename(r = est.std,
                    ci_lower = ci.lower,
                    ci_upper = ci.upper)
  }
  # the function above will only get parameters without equality constraints
  # which sort of makes sense for this (it only gives you the results of group moderated analyses).
  # However, you might have a case in which some (but not all) groups or parameters are equal, and you
  # want to table them all together. This adds the equality constrained rows
  rep_parameter_table_eqls <- ez_differential_table(rep_model = rep_model,
                                                    what = what)# %>%
    #mutate(group_label = NA) %>%
    #select(group_label, parameter, r, ci_lower, ci_cupper, pvalue)
  rep_parameter_table <- dplyr::full_join(rep_parameter_table, rep_parameter_table_eqls)
  message("The Model you provided had some between-group equality constraints.
          Those pooled estimates are in the rows where group is marked NA")
  return(rep_parameter_table)}


#' Easily Table Elevation Results
#'
#' This takes output from one of the reputational analysis models
#' (e.g., rep_analyses_auto) and returns a list of two tibbles corresponding to
#' elevation (mean-differences) estimates and the relevant pooled Means and SDs respectively.
#' The elevation table contains the raw difference, 95% CI around the raw difference,
#' z scores, p values, and a cohen's d (difference / pooled SD). It has only 1 parameter.
#'
#' @param rep_model The results from one of the ReputationAnalyses
#' Models (e.g., rep_analyses_auto).
#' @import lavaan tidyverse
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
#' @return The function returns a list of 2 objects of class \code{\link[tibble::tibble()]{tibble}}.

ez_elevation_table <- function(rep_model){

  rep_elevation_table <- rep_model %>%
    parameterestimates() %>%
    tibble::as_tibble() %>%
    dplyr::distinct(label, .keep_all = TRUE) %>%
    dplyr::filter(label == "p1_p2_rel_el"|
                    label == "self_p2_rel_el"|
                    label == "self_p1_rel_el"|
                    label == "p1_meta_rel_el"|
                    label == "p2_meta_rel_el"|
                    # and get the variances for calculating a
                    # cohen's d
                    stringr::str_detect(label, "v_") |
                    stringr::str_detect(label, "int_")) %>%
    dplyr::select(label, est) %>%
    tidyr::spread(label, est)
  # calculate d's
  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "p1_p2_rel_el"))) > 0){
    rep_elevation_table <- dplyr::mutate(rep_elevation_table,
                                         p1_p2_std_d = p1_p2_rel_el / sqrt((v_p1 + v_p2) / 2))}

  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "self_p2_rel_el"))) > 0){
    rep_elevation_table <- dplyr::mutate(rep_elevation_table,
                                         self_p2_std_d = self_p2_rel_el / sqrt((v_self + v_p2) / 2))}

  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "self_p1_rel_el"))) > 0){
    rep_elevation_table <- dplyr::mutate(rep_elevation_table,
                                         self_p1_std_d = self_p1_rel_el / sqrt((v_self + v_p1) / 2))}

  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "p1_meta_rel_el"))) > 0){
    rep_elevation_table <- dplyr::mutate(rep_elevation_table,
                                         p1_meta_std_d = p1_meta_rel_el / sqrt((v_mp1 + v_p2) / 2))}

  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "p2_meta_rel_el"))) > 0){
    rep_elevation_table <- dplyr::mutate(rep_elevation_table,
                                         p2_meta_std_d = p2_meta_rel_el / sqrt((v_mp2 + v_p1) / 2))}

  rep_elevation_table <- rep_elevation_table %>%
    dplyr::select(dplyr::ends_with("std_d")) %>%
    tidyr::gather(label, cohen_d) %>%
    dplyr::mutate(label = stringr::str_replace_all(label, "std_d", "rel_el")) %>%
    dplyr::left_join(parameterestimates(rep_model)) %>%
    # give them their substantive labels
    dplyr::mutate(parameter = ifelse(label == "p1_p2_rel_el", "P1-P2 Relative Elevation",
                                     ifelse(label == "self_p2_rel_el", "Self-P2 Relative Elevation",
                                            ifelse(label == "self_p1_rel_el", "Self-P1 Relative Elevation",
                                                   ifelse(label == "p1_meta_rel_el", "P1 Meta-Elevation",
                                                          ifelse(label == "p2_meta_rel_el", "P2 Meta-Elevation", NA)))))) %>%
    dplyr::select(parameter, est, ci.lower, ci.upper, cohen_d, z, pvalue) %>%
    dplyr::rename(raw_diff = est,
                  ci_lower = ci.lower,
                  ci_upper = ci.upper)

  rep_descrips <- rep_model %>%
    parameterestimates() %>%
    dplyr::distinct(label, .keep_all = TRUE) %>%
    dplyr::select(label, est) %>%
    tidyr::spread(label, est)

  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_self"))) > 0){
    rep_descrips <- dplyr::mutate(rep_descrips,
                                  self_sd = sqrt(v_self))}

  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_p1"))) > 0){
    rep_descrips <- dplyr::mutate(rep_descrips,
                                  p1_sd = sqrt(v_p1))}

  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_p2"))) > 0){
    rep_descrips <- dplyr::mutate(rep_descrips,
                                  p2_sd = sqrt(v_p2))}

  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_mp1"))) > 0){
    rep_descrips <- dplyr::mutate(rep_descrips,
                                  mp1_sd = sqrt(v_mp1))}

  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_mp2"))) > 0){
    rep_descrips <- dplyr::mutate(rep_descrips,
                                  mp2_sd = sqrt(v_mp2))}

  rep_descrips <- rep_descrips %>%
    dplyr::select(dplyr::starts_with("int"), dplyr::ends_with("sd"))
  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "p1_p2_rel_el"))) > 0){
    rep_descrips <- dplyr::rename(rep_descrips,
                                  p1_t_mean = int_p1,
                                  p1_t_sd = p1_sd,
                                  p2_t_mean = int_p2,
                                  p2_t_sd = p2_sd)}

  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_self"))) > 0){
    rep_descrips <- dplyr::rename(rep_descrips,
                                  t_t_mean  = int_self,
                                  t_t_sd    = self_sd)}

  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_mp1"))) > 0){
    rep_descrips <- dplyr::rename(rep_descrips,
                                  p1_p2t_mean = int_mp1,
                                  p1_p2t_sd = mp1_sd)}
  if(sum(as.numeric(stringr::str_detect(parameterestimates(rep_model)$label, "v_mp2"))) > 0){
    rep_descrips <- dplyr::rename(rep_descrips,
                                  p2_p1t_mean = int_mp2,
                                  p2_p1t_sd = mp2_sd)}

  rep_descrips <- rep_descrips %>%
    tidyr::gather(variable, value) %>%
    tidyr::separate(variable, c("perceiver", "target", "stat"), extra = "merge") %>%
    dplyr::mutate(target = stringr::str_replace(target, "p1t", "p1_t"),
                  target = stringr::str_replace(target, "p2t", "p2_t")) %>%
    tidyr::spread(stat, value)


  return(list(elevation_table = rep_elevation_table,
              pooled_means_sd = rep_descrips))}

#' Easy Differential & Elevation Tables
#'
#' This takes output from one of the reputational analysis models
#' (e.g., rep_analyses_auto) and returns a list of length three. The first element of the list is a
#'  a tibble of differential (correlational) parameters.
#'  The second element is a table of elevation results. The third element is a tibble containing
#'  the table of descriptives (means and SDs, pooling across exchangeable roles).
#'
#' @param rep_model The results from one of the ReputationAnalyses
#' Models (e.g., rep_analyses_auto).
#' @param what The parameters you want in the differential table. Current options are
#' main and all. If what = "main", then just the 'main' model parameters are provided.
#' This will include, when avaiable, hearsay consensus, hearsay accuracy,
#' direct accuracy (P1-P1 agreement), P1 Meta-Accuracy, and P2 Meta-Accuracy.
#'
#' @import lavaan tidyverse
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
#' @return The function returns a list of 2 objects of class \code{\link[tibble::tibble()]{tibble}}.

ez_tables <- function(rep_model, what = "main"){
  differential_table <- ez_differential_table(rep_model = rep_model, what = what)
  elevation_table <- ez_elevation_table(rep_model = rep_model)[[1]]
  pooled_means_sd <- ez_elevation_table(rep_model = rep_model)[[2]]

  return(list(differential_table = differential_table,
              elevation_table = elevation_table,
              pooled_means_sd = pooled_means_sd))
}
