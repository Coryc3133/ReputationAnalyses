# This file contains the functions for building and fitting the group moderated reputation models.

#' Reputation Consensus with Group Moderator Model Builder
#'
#' This builds a model of lavaan syntax for estimating the possible hearsay reputation parameters
#' as a multi-group path model given vectors of P1 and P2 reports (vectors of quoted variable names)
#' and a group-level categorical variable.
#' The baseline model estimates each parameter seperately,
#' labelling parameters based on the labels of moderator.
#' Those parameters are:
#' \describe{
#' \item{hc}{hearsay consensus; the correlation between P1(T) & P2(T)}
#' \item{int_p1}{Intercept for P1(T)}
#' \item{int_p2}{Intercept for P2(T)}
#' \item{v_p1}{variance for P1(T)}
#' \item{v_p2}{variance for P2(T)}
#' \item{p1_p2_rel_el}{P1-P2 Relative Elevation (i.e., Mean P1(T) - Mean P2(T))}
#' }
#' \emph{If n exchangeable triads > 1:}
#' \describe{
#' \item{rec}{direct reciprocity; the correlation between opposit P1(T)s (e.g., A(C) <-> C(A))}
#' \item{h}{hearsay reciprocity; the correlation between exchangeable P2(T)s (e.g., B(C) <-> D(A))}
#' \item{m}{unnamed parameter; The correlation between P2(T) and the opposite P1(T) in a group. (e.g., B(C) <-> C(A))}
#' }
#' The function can handle up to n exchangeable triads.
#' @param p1_reports Quoted column names that contain P1 reports,
#' or ratings made by the person that knows the target directly.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param p2_reports Quoted column names that contain P2 reports,
#' or ratings made by the person that knows the target indirectly through the corresponding P1.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param groups Vector of quoted group labels (from group-level categorical moderator).
#' @param n_triads The number of exchangeable triads in each group. By default, this is determined by
#' counting the number of P1 reports. This parameter rarely needs to be changed.
#' @param n_p1s_per_p2s The number of P1s for every P2. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p2s_per_p1s The number of P2s for every P1;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @import lavaan
#' @export
#' @examples
#'  # build the model
#'   agree_consensus_model_grpmod <- rep_consensus_group_mod_builder(p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'   p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
#'   groups = c("Study_1, "Study_2"))
#'
#'  # view the model
#'  agree_consensus_model_grpmod$model
#'
#'  # view the model information
#'  # agree_consensus_model_grpmod$rep_model_info
#'
#' @return The function returns a list containing an
#' object of class \code{\link[tibble:tibble-class]{tibble}} with model information
#' and a string object of the model in lavaan syntax. Model information
#' includes the type of model, the number of exchangeable triads, and the number
#' of p1s per p2s, and the number of p2s per p1s.
rep_consensus_group_mod_builder <- function(p1_reports, p2_reports, groups = NULL, n_triads = length(p1_reports),
                                            n_p1s_per_p2s = 1, n_p2s_per_p1s = 1){
  if(is.null(groups)){warning("You need to supply a group variable to run a group-moderator Reputation Model.
                            If you don't have a group moderator, try rep_consensus if you have no moderator or
                            rep_id_mods_consensus if you have an individual difference moderator.")
    }
  else{
    # get number of groups
    n_groups <- length(groups)
    groups <- paste0("grp", 1:n_groups)
    if(n_triads > 0 &
       n_p1s_per_p2s == 1 &
       n_p2s_per_p1s == 1){
      rep_consensus_model <- rep_consensus_builder(p1_reports, p2_reports, n_triads = length(p1_reports),
                                                   n_p1s_per_p2s = 1, n_p2s_per_p1s = 1)

      # make vector of parameter labels
      # this combines the parameter labels used
      # in unmoderated model with the group labels
      # specified in the function call.
      param_labs <- rep_consensus_model$model %>%
        lavaanify() %>%
        dplyr::select(label) %>%
        dplyr::filter(str_detect(label, "")) %>%
        tidyr::crossing(groups) %>%
        tidyr::unite(grp_labs, groups, label, remove = FALSE) %>%
        dplyr::select(-groups) %>%
        dplyr::distinct() %>%
        split(.$label) %>%
        purrr::map(~dplyr::select(., -label)) %>%
        purrr::map(~str_flatten(.)) %>%
        unlist() %>%
        sort(decreasing = TRUE)

      # Relative Elevation are a little different
      # since they are defined parameters. We'll
      # extract those labels and deal with them separately.
      rel_el_labs <- rep_consensus_model$model %>%
        lavaanify() %>%
        dplyr::select(label) %>%
        dplyr::filter(str_detect(label, "")) %>%
        tidyr::crossing(groups) %>%
        tidyr::unite(grp_labs, groups, label, remove = FALSE) %>%
        dplyr::select(-groups) %>%
        dplyr::distinct() %>%
        split(.$label) %>%
        purrr::map(~dplyr::select(., -label)) %>%
        tibble::as_tibble() %>%
        dplyr::select(dplyr::contains("rel_el"))

      # Relative Elevation requires subtracting
      # the pooled intercepts, so we need to separate
      # out a df of those too.
      int_labs <- rep_consensus_model$model %>%
        lavaanify() %>%
        dplyr::select(label) %>%
        dplyr::filter(str_detect(label, "")) %>%
        tidyr::crossing(groups) %>%
        tidyr::unite(grp_labs, groups, label, remove = FALSE) %>%
        dplyr::select(-groups) %>%
        dplyr::distinct() %>%
        split(.$label) %>%
        purrr::map(~dplyr::select(., -label)) %>%
        tibble::as_tibble() %>%
        dplyr::select(dplyr::contains("int_"))

      # code for 1 triad - much simpler
      if(n_triads == 1){

        model <-
          # hearsay (P1-P2) consensus
          paste(paste(p1_reports, param_labs["hc"], "*", p2_reports),
                # intercepts
                paste(p1_reports, "~", param_labs["int_p1"], "*1"),
                paste(p2_reports, "~", param_labs["int_p2"], "*1"),

                # variances
                paste(p1_reports, "~~", param_labs["v_p1"], "*", p1_reports),

                paste(p2_reports, "~~", param_labs["v_p2"], "*", p2_reports, "\n"), sep = "\n") %>%
          stringr::str_flatten(collaps = "\n")
      }
      # code for 2 triads
      if(n_triads > 1 &
         n_p1s_per_p2s == 1 &
         n_p2s_per_p1s == 1){
        # create empty model
        model <- ""
        for(i in 1:n_triads){
          # cross-target correlations
          if(i < n_triads){
            prev_i <- i:1
            m   <- paste(p1_reports[i], param_labs["m"], "*", p2_reports[-prev_i]) %>% stringr::str_flatten(collapse = "\n")
            rec <- paste(p1_reports[i], param_labs["rec"], "*", p1_reports[-prev_i]) %>% stringr::str_flatten(collapse = "\n")
            h   <-  paste(p2_reports[i],param_labs["h"], "*", p2_reports[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
            xtrs <- paste(m, rec, h, sep = "\n")
            model <- paste(model, xtrs)}}
        hc <- paste(p1_reports, param_labs["hc"], "*", p2_reports) %>% stringr::str_flatten(collapse = "\n")
        int_p1 <- paste(p1_reports, "~", param_labs["int_p1"], "*1") %>% stringr::str_flatten(collapse = "\n")
        int_p2 <- paste(p2_reports, "~", param_labs["int_p2"], "*1") %>% stringr::str_flatten(collapse = "\n")
        v_p1 <- paste(p1_reports, "~~", param_labs["v_p1"], "*", p1_reports) %>% stringr::str_flatten(collapse = "\n")
        v_p2 <- paste(p2_reports, "~~", param_labs["v_p2"], "*", p2_reports) %>% stringr::str_flatten(collapse = "\n")

        model <- paste(hc, model,  int_p1, int_p2, v_p1, v_p2, sep = "\n")
      }
      # define relative elevation for each group
      # and paste that into the model.
      for(i in 1:n_groups){
        rel_el <- paste(rel_el_labs[[1]][i,], ":=", "1*", int_labs[[1]][i,], "- (1*", int_labs[[2]][i,], ")")
        model <- paste(model, rel_el, sep = "\n")
      }
      # Put the model info together.
      rep_model_info <- tibble::as_tibble(list(model_type = "Simple Hearsay Consensus (P1-P2) with Group Moderator",
                                               ex_triads = n_triads,
                                               n_groups = n_groups,
                                               p1s_per_p2s = n_p1s_per_p2s,
                                               p2s_per_p1s = n_p2s_per_p1s))
      return(list(model = model,
                  rep_model_info = rep_model_info))}
    if(n_p1s_per_p2s > 1){print("I'm sorry, this function can only handle designs with 1 P1 per P2; check back for changes")}
    if(n_p2s_per_p1s > 1){print("I'm sorry, this function can only handle designs with 1 P1 per P2; check back for changes")}
  }
}

#' Reputation Consensus with Group Moderator Model
#'
#' This fits a model estimating the possible hearsay reputation parameters
#' as a multi-group path model given vectors of P1 and P2 reports (vectors of quoted variable names)
#' and a group-level categorical variable.
#' The baseline model estimates each parameter seperately,
#' labelling parameters based on the labels of moderator variable.
#' Those parameters are:
#' \describe{
#' \item{hc}{hearsay consensus; the correlation between P1(T) & P2(T)}
#' \item{int_p1}{Intercept for P1(T)}
#' \item{int_p2}{Intercept for P2(T)}
#' \item{v_p1}{variance for P1(T)}
#' \item{v_p2}{variance for P2(T)}
#' \item{p1_p2_rel_el}{P1-P2 Relative Elevation (i.e., Mean P1(T) - Mean P2(T))}
#' }
#' \emph{If n exchangeable triads > 1:}
#' \describe{
#' \item{rec}{direct reciprocity; the correlation between opposit P1(T)s (e.g., A(C) <-> C(A))}
#' \item{h}{hearsay reciprocity; the correlation between exchangeable P2(T)s (e.g., B(C) <-> D(A))}
#' \item{m}{unnamed parameter; The correlation between P2(T) and the opposite P1(T) in a group. (e.g., B(C) <-> C(A))}
#' }
#' The function can handle up to n exchangeable triads.
#' @param data The dataframe that contains P1 & P2 ratings and the group-level moderator.
#' Data should be wide, with a row for every group of participants.
#' At a minimum, it must contain three columns: one for P1 reports, one for P2 reports, and one for the group-level moderator.
#' @param p1_reports Quoted column names that contain P1 reports,
#' or ratings made by the person that knows the target directly.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param p2_reports Quoted column names that contain P2 reports,
#' or ratings made by the person that knows the target indirectly through the corresponding P1.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param group_mod The quoted column name that contains a group-level categorical moderator.
#' @param n_triads The number of exchangeable triads in each group. By default, this is determined by
#' counting the number of P1 reports. This parameter rarely needs to be changed.
#' @param n_p1s_per_p2s The number of P1s for every P2. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p2s_per_p1s The number of P2s for every P1;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @import lavaan
#' @export
#' @examples data("rep_sim_data")
#'           agree_consensus_grpmod <- rep_consensus_group_mod(data = rep_sim_data,
#'                                            p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                                            p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
#'                                            group = "study")
#'          # alternatively
#'          # build the model
#'           agree_consensus_grpmod_model <- rep_consensus_group_mod_builder(p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                                                              p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
#'                                                              group = levels(rep_sim_data$study))
#'          # then fit it
#'          agree_consensus_grpmod <- rep_consensus_group_mod(data = rep_sim_data,
#'                                                            agree_consensus_model)
#'
#'
#' @return The function returns an object of class \code{\link[lavaan:lavaan-class]{lavaan}}.

rep_consensus_group_mod <- function(data, model = NULL, p1_reports, p2_reports, group_mod = NULL, n_triads = length(p1_reports),
                          n_p1s_per_p2s = 1, n_p2s_per_p1s = 1){
  if(is.null(group_mod)){warning("You need to supply a group variable to run a group-moderator Reputation Model.
                            If you don't have a group moderator, try rep_consensus if you have no moderator or
                            rep_id_mods_consensus if you have an individual difference moderator.")
  }
  else{
    groups <- levels(as.factor(data[,group_mod]))
    if(is.null(model)){
      rep_consensus_groups_model <- rep_consensus_group_mod_builder(p1_reports, p2_reports, groups, n_triads = length(p1_reports),
                                                                    n_p1s_per_p2s = 1, n_p2s_per_p1s = 1)}

    else{rep_consensus_groups_model <- model}
    fitted_model <- lavaan::sem(rep_consensus_groups_model$model, data = data, missing = "FIML", group = group_mod)
    return(fitted_model)}
  }

#' Reputation Consensus & Accuracy with Group Moderator Model Builder
#'
#' This builds a model of lavaan syntax for estimating the possible hearsay reputation parameters
#' as a multi-group path model given vectors of P1, P2, and target self-reports
#' (vectors of quoted variable names) and a group-level categorical variable.
#' The baseline model estimates each parameter seperately,
#' labelling parameters based on the labels of moderator.
#' Those parameters are:
#' \describe{
#' \item{hc}{hearsay consensus; the correlation between P1(T) & P2(T)}
#' \item{ha}{hearsay accuracy; the correation between P2(T) & T(T)}
#' \item{da}{direct accuracy; the correlation between P1(T) & T(T)}
#' \item{int_p1}{Intercept for P1(T)}
#' \item{int_p2}{Intercept for P2(T)}
#' \item{int_self}{Intercept for T(T)}
#' \item{v_p1}{variance for P1(T)}
#' \item{v_p2}{variance for P2(T)}
#' \item{v_self}{variance for T(T)}
#' \item{p1_p2_rel_el}{P1-P2 Relative Elevation (i.e., Mean P1(T) - Mean P2(T))}
#' \item{self_p2_rel_el}{Self-P2 Relative Elevation (i.e., Mean T(T) - Mean P2(T))}
#' \item{self_p1_rel_el}{Self-P1 Relative Elevation (i.e., Mean T(T) - Mean P1(T))}
#' }
#' \emph{If n exchangeable triads > 1:}
#' \describe{
#' \item{rec}{direct reciprocity; the correlation between opposit P1(T)s (e.g., A(C) <-> C(A))}
#' \item{h}{hearsay reciprocity; the correlation between exchangeable P2(T)s (e.g., B(C) <-> D(A))}
#' \item{m}{unnamed parameter; The correlation between P2(T) and the opposite P1(T) in a group. (e.g., B(C) <-> C(A))}
#' \item{tru_sim}{True Similarity; the correlation between targets' self-reports. (e.g., A(A) <-> C(C))}
#' \item{as_sim_3p}{Third-person assumed similarity; correlation between P2(T) and P1's self-report (e.g., B(C) <- A(A))}
#' \item{as_sim_1p}{First-person assumed similarity (i.e., interpersonal assumed similarity); correlation betweenP1(T) and P1's self-report (e.g., A(C) <-> A(A))}
#' }
#' The function can handle up to n exchangeable triads.
#' @param data The dataframe.
#' Data should be wide, with a row for every group of participants.
#' At a minimum, it must contain three columns: one for P1-reports, one for P2-reports, and
#' one for targets' self-ratings.
#' @param model Optional. A model from the corresponding ReputationAnalyses model builder function. If this
#' is supplied, no additional arguments need to be specified.
#' @param  p1_reports Quoted column names that contain P1 reports,
#' or ratings made by the person that knows the target directly.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param p2_reports Quoted column names that contain P2 reports,
#' or ratings made by the person that knows the target indirectly through the corresponding P1.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param target_self Quoted column names that contain target self-reports.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param groups Vector of quoted group labels (from group-level categorical moderator).
#' @param n_triads The number of exchangeable triads in each group. By default, this is determined by
#' counting the number of P1 reports. It is rare that this parameter would need to be changed.
#' @param n_p1s_per_p2s The number of P1s for every P2. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p2s_per_p1s The number of P2s for every P1;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p1s_per_ts The number of P1s for every target;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p2s_per_ts The number of P2s for every target;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_ts_per_p1s The number of targets for every P1;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_ts_per_p2s The number of targets for every P2;. This defaults to 1.
#' Currently, only values of 1 are supported.
#'
#' @import lavaan
#' @export
#' @examples
#'  # build the model
#'   agree_con_acc_model_grpmod <- rep_con_acc_group_mod_builder(p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'   p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
#'   target_self = c("C_C_agreeableness", "A_A_agreeableness"),
#'   groups = c("Study_1", "Study_2"))
#'
#'  # view the model
#'  agree_consensus_model_grpmod$model
#'
#'  # view the model information
#'  # agree_consensus_model_grpmod$rep_model_info
#'
#' @return The function returns a list containing an
#' object of class \code{\link[tibble:tibble-class]{tibble}} with model information
#' and a string object of the model in lavaan syntax. Model information
#' includes the type of model, the number of exchangeable triads, and the number
#' of p1s per p2s, and the number of p2s per p1s.
rep_con_acc_group_mod_builder <- function(p1_reports, p2_reports, target_self,
                                           groups = NULL, n_triads = length(p1_reports),
                                            n_p1s_per_p2s = 1, n_p2s_per_p1s = 1){
  if(is.null(groups)){warning("You need to supply a group variable to run a group-moderator Reputation Model.
                              If you don't have a group moderator, try rep_consensus if you have no moderator or
                              rep_id_mods_consensus if you have an individual difference moderator.")
  }
  else{
    # get number of groups
    n_groups <- length(groups)
    groups <- paste0("grp", 1:n_groups)
    if(n_triads > 0 &
       n_p1s_per_p2s == 1 &
       n_p2s_per_p1s == 1){
      rep_consensus_model <- rep_consensus_accuracy_builder(p1_reports, p2_reports, target_self,
                                                            n_triads = length(p1_reports),
                                                            n_p1s_per_p2s = 1, n_p2s_per_p1s = 1)

      # make vector of parameter labels
      # this combines the parameter labels used
      # in unmoderated model with the group labels
      # specified in the function call.
      param_labs <- rep_consensus_model$model %>%
        lavaanify() %>%
        dplyr::select(label) %>%
        dplyr::filter(str_detect(label, "")) %>%
        tidyr::crossing(groups) %>%
        tidyr::unite(grp_labs, groups, label, remove = FALSE) %>%
        dplyr::select(-groups) %>%
        dplyr::distinct() %>%
        split(.$label) %>%
        purrr::map(~dplyr::select(., -label)) %>%
        purrr::map(~str_flatten(.)) %>%
        unlist() %>%
        sort(decreasing = TRUE)

      # Relative Elevation are a little different
      # since they are defined parameters. We'll
      # extract those labels and deal with them separately.
      rel_el_labs <- rep_consensus_model$model %>%
        lavaanify() %>%
        dplyr::select(label) %>%
        dplyr::filter(str_detect(label, "")) %>%
        tidyr::crossing(groups) %>%
        tidyr::unite(grp_labs, groups, label, remove = FALSE) %>%
        dplyr::select(-groups) %>%
        dplyr::distinct() %>%
        split(.$label) %>%
        purrr::map(~dplyr::select(., -label)) %>%
        tibble::as_tibble() %>%
        dplyr::select(dplyr::contains("rel_el"))

      # Relative Elevation requires subtracting
      # the pooled intercepts, so we need to separate
      # out a df of those too.
      int_labs <- rep_consensus_model$model %>%
        lavaanify() %>%
        dplyr::select(label) %>%
        dplyr::filter(str_detect(label, "")) %>%
        tidyr::crossing(groups) %>%
        tidyr::unite(grp_labs, groups, label, remove = FALSE) %>%
        dplyr::select(-groups) %>%
        dplyr::distinct() %>%
        split(.$label) %>%
        purrr::map(~dplyr::select(., -label)) %>%
        tibble::as_tibble() %>%
        dplyr::select(dplyr::contains("int_"))

      # code for 1 triad - much simpler
      if(n_triads == 1){

        model <-
                # hearsay consensus (P1-P2 agreement)
          paste(paste(p1_reports, param_labs["hc"], "*", p2_reports),
                # hearsay accuracy (P2-self agreement)
                paste(p2_reports, param_labs["ha"], "*", target_self),
                #direct accuracy (P1-self agreement)
                paste(p1_reports, param_labs["da"], "*", target_self),




                # intercepts
                paste(p1_reports, "~", param_labs["int_p1"], "*1"),
                paste(p2_reports, "~", param_labs["int_p2"], "*1"),
                paste(target_self, "~", param_labs["int_self"], "*1"),

                # variances
                paste(p1_reports, "~~", param_labs["v_p1"], "*", p1_reports),

                paste(p2_reports, "~~", param_labs["v_p2"], "*", p2_reports, "\n"),
                paste(target_self, "~~", param_labs["v_p2"], "*", target_self, "\n"), sep = "\n") %>%
          stringr::str_flatten(collaps = "\n")
      }
      # code for 2 triads
      if(n_triads > 1 &
         n_p1s_per_p2s == 1 &
         n_p2s_per_p1s == 1){
        # create empty model
        model <- ""
        for(i in 1:n_triads){
          # cross-target correlations
          if(i < n_triads){
            prev_i <- i:1
            m   <- paste(p1_reports[i], param_labs["m"], "*", p2_reports[-prev_i]) %>% stringr::str_flatten(collapse = "\n")
            rec <- paste(p1_reports[i], param_labs["rec"], "*", p1_reports[-prev_i]) %>% stringr::str_flatten(collapse = "\n")
            h   <-  paste(p2_reports[i], param_labs["h"], "*", p2_reports[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
            tru_sim <- paste(target_self[i], param_labs["tru_sim"], "*", target_self[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
            as_sim_3p <- paste(p2_reports[i], param_labs["as_sim_3p"], "*", target_self[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
            as_sim_1p <- paste(p1_reports[i], param_labs["as_sim_1p"], "*", target_self[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
            xtrs <- paste(m, rec, h, tru_sim, as_sim_3p, as_sim_1p, sep = "\n")
            model <- paste(model, xtrs)}}
        # params shared with single triad model (within triad correlations)
        hc <- paste(p1_reports, param_labs["hc"], "*", p2_reports) %>% stringr::str_flatten(collapse = "\n")
        ha <- paste(target_self, param_labs["ha"], "*", p2_reports) %>% stringr::str_flatten(collapse = "\n")
        da <- paste(target_self, param_labs["da"], "*", p1_reports) %>% stringr::str_flatten(collapse = "\n")

        int_p1 <- paste(p1_reports, "~", param_labs["int_p1"], "*1") %>% stringr::str_flatten(collapse = "\n")
        int_p2 <- paste(p2_reports, "~", param_labs["int_p2"], "*1") %>% stringr::str_flatten(collapse = "\n")
        int_self <- paste(target_self, "~", param_labs["int_self"], "*1") %>% stringr::str_flatten(collapse = "\n")
        v_p1 <- paste(p1_reports, "~~", param_labs["v_p1"], "*", p1_reports) %>% stringr::str_flatten(collapse = "\n")
        v_p2 <- paste(p2_reports, "~~", param_labs["v_p2"], "*", p2_reports) %>% stringr::str_flatten(collapse = "\n")
        v_self <- paste(target_self, "~~", param_labs["v_self"], "*", target_self) %>% stringr::str_flatten(collapse = "\n")

        model <- paste(hc, ha, da, model,  int_self, int_p1,
                       int_p2, v_self, v_p1, v_p2, sep = "\n")
      }
      # define relative elevation for each group
      # and paste that into the model.
      for(i in 1:n_groups){
        p1_p2_rel_el <- paste(rel_el_labs[[1]][i,], ":=", "1*", int_labs[[1]][i,], "- (1*", int_labs[[2]][i,], ")")
        self_p1_rel_el <- paste(rel_el_labs[[2]][i,], ":=", "1*", int_labs[[3]][i,], "- (1*", int_labs[[1]][i,], ")")
        self_p2_rel_el <-  paste(rel_el_labs[[3]][i,], ":=", "1*", int_labs[[3]][i,], "- (1*", int_labs[[2]][i,], ")")
        model <- paste(model, p1_p2_rel_el, self_p1_rel_el, self_p2_rel_el, sep = "\n")
      }
      # Put the model info together.
      rep_model_info <- tibble::as_tibble(list(model_type = "Simple Hearsay Consensus (P1-P2) with Group Moderator",
                                               ex_triads = n_triads,
                                               n_groups = n_groups,
                                               p1s_per_p2s = n_p1s_per_p2s,
                                               p2s_per_p1s = n_p2s_per_p1s))
      return(list(model = model,
                  rep_model_info = rep_model_info))}
    if(n_p1s_per_p2s > 1){print("I'm sorry, this function can only handle designs with 1 P1 per P2; check back for changes")}
    if(n_p2s_per_p1s > 1){print("I'm sorry, this function can only handle designs with 1 P1 per P2; check back for changes")}
  }
}
