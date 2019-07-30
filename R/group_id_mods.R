#' Generic Individual-Level Moderator with Group Moderator Builder
#'
#' This is a generic function for building a lavaan model for
#' individual-level moderators on two distinguishable ratings on the same target across multiple groups..
#' This could be P1- and P2- reports, P2- and self-reports, P1- and self-reports, or any other sets of
#' distinguishable ratings.
#'
#' The parameters for the individual-level moderator analyses are:
#'
#' \describe{
#' \item{rating_me}{main effect of other rating; this should correspond to correlation between ratings at average
#' level of moderator variable (if data were properly mean-centered).}
#' \item{mod_me}{The meain effect of the moderator variable; it can be interpreted as the difference in
#' rating_1 to differences in the individual-level moderator variable.}
#' \item{interaction}{This is the interaction term. It indicates the extent to which
#' the correlation between ratings depends on the moderator variable}
#' \item{v_rating_1}{variance for first rating}
#' \item{v_rating_2}{variance for second rating}
#' \item{v_mod}{variance for moderator variable}
#' \item{v_interaction}{variance for interaction term}
#' \item{int_rating_1}{intercept for first rating}
#' \item{int_rating_2}{intercept for second rating}
#' \item{int_mod}{intercept for moderator variable}
#' \item{int_interaction}{intercept for interaction term}
#' }
#' The function can handle up to n exchangeable triads.
#' @param rating_1 Quoted column names that contain  the first rating variable. This might be P1 reports
#' if investigating moderation of hearsay consensus or self-reports for moderation of hearsay accuracy.
#' If more than one is supplied, the target-wise order must match across variables.
#' @param rating_2 Quoted column names that contain second rating variable. For hearsay consensus or accuracy,
#' this would be P2 reports. If more than one is supplied, the target-wise order must match across variables.
#' @param id_mod_variable Quoted column names that contain the individual-level moderator of interest.
#' If more than one is supplied from multiple exchangeable dyads/triads,
#' the order must match the order of the ratings. Like P2-reports, the variable should be mean-centered
#' to facilitate interpretability.
#' @param interaction_term Quoted column names that contain the interaction term, or the product of the
#' mean-centered P2-report and the mean-centered moderator variable. If more than one is supplied from multiple
#' exchangeable dyads/triads, the target-wise order must match the order of the ratings.
#' @param groups Vector of quoted group labels (from group-level categorical moderator).
#' @param use_labs Logical indicating whether or not to use the group labels to create the parameter labels.
#' If FALSE, generic labels (grp1 to grpk, where k is the number of groups) are used.
#' @param n_triads The number of exchangeable triads in each group. By default, this is determined by
#' counting the number of P1 reports. This parameter rarely needs to be changed.
#' @param n_r1_per_r2 The number of first ratings for each second rating. Currently, only 1:1 is supported.
#' @param n_r2_per_r1 The number of second ratings for each first rating. Currently, only 1:1 is supported.
#' @import magrittr stringr lavaan
#' @export
#' @examples data("rep_sim_data")
#'           # Prepare data
#'           moderator_data <- rep_sim_data %>%
#'            dplyr::mutate(B_C_agreeableness_cent = scale(B_C_agreeableness, scale = FALSE),
#'                   D_A_agreeableness_cent = scale(D_A_agreeableness, scale = FALSE),
#'                   B_iri_perspective_cent = scale(B_iri_perspective, scale = FALSE),
#'                   D_iri_perspective_cent = scale(D_iri_perspective, scale = FALSE),
#'                   B_ptXagree_interaction = B_C_agreeableness_cent*B_iri_perspective_cent,
#'                   D_ptXagree_interaction = D_A_agreeableness_cent*D_iri_perspective_cent)
#'
#' # build a model examining perspective taking moderating hearsay consensus across two studies
#' agree_pt_mod_model <- rep_generic_group_id_mods_builder (rating_1 = c("C_C_agreeableness", "A_A_agreeableness"),
#'                                                      rating_2 = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                                      id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                                      interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"),
#'                                                      groups = c("study1", "study2"))
#'
#' @return The function returns an object of class \code{\link[lavaan:lavaan-class]{lavaan}}.

rep_generic_group_id_mods_builder <- function(rating_1, rating_2, id_mod_variable,
                                              interaction_term, groups = NULL, use_labs = TRUE,
                                              n_triads = length(rating_1),
                                              n_r1_per_r2 = 1, n_r2_per_r1 = 1){
  # Global Variable Binding
  label <- grp_labs <- . <- NULL

  if(is.null(groups)){warning("You need to supply a group variable to run a group-moderator Reputation Model.
                              If you don't have a group moderator, try try rep_analyses_auto if you have no moderator or
                              rep_auto_id_mods if you have an individual difference moderator.")
  }
  else{
    # get number of groups
    n_groups <- length(groups)
    # Make sure each group label is unique
    if(length(unique(groups)) != n_groups){stop("You provided one or more non-unique group labels. Each group labeel provided needs to be unique.")}
    # Make sure group labels aren't numbers, which
    # screw up the lavaan syntax. If they are, change use_labs
    # to TRUE, which creates generic labels that will work.
    if(!(is.numeric(groups)) && use_labs == TRUE){
      use_labs = FALSE
      message("Labels are numeric variables and use_labs was set to TRUE. This creates
              problems in the underlying lavaan syntax. use_labs is being set to FALSE,
              creating group labels of grp1 to grpk where k is the number of groups")
    }
    # Create labels
    # if use_labs is true, then labels are made
    # based on the group labels used in groups =
    # argument.
    # removing _ to make it clearer that whole string is part of label
    # should make model syntax a little cleaner. Also removing symbols that could cause issues
    if(use_labs == TRUE){
      groups <- str_replace_all(groups, c("_" = "",
                                          "/" = ""))
      if(sum(str_count(groups)) > 5){message("group labels exceed 5 characters; parameter labels may be very verbose.")}
    }
    if(use_labs == FALSE){
      groups <- paste0("grp", 1:n_groups)
    }
    if(n_triads > 0 &&
       n_r1_per_r2 == 1 &&
       n_r1_per_r2 == 1){

      rep_model <- rep_generic_id_mods_builder(rating_1 = rating_1, rating_2 = rating_2,
                                               id_mod_variable = id_mod_variable, interaction_term = interaction_term,
                                               n_triads = length(rating_1), n_r1_per_r2 = n_r1_per_r2, n_r2_per_r1 = n_r2_per_r1)

      # make vector of parameter labels
      # this combines the parameter labels used
      # in unmoderated model with the group labels
      # specified in the function call.
      param_labs <- rep_model$model %>%
        lavaanify(fixed.x = FALSE) %>%
        dplyr::select(.data$label) %>%
        dplyr::filter(str_detect(.data$label, "")) %>%
        tidyr::crossing(groups) %>%
        tidyr::unite(grp_labs, .data$groups, .data$label, remove = FALSE) %>%
        dplyr::select(-.data$groups) %>%
        dplyr::distinct() %>%
        split(.$label) %>%
        purrr::map(~dplyr::select(., -.data$label)) %>%
        purrr::map(~str_flatten(.)) %>%
        unlist() %>%
        sort(decreasing = TRUE)

      model <-
        # moderated regression model
        paste(paste(rating_1, "~", param_labs["rating_me"], "*", rating_2, "+",
                    param_labs["mod_me"], "*", id_mod_variable, "+",
                    param_labs["interaction"], "*", interaction_term),

              # variances
              paste(rating_1,       "~~", param_labs["v_rating_1"], "*", rating_1),
              paste(rating_2,       "~~", param_labs["v_rating_2"], "*", rating_2),
              paste(id_mod_variable,  "~~", param_labs["v_mod"], "*", id_mod_variable),
              paste(interaction_term, "~~", param_labs["v_interaction"], "*", interaction_term),

              # intercepts
              paste(rating_1,       "~", param_labs["int_rating_1"], "*1"),
              paste(rating_2,       "~", param_labs["int_rating_2"], "*1"),
              paste(id_mod_variable,  "~", param_labs["int_mod"], "*1"),
              paste(interaction_term, "~", param_labs["int_interaction"], "*1"),
              sep = "\n")
      # Put the model info together.
      rep_model_info <- tibble::as_tibble(list(model_type = "Individual-level moderator on Hearsay Consensus",
                                               ex_triads = n_triads,
                                               r1_per_r2 = n_r1_per_r2,
                                               r2_per_r1 = n_r2_per_r1))
    }
  }
  return(list(model = model,
              rep_model_info = rep_model_info))
}

#' Generic Individual-Level Moderator and Group Moderators
#'
#' This is a generic function for fitting a lavaan model for
#' individual-level moderators on two distinguishable ratings on the same target across multiple groups.
#' This could be P1- and P2- reports, P2- and self-reports, P1- and self-reports, or any other sets of
#' distinguishable ratings.
#'
#' The parameters estimated by this model are:
#' \describe{
#' \item{rating_me}{main effect of other rating; this should correspond to correlation between ratings at average
#' level of moderator variable (if data were properly mean-centered).}
#' \item{mod_me}{The meain effect of the moderator variable; it can be interpreted as the difference in
#' rating_1 to differences in the individual-level moderator variable.}
#' \item{interaction}{This is the interaction term. It indicates the extent to which
#' the correlation between ratings depends on the moderator variable}
#' \item{v_rating_1}{variance for first rating}
#' \item{v_rating_2}{variance for second rating}
#' \item{v_mod}{variance for moderator variable}
#' \item{v_interaction}{variance for interaction term}
#' \item{int_rating_1}{intercept for first rating}
#' \item{int_rating_2}{intercept for second rating}
#' \item{int_mod}{intercept for moderator variable}
#' \item{int_interaction}{intercept for interaction term}
#' }
#' The function can handle up to n exchangeable triads.
#' @param data The dataframe.
#' Data should be wide, with a row for every group of participants.
#' At a minimum, it must contain five columns: one for rating 1, one for rating 2, one for the individual
#' difference moderator, one for the interaction term, and one for the group moderated variable.
#' @param model Optional. A model from the corresponding ReputationModelR model builder function. If this
#' is supplied, no additional arguments need to be specified.
#' @param  rating_1 Quoted column names that contain  the first rating variable. This might be P1 reports
#' if investigating moderation of hearsay consensus or self-reports for moderation of hearsay accuracy.
#' If more than one is supplied, the target-wise order must match across variables.
#' @param rating_2 Quoted column names that contain second rating variable. For hearsay consensus or accuracy,
#' this would be P2 reports. If more than one is supplied, the target-wise order must match across variables.
#' @param id_mod_variable Quoted column names that contain the individual-level moderator of interest.
#' If more than one is supplied from multiple exchangeable dyads/triads,
#' the order must match the order of the ratings. Like P2-reports, the variable should be mean-centered
#' to facilitate interpretability.
#' @param interaction_term Quoted column names that contain the interaction term, or the product of the
#' mean-centered P2-report and the mean-centered moderator variable. If more than one is supplied from multiple
#' exchangeable dyads/triads, the target-wise order must match the order of the ratings.
#' @param group_mod The quoted column name that contains a group-level categorical moderator.
#' @param use_labs Logical indicating whether or not to use the group labels to create the parameter labels.
#' If FALSE, generic labels (grp1 to grpk, where k is the number of groups) are used.
#' @param groups_eql Optional. Groups that you want to constrain to be equal across some or all parameters. If
#' you have use_labs set to TRUE, provide a vector of group labels corresponding to the groups you want to constrain to be equal. If you
#' have use_labs set to FALSE, provide a vector of numbers corresponding to the position of the groups you want to constrain to be equal. If you provide
#' "all", all groups will be constrained to be equal.
#' @param params_eql Optional. Parameters that you want to constrain to be equal across the groups specified in groups_eql. You can provide
#' one or more specific parameters (e.g., "hc_me" for hearsay consensus main effect), or use a built-in options including "all" which constrains
#' all parameters to be equal across groups.
#' @param n_triads The number of exchangeable triads in each group. By default, this is determined by
#' counting the number of P1 reports. This parameter rarely needs to be changed.
#' @param n_r1_per_r2 The number of first ratings for each second rating. Currently, only 1:1 is supported.
#' @param n_r2_per_r1 The number of second ratings for each first rating. Currently, only 1:1 is supported.
#' @import magrittr stringr lavaan
#' @export
#' @examples data("rep_sim_data")
#'           # Prepare data
#'          moderator_data <- rep_sim_data %>%
#'            dplyr::mutate(B_C_agreeableness_cent = scale(B_C_agreeableness, scale = FALSE),
#'                   D_A_agreeableness_cent = scale(D_A_agreeableness, scale = FALSE),
#'                   B_iri_perspective_cent = scale(B_iri_perspective, scale = FALSE),
#'                   D_iri_perspective_cent = scale(D_iri_perspective, scale = FALSE),
#'                   B_ptXagree_interaction = B_C_agreeableness_cent*B_iri_perspective_cent,
#'                   D_ptXagree_interaction = D_A_agreeableness_cent*D_iri_perspective_cent)
#'
#' # build a bsaeline model examining perspective taking moderating hearsay consensus across groups (studies in this case)
#' agree_pt_mod_model <- rep_generic_group_id_mods (moderator_data,
#'                                            rating_1 = c("C_C_agreeableness", "A_A_agreeableness"),
#'                                            rating_2 = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                            id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                            interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"),
#'                                            group_mod = "study")
#'
#'          # fit model with group equality constraints:
#'          # if we wanted to constrain all parameters to be equal across the 2 groups, that can be done
#'          # by setting groups_eql and params_eql both to "all".
#' agree_pt_mod_model <- rep_generic_group_id_mods (moderator_data,
#'                                            rating_1 = c("C_C_agreeableness", "A_A_agreeableness"),
#'                                            rating_2 = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                            id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                            interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"),
#'                                            group_mod = "study", groups_eql = "all", params_eql = "all")
#'
#' @return The function returns an object of class \code{\link[lavaan:lavaan-class]{lavaan}}.

rep_generic_group_id_mods <- function(data, model = NULL, rating_1, rating_2, id_mod_variable, interaction_term,
                                      group_mod = NULL, use_labs = TRUE, groups_eql = "none", params_eql = "none",
                                      n_triads = length(rating_1), n_r1_per_r2 = 1, n_r2_per_r1 = 1){
  if(is.null(group_mod)){warning("You need to supply a group variable to run a group-moderator Reputation Model.
                                 If you don't have a group moderator, try rep_consensus if you have no moderator or
                                 rep_id_mods_consensus if you have an individual difference moderator.")
  }


  else{
    # Note that lavaan orders groups based on the order
    # they appear in the datafile. Everything I wrote does
    # alphabetical to match default factor level conventions.
    # re-arranging data to be in alphabetical order by
    # group moderator to make lavaan's method consistent with
    # what I've written.
    data <- data[order(data[,group_mod]),]
    groups <- levels(as.factor(data[,group_mod]))

    # Make sure group labels aren't numbers, which
    # screw up the lavaan syntax. If they are, change use_labs
    # to TRUE, which creates generic labels that will work.
    if(!(is.numeric(groups)) && use_labs == TRUE){
      use_labs = FALSE
      message("Labels are numeric variables and use_labs was set to TRUE. This creates
              problems in the underlying lavaan syntax. use_labs is being set to FALSE,
              creating group labels of grp1 to grpk where k is the number of groups")
    }
    if(is.null(model)){
      rep_generic_group_id_model <- rep_generic_group_id_mods_builder(rating_1 = rating_1, rating_2 = rating_2,
                                                                      id_mod_variable = id_mod_variable, interaction_term = interaction_term,
                                                                      groups = groups, use_labs = use_labs, n_triads = length(rating_1),
                                                                      n_r1_per_r2 = n_r1_per_r2, n_r2_per_r1 = n_r2_per_r1)}

    else{rep_generic_group_id_model <- model}


    if(!("none" %in% params_eql) &&
       !("all" %in% groups_eql) &&
       use_labs == FALSE){
      groups_eql <- paste0("grp", groups_eql)
    }

    if("all" %in% groups_eql && use_labs == FALSE){
      groups_eql <- paste0("grp", c(1:length(groups)))
    }

    if("all" %in% groups_eql && use_labs == TRUE){
      groups_eql <- groups
    }

    if("all" %in% params_eql){
      params_eql <- c("rating_me", "mod_me", "interaction",
                      "int_rating_1", "int_rating_2", "int_mod", "int_interaction",
                      "v_rating_", "v_rating_2", "v_mod", "v_interaction")
    }

    if(!("none" %in% groups_eql)){
      groups_eql <- groups_eql %>% str_remove_all("_")
      eql_labs <- expand.grid(groups_eql, params_eql)
      old_labs <- paste(eql_labs$Var1, eql_labs$Var2, sep = "_")
      new_labs <- eql_labs$Var2 %>% as.character()
      names(new_labs) <- old_labs
      rep_generic_group_id_model$model <- str_replace_all(rep_generic_group_id_model$model, new_labs)
    }
    fitted_model <- lavaan::sem(rep_generic_group_id_model$model, data = data, missing = "FIML", group = group_mod, fixed.x = FALSE)
    return(fitted_model)}
}

#' Hearsay Consensus Model with Individual-Level and Group Moderators Builder
#'
#' This function builds a lavaan model for asssessing individual-level moderators of
#' hearsay consensus across multiple groups.
#' It requires names of columns for P1 reports, P2 reports, an Individual Difference Moderator Variable,
#' an interaction term, and a set of group labels. The baseline model (built by this function) estimates the following
#' parameters per group:
#'
#' \describe{
#' \item{hc_me}{hearsay consensus main effect; this should correspond to hearsay consensus at average
#' level of moderator variable (if data were properly mean-centered).}
#' \item{mod_me}{The meain effect of the moderator variable; it can be interpreted as the difference in
#' P1-reports related to differences in the individual-level moderator variable.}
#' \item{interaction}{This is the interaction term. It indicates the extent to which hearsay consensus,
#' depends on the moderator variable}
#' \item{v_p1}{variance for P1(T)}
#' \item{v_p2}{variance for P2(T)}
#' \item{v_mod}{variance for moderator variable}
#' \item{v_interaction}{variance for interaction term}
#' \item{int_p1}{intercept for P1(T)}
#' \item{int_p2}{intercept for P2(T)}
#' \item{int_mod}{intercept for moderator variable}
#' \item{int_interaction}{intercept for interaction term}
#' }
#' The function can handle up to n exchangeable triads.
#' @param  p1_reports Quoted column names that contain P1 reports,
#' or ratings made by the person that knows the target directly.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param p2_reports Quoted column names that contain P2 reports,
#' or ratings made by the person that knows the target indirectly through the corresponding P1.
#' Ratings should be grand-mean-centered to increase the interpretibility of the model parameters.
#' If more than one is supplied, the target-wise order must match the other rating types.
#' @param id_mod_variable Quoted column names that contain the individual-level moderator of interest.
#' If more than one is supplied from multiple exchangeable triads,
#' the order must match the order of the ratings. Like P2-reports, the variable should be mean-centered
#' to facilitate interpretability.
#' @param interaction_term Quoted column names that contain the interaction term, or the product of the
#' mean-centered P2-report and the mean-centered moderator variable. If more than one is supplied from multiple
#' exchangeable triads, the target-wise order must match the order of the ratings.
#' @param groups Vector of quoted group labels (from group-level categorical moderator).
#' @param use_labs Logical indicating whether or not to use the group labels to create the parameter labels.
#' If FALSE, generic labels (grp1 to grpk, where k is the number of groups) are used.
#' @param n_triads The number of exchangeable triads in each group. By default, this is determined by
#' counting the number of P1 reports. This parameter rarely needs to be changed.
#' @param n_p1s_per_p2s The number of P1s for every P2. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p2s_per_p1s The number of P2s for every P1;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @import magrittr stringr lavaan
#' @export
#' @examples data("rep_sim_data")
#'           # Prepare data
#'          moderator_data <- rep_sim_data %>%
#'            dplyr::mutate(B_C_agreeableness_cent = scale(B_C_agreeableness, scale = FALSE),
#'                   D_A_agreeableness_cent = scale(D_A_agreeableness, scale = FALSE),
#'                   B_iri_perspective_cent = scale(B_iri_perspective, scale = FALSE),
#'                   D_iri_perspective_cent = scale(D_iri_perspective, scale = FALSE),
#'                   B_ptXagree_interaction = B_C_agreeableness_cent*B_iri_perspective_cent,
#'                   D_ptXagree_interaction = D_A_agreeableness_cent*D_iri_perspective_cent)
#'
#' # build a model examining perspective taking moderating hearsay consensus across two studies
#' agree_pt_mod_model <- rep_consensus_group_id_mods_builder (p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                                                      p2_reports = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                                      id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                                      interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"),
#'                                                      groups = c("study1", "study2"))
#'
#' @return The function returns an object of class \code{\link[lavaan:lavaan-class]{lavaan}}.

rep_consensus_group_id_mods_builder <- function(p1_reports, p2_reports, id_mod_variable,
                                                interaction_term, groups = NULL, use_labs = TRUE,
                                                n_triads = length(p1_reports),
                                                n_p1s_per_p2s = 1, n_p2s_per_p1s = 1){
  # Global Variable Binding
  . <- label <- grp_labs <- NULL

  if(is.null(groups)){warning("You need to supply a group variable to run a group-moderator Reputation Model.
                              If you don't have a group moderator, try rep_analyses_auto if you have no moderator or
                              rep_auto_id_mods if you have an individual difference moderator.")
  }
  else{
    # get number of groups
    n_groups <- length(groups)
    # Make sure each group label is unique
    if(length(unique(groups)) != n_groups){stop("You provided one or more non-unique group labels. Each group labeel provided needs to be unique.")}
    # Make sure group labels aren't numbers, which
    # screw up the lavaan syntax. If they are, change use_labs
    # to TRUE, which creates generic labels that will work.
    if(!(is.numeric(groups)) && use_labs == TRUE){
      use_labs = FALSE
      message("Labels are numeric variables and use_labs was set to TRUE. This creates
              problems in the underlying lavaan syntax. use_labs is being set to FALSE,
              creating group labels of grp1 to grpk where k is the number of groups")
    }
    # Create labels
    # if use_labs is true, then labels are made
    # based on the group labels used in groups =
    # argument.
    # removing _ to make it clearer that whole string is part of label
    # should make model syntax a little cleaner. Also removing symbols that could cause issues
    if(use_labs == TRUE){
      groups <- str_replace_all(groups, c("_" = "",
                                          "/" = ""))
      if(sum(str_count(groups)) > 5){message("group labels exceed 5 characters; parameter labels may be very verbose.")}
    }
    if(use_labs == FALSE){
      groups <- paste0("grp", 1:n_groups)
    }
    if(n_triads > 0 &&
       n_p1s_per_p2s == 1 &&
       n_p2s_per_p1s == 1){

      rep_model <- rep_consensus_id_mods_builder(p1_reports = p1_reports, p2_reports = p2_reports,
                                                 id_mod_variable = id_mod_variable, interaction_term = interaction_term,
                                                 n_triads = length(p1_reports), n_p1s_per_p2s = 1, n_p2s_per_p1s = 1)

      # make vector of parameter labels
      # this combines the parameter labels used
      # in unmoderated model with the group labels
      # specified in the function call.
      param_labs <- rep_model$model %>%
        lavaanify(fixed.x = FALSE) %>%
        dplyr::select(.data$label) %>%
        dplyr::filter(str_detect(.data$label, "")) %>%
        tidyr::crossing(groups) %>%
        tidyr::unite(grp_labs, .data$groups, .data$label, remove = FALSE) %>%
        dplyr::select(-.data$groups) %>%
        dplyr::distinct() %>%
        split(.$label) %>%
        purrr::map(~dplyr::select(., -.data$label)) %>%
        purrr::map(~str_flatten(.)) %>%
        unlist() %>%
        sort(decreasing = TRUE)

      model <-
        # moderated regression model
        paste(paste(p1_reports, "~", param_labs["hc_me"], "*", p2_reports, "+",
                    param_labs["mod_me"], "*", id_mod_variable, "+",
                    param_labs["interaction"], "*", interaction_term),

              # variances
              paste(p1_reports,       "~~", param_labs["v_p1"], "*", p1_reports),
              paste(p2_reports,       "~~", param_labs["v_p2"], "*", p2_reports),
              paste(id_mod_variable,  "~~", param_labs["v_mod"], "*", id_mod_variable),
              paste(interaction_term, "~~", param_labs["v_interaction"], "*", interaction_term),

              # intercepts
              paste(p1_reports,       "~", param_labs["int_p1"], "*1"),
              paste(p2_reports,       "~", param_labs["int_p2"], "*1"),
              paste(id_mod_variable,  "~", param_labs["int_mod"], "*1"),
              paste(interaction_term, "~", param_labs["int_interaction"], "*1"),
              sep = "\n")
      # Put the model info together.
      rep_model_info <- tibble::as_tibble(list(model_type = "Individual-level moderator on Hearsay Consensus",
                                               ex_triads = n_triads,
                                               p1_per_p2 = n_p1s_per_p2s,
                                               p2_per_p1 = n_p2s_per_p1s))
    }
  }
  return(list(model = model,
              rep_model_info = rep_model_info))
}

#' Hearsay Consensus Model with Individual-Level and Group Moderators
#'
#' This function builds and fits a lavaan model for asssessing individual-level moderators of
#' hearsay consensus across multiple groups.
#' It requires names of columns for P1 reports, P2 reports, an Individual Difference Moderator Variable,
#' an interaction term, and a group-level moderator variable. The baseline/default model allows all estimated
#' parameters to differ across groups. You can set some or all parameters to be equal across some or all groups
#' by using the groups_eql and params_eql arguments.
#'
#' The parameters estimated by this model are:
#' \describe{
#' \item{hc_me}{hearsay consensus main effect; this should correspond to hearsay consensus at average
#' level of moderator variable (if data were properly mean-centered).}
#' \item{mod_me}{The meain effect of the moderator variable; it can be interpreted as the difference in
#' P1-reports related to differences in the individual-level moderator variable.}
#' \item{interaction}{This is the interaction term. It indicates the extent to which hearsay consensus,
#' depends on the moderator variable}
#' \item{v_p1}{variance for P1(T)}
#' \item{v_p2}{variance for P2(T)}
#' \item{v_mod}{variance for moderator variable}
#' \item{v_interaction}{variance for interaction term}
#' \item{int_p1}{intercept for P1(T)}
#' \item{int_p2}{intercept for P2(T)}
#' \item{int_mod}{intercept for moderator variable}
#' \item{int_interaction}{intercept for interaction term}
#' }
#' The function can handle up to n exchangeable triads.
#' @param data The dataframe.
#' Data should be wide, with a row for every group of participants.
#' At a minimum, it must contain five columns: one for P1-reports, one for P2-reports, one for the individual
#' difference moderator, one for the interaction term, and one for the group moderated variable.
#' @param model Optional. A model from the corresponding ReputationModelR model builder function. If this
#' is supplied, no additional arguments need to be specified.
#' @param  p1_reports Quoted column names that contain P1 reports,
#' or ratings made by the person that knows the target directly.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param p2_reports Quoted column names that contain P2 reports,
#' or ratings made by the person that knows the target indirectly through the corresponding P1.
#' Ratings should be grand-mean-centered to increase the interpretibility of the model parameters.
#' If more than one is supplied, the target-wise order must match the other rating types.
#' @param id_mod_variable Quoted column names that contain the individual-level moderator of interest.
#' If more than one is supplied from multiple exchangeable triads,
#' the order must match the order of the ratings. Like P2-reports, the variable should be mean-centered
#' to facilitate interpretability.
#' @param interaction_term Quoted column names that contain the interaction term, or the product of the
#' mean-centered P2-report and the mean-centered moderator variable. If more than one is supplied from multiple
#' exchangeable triads, the target-wise order must match the order of the ratings.
#' @param group_mod The quoted column name that contains a group-level categorical moderator.
#' @param use_labs Logical indicating whether or not to use the group labels to create the parameter labels.
#' If FALSE, generic labels (grp1 to grpk, where k is the number of groups) are used.
#' @param groups_eql Optional. Groups that you want to constrain to be equal across some or all parameters. If
#' you have use_labs set to TRUE, provide a vector of group labels corresponding to the groups you want to constrain to be equal. If you
#' have use_labs set to FALSE, provide a vector of numbers corresponding to the position of the groups you want to constrain to be equal. If you provide
#' "all", all groups will be constrained to be equal.
#' @param params_eql Optional. Parameters that you want to constrain to be equal across the groups specified in groups_eql. You can provide
#' one or more specific parameters (e.g., "hc_me" for hearsay consensus main effect), or use a built-in options including "all" which constrains
#' all parameters to be equal across groups.
#' @param n_triads The number of exchangeable triads in each group. By default, this is determined by
#' counting the number of P1 reports. This parameter rarely needs to be changed.
#' @param n_p1s_per_p2s The number of P1s for every P2. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p2s_per_p1s The number of P2s for every P1;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @import magrittr stringr lavaan
#' @export
#' @examples data("rep_sim_data")
#'           # Prepare data
#'          moderator_data <- rep_sim_data %>%
#'            dplyr::mutate(B_C_agreeableness_cent = scale(B_C_agreeableness, scale = FALSE),
#'                   D_A_agreeableness_cent = scale(D_A_agreeableness, scale = FALSE),
#'                   B_iri_perspective_cent = scale(B_iri_perspective, scale = FALSE),
#'                   D_iri_perspective_cent = scale(D_iri_perspective, scale = FALSE),
#'                   B_ptXagree_interaction = B_C_agreeableness_cent*B_iri_perspective_cent,
#'                   D_ptXagree_interaction = D_A_agreeableness_cent*D_iri_perspective_cent)
#'
#'     # Fit a model examining perspective taking moderating hearsay consensus across two studies
#'     # allowing estimates to differ across studies
#' agree_pt_mod_fit <- rep_consensus_group_id_mods(moderator_data,
#'                                           p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                                           p2_reports = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                           id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                           interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"),
#'                                           group_mod = "study")
#'
#' # Alternatively:
#' agree_pt_mod_model <- rep_consensus_group_id_mods_builder (p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                                                      p2_reports = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                                      id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                                      interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"),
#'                                                      groups = levels(rep_sim_data$study))
#'
#' agree_pt_mod_fit <- rep_consensus_group_id_mods(moderator_data, model = agree_pt_mod_model, group_mod = "study")
#'
#' # constrain all parameters to be equal across all groups
#' agree_pt_mod_all_eq_fit<- rep_consensus_group_id_mods(moderator_data,
#'                                                 p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                                                 p2_reports = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                                 id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                                 interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"),
#'                                                 group_mod = "study", groups_eql = "all", params_eql = "all")
#'
#' # Or you could constrain one parameter to be equal across all groups
#' agree_pt_mod_hc_eq_fit<- rep_consensus_group_id_mods(moderator_data,
#'                                                p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                                                p2_reports = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                                id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                                interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"),
#'                                                group_mod = "study", groups_eql = "all", params_eql = "hc_me")
#'
#' @return The function returns an object of class \code{\link[lavaan:lavaan-class]{lavaan}}.

rep_consensus_group_id_mods <- function(data, model = NULL, p1_reports, p2_reports, id_mod_variable, interaction_term,
                                        group_mod = NULL, use_labs = TRUE, groups_eql = "none", params_eql = "none",
                                        n_triads = length(p1_reports), n_p1s_per_p2s = 1, n_p2s_per_p1s = 1){
  if(is.null(group_mod)){warning("You need to supply a group variable to run a group-moderator Reputation Model.
                                 If you don't have a group moderator, try rep_consensus if you have no moderator or
                                 rep_id_mods_consensus if you have an individual difference moderator.")
  }


  else{
    # Note that lavaan orders groups based on the order
    # they appear in the datafile. Everything I wrote does
    # alphabetical to match default factor level conventions.
    # re-arranging data to be in alphabetical order by
    # group moderator to make lavaan's method consistent with
    # what I've written.
    data <- data[order(data[,group_mod]),]
    groups <- levels(as.factor(data[,group_mod]))

    # Make sure group labels aren't numbers, which
    # screw up the lavaan syntax. If they are, change use_labs
    # to TRUE, which creates generic labels that will work.
    if(!(is.numeric(groups)) && use_labs == TRUE){
      use_labs = FALSE
      message("Labels are numeric variables and use_labs was set to TRUE. This creates
              problems in the underlying lavaan syntax. use_labs is being set to FALSE,
              creating group labels of grp1 to grpk where k is the number of groups")
    }
    if(is.null(model)){
      rep_consensus_group_id_model <- rep_consensus_group_id_mods_builder(p1_reports = p1_reports, p2_reports = p2_reports,
                                                                          id_mod_variable = id_mod_variable, interaction_term = interaction_term,
                                                                          groups = groups, use_labs = use_labs, n_triads = length(p1_reports),
                                                                          n_p1s_per_p2s = n_p1s_per_p2s, n_p2s_per_p1s = n_p2s_per_p1s)}

    else{rep_consensus_group_id_model <- model}


    if(!("none" %in% params_eql) &&
       !("all" %in% groups_eql) &&
       use_labs == FALSE){
      groups_eql <- paste0("grp", groups_eql)
    }

    if("all" %in% groups_eql && use_labs == FALSE){
      groups_eql <- paste0("grp", c(1:length(groups)))
    }

    if("all" %in% groups_eql && use_labs == TRUE){
      groups_eql <- groups
    }

    if("all" %in% params_eql){
      params_eql <- c("hc_me", "mod_me", "interaction",
                      "int_p1", "int_p2", "int_mod", "int_interaction",
                      "v_p1", "v_p2", "v_mod", "v_interaction")
    }

    if(!("none" %in% groups_eql)){
      groups_eql <- groups_eql %>% str_remove_all("_")
      eql_labs <- expand.grid(groups_eql, params_eql)
      old_labs <- paste(eql_labs$Var1, eql_labs$Var2, sep = "_")
      new_labs <- eql_labs$Var2 %>% as.character()
      names(new_labs) <- old_labs
      rep_consensus_group_id_model$model <- str_replace_all(rep_consensus_group_id_model$model, new_labs)
    }
    fitted_model <- lavaan::sem(rep_consensus_group_id_model$model, data = data, missing = "FIML", group = group_mod, fixed.x = FALSE)
    return(fitted_model)}
}

#' Hearsay Accuracy Model with Individual-Level and Group Moderators Builder
#'
#' This function builds a lavaan model for asssessing individual-level moderators of
#' hearsay accuracy across multiple groups.
#' It requires names of columns for Target Self-Reports, P2 reports, an Individual Difference Moderator Variable,
#' an interaction term, and a set of group labels. The baseline model (built by this function) builds a model
#' that allows groups to differ for all parameters.
#'
#' The parameters for this model are:
#'
#' \describe{
#' \item{ha_me}{hearsay accuracy main effect; this should correspond to hearsay accuracy at average
#' level of moderator variable (if data were properly mean-centered).}
#' \item{mod_me}{The meain effect of the moderator variable; it can be interpreted as the difference in
#' target self-reports related to differences in the individual-level moderator variable.}
#' \item{interaction}{This is the interaction term. It indicates the extent to which hearsay accuracy,
#' depends on the moderator variable}
#' \item{v_self}{variance for T(T)}
#' \item{v_p2}{variance for P2(T)}
#' \item{v_mod}{variance for moderator variable}
#' \item{v_interaction}{variance for interaction term}
#' \item{int_self}{intercept for T(T)}
#' \item{int_p2}{intercept for P2(T)}
#' \item{int_mod}{intercept for moderator variable}
#' \item{int_interaction}{intercept for interaction term}
#' }
#' The function can handle up to n exchangeable triads.
#' @param target_self Quoted column names that contain target self-reports.
#' If more than one is supplied, the order must match the other
#' rating types.
#' @param p2_reports Quoted column names that contain P2 reports,
#' or ratings made by the person that knows the target indirectly through the corresponding P1.
#' Ratings should be grand-mean-centered to increase the interpretibility of the model parameters.
#' If more than one is supplied, the target-wise order must match the other rating types.
#' @param id_mod_variable Quoted column names that contain the individual-level moderator of interest.
#' If more than one is supplied from multiple exchangeable triads,
#' the order must match the order of the ratings. Like P2-reports, the variable should be mean-centered
#' to facilitate interpretability.
#' @param interaction_term Quoted column names that contain the interaction term, or the product of the
#' mean-centered P2-report and the mean-centered moderator variable. If more than one is supplied from multiple
#' exchangeable triads, the target-wise order must match the order of the ratings.
#' @param groups Vector of quoted group labels (from group-level categorical moderator).
#' @param use_labs Logical indicating whether or not to use the group labels to create the parameter labels.
#' If FALSE, generic labels (grp1 to grpk, where k is the number of groups) are used.
#' @param n_triads The number of exchangeable triads in each group. By default, this is determined by
#' counting the number of Target self-reports. This parameter rarely needs to be changed.
#' @param n_ts_per_p2s The number of targets that each P2 rated. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p2s_per_ts The number of P2s that rated each target;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @import magrittr stringr lavaan
#' @export
#' @examples data("rep_sim_data")
#'           # Prepare data
#'          moderator_data <- rep_sim_data %>%
#'            dplyr::mutate(B_C_agreeableness_cent = scale(B_C_agreeableness, scale = FALSE),
#'                   D_A_agreeableness_cent = scale(D_A_agreeableness, scale = FALSE),
#'                   B_iri_perspective_cent = scale(B_iri_perspective, scale = FALSE),
#'                   D_iri_perspective_cent = scale(D_iri_perspective, scale = FALSE),
#'                   B_ptXagree_interaction = B_C_agreeableness_cent*B_iri_perspective_cent,
#'                   D_ptXagree_interaction = D_A_agreeableness_cent*D_iri_perspective_cent)
#'
#' # build a model examining perspective taking moderating hearsay consensus across two studies
#' agree_pt_mod_model <- rep_accuracy_group_id_mods_builder (target_self = c("C_C_agreeableness", "A_A_agreeableness"),
#'                                                      p2_reports = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                                      id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                                      interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"),
#'                                                      groups = c("study1", "study2"))
#'
#' @return The function returns an object of class \code{\link[lavaan:lavaan-class]{lavaan}}.

rep_accuracy_group_id_mods_builder <- function(target_self, p2_reports, id_mod_variable,
                                               interaction_term, groups = NULL, use_labs = TRUE,
                                               n_triads = length(target_self),
                                               n_ts_per_p2s = 1, n_p2s_per_ts = 1){
  grp_labs <- . <- NULL

  if(is.null(groups)){warning("You need to supply a group variable to run a group-moderator Reputation Model.
                              If you don't have a group moderator,try rep_analyses_auto if you have no moderator or
                            rep_auto_id_mods if you have an individual difference moderator.")
  }
  else{
    # get number of groups
    n_groups <- length(groups)
    # Make sure each group label is unique
    if(length(unique(groups)) != n_groups){stop("You provided one or more non-unique group labels. Each group labeel provided needs to be unique.")}
    # Make sure group labels aren't numbers, which
    # screw up the lavaan syntax. If they are, change use_labs
    # to TRUE, which creates generic labels that will work.
    if(!(is.numeric(groups)) && use_labs == TRUE){
      use_labs = FALSE
      message("Labels are numeric variables and use_labs was set to TRUE. This creates
              problems in the underlying lavaan syntax. use_labs is being set to FALSE,
              creating group labels of grp1 to grpk where k is the number of groups")
    }
    # Create labels
    # if use_labs is true, then labels are made
    # based on the group labels used in groups =
    # argument.
    # removing _ to make it clearer that whole string is part of label
    # should make model syntax a little cleaner. Also removing symbols that could cause issues
    if(use_labs == TRUE){
      groups <- str_replace_all(groups, c("_" = "",
                                          "/" = ""))
      if(sum(str_count(groups)) > 5){message("group labels exceed 5 characters; parameter labels may be very verbose.")}
    }
    if(use_labs == FALSE){
      groups <- paste0("grp", 1:n_groups)
    }
    if(n_triads > 0 &&
       n_ts_per_p2s == 1 &&
       n_p2s_per_ts == 1){

      rep_model <- rep_accuracy_id_mods_builder(target_self = target_self, p2_reports = p2_reports,
                                                id_mod_variable = id_mod_variable, interaction_term = interaction_term,
                                                n_triads = length(target_self), n_ts_per_p2s = 1, n_p2s_per_ts = 1)

      # make vector of parameter labels
      # this combines the parameter labels used
      # in unmoderated model with the group labels
      # specified in the function call.
      param_labs <- rep_model$model %>%
        lavaanify(fixed.x = FALSE) %>%
        dplyr::select(.data$label) %>%
        dplyr::filter(str_detect(.data$label, "")) %>%
        tidyr::crossing(groups) %>%
        tidyr::unite(grp_labs, .data$groups, .data$label, remove = FALSE) %>%
        dplyr::select(-.data$groups) %>%
        dplyr::distinct() %>%
        split(.$label) %>%
        purrr::map(~dplyr::select(., -.data$label)) %>%
        purrr::map(~str_flatten(.)) %>%
        unlist() %>%
        sort(decreasing = TRUE)

      model <-
        # moderated regression model
        paste(paste(target_self, "~", param_labs["ha_me"], "*", p2_reports, "+",
                    param_labs["mod_me"], "*", id_mod_variable, "+",
                    param_labs["interaction"], "*", interaction_term),

              # variances
              paste(target_self,       "~~", param_labs["v_self"], "*", target_self),
              paste(p2_reports,       "~~", param_labs["v_p2"], "*", p2_reports),
              paste(id_mod_variable,  "~~", param_labs["v_mod"], "*", id_mod_variable),
              paste(interaction_term, "~~", param_labs["v_interaction"], "*", interaction_term),

              # intercepts
              paste(target_self,       "~", param_labs["int_self"], "*1"),
              paste(p2_reports,       "~", param_labs["int_p2"], "*1"),
              paste(id_mod_variable,  "~", param_labs["int_mod"], "*1"),
              paste(interaction_term, "~", param_labs["int_interaction"], "*1"),
              sep = "\n")
      # Put the model info together.
      rep_model_info <- tibble::as_tibble(list(model_type = "Individual-level moderator on Hearsay Consensus",
                                               ex_triads = n_triads,
                                               t_per_p2 = n_ts_per_p2s,
                                               p2_per_t = n_p2s_per_ts))
    }
  }
  return(list(model = model,
              rep_model_info = rep_model_info))
}

#' Hearsay Accuracy Model with Individual-Level and Group Moderators
#'
#' This function builds and fits a lavaan model for asssessing individual-level moderators of
#' hearsay accuracy across multiple groups.
#' It requires names of columns for target self-reports, P2 reports, an Individual Difference Moderator Variable,
#' an interaction term, and a group-level moderator variable. The baseline/default model allows all estimated
#' parameters to differ across groups. You can set some or all parameters to be equal across some or all groups
#' by using the groups_eql and params_eql arguments.
#'
#' The parameters estimated by this model are:
#' \describe{
#' \item{ha_me}{hearsay accuracy main effect; this should correspond to hearsay accuracy at average
#' level of moderator variable (if data were properly mean-centered).}
#' \item{mod_me}{The meain effect of the moderator variable; it can be interpreted as the difference in
#' target self-reports related to differences in the individual-level moderator variable.}
#' \item{interaction}{This is the interaction term. It indicates the extent to which hearsay accuracy,
#' depends on the moderator variable}
#' \item{v_self}{variance for T(T)}
#' \item{v_p2}{variance for P2(T)}
#' \item{v_mod}{variance for moderator variable}
#' \item{v_interaction}{variance for interaction term}
#' \item{int_self}{intercept for T(T)}
#' \item{int_p2}{intercept for P2(T)}
#' \item{int_mod}{intercept for moderator variable}
#' \item{int_interaction}{intercept for interaction term}
#' }
#' The function can handle up to n exchangeable triads.
#' @param data The dataframe that contains the ratings, moderator variables, and the interaction term.
#' Data should be wide, with a row for every group of participants.
#' At a minimum, it must contain five columns: one for target self-reports, one for mean-centered P2 reports,
#' one for the mean-centered individual-level moderator variable, one for the interaction term, and one for the group variable.
#' @param model Optional. A model from the corresponding ReputationModelR model builder function. If this
#' is supplied, no additional arguments need to be specified.
#' @param target_self Quoted column names that contain target self-reports.
#' If more than one is supplied, the order must match the other
#' rating types.
#' @param p2_reports Quoted column names that contain P2 reports,
#' or ratings made by the person that knows the target indirectly through the corresponding P1.
#' Ratings should be grand-mean-centered to increase the interpretibility of the model parameters.
#' If more than one is supplied, the target-wise order must match the other rating types.
#' @param id_mod_variable Quoted column names that contain the individual-level moderator of interest.
#' If more than one is supplied from multiple exchangeable triads,
#' the order must match the order of the ratings. Like P2-reports, the variable should be mean-centered
#' to facilitate interpretability.
#' @param interaction_term Quoted column names that contain the interaction term, or the product of the
#' mean-centered P2-report and the mean-centered moderator variable. If more than one is supplied from multiple
#' exchangeable triads, the target-wise order must match the order of the ratings.
#' @param group_mod The quoted column name that contains a group-level categorical moderator.
#' @param use_labs Logical indicating whether or not to use the group labels to create the parameter labels.
#' If FALSE, generic labels (grp1 to grpk, where k is the number of groups) are used.
#' @param groups_eql Optional. Groups that you want to constrain to be equal across some or all parameters. If
#' you have use_labs set to TRUE, provide a vector of group labels corresponding to the groups you want to constrain to be equal. If you
#' have use_labs set to FALSE, provide a vector of numbers corresponding to the position of the groups you want to constrain to be equal. If you provide
#' "all", all groups will be constrained to be equal.
#' @param params_eql Optional. Parameters that you want to constrain to be equal across the groups specified in groups_eql. You can provide
#' one or more specific parameters (e.g., "hc_me" for hearsay consensus main effect), or use a built-in options including "all" which constrains
#' all parameters to be equal across groups.
#' @param n_triads The number of exchangeable triads in each group. By default, this is determined by
#' counting the number of P1 reports. This parameter rarely needs to be changed.
#' @param n_ts_per_p2s The number of targets for every P2. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p2s_per_ts The number of P2s for every target;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @import magrittr stringr lavaan
#' @export
#' @examples data("rep_sim_data")
#'           # Prepare data
#'          moderator_data <- rep_sim_data %>%
#'            dplyr::mutate(B_C_agreeableness_cent = scale(B_C_agreeableness, scale = FALSE),
#'                   D_A_agreeableness_cent = scale(D_A_agreeableness, scale = FALSE),
#'                   B_iri_perspective_cent = scale(B_iri_perspective, scale = FALSE),
#'                   D_iri_perspective_cent = scale(D_iri_perspective, scale = FALSE),
#'                   B_ptXagree_interaction = B_C_agreeableness_cent*B_iri_perspective_cent,
#'                   D_ptXagree_interaction = D_A_agreeableness_cent*D_iri_perspective_cent)
#'
#'     # Fit a model examining perspective taking moderating hearsay accuracy across two studies
#'     # allowing estimates to differ across studies
#' agree_pt_mod_fit <- rep_accuracy_group_id_mods(moderator_data,
#'                                           target_self = c("C_C_agreeableness", "A_A_agreeableness"),
#'                                           p2_reports = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                           id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                           interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"),
#'                                           group_mod = "study")
#'
#' # Alternatively:
#' agree_pt_mod_model <- rep_accuracy_group_id_mods_builder(target_self = c("C_C_agreeableness", "A_A_agreeableness"),
#'                                                      p2_reports = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                                      id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                                      interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"),
#'                                                      groups = c("study1", "study2"))
#'
#' agree_pt_mod_fit <- rep_accuracy_group_id_mods(moderator_data, model = agree_pt_mod_model, group_mod = "study")
#'
#' # You could constrain all parameters to be equal across all groups
#' # by setting both the groups_eql and params_eql arguments to "all"
#' agree_pt_mod_all_eq_fit<- rep_accuracy_group_id_mods(moderator_data,
#'                                                 target_self = c("C_C_agreeableness", "A_A_agreeableness"),
#'                                                 p2_reports = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                                 id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                                 interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"),
#'                                                 group_mod = "study", groups_eql = "all", params_eql = "all")
#'
#' # Or you could constrain one parameter to be equal across all groups
#' agree_pt_mod_ha_eq_fit<- rep_accuracy_group_id_mods(moderator_data,
#'                                                target_self = c("C_C_agreeableness", "A_A_agreeableness"),
#'                                                p2_reports = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                                id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                                interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"),
#'                                                group_mod = "study", groups_eql = "all", params_eql = "ha_me")
#'
#' @return The function returns an object of class \code{\link[lavaan:lavaan-class]{lavaan}}.

rep_accuracy_group_id_mods <- function(data, model = NULL, target_self, p2_reports, id_mod_variable, interaction_term,
                                       group_mod = NULL, use_labs = TRUE,groups_eql = "none", params_eql = "none",
                                       n_triads = length(target_self), n_ts_per_p2s = 1, n_p2s_per_ts = 1){
  if(is.null(group_mod)){warning("You need to supply a group variable to run a group-moderator Reputation Model.
                                 If you don't have a group moderator, try try rep_analyses_auto if you have no moderator or
                                 rep_auto_id_mods if you have an individual difference moderator.")
  }


  else{
    # Note that lavaan orders groups based on the order
    # they appear in the datafile. Everything I wrote does
    # alphabetical to match default factor level conventions.
    # re-arranging data to be in alphabetical order by
    # group moderator to make lavaan's method consistent with
    # what I've written.
    data <- data[order(data[,group_mod]),]
    groups <- levels(as.factor(data[,group_mod]))

    # Make sure group labels aren't numbers, which
    # screw up the lavaan syntax. If they are, change use_labs
    # to TRUE, which creates generic labels that will work.
    if(!(is.numeric(groups))&& use_labs == TRUE){
      use_labs = FALSE
      message("Labels are numeric variables and use_labs was set to TRUE. This creates
              problems in the underlying lavaan syntax. use_labs is being set to FALSE,
              creating group labels of grp1 to grpk where k is the number of groups")
    }
    if(is.null(model)){
      rep_accuracy_group_id_model <- rep_accuracy_group_id_mods_builder(target_self = target_self, p2_reports = p2_reports,
                                                                        id_mod_variable = id_mod_variable, interaction_term = interaction_term,
                                                                        groups = groups, use_labs = use_labs, n_triads = length(target_self),
                                                                        n_ts_per_p2s = n_ts_per_p2s, n_p2s_per_ts = n_p2s_per_ts)}

    else{rep_accuracy_group_id_model <- model}


    if(!("none" %in% params_eql) &&
       !("all" %in% groups_eql) &&
       use_labs == FALSE){
      groups_eql <- paste0("grp", groups_eql)
    }

    if("all" %in% groups_eql && use_labs == FALSE){
      groups_eql <- paste0("grp", c(1:length(groups)))
    }

    if("all" %in% groups_eql && use_labs == TRUE){
      groups_eql <- groups
    }

    if("all" %in% params_eql){
      params_eql <- c("ha_me", "mod_me", "interaction",
                      "int_self", "int_p2", "int_mod", "int_interaction",
                      "v_self", "v_p2", "v_mod", "v_interaction")
    }

    if(!("none" %in% groups_eql)){
      groups_eql <- groups_eql %>% str_remove_all("_")
      eql_labs <- expand.grid(groups_eql, params_eql)
      old_labs <- paste(eql_labs$Var1, eql_labs$Var2, sep = "_")
      new_labs <- eql_labs$Var2 %>% as.character()
      names(new_labs) <- old_labs
      rep_accuracy_group_id_model$model <- str_replace_all(rep_accuracy_group_id_model$model, new_labs)
    }
    fitted_model <- lavaan::sem(rep_accuracy_group_id_model$model, data = data, missing = "FIML", group = group_mod, fixed.x = FALSE)
    return(fitted_model)}
}
