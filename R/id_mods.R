#' Individual level Moderators (Generic) Model Builder
#'
#' This is a generic function for building a lavaan model for
#' individual-level moderators on two distinguishable ratings on the same target.
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
#' @param  rating_1 The column(s) that contain the first rating variable. This might be P1 reports
#' if investigating moderation of hearsay consensus or self-reports for moderation of hearsay accuracy.
#' If more than one is supplied, the target-wise order must match across variables.
#' @param rating_2 The column(s) that second rating variable. For hearsay consensus or accuracy,
#' this would be P2 reports. If more than one is supplied, the target-wise order must match across variables.
#' @param id_mod_variable The column(s) that contain the individual-level moderator of interest.
#' If more than one is supplied from multiple exchangeable dyads/triads,
#' the order must match the order of the ratings. Like P2-reports, the variable should be mean-centered
#' to facilitate interpretability.
#' @param interaction_term The column(s) that contain the interaction term, or the product of the
#' mean-centered P2-report and the mean-centered moderator variable. If more than one is supplied from multiple
#' exchangeable dyads/triads, the target-wise order must match the order of the ratings.
#' @param n_triads The number of exchangeable triads in each group. By default, this is determined by
#' counting the number of P1 reports. This parameter rarely needs to be changed.
#' @param n_r1_per_r2 The number of first ratings for each second rating. Currently, only 1:1 is supported.
#' @param n_r1_per_r2 The number of second ratings for each first rating. Currently, only 1:1 is supported.
#' @import lavaan
#' @export
#' @examples data("rep_sim_data")
#'           # Prepare data
#'           library(tidyverse)
#'          moderator_data <- rep_sim_data %>%
#'            mutate(B_C_agreeableness_cent = scale(B_C_agreeableness, scale = FALSE),
#'                   D_A_agreeableness_cent = scale(D_A_agreeableness, scale = FALSE),
#'                   B_iri_perspective_cent = scale(B_iri_perspective, scale = FALSE),
#'                   D_iri_perspective_cent = scale(D_iri_perspective, scale = FALSE),
#'                   B_ptXagree_interaction = B_C_agreeableness_cent*B_iri_perspective_cent,
#'                   D_ptXagree_interaction = D_A_agreeableness_cent*D_iri_perspective_cent)
#'
#'           # Buld Model
#'           agree_pt_mod_model <- rep_id_mods_generic_builder (rating_1 = c("A_C_agreeableness", "C_A_agreeableness"),
#'                                 rating_2 = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                 id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                 interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"))
#'          # view model
#'          cat(agree_pt_mod_model$model)
#'          # view model Information
#'          agree_pt_mod_model$rep_model_info
#'
#' @return The function returns a list containing an
#' object of class \code{\link[tibble:tibble-class]{tibble}} and a string object of the model
#' in lavaan syntax. Model information
#' includes the type of model, the number of exchangeable triads, and the number
#' of p1s per p2s, and the number of p2s per p1s.


rep_id_mods_generic_builder <- function(rating_1, rating_2, id_mod_variable,
                                        interaction_term, n_triads = length(rating_1),
                                        n_r1_per_r2 = 1, n_r2_per_r1 = 1){
  if(n_triads > 0 &
     n_r1_per_r2 == 1 &
     n_r2_per_r1 == 1){

    # code for 1 triad is simpler
    if(n_triads == 1){

      model <-
        # moderated regression model
        paste(paste(rating_1, "~", "rating_me*", rating_2, "+",
                    "mod_me*", id_mod_variable, "+",
                    "interaction*", interaction_term),

              # variances
              paste(rating_1, "~~ v_rating_1*", rating_1),

              paste(rating_2, " ~~ v_rating_2*", rating_2),

              paste(id_mod_variable, "~~ v_mod*", id_mod_variable),

              paste(interaction_term, "~~ v_interaction*", interaction_term),

              # intercepts
              paste(rating_1, "~ int_rating_1*1"),

              paste(rating_2, " ~ int_rating_2*1"),

              paste(id_mod_variable, "~ int_mod*1"),

              paste(interaction_term, "~ int_interaction*1"),

              sep = "\n")
    }
    if(n_triads > 1){
      model <-
        # moderated regression model
        paste(paste(rating_1, "~", "hc_me*", rating_2, "+",
                    "mod_me*", id_mod_variable, "+",
                    "interaction*", interaction_term),

              # variances
              paste(rating_1, "~~ v_rating_1*", rating_1),

              paste(rating_2, " ~~ v_rating_2*", rating_2),

              paste(id_mod_variable, "~~ v_mod*", id_mod_variable),

              paste(interaction_term, "~~ v_interaction*", interaction_term),
              # intercepts
              paste(rating_1, "~ int_rating_1*1"),

              paste(rating_2, " ~ int_rating_2*1"),

              paste(id_mod_variable, "~ int_mod*1"),

              paste(interaction_term, "~ int_interaction*1"),
              sep = "\n")
    }
    # Put the model info together.
    rep_model_info <- tibble::as_tibble(list(model_type =  "Individual-level moderator (generic function)",
                                             ex_triads = n_triads,
                                             r1_per_r2 = n_r1_per_r2,
                                             r2_per_r1 = n_r2_per_r1))
    return(list(model = model,
                rep_model_info = rep_model_info))
    if(round(mean(colMeans(data[,rating_2], na.rm = TRUE)), 4) != 0){warning("It looks like you didn't center P2 reports. You might want to for interpretability's sake.")}
    if(round(mean(colMeans(data[,id_mod_variable], na.rm = TRUE)), 4) != 0){warning("It looks like you didn't center the moderator variable. You might want to for interpretability's sake.")}
}}

#' Individual level Moderators (Generic)
#'
#' This is a generic function for individual-level moderators on two distinguishable ratings on the same target.
#' This could be P1- and P2- reports, P2- and self-reports, P1- and self-reports, or any other sets of
#' distinguishable ratings. It requires a dataframe and either a model from the relevant model builder
#' function or names of columns for rating_1, rating_2, and id_mod_variable.
#' The estimated parameters are:
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
#' @param data The dataframe that contains the ratings, moderator variable, and the interaction term.
#' Data should be wide, with a row for every group of participants.
#' At a minimum, it must contain four columns: two for ratings (of the same target),
#' one for the mean-centered moderator variable, and one for the interaction term.
#' @param model Optional. A model from the corresponding ReputationAnalyses model builder function. If this
#' is supplied, no additional arguments need to be specified.
#' @param  rating_1 The column(s) that contain the first rating variable. This might be P1 reports
#' if investigating moderation of hearsay consensus or self-reports for moderation of hearsay accuracy.
#' If more than one is supplied, the target-wise order must match across variables.
#' @param rating_2 The column(s) that second rating variable. For hearsay consensus or accuracy,
#' this would be P2 reports. If more than one is supplied, the target-wise order must match across variables.
#' @param id_mod_variable The column(s) that contain the individual-level moderator of interest.
#' If more than one is supplied from multiple exchangeable dyads/triads,
#' the order must match the order of the ratings. Like P2-reports, the variable should be mean-centered
#' to facilitate interpretability.
#' @param interaction_term The column(s) that contain the interaction term, or the product of the
#' mean-centered P2-report and the mean-centered moderator variable. If more than one is supplied from multiple
#' exchangeable dyads/triads, the target-wise order must match the order of the ratings.
#' @param n_triads The number of exchangeable triads in each group. By default, this is determined by
#' counting the number of P1 reports. This parameter rarely needs to be changed.
#' @param n_r1_per_r2 The number of first ratings for each second rating. Currently, only 1:1 is supported.
#' @param n_r1_per_r2 The number of second ratings for each first rating. Currently, only 1:1 is supported.
#' @import lavaan
#' @export
#' @examples data("rep_sim_data")
#'           # Prepare data
#'           library(tidyverse)
#'          moderator_data <- rep_sim_data %>%
#'            mutate(B_C_agreeableness_cent = scale(B_C_agreeableness, scale = FALSE),
#'                   D_A_agreeableness_cent = scale(D_A_agreeableness, scale = FALSE),
#'                   B_iri_perspective_cent = scale(B_iri_perspective, scale = FALSE),
#'                   D_iri_perspective_cent = scale(D_iri_perspective, scale = FALSE),
#'                   B_ptXagree_interaction = B_C_agreeableness_cent*B_iri_perspective_cent,
#'                   D_ptXagree_interaction = D_A_agreeableness_cent*D_iri_perspective_cent)
#'
#'        agree_pt_mod <- rep_id_mods_generic(data = moderator_data,
#'                                 rating_1 = c("A_C_agreeableness", "C_A_agreeableness"),
#'                                 rating_2 = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                 id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                 interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"))
#'
#'        # alternatively
#'         agree_pt_mod_model <- rep_id_mods_generic_builder (rating_1 = c("A_C_agreeableness", "C_A_agreeableness"),
#'                                                            rating_2 = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                                            id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                                            interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"))
#'         agree_pt_mod <- rep_id_mods_generic(data = moderator_data,
#'                                            model = agree_pt_mod_model)
#'
#' @return The function returns an object of class \code{\link[lavaan:lavaan-class]{lavaan}}.

rep_id_mods_generic <- function(data, model = NULL, rating_1, rating_2,
                                id_mod_variable,
                                interaction_term, n_triads = length(rating_1),
                                n_r1_per_r2 = 1,n_r2_per_r1 = 1){
  if(is.null(model)){
    rep_id_mods_model <- rep_id_mods_generic_builder(rating_1, rating_2, id_mod_variable,
                                               interaction_term, n_triads = length(rating_1),
                                               n_r1_per_r2 = 1,n_r2_per_r1 = 1)
    }

  else{rep_id_mods_model <- model}
  fitted_model <- lavaan::sem(rep_id_mods_model$model, data = data, missing = "FIML")
  return(fitted_model)
}

#' Individual level Moderators of Hearsay Consensus Model Builder
#'
#' This takes the variables needed to assess an individual-level moderator on hearsay consensus, and builds
#' a model for lavaan estimating the corresponding parameters. At a minimum, it requires P1-reports, P2-reports,
#' an individual-level moderator, and the interaction term. Note that the P2-reports and moderator variable should
#' be mean-centered.
#'
#' The parameters for the individual-level moderator analyses are:
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
#' @param data The dataframe that contains the ratings, moderator variable, and the interaction term.
#' Data should be wide, with a row for every group of participants.
#' At a minimum, it must contain four columns: one for P1 reports, one for mean-centered P2 reports,
#' one for the mean-centered moderator variable, and one for the interaction term.
#' @param  p1_reports The column(s) that contain P1 reports,
#' or ratings made by the person that knows the target directly.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param p2_reports The column(s) that contain P2 reports,
#' or ratings made by the person that knows the target indirectly through the corresponding P1.
#' Ratings should be grand-mean-centered to increase the interpretibility of the model parameters.
#' If more than one is supplied, the target-wise order must match the other rating types.
#' @param id_mod_variable The column(s) that contain the individual-level moderator of interest.
#' If more than one is supplied from multiple exchangeable triads,
#' the order must match the order of the ratings. Like P2-reports, the variable should be mean-centered
#' to facilitate interpretability.
#' @param interaction_term The column(s) that contain the interaction term, or the product of the
#' mean-centered P2-report and the mean-centered moderator variable. If more than one is supplied from multiple
#' exchangeable triads, the target-wise order must match the order of the ratings.
#' @param n_triads The number of exchangeable triads in each group. By default, this is determined by
#' counting the number of P1 reports. This parameter rarely needs to be changed.
#' @param n_p1s_per_p2s The number of P1s for every P2. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p2s_per_p1s The number of P2s for every P1;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @import lavaan
#' @export
#' @examples data("rep_sim_data")
#'           # Prepare data
#'           library(tidyverse)
#'          moderator_data <- rep_sim_data %>%
#'            mutate(B_C_agreeableness_cent = scale(B_C_agreeableness, scale = FALSE),
#'                   D_A_agreeableness_cent = scale(D_A_agreeableness, scale = FALSE),
#'                   B_iri_perspective_cent = scale(B_iri_perspective, scale = FALSE),
#'                   D_iri_perspective_cent = scale(D_iri_perspective, scale = FALSE),
#'                   B_ptXagree_interaction = B_C_agreeableness_cent*B_iri_perspective_cent,
#'                   D_ptXagree_interaction = D_A_agreeableness_cent*D_iri_perspective_cent)
#'
#' agree_pt_mod_consensus_model <- rep_id_mods_consensus_builder (data = moderator_data,
#'                                                           p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                                                           p2_reports = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                                           id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                                           interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"))
#'
#'  # view model
#'    cat(agree_pt_mod_model$model)
#'  # view model Information
#'    agree_pt_mod_model$rep_model_info
#'
#' @return The function returns a list containing an
#' object of class \code{\link[tibble:tibble-class]{tibble}} and a string object of the model
#' in lavaan syntax. Model information
#' includes the type of model, the number of exchangeable triads, and the number
#' of p1s per p2s, and the number of p2s per p1s.

rep_id_mods_consensus_builder <- function(data, p1_reports, p2_reports, id_mod_variable,
                                          interaction_term, n_triads = length(p1_reports),
                                          n_p1s_per_p2s = 1, n_p2s_per_p1s = 1){
  if(n_triads > 0 &
     n_p1s_per_p2s == 1 &
     n_p2s_per_p1s == 1){

    # code for 1 triad is simpler
    if(n_triads == 1){

      model <-
        # moderated regression model
        paste(paste(p1_reports, "~", "hc_me*", p2_reports, "+",
                    "mod_me*", id_mod_variable, "+",
                    "interaction*", interaction_term),

              # variances
              paste(p1_reports, "~~ v_p1*", p1_reports),

              paste(p2_reports, " ~~ v_p2*", p2_reports),

              paste(id_mod_variable, "~~ v_mod*", id_mod_variable),

              paste(interaction_term, "~~ v_interaction*", interaction_term),

              # intercepts
              paste(p1_reports, "~ int_p1*1"),

              paste(p2_reports, " ~ int_p2*1"),

              paste(id_mod_variable, "~ int_mod*1"),

              paste(interaction_term, "~ int_interaction*1"),
              sep = "\n")
    }
    if(n_triads > 1){
      model <-
        # moderated regression model
        paste(paste(p1_reports, "~", "hc_me*", p2_reports, "+",
                    "mod_me*", id_mod_variable, "+",
                    "interaction*", interaction_term),

              # variances
              paste(p1_reports, "~~ v_p1*", p1_reports),

              paste(p2_reports, " ~~ v_p2*", p2_reports),

              paste(id_mod_variable, "~~ v_mod*", id_mod_variable),

              paste(interaction_term, "~~ v_interaction*", interaction_term),

              # intercepts
              paste(p1_reports, "~ int_p1*1"),

              paste(p2_reports, " ~ int_p2*1"),

              paste(id_mod_variable, "~ int_mod*1"),

              paste(interaction_term, "~ int_interaction*1"),
              sep = "\n")
    }
    # Put the model info together.
    rep_model_info <- tibble::as_tibble(list(model_type = "Individual-level moderator on Hearsay Consensus",
                                             ex_triads = n_triads,
                                             r1_per_r2 = n_p1s_per_p2s,
                                             r2_per_r1 = n_p2s_per_p1s))

    if(round(mean(colMeans(data[,p2_reports], na.rm = TRUE)), 4) != 0){warning("It looks like you didn't center P2 reports. You might want to for interpretability's sake.")}
    if(round(mean(colMeans(data[,id_mod_variable], na.rm = TRUE)), 4) != 0){warning("It looks like you didn't center the moderator variable. You might want to for interpretability's sake.")}
    return(list(model = model,
                rep_model_info = rep_model_info))
  }
  if(n_p1s_per_p2s > 1){warning("I'm sorry, this function can only handle designs with 1 P1 per P2; check back for changes")}
  if(n_p2s_per_p1s > 1){warning("I'm sorry, this function can only handle designs with 1 P2 per P1; check back for changes")}
}

#' Individual level Moderators of Hearsay Consensus Model
#'
#' This function fits a model for individual-level moderator on hearsay consensus, and builds
#' a model for lavaan estimating the corresponding parameters.
#' It requires a dataframe and either a model from the relevant model builder
#' function or names of columns for p1_reports, p2_reports, and id_mod_variable.
#' The estimated parameters are:
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
#' @param data The dataframe that contains the ratings, moderator variable, and the interaction term.
#' Data should be wide, with a row for every group of participants.
#' At a minimum, it must contain four columns: one for P1 reports, one for mean-centered P2 reports,
#' one for the mean-centered moderator variable, and one for the interaction term.
#' @param model Optional. A model from the corresponding ReputationAnalyses model builder function. If this
#' is supplied, no additional arguments need to be specified.
#' @param  p1_reports The column(s) that contain P1 reports,
#' or ratings made by the person that knows the target directly.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param p2_reports The column(s) that contain P2 reports,
#' or ratings made by the person that knows the target indirectly through the corresponding P1.
#' Ratings should be grand-mean-centered to increase the interpretibility of the model parameters.
#' If more than one is supplied, the target-wise order must match the other rating types.
#' @param id_mod_variable The column(s) that contain the individual-level moderator of interest.
#' If more than one is supplied from multiple exchangeable triads,
#' the order must match the order of the ratings. Like P2-reports, the variable should be mean-centered
#' to facilitate interpretability.
#' @param interaction_term The column(s) that contain the interaction term, or the product of the
#' mean-centered P2-report and the mean-centered moderator variable. If more than one is supplied from multiple
#' exchangeable triads, the target-wise order must match the order of the ratings.
#' @param n_triads The number of exchangeable triads in each group. By default, this is determined by
#' counting the number of P1 reports. This parameter rarely needs to be changed.
#' @param n_p1s_per_p2s The number of P1s for every P2. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p2s_per_p1s The number of P2s for every P1;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @import lavaan
#' @export
#' @examples data("rep_sim_data")
#'           # Prepare data
#'           library(tidyverse)
#'          moderator_data <- rep_sim_data %>%
#'            mutate(B_C_agreeableness_cent = scale(B_C_agreeableness, scale = FALSE),
#'                   D_A_agreeableness_cent = scale(D_A_agreeableness, scale = FALSE),
#'                   B_iri_perspective_cent = scale(B_iri_perspective, scale = FALSE),
#'                   D_iri_perspective_cent = scale(D_iri_perspective, scale = FALSE),
#'                   B_ptXagree_interaction = B_C_agreeableness_cent*B_iri_perspective_cent,
#'                   D_ptXagree_interaction = D_A_agreeableness_cent*D_iri_perspective_cent)
#'
#' agree_pt_mod <- rep_id_mods_consensus(data = moderator_data,
#'                                     p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                                     p2_reports = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                     id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                     interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"))
#' # alternatively
#' agree_pt_mod_model <- rep_id_mods_consensus_builder (p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                                                     p2_reports = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                                    id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                                    interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"))
#' agree_pt_mod <- rep_id_mods_consensus(data = moderator_data,
#'                                     model = agree_pt_mod_model)
#'
#' @return The function returns an object of class \code{\link[lavaan:lavaan-class]{lavaan}}.


rep_id_mods_consensus <- function(data, model = NULL, p1_reports, p2_reports, id_mod_variable,
                                  interaction_term, n_triads = length(p1_reports),
                                  n_p1s_per_p2s = 1, n_p2s_per_p1s = 1){
  if(is.null(model)){
    rep_id_mods_model <- rep_id_mods_consensus_builder(data, p1_reports, p2_reports, id_mod_variable,
                                                     interaction_term, n_triads = length(p1_reports),
                                                     n_p1s_per_p2s = n_p1s_per_p2s, n_p2s_per_p1s = n_p1s_per_p2s)
  }

  else{rep_id_mods_model <- model}
  fitted_model <- lavaan::sem(rep_id_mods_model$model, data = data, missing = "FIML")
  return(fitted_model)
}

#' Individual level Moderators of Hearsay Accuracy
#'
#' This takes the variables needed to assess an individual-level moderator on hearsay acuracy, and fits
#' a model estimating the corresponding parameters. At a minimum, it requires target self-reports, P2-reports,
#' an individual-level moderator, and the interaction term.
#' Note that the P2-reports and moderator variable should be mean-centered.
#'
#' The parameters for the individual-level moderator analyses are:
#'
#' \describe{
#' \item{ha_me}{hearsay accuracy main effect; this should correspond to hearsay accuracy at average
#' level of moderator variable (if data were properly mean-centered).}
#' \item{mod_me}{The meain effect of the moderator variable; it can be interpreted as the difference in
#' target self-reports related to differences in the individual-level moderator variable.}
#' \item{interaction}{This is the interaction term. It indicates the extent to which hearsay accuracy,
#' depends on the moderator variable}
#' \item{v_t}{variance for T(T)}
#' \item{v_p2}{variance for P2(T)}
#' \item{v_mod}{variance for moderator variable}
#' \item{v_interaction}{variance for interaction term}
#' \item{int_t}{intercept for T(T)}
#' \item{int_p2}{intercept for P2(T)}
#' \item{int_mod}{intercept for moderator variable}
#' \item{int_interaction}{intercept for interaction term}
#' }
#' The function can handle up to n exchangeable triads.
#' @param data The dataframe that contains the ratings, moderator variable, and the interaction term.
#' Data should be wide, with a row for every group of participants.
#' At a minimum, it must contain four columns: one for target self-reports, one for mean-centered P2 reports,
#' one for the mean-centered moderator variable, and one for the interaction term.
#' @param target_self The column(s) that contain target self-reports.
#' If more than one is supplied, the order must match the other
#' rating types.
#' @param p2_reports The column(s) that contain P2 reports,
#' or ratings made by the person that knows the target indirectly through the corresponding P1.
#' Ratings should be grand-mean-centered to increase the interpretibility of the model parameters.
#' If more than one is supplied, the target-wise order must match the other rating types.
#' @param id_mod_variable The column(s) that contain the individual-level moderator of interest.
#' If more than one is supplied from multiple exchangeable triads,
#' the order must match the order of the ratings. Like P2-reports, the variable should be mean-centered
#' to facilitate interpretability.
#' @param interaction_term The column(s) that contain the interaction term, or the product of the
#' mean-centered P2-report and the mean-centered moderator variable. If more than one is supplied from multiple
#' exchangeable triads, the target-wise order must match the order of the ratings.
#' @param n_triads The number of exchangeable triads in each group. By default, this is determined by
#' counting the number of target self-reports. This parameter rarely needs to be changed.
#' @param n_ts_per_p2s The number of targets that each P2 rated. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p2s_per_ts The number of P2s that rated each target;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @import lavaan
#' @export
#' @examples data("rep_sim_data")
#'           # Prepare data
#'           library(tidyverse)
#'          moderator_data <- rep_sim_data %>%
#'            mutate(B_C_agreeableness_cent = scale(B_C_agreeableness, scale = FALSE),
#'                   D_A_agreeableness_cent = scale(D_A_agreeableness, scale = FALSE),
#'                   B_iri_perspective_cent = scale(B_iri_perspective, scale = FALSE),
#'                   D_iri_perspective_cent = scale(D_iri_perspective, scale = FALSE),
#'                   B_ptXagree_interaction = B_C_agreeableness_cent*B_iri_perspective_cent,
#'                   D_ptXagree_interaction = D_A_agreeableness_cent*D_iri_perspective_cent)
#'
#'           rep_id_mods_accuracy(data = moderator_data,
#'                                 target_self = c("C_C_agreeableness", "A_A_agreeableness"),
#'                                 p2_reports = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                 id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                 interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"))
#'
#' @return The function returns an object of class \code{\link[lavaan:lavaan-class]{lavaan}}.

rep_id_mods_accuracy <- function(data, target_self, p2_reports, id_mod_variable,
                                 interaction_term, n_triads = length(target_self),
                                 n_ts_per_p2s = 1, n_p2s_per_ts = 1){
  if(n_triads > 0 &
     n_ts_per_p2s == 1 &
     n_p2s_per_ts == 1){

    # code for 1 triad is simpler
    if(n_triads == 1){

      model <-
        # moderated regression model
        paste(paste(target_self, "~", "ha_me*", p2_reports, "+",
                    "mod_me*", id_mod_variable, "+",
                    "interaction*", interaction_term),

              # variances
              paste(target_self, "~~ v_t*", target_self),

              paste(p2_reports, " ~~ v_p2*", p2_reports),

              paste(id_mod_variable, "~~ v_mod*", id_mod_variable),

              paste(interaction_term, "~~ v_interaction*", interaction_term),

              # intercepts
              paste(target_self, "~ int_t*1"),

              paste(p2_reports, " ~ int_p2*1"),

              paste(id_mod_variable, "~ int_mod*1"),

              paste(interaction_term, "~ int_interaction*1"),
              sep = "\n")
    }
    if(n_triads > 1){
      model <-
        # moderated regression model
        paste(paste(target_self, "~", "ha_me*", p2_reports, "+",
                    "mod_me*", id_mod_variable, "+",
                    "interaction*", interaction_term),

              # variances
              paste(target_self, "~~ v_t*", target_self),

              paste(p2_reports, " ~~ v_p2*", p2_reports),

              paste(id_mod_variable, "~~ v_mod*", id_mod_variable),

              paste(interaction_term, "~~ v_interaction*", interaction_term),

              # intercepts
              paste(target_self, "~ int_t*1"),

              paste(p2_reports, " ~ int_p2*1"),

              paste(id_mod_variable, "~ int_mod*1"),

              paste(interaction_term, "~ int_interaction*1"),
              sep = "\n")
    }
    # fit model and print some informaiton about it
    fitted_model <- lavaan::sem(model, data = data, missing = "FIML")
    model_type <- "Individual-level moderator on Hearsay Accuracy"
    print(paste("Returning Results for", model_type, ". If this is not the model you intended to run, please check the data and variables you supplied."))
    cat(paste("design:\nnumber of exchangeable triads =", n_triads, "\n",
              "number of Ts per P2s", n_ts_per_p2s, "\n",
              "number of P2s per Ts", n_p2s_per_ts, "\n"))
    if(round(mean(colMeans(data[,p2_reports], na.rm = TRUE)), 4) != 0){warning("It looks like you didn't center P2 reports. You might want to for interpretability's sake.")}
    if(round(mean(colMeans(data[,id_mod_variable], na.rm = TRUE)), 4) != 0){warning("It looks like you didn't center the moderator variable. You might want to for interpretability's sake.")}
    return(fitted_model)
  }
  if(n_ts_per_p2s > 1){warning("I'm sorry, this function can only handle designs with 1 T per P2; check back for changes")}
  if(n_p2s_per_ts > 1){warning("I'm sorry, this function can only handle designs with 1 P2 per T; check back for changes")}
}

#' Individual level Moderators (automatic)
#'
#' This is a wrapper function around the Individual-level moderator functions.
#'
#' This chooses a function depending on which
#' variables are supplied. At a minimum, it requires two ratings,
#' an individual-level moderator variable, and the interaction term.
#'
#' The estimated parameters depend upon which variables are supplied.
#'
#' The function can handle up to n exchangeable triads.
#' @param data The dataframe that contains the ratings, moderator variable, and the interaction term.
#' Data should be wide, with a row for every group of participants.
#' At a minimum, it must contain four columns: two ratings (one which is mean-centered),
#' one for the mean-centered moderator variable, and one for the interaction term.
#' @param target_self The column(s) that contain target self-reports.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param p1_reports The column(s) that contain P1 reports,
#' or ratings made by the person that knows the target directly.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param p2_reports The column(s) that contain P2 reports,
#' or ratings made by the person that knows the target indirectly through the corresponding P1.
#' Ratings should be grand-mean-centered to increase the interpretibility of the model parameters.
#' If more than one is supplied, the target-wise order must match the other rating types.
#' @param id_mod_variable The column(s) that contain the individual-level moderator of interest.
#' If more than one is supplied from multiple exchangeable triads,
#' the order must match the order of the ratings. Like P2-reports, the variable should be mean-centered
#' to facilitate interpretability.
#' @param interaction_term The column(s) that contain the interaction term, or the product of the
#' mean-centered rating (often P2-reports) and the mean-centered moderator variable.
#' If more than one is supplied from multiple exchangeable triads,
#' the target-wise order must match the order of the ratings.
#' @param n_triads The number of exchangeable triads in each group. By default, this is determined by
#' the method used by whichever specific model funciton is called.
#' For consensus, this is done by counting P1-reports; foraccuracy, this is done by counting
#' target self-reports. This parameter rarely needs to be changed.
#' @param n_ts_per_p2s The number of targets that each P2 rated. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p2s_per_ts The number of P2s that rated each target;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p1s_per_p2s The number of P1s for every P2. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p2s_per_p1s The number of P2s for every P1;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_r1_per_r2 The number of first ratings for each second rating. Currently, only 1:1 is supported.
#' @param n_r1_per_r2 The number of second ratings for each first rating. Currently, only 1:1 is supported.
#' @import tidyverse
#' @export
#' @examples data("rep_sim_data")
#' # Prepare data
#' library(tidyverse)
#' moderator_data <- rep_sim_data %>%
#' mutate(B_C_agreeableness_cent = scale(B_C_agreeableness, scale = FALSE),
#'        D_A_agreeableness_cent = scale(D_A_agreeableness, scale = FALSE),
#'        B_iri_perspective_cent = scale(B_iri_perspective, scale = FALSE),
#'        D_iri_perspective_cent = scale(D_iri_perspective, scale = FALSE),
#'        B_ptXagree_interaction = B_C_agreeableness_cent*B_iri_perspective_cent,
#'        D_ptXagree_interaction = D_A_agreeableness_cent*D_iri_perspective_cent)
#'
#' # Example for hearsay accuracy
#' rep_id_mods_auto(data = moderator_data,
#'                  target_self = c("C_C_agreeableness", "A_A_agreeableness"),
#'                  p2_reports = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                  id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                  interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"))
#'
#' # Example for hearsay consensus
#' rep_id_mods_auto(data = moderator_data,
#'                  p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                  p2_reports = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                  id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                  interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"))
#'
#' @return The function returns an object of class \code{\link[lavaan:lavaan-class]{lavaan}}.

rep_id_mods_auto <- function(data, target_self = NULL, p1_reports = NULL, p2_reports = NULL,
                             id_mod_variable = NULL, interaction_term = NULL,
                             n_triads = NULL, n_ts_per_p2s = 1, n_p2s_per_ts = 1,
                             n_p1s_per_p2s = 1, n_p2s_per_p1s = 1, n_r1_per_r2 = 1, n_r2_per_r1 = 1){
  # Check What variables are entered
  if(is.null(id_mod_variable)){
    stop("You need to enter a individual-level moderator for this function to run.
         Either enter a moderator variable or use one of the non-moderator functions.")
  }
  # setup for hearsay consensus moderation model
  if(is.null(target_self) &
     !is.null(p1_reports) &
     !is.null(p2_reports) &
     !is.null(id_mod_variable)){
    fitted_model <- rep_id_mods_consensus(data = data, p1_reports = p1_reports,
                                          p2_reports = p2_reports,
                                          id_mod_variable = id_mod_variable,
                                          interaction_term = interaction_term,
                                          n_triads = length(p1_reports),
                                          n_p1s_per_p2s = n_p1s_per_p2s, n_p2s_per_p1s = n_p2s_per_p1s)

  }
  # setup for hearsay accuracy moderation model
  else if(is.null(p1_reports) &
          !is.null(target_self) &
          !is.null(p2_reports) &
          !is.null(id_mod_variable)){
    fitted_model <- rep_id_mods_accuracy(data = data, target_self = target_self,
                                         p2_reports = p2_reports,
                                         id_mod_variable = id_mod_variable,
                                         interaction_term = interaction_term,
                                         n_triads = length(target_self),
                                         n_ts_per_p2s = n_ts_per_p2s, n_p2s_per_ts = n_p2s_per_ts)
  }
  # setup for direct accuracy moderation model
  else if(is.null(p2_reports) &
          !is.null(target_self) &
          !is.null(p1_reports) &
          !is.null(id_mod_variable)){
    fitted_model <- rep_id_mods_generic(data = data, rating_1 = target_self, rating_2 = p1_reports,
                                        id_mod_variable = id_mod_variable,
                                        interaction_term = interaction_term,
                                        n_triads = length(rating_1),
                                        n_r1_per_r2 = n_r1_per_r2, n_r2_per_r1 = n_r1_per_r2)
  } else{
    stop("There is no default for the variables you entered.")
  }
}
