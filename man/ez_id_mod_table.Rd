% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ez_tables.R
\name{ez_id_mod_table}
\alias{ez_id_mod_table}
\title{Easily Table Individual Difference Moderator Results}
\usage{
ez_id_mod_table(rep_model)
}
\arguments{
\item{rep_model}{The results from one of the ReputationModelR
individual-level moderator Models (e.g., rep_auto_id_mods).}
}
\value{
The function returns an object of class \code{\link[tibble:tbl_df-class]{tbl_df}}.
}
\description{
This takes output from one of the individual-level moderated reputation models
(e.g., rep_auto_id_mods) and returns a tibble of model
(regression) parameters. It works with any of the Individual-level moderated models.
}
\examples{
data("rep_sim_data")
moderator_data <- rep_sim_data \%>\%
dplyr::mutate(B_C_agreeableness_cent = scale(B_C_agreeableness, scale = FALSE),
       D_A_agreeableness_cent = scale(D_A_agreeableness, scale = FALSE),
       B_iri_perspective_cent = scale(B_iri_perspective, scale = FALSE),
       D_iri_perspective_cent = scale(D_iri_perspective, scale = FALSE),
       B_ptXagree_interaction = B_C_agreeableness_cent*B_iri_perspective_cent,
       D_ptXagree_interaction = D_A_agreeableness_cent*D_iri_perspective_cent)

# Example for hearsay accuracy
agree_ha_p2ptmod_model <- rep_auto_id_mods(data = moderator_data,
                 target_self = c("C_C_agreeableness", "A_A_agreeableness"),
                 p2_reports = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
                 id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
                 interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"))
 ez_id_mod_table(agree_ha_p2ptmod_model)


}
