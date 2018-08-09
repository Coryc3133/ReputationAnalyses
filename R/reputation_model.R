# rep_consensus documentation

#' Reputation Consensus Model
#'
#' This takes a vector of P1 and P2 reports and fits
#' a model estimating the possible hearsay reputation parameters.
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
#' @param data The dataframe that contains P1 and P2 ratings.
#' Data should be wide, with a row for every group of participants.
#' At a minimum, it must contain two columns: one for P1 reports and one for P2 reports.
#' @param p1_reports The column(s) that contain P1 reports,
#' or ratings made by the person that knows the target directly.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param p2_reports The column(s) that contain P2 reports,
#' or ratings made by the person that knows the target indirectly through the corresponding P1.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param n_triads The number of exchangeable triads in each group. By default, this is determined by
#' counting the number of P1 reports. This parameter rarely needs to be changed.
#' @param n_p1s_per_p2s The number of P1s for every P2. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @param n_p2s_per_p1s The number of P2s for every P1;. This defaults to 1.
#' Currently, only values of 1 are supported.
#' @import lavaan
#' @export
#' @examples data("rep_sim_data")
#'           rep_consensus(data = rep_sim_data,
#'                         p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                         p2_reports = c("B_C_agreeableness", "D_A_agreeableness"))
#'
#' @return The function returns an object of class \code{\link[lavaan:lavaan-class]{lavaan}}.

rep_consensus <- function(data, p1_reports, p2_reports, n_triads = length(p1_reports),
                          n_p1s_per_p2s = 1, n_p2s_per_p1s = 1){
  if(n_triads > 0 &
     n_p1s_per_p2s == 1 &
     n_p2s_per_p1s == 1){

    # code for 1 triad - much simpler
    if(n_triads == 1){

      model <-
        # hearsay (P1-P2) consensus
        paste(paste(p1_reports, "~~ hc*", p2_reports),
              # intercepts
              paste(p1_reports, "~ int_p1*1"),
              paste(p2_reports, "~ int_p2*1"),

              # variances
              paste(p1_reports, "~~ v_p1*", p1_reports),

              paste(p2_reports, " ~~ v_p2*", p2_reports),

              # P1-P2 relative elevation
              "p1_p2_rel_el := 1*int_p1 - (1*int_p2)", sep = "\n")}

    if(n_triads > 1 &
       n_p1s_per_p2s == 1 &
       n_p2s_per_p1s == 1){
      # create empty model
      model <- ""
      for(i in 1:n_triads){
        # cross-target correlations
        if(i < n_triads){
          prev_i <- i:1
          m   <- paste(p1_reports[i], "~~ m*", p2_reports[-prev_i]) %>% stringr::str_flatten(collapse = "\n")
          rec <- paste(p1_reports[i], "~~ rec*", p1_reports[-prev_i]) %>% stringr::str_flatten(collapse = "\n")
          h   <-  paste(p2_reports[i], "~~ h*", p2_reports[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          xtrs <- paste(m, rec, h, sep = "\n")
          model <- paste(model, xtrs)}}
      hc <- paste(p1_reports, "~~ hc*", p2_reports) %>% stringr::str_flatten(collapse = "\n")
      int_p1 <- paste(p1_reports, "~ int_p1*1") %>% stringr::str_flatten(collapse = "\n")
      int_p2 <- paste(p2_reports, "~ int_p2*1") %>% stringr::str_flatten(collapse = "\n")
      v_p1 <- paste(p1_reports, "~~ v_p1*", p1_reports) %>% stringr::str_flatten(collapse = "\n")
      v_p2 <- paste(p2_reports, " ~~ v_p2*", p2_reports) %>% stringr::str_flatten(collapse = "\n")
      #  P1-P2 relative elevation
      p1p2_el <- "p1_p2_rel_el := 1*int_p1 - (1*int_p2)"

      model <- paste(hc, model,  int_p1, int_p2, v_p1, v_p2, p1p2_el, sep = "\n")}

  # fit model and print some informaiton about it
  fitted_model <- lavaan::sem(model, data = data, missing = "FIML")
  model_type <- "Simple Hearsay Consensus (P1-P2)"
  print(paste("Returning Results for", model_type, ". If this is not the model you intended to run, please check the data and variables you supplied."))
  cat(paste("design:\nnumber of exchangeable triads =", n_triads, "\n",
              "number of P1s per P2s", n_p1s_per_p2s, "\n",
              "number of P2s per P1s", n_p2s_per_p1s))

  return(fitted_model) }
  if(n_p1s_per_p2s > 1){print("I'm sorry, this function can only handle designs with 1 P1 per P2; check back for changes")}
  if(n_p2s_per_p1s > 1){print("I'm sorry, this function can only handle designs with 1 P1 per P2; check back for changes")}
}

# rep_consensus documentation

#' Reputation Consensus & Accuracy Model
#'
#' This takes a vector of P1-, P2-, and target self-reports and fits
#' a model estimating the possible hearsay reputation parameters.
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
#' one for targets' self-ratings..
#' @param  p1_reports The column(s) that contain P1 reports,
#' or ratings made by the person that knows the target directly.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param p2_reports The column(s) that contain P2 reports,
#' or ratings made by the person that knows the target indirectly through the corresponding P1.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param target_self The column(s) that contain target self-reports.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
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
#'
#' @export
#' @examples data("rep_sim_data")
#'           rep_consensus_accuracy(data = rep_sim_data,
#'                                  p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                                  p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
#'                                  target_self = c("C_C_agreeableness", "A_A_agreeableness"))
#'
#' @return The function returns an object of class \code{\link[lavaan:lavaan-class]{lavaan}}.
rep_consensus_accuracy <- function(data, p1_reports, p2_reports, target_self, n_triads = length(p1_reports),
                          n_p1s_per_p2s = 1, n_p2s_per_p1s = 1, n_p1s_per_ts = 1,
                          n_p2s_per_ts = 1, n_ts_per_p1s = 1, n_ts_per_p2s = 1){
  if(n_triads > 0 &
     n_p1s_per_p2s == 1 &
     n_p2s_per_p1s == 1 &
     n_p1s_per_ts == 1 &
     n_p2s_per_ts == 1 &
     n_ts_per_p1s == 1 &
     n_ts_per_p2s == 1 ){

    # code for 1 triad - much simpler
    if(n_triads == 1){

      model <-
        # hearsay (P1-P2) consensus
        paste(paste(p1_reports, "~~ hc*", p2_reports),
              paste(target_self, "~~ ha*", p2_reports),
              paste(target_self, "~~ da*", p1_reports),
              # intercepts
              paste(p1_reports, "~ int_p1*1"),
              paste(p2_reports, "~ int_p2*1"),
              paste(target_self, "~ int_self*1"),

              # variances
              paste(p1_reports, "~~ v_p1*", p1_reports),
              paste(p2_reports, " ~~ v_p2*", p2_reports),
              paste(target_self, " ~~ v_self*", target_self),

              # P1-P2 relative elevation
              "p1_p2_rel_el := 1*int_p1 - (1*int_p2)",
              "self_p2_rel_el := 1*int_self - (1*int_p2)",
              "self_p1_rel_el := 1*int_self - (1*int_p1)", sep = "\n")}

    if(n_triads > 1 &
       n_p1s_per_p2s == 1 &
       n_p2s_per_p1s == 1){
      # create empty model
      model <- ""
      for(i in 1:n_triads){
        # cross-target correlations
        if(i < n_triads){
          prev_i <- i:1

          m   <- paste(p1_reports[i], "~~ m*", p2_reports[-prev_i]) %>% stringr::str_flatten(collapse = "\n")
          rec <- paste(p1_reports[i], "~~ rec*", p1_reports[-prev_i]) %>% stringr::str_flatten(collapse = "\n")
          h   <-  paste(p2_reports[i], "~~ h*", p2_reports[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          tru_sim <- paste(target_self[i], "~~ tru_sim*", target_self[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          as_sim_3p <- paste(p2_reports[i], "~~ as_sim_3p*", target_self[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          as_sim_1p <- paste(p1_reports[i], "~~ as_sim_1p*", target_self[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          xtrs <- paste(m, rec, h, tru_sim, as_sim_3p, as_sim_1p, sep = "\n")
          model <- paste(model, xtrs)}}

      hc <- paste(p1_reports, "~~ hc*", p2_reports) %>% stringr::str_flatten(collapse = "\n")
      ha <- paste(target_self, "~~ ha*", p2_reports) %>% stringr::str_flatten(collapse = "\n")
      da <- paste(target_self, "~~ da*", p1_reports) %>% stringr::str_flatten(collapse = "\n")

      int_p1 <- paste(p1_reports, "~ int_p1*1") %>% stringr::str_flatten(collapse = "\n")
      int_p2 <- paste(p2_reports, "~ int_p2*1") %>% stringr::str_flatten(collapse = "\n")
      int_self <- paste(target_self, "~ int_self*1") %>% stringr::str_flatten(collapse = "\n")
      v_p1 <- paste(p1_reports, "~~ v_p1*", p1_reports) %>% stringr::str_flatten(collapse = "\n")
      v_p2 <- paste(p2_reports, " ~~ v_p2*", p2_reports) %>% stringr::str_flatten(collapse = "\n")
      v_self <- paste(target_self, " ~~ v_self*", target_self) %>% stringr::str_flatten(collapse = "\n")
      #  P1-P2 relative elevation
      p1p2_el <- "p1_p2_rel_el := 1*int_p1 - (1*int_p2)"
      selfp2_el <- "self_p2_rel_el := 1*int_self - (1*int_p2)"
      selfp1_el <- "self_p1_rel_el := 1*int_self - (1*int_p1)"

      model <- paste(hc, ha, da, model,  int_self, int_p1,
                     int_p2, v_self, v_p1, v_p2, p1p2_el, selfp2_el, selfp1_el, sep = "\n")}
    # fit model and print some informaiton about it
    fitted_model <- lavaan::sem(model, data = data, missing = "FIML")
    model_type <- "Full Triadic Model: Hearsay Consensus and Accuracy"
    print(paste("Returning Results for", model_type, ". If this is not the model you intended to run, please check the data and variables you supplied."))
    cat(paste("design:\nnumber of exchangeable triads =", n_triads, "\n",
              "number of P1s per P2s", n_p1s_per_p2s, "\n",
              "number of P2s per P1s", n_p2s_per_p1s, "\n",
              "number of p1s per targets", n_p1s_per_ts, "\n",
              "number of p2s per targets", n_p2s_per_ts, "\n",
              "number of targets per p1s", n_ts_per_p1s, "\n",
              "number of targets per p2s", n_ts_per_p2s, "\n"))

    return(fitted_model) }
  if(n_p1s_per_p2s > 1){print("I'm sorry, this function can only handle designs with 1 P1 per P2; check back for changes")}
  if(n_p2s_per_p1s > 1){print("I'm sorry, this function can only handle designs with 1 P1 per P2; check back for changes")}
  if(n_ts_per_p1s  > 1){print("I'm sorry, this function can only handle designs with 1 target per P1; check back for changes")}
  if(n_ts_per_p2s  > 1){print("I'm sorry, this function can only handle designs with 1 target per P2; check back for changes")}
  if(n_p1s_per_ts > 1){print("I'm sorry, this function can only handle designs with 1 P1 per target; check back for changes")}
  if(n_p2s_per_ts > 1){print("I'm sorry, this function can only handle designs with 1 P2 per target; check back for changes")}
}

#' Reputation Consensus, Accuracy, and 3rd-Person Meta-Perception Model
#'
#' This takes a vector of P1-, P2-, target self-reports, and
#' third-person meta-perceptions (for P1 and P2) and fits
#' a model estimating the possible hearsay reputation parameters.
#' Those parameters are:
#' \describe{
#' \item{hc}{hearsay consensus; the correlation between P1(T) & P2(T)}
#' \item{ha}{hearsay accuracy; the correation between P2(T) & T(T)}
#' \item{da}{direct accuracy; the correlation between P1(T) & T(T)}
#' \item{p1ma}{P1 Meta-Accuracy; the correlation between P1(P2(T)) & P2(T)}
#' \item{p2ma}{P2 Meta-Accuracy; the correlation between P2(P1(T)) & P1(T)}
#' \item{as_ac1}{ P1 Assumed Accuracy; the correlation between P1(P2(T)) & T(T)}
#' \item{as_con1}{P1 Assumed Consensus; the correlation between P1(P2(T)) & P1(T)}
#' \item{mp_rec}{ Meta-Perception Reciprocity; the correlation between P1(P2(T)) & P2(P1(T))}
#' \item{as_ac2 }{P2 Assumed Accuracy; the correlation between P2(P1(T)) & T(T)}
#' \item{as_con2}{P2 Assumed Consensus; the correlation between P2(P1(T)) & P2(T)}
#' \item{int_p1}{Intercept for P1(T)}
#' \item{int_p2}{Intercept for P2(T)}
#' \item{int_self}{Intercept for T(T)}
#' \item{int_mp1}{Intercept for P1(P2(T))}
#' \item{int_mp2}{Intercept for P2(P1(T))}
#' \item{v_p1}{variance for P1(T)}
#' \item{v_p2}{variance for P2(T)}
#' \item{v_self}{variance for T(T)}
#' \item{v_mp1}{variance for P1(P2(T))}
#' \item{v_mp2}{variance for P2(P1(T))}
#' \item{p1_p2_rel_el}{P1-P2 Relative Elevation (i.e., Mean P1(T) - Mean P2(T))}
#' \item{self_p2_rel_el}{Self-P2 Relative Elevation (i.e., Mean T(T) - Mean P2(T))}
#' \item{self_p1_rel_el}{Self-P1 Relative Elevation (i.e., Mean T(T) - Mean P1(T))}
#' \item{p1_meta_rel_el}{P1 Meta Relative Elevation (i.e., mean P2(T) - Mean P1(P2(T)))}
#' \item{p2_meta_rel_el}{P2 Meta Relative Elevation (i.e., mean P1(T) - Mean P2(P1(T)))}
#' }
#' \emph{If n exchangeable triads > 1:}
#' \describe{
#' {rec}{direct reciprocity; the correlation between opposit P1(T)s (e.g., A(C) <-> C(A))}
#' {h}{hearsay reciprocity; the correlation between exchangeable P2(T)s (e.g., B(C) <-> D(A))}
#' {m}{unnamed parameter; The correlation between P2(T) and the opposite P1(T) in a group. (e.g., B(C) <-> C(A))}
#' {tru_sim}{True Similarity; the correlation between targets' self-reports. (e.g., A(A) <-> C(C))}
#' {as_sim_3p}{Third-person assumed similarity; correlation between P2(T) and P1's self-report (e.g., B(C) <- A(A))}
#' {as_sim_1p}{First-person assumed similarity (i.e., interpersonal assumed similarity); correlation between P1(T) and P1's self-report (e.g., A(C) <-> A(A))}
#' {as_sim_p1m}{P1 Meta-assumed similarity (e.g., A(B(C)) <-> A(A))}
#' {ukp1m1}{unknown p1-meta 1}{P1 meta-perception with opposite P1-report (e.g., A(B(C)) <-> C(A))).}
#' {p1meta_sim}{P1 Meta-Similarity}{correlation between exchangeable P1 meta-perceptions (e.g., A(B(C)) <-> C(D(A))).}
#' {ukp2m1}{unknown P2-meta 1}{P2 Meta-perception with exchangeable P2-reports (e.g., B(A(C)) <-> D(A))}
#' {ukp2m2}{unknown P2-meta 2}{P2 Meta-perception with exchangeable target self-report (e.g., B(A(C) <-> A(A)))}
#' {ukp2m3}{unknown P2-meta 3}{P2 Meta-perception with exchangeable P1-reports (e.g., B(A(C)) <-> C(A))}
#' {p2meta_sim}{P2 Meta-Similarity}{correlation between exchangeable P2 meta-perceptions (e.g., B(A(C)) <-> D(C(A))).}
#' {ukm1}{unknown Meta-perception}{P1 Meta-perception with exchangeable P2 Meta-Perception (e.g., A(B(C)) <-> D(C(A)))}
#'}
#' The function can handle up to n exchangeable triads.
#' @param data The dataframe that contains ratings (P1, P2, target self-report, P1, and P2 meta-perceptions).
#' Data should be wide, with a row for every group of participants.
#' At a minimum, it must contain 5 columns: 1 for each of the five rating types (P1-, P2-, self-,
#' P1-meta-, P2-meta-perceptions).
#' @param  p1_reports The column(s) that contain P1 reports,
#' or ratings made by the person that knows the target directly.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param p2_reports The column(s) that contain P2 reports,
#' or ratings made by the person that knows the target indirectly through the corresponding P1.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param target_self The column(s) that contain target self-reports.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param  p1_meta The column(s) that contain P1 3rd person Meta-perceptions,
#' or P1's ratings of how they think P2 sees the target.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param p2_meta The column(s) that contain P2 3rd person Meta-perceptions,
#' or P2's ratings of how they think P1 sees the target.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
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
#' @export
#' @examples data("rep_sim_data")
#'           rep_full_w_3pmeta(data = rep_sim_data,
#'                             p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                             p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
#'                             target_self = c("C_C_agreeableness", "A_A_agreeableness"),
#'                             p1_meta = c("A_B_C_agreeableness", "C_D_A_agreeableness"),
#'                             p2_meta = c("B_A_C_agreeableness", "D_C_A_agreeableness"))
#' @return The function returns an object of class \code{\link[lavaan:lavaan-class]{lavaan}}.

rep_full_w_3pmeta <- function(data, p1_reports, p2_reports, target_self, p1_meta, p2_meta,
                              n_triads = length(p1_reports), n_p1s_per_p2s = 1,
                              n_p2s_per_p1s = 1, n_p1s_per_ts = 1,
                              n_p2s_per_ts = 1, n_ts_per_p1s = 1, n_ts_per_p2s = 1){
  if(n_triads > 0 &
     n_p1s_per_p2s == 1 &
     n_p2s_per_p1s == 1 &
     n_p1s_per_ts == 1 &
     n_p2s_per_ts == 1 &
     n_ts_per_p1s == 1 &
     n_ts_per_p2s == 1 ){

    # code for 1 triad - much simpler
    if(n_triads == 1){

      model <-
        # hearsay (P1-P2) consensus
        paste(paste(p1_reports, "~~ hc*", p2_reports),
              paste(target_self, "~~ ha*", p2_reports),
              paste(target_self, "~~ da*", p1_reports),
              paste(p1_meta, "~~p1ma*", p2_reports),
              paste(p2_meta, "~~p2ma*", p1_reports),
              paste(p1_meta, "~~as_ac1*", target_self),
              paste(p1_meta, "~~as_con1*", p1_reports),
              paste(p1_meta, "~~mp_rec*", p2_meta),
              paste(p2_meta, "~~as_ac2*", target_self),
              paste(p2_meta, "~~as_con2*", p2_reports),

              # intercepts
              paste(p1_reports, "~ int_p1*1"),
              paste(p2_reports, "~ int_p2*1"),
              paste(target_self, "~ int_self*1"),
              paste(p1_meta, "~ int_mp1*1"),
              paste(p2_meta, "~ int_mp2*1"),

              # variances
              paste(p1_reports, "~~ v_p1*", p1_reports),
              paste(p2_reports, "~~ v_p2*", p2_reports),
              paste(target_self, "~~ v_self*", target_self),
              paste(p1_meta, "~~ v_mp1*", p1_meta),
              paste(p2_meta, "~~ v_mp2*", p2_meta),

              # P1-P2 relative elevation
              "p1_p2_rel_el := 1*int_p1 - (1*int_p2)",
              "self_p2_rel_el := 1*int_self - (1*int_p2)",
              "self_p1_rel_el := 1*int_self - (1*int_p1)",
              "p1_meta_rel_el := 1*int_p2 - (1*int_mp1)",
              "p2_meta_rel_el := 1*int_p1 - (1*int_mp2)", sep = "\n")}

    if(n_triads > 1 &
       n_p1s_per_p2s == 1 &
       n_p2s_per_p1s == 1){
      # create empty model
      model <- ""
      for(i in 1:n_triads){
        # cross-target correlations
        if(i < n_triads){
          prev_i <- i:1

          m   <- paste(p1_reports[i], "~~ m*", p2_reports[-prev_i]) %>% stringr::str_flatten(collapse = "\n")
          rec <- paste(p1_reports[i], "~~ rec*", p1_reports[-prev_i]) %>% stringr::str_flatten(collapse = "\n")
          h   <-  paste(p2_reports[i], "~~ h*", p2_reports[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          tru_sim <- paste(target_self[i], "~~ tru_sim*", target_self[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          as_sim_3p <- paste(p2_reports[i], "~~ as_sim_3p*", target_self[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          as_sim_1p <- paste(p1_reports[i], "~~ as_sim_1p*", target_self[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          as_sim_p1m <- paste(p1_meta[i], "~~ as_sim_p1m*", target_self[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          ukp1m1 <- paste(p1_meta[i], "~~ ukp1m1*", p1_reports[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          p1meta_sim <- paste(p1_meta[i], "~~ p1meta_sim*", p1_meta[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          ukp2m1 <- paste(p2_meta[i], "~~ ukp2m1*", p2_reports[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          ukp2m2 <- paste(p2_meta[i], "~~ ukp2m2*", target_self[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          ukp2m3 <- paste(p2_meta[i], "~~ ukp2m3*", p1_reports[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          p2meta_sim <- paste(p2_meta[i], "~~ p2meta_sim*", p2_meta[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          ukm1 <- paste(p1_meta[i], "~~ ukm1*", p2_meta[-prev_i], "\n") %>% stringr::str_flatten(collapse = "\n")
          xtrs <- paste(m, rec, h, tru_sim, as_sim_3p, as_sim_1p,
                        ukp1m1, p1meta_sim, ukp2m1, ukp2m2, ukp2m3, p2meta_sim, ukm1, sep = "\n")
          model <- paste(model, xtrs)}}

      hc <- paste(p1_reports, "~~ hc*", p2_reports) %>% stringr::str_flatten(collapse = "\n")
      ha <- paste(target_self, "~~ ha*", p2_reports) %>% stringr::str_flatten(collapse = "\n")
      da <- paste(target_self, "~~ da*", p1_reports) %>% stringr::str_flatten(collapse = "\n")
      p1ma <- paste(p1_meta, "~~p1ma*", p2_reports) %>% stringr::str_flatten(collapse = "\n")
      p2ma <- paste(p2_meta, "~~p2ma*", p1_reports) %>% stringr::str_flatten(collapse = "\n")
      as_ac1 <- paste(p1_meta, "~~as_ac1*", target_self) %>% stringr::str_flatten(collapse = "\n")
      as_con1 <- paste(p1_meta, "~~as_con1*", p1_reports) %>% stringr::str_flatten(collapse = "\n")
      mp_rec <- paste(p1_meta, "~~mp_rec*", p2_meta) %>% stringr::str_flatten(collapse = "\n")
      as_ac2  <- paste(p2_meta, "~~as_ac2*", target_self) %>% stringr::str_flatten(collapse = "\n")
      as_con2 <- paste(p2_meta, "~~as_con2*", p2_reports) %>% stringr::str_flatten(collapse = "\n")

      int_p1 <- paste(p1_reports, "~ int_p1*1") %>% stringr::str_flatten(collapse = "\n")
      int_p2 <- paste(p2_reports, "~ int_p2*1") %>% stringr::str_flatten(collapse = "\n")
      int_self <- paste(target_self, "~ int_self*1") %>% stringr::str_flatten(collapse = "\n")
      int_mp1 <- paste(p1_meta, "~ int_mp1*1") %>% stringr::str_flatten(collapse = "\n")
      int_mp2 <- paste(p2_meta, "~ int_mp2*1") %>% stringr::str_flatten(collapse = "\n")

      v_p1 <- paste(p1_reports, "~~ v_p1*", p1_reports) %>% stringr::str_flatten(collapse = "\n")
      v_p2 <- paste(p2_reports, " ~~ v_p2*", p2_reports) %>% stringr::str_flatten(collapse = "\n")
      v_mp1 <- paste(p1_meta, "~~ v_mp1*", p1_meta) %>% stringr::str_flatten(collapse = "\n")
      v_mp2 <- paste(p2_meta, "~~ v_mp2*", p2_meta) %>% stringr::str_flatten(collapse = "\n")
      v_self <- paste(target_self, " ~~ v_self*", target_self) %>% stringr::str_flatten(collapse = "\n")
      #  P1-P2 relative elevation
      p1p2_el <- "p1_p2_rel_el := 1*int_p1 - (1*int_p2)"
      selfp2_el <- "self_p2_rel_el := 1*int_self - (1*int_p2)"
      selfp1_el <- "self_p1_rel_el := 1*int_self - (1*int_p1)"
      p1_meta_el <- "p1_meta_rel_el := 1*int_p2 - (1*int_mp1)"
      p2_meta_el <- "p2_meta_rel_el := 1*int_p1 - (1*int_mp2)"
      model <- paste(hc, ha, da, p1ma, p2ma, as_ac1, as_con1,
                     mp_rec, as_ac2, as_con2, model, int_self, int_p1,
                     int_p2, int_mp1, int_mp2, v_self, v_p1, v_p2, v_mp1, v_mp2,
                     p1p2_el, selfp2_el, selfp1_el, p1_meta_el, p2_meta_el,
                     sep = "\n")}
    # fit model and print some informaiton about it
    fitted_model <- lavaan::sem(model, data = data, missing = "FIML")
    model_type <- "Full Triadic Model: Hearsay Consensus and Accuracy"
    print(paste("Returning Results for", model_type, ". If this is not the model you intended to run, please check the data and variables you supplied."))
    cat(paste("design:\nnumber of exchangeable triads =", n_triads, "\n",
              "number of P1s per P2s", n_p1s_per_p2s, "\n",
              "number of P2s per P1s", n_p2s_per_p1s, "\n",
              "number of p1s per targets", n_p1s_per_ts, "\n",
              "number of p2s per targets", n_p2s_per_ts, "\n",
              "number of targets per p1s", n_ts_per_p1s, "\n",
              "number of targets per p2s", n_ts_per_p2s, "\n"))

    return(fitted_model) }
  if(n_p1s_per_p2s > 1){print("I'm sorry, this function can only handle designs with 1 P1 per P2; check back for changes")}
  if(n_p2s_per_p1s > 1){print("I'm sorry, this function can only handle designs with 1 P1 per P2; check back for changes")}
  if(n_ts_per_p1s  > 1){print("I'm sorry, this function can only handle designs with 1 target per P1; check back for changes")}
  if(n_ts_per_p2s  > 1){print("I'm sorry, this function can only handle designs with 1 target per P2; check back for changes")}
  if(n_p1s_per_ts > 1){print("I'm sorry, this function can only handle designs with 1 P1 per target; check back for changes")}
  if(n_p2s_per_ts > 1){print("I'm sorry, this function can only handle designs with 1 P2 per target; check back for changes")}
}

#' Reputation Analyses (automatic)
#'
#' This is a wrapper function around the reputation model functions.
#'
#' This chooses a function depending on which
#' variables sre supplied. At a minimum, it requires a P1- & P2-ratings. It can
#' optionally take target self-reports and third-person meta-perceptions (for P1 and P2).
#' It fits a model estimating the possible hearsay reputation parameters given the input.
#' See specific model functions for which parameters are estimated for each model.
#'
#' The function can handle up to n exchangeable triads.
#'
#' @param data The dataframe that contains ratings (P1, P2, target self-report, P1, and P2 meta-perceptions).
#' Data should be wide, with a row for every group of participants.
#' At a minimum, it must contain two columns: one for P1 reports and one for P2 reports.
#' @param  p1_reports The column(s) that contain P1 reports,
#' or ratings made by the person that knows the target directly.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param p2_reports The column(s) that contain P2 reports,
#' or ratings made by the person that knows the target indirectly through the corresponding P1.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param target_self Optional. The column(s) that contain target self-reports.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param  p1_meta Optional. The column(s) that contain P1 3rd person Meta-perceptions,
#' or P1's ratings of how they think P2 sees the target.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param p2_meta Optional. The column(s) that contain P2 3rd person Meta-perceptions,
#' or P2's ratings of how they think P1 sees the target.
#' If more than one is supplied, the target-wise order must match the other
#' rating types.
#' @param n_triads The number of exhangeable triads in each group. By default, this is determined by
#' counting the number of P1 reports. It is rare that this parameter would need to be changed manually.
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
#' @export
#' @examples data("rep_sim_data")
#'  rep_analyses_auto(data = rep_sim_data,
#'                   p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                   p2_reports = c("B_C_agreeableness", "D_A_agreeableness"))
#'
#'  rep_analyses_auto(data = rep_sim_data,
#'                   p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                   p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
#'                   target_self = c("C_C_agreeableness", "A_A_agreeableness"))
#'
#'  rep_analyses_auto(data = rep_sim_data,
#'                   p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                   p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
#'                   target_self = c("C_C_agreeableness", "A_A_agreeableness"),
#'                   p1_meta = c("A_B_C_agree_meta", "C_D_A_agree_meta"),
#'                   p2_meta = c("B_A_C_agree_meta", "D_C_A_agree_meta"))
#' @return The function returns an object of class \code{\link[lavaan:lavaan-class]{lavaan}}.

rep_analyses_auto <- function(data, p1_reports, p2_reports, target_self = NULL, p1_meta = NULL, p2_meta = NULL,
                    n_triads = length(p1_reports), n_p1s_per_p2s = 1,
                    n_p2s_per_p1s = 1, n_p1s_per_ts = 1,
                    n_p2s_per_ts = 1, n_ts_per_p1s = 1, n_ts_per_p2s = 1){
  if(is.null(target_self) &
     is.null(p1_meta) &
     is.null(p2_meta)){
    fitted_model <- rep_consensus(data = data, p1_reports = p1_reports, p2_reports = p2_reports)
  }
  else if(is.null(p1_meta) &
          is.null(p2_meta)){
    fitted_model <- rep_consensus_accuracy(data = data, p1_reports = p1_reports, p2_reports = p2_reports,
                                           target_self = target_self)
  }
  else if(!is.null(target_self) &
          !is.null(p1_meta) &
          !is.null(p2_meta)){
    fitted_model <- rep_full_w_3pmeta(data = data, p1_reports = p1_reports,  p2_reports = p2_reports,
                                      target_self = target_self, p1_meta = p1_meta, p2_meta = p2_meta)
  }
  else {stop("You did not supply a workable combination of variables.")}
  return(fitted_model)
}

#' Individual level Moderators (Generic)
#'
#' This is a generic function for individual-level moderators on two distinguishable ratings on the same target.
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
#' @param data The dataframe that contains the ratings, moderator variable, and the interaction term.
#' Data should be wide, with a row for every group of participants.
#' At a minimum, it must contain four columns: two for ratings (of the same target),
#' one for the mean-centered moderator variable, and one for the interaction term.
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
#'           rep_id_mods_generic(data = moderator_data,
#'                                 p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                                 p2_reports = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                 id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                 interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"))
#'
#' @return The function returns an object of class \code{\link[lavaan:lavaan-class]{lavaan}}.

rep_id_mods_generic <- function(data, rating_1, rating_2, id_mod_variable,
                                  interaction_term, n_triads = length(rating_1),
                                  n_r1_per_r2 = 1,n_r2_per_r1 = 1){
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
              paste(rating_1, "~ v_rating_1*1"),

              paste(rating_2, " ~ v_rating_2*1"),

              paste(id_mod_variable, "~ v_mod*1"),

              paste(interaction_term, "~ iny_interaction*1"),

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
              paste(rating_1, "~ v_rating_1*1"),

              paste(rating_2, " ~ v_rating_2*1"),

              paste(id_mod_variable, "~ v_mod*1"),

              paste(interaction_term, "~ iny_interaction*1"),
              sep = "\n")
    }
    # fit model and print some informaiton about it
    fitted_model <- lavaan::sem(model, data = data, missing = "FIML")
    model_type <- "Individual-level moderator (generic function)"
    print(paste("Returning Results for", model_type, ". If this is not the model you intended to run, please check the data and variables you supplied."))
    cat(paste("design:\nnumber of exchangeable triads =", n_triads))
    return(fitted_model)
    if(round(mean(colMeans(data[,rating_2], na.rm = TRUE)), 4) != 0){warning("It looks like you didn't center P2 reports. You might want to for interpretability's sake.")}
    if(round(mean(colMeans(data[,id_mod_variable], na.rm = TRUE)), 4) != 0){warning("It looks like you didn't center the moderator variable. You might want to for interpretability's sake.")}
    }}

#' Individual level Moderators of Hearsay Consensus
#'
#' This takes the variables needed to assess an individual-level moderator on hearsay consensus, and fits
#' a model estimating the corresponding parameters. At a minimum, it requires P1-reports, P2-reports,
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
#'           rep_id_mods_consensus(data = moderator_data,
#'                                 p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
#'                                 p2_reports = c("B_C_agreeableness_cent", "D_A_agreeableness_cent"),
#'                                 id_mod_variable = c("B_iri_perspective_cent", "D_iri_perspective_cent"),
#'                                 interaction_term = c("B_ptXagree_interaction", "D_ptXagree_interaction"))
#'
#' @return The function returns an object of class \code{\link[lavaan:lavaan-class]{lavaan}}.

rep_id_mods_consensus <- function(data, p1_reports, p2_reports, id_mod_variable,
                                  interaction_term, n_triads = length(p1_reports),
                                  n_p1s_per_p2s = 1,n_p2s_per_p1s = 1){
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
    # fit model and print some informaiton about it
    fitted_model <- lavaan::sem(model, data = data, missing = "FIML")
    model_type <- "Individual-level moderator on Hearsay Consensus"
    print(paste("Returning Results for", model_type, ". If this is not the model you intended to run, please check the data and variables you supplied."))
    cat(paste("design:\nnumber of exchangeable triads =", n_triads, "\n",
              "number of P1s per P2s", n_p1s_per_p2s, "\n",
              "number of P2s per P1s", n_p2s_per_p1s, "\n"))
    if(round(mean(colMeans(data[,p2_reports], na.rm = TRUE)), 4) != 0){warning("It looks like you didn't center P2 reports. You might want to for interpretability's sake.")}
    if(round(mean(colMeans(data[,id_mod_variable], na.rm = TRUE)), 4) != 0){warning("It looks like you didn't center the moderator variable. You might want to for interpretability's sake.")}
    return(fitted_model)
    }
if(n_p1s_per_p2s > 1){warning("I'm sorry, this function can only handle designs with 1 P1 per P2; check back for changes")}
if(n_p2s_per_p1s > 1){warning("I'm sorry, this function can only handle designs with 1 P2 per P1; check back for changes")}
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
#' @import lavaan
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
