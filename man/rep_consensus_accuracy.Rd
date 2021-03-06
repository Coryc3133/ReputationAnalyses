% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reputation_model.R
\name{rep_consensus_accuracy}
\alias{rep_consensus_accuracy}
\title{Reputation Consensus & Accuracy Model}
\usage{
rep_consensus_accuracy(data, model = NULL, p1_reports, p2_reports,
  target_self, n_triads = length(p1_reports), n_p1s_per_p2s = 1,
  n_p2s_per_p1s = 1, n_p1s_per_ts = 1, n_p2s_per_ts = 1,
  n_ts_per_p1s = 1, n_ts_per_p2s = 1)
}
\arguments{
\item{data}{The dataframe.
Data should be wide, with a row for every group of participants.
At a minimum, it must contain three columns: one for P1-reports, one for P2-reports, and
one for targets' self-ratings.}

\item{model}{Optional. A model from the corresponding ReputationModelR model builder function. If this
is supplied, no additional arguments need to be specified.}

\item{p1_reports}{Quoted column names that contain P1 reports,
or ratings made by the person that knows the target directly.
If more than one is supplied, the target-wise order must match the other
rating types.}

\item{p2_reports}{Quoted column names that contain P2 reports,
or ratings made by the person that knows the target indirectly through the corresponding P1.
If more than one is supplied, the target-wise order must match the other
rating types.}

\item{target_self}{Quoted column names that contain target self-reports.
If more than one is supplied, the target-wise order must match the other
rating types.}

\item{n_triads}{The number of exchangeable triads in each group. By default, this is determined by
counting the number of P1 reports. It is rare that this parameter would need to be changed.}

\item{n_p1s_per_p2s}{The number of P1s for every P2. This defaults to 1.
Currently, only values of 1 are supported.}

\item{n_p2s_per_p1s}{The number of P2s for every P1;. This defaults to 1.
Currently, only values of 1 are supported.}

\item{n_p1s_per_ts}{The number of P1s for every target;. This defaults to 1.
Currently, only values of 1 are supported.}

\item{n_p2s_per_ts}{The number of P2s for every target;. This defaults to 1.
Currently, only values of 1 are supported.}

\item{n_ts_per_p1s}{The number of targets for every P1;. This defaults to 1.
Currently, only values of 1 are supported.}

\item{n_ts_per_p2s}{The number of targets for every P2;. This defaults to 1.
Currently, only values of 1 are supported.}
}
\value{
The function returns an object of class \code{\link[lavaan:lavaan-class]{lavaan}}.
}
\description{
This takes a dataset containing P1-, P2-, and target self-reports and either the
names of columns containing those reports or a model from one of the ReputationModelR
model builder function, and fits a model estimating the possible hearsay reputation parameters.
Those parameters are:
\describe{
\item{hc}{hearsay consensus; the correlation between P1(T) & P2(T)}
\item{ha}{hearsay accuracy; the correation between P2(T) & T(T)}
\item{da}{direct accuracy; the correlation between P1(T) & T(T)}
\item{int_p1}{Intercept for P1(T)}
\item{int_p2}{Intercept for P2(T)}
\item{int_self}{Intercept for T(T)}
\item{v_p1}{variance for P1(T)}
\item{v_p2}{variance for P2(T)}
\item{v_self}{variance for T(T)}
\item{p1_p2_rel_el}{P1-P2 Relative Elevation (i.e., Mean P1(T) - Mean P2(T))}
\item{self_p2_rel_el}{Self-P2 Relative Elevation (i.e., Mean T(T) - Mean P2(T))}
\item{self_p1_rel_el}{Self-P1 Relative Elevation (i.e., Mean T(T) - Mean P1(T))}
}
\emph{If n exchangeable triads > 1:}
\describe{
\item{rec}{direct reciprocity; the correlation between opposit P1(T)s (e.g., A(C) <-> C(A))}
\item{h}{hearsay reciprocity; the correlation between exchangeable P2(T)s (e.g., B(C) <-> D(A))}
\item{m}{unnamed parameter; The correlation between P2(T) and the opposite P1(T) in a group. (e.g., B(C) <-> C(A))}
\item{tru_sim}{True Similarity; the correlation between targets' self-reports. (e.g., A(A) <-> C(C))}
\item{as_sim_3p}{Third-person assumed similarity; correlation between P2(T) and P1's self-report (e.g., B(C) <- A(A))}
\item{as_sim_1p}{First-person assumed similarity (i.e., interpersonal assumed similarity); correlation betweenP1(T) and P1's self-report (e.g., A(C) <-> A(A))}
}
The function can handle up to n exchangeable triads.
}
\examples{
data("rep_sim_data")
          agree_con_acc <- rep_consensus_accuracy(data = rep_sim_data,
                                                  p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
                                                  p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
                                                  target_self = c("C_C_agreeableness", "A_A_agreeableness"))
         # alternatively
         agree_con_acc_model <- rep_consensus_accuracy_builder(p1_reports = c("A_C_agreeableness", "C_A_agreeableness"),
                                                                   p2_reports = c("B_C_agreeableness", "D_A_agreeableness"),
                                                                   target_self = c("C_C_agreeableness", "A_A_agreeableness"))

         agree_con_acc <- rep_consensus_accuracy(data = rep_sim_data,
                                                 model = agree_con_acc_model)

}
