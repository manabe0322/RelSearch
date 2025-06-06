% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis_auto.R
\name{analyze_auto}
\alias{analyze_auto}
\title{analyze_auto}
\usage{
analyze_auto(
  dt_v_auto,
  dt_r_auto,
  dt_af,
  dt_rel,
  dt_myu,
  dt_criteria,
  show_progress = TRUE
)
}
\arguments{
\item{dt_v_auto}{A data.table of victim profiles (autosomal STR)}

\item{dt_r_auto}{A data.table of reference profiles (autosomal STR)}

\item{dt_af}{A data.table of allele frequencies (autosomal STR)}

\item{dt_rel}{A data.table of information on relationships}

\item{dt_myu}{A data.table of mutation rates}

\item{dt_criteria}{A data.table of criteria}

\item{show_progress}{Whether the progress is shown or not}
}
\description{
The function to analyze data for autosomal STR
}
