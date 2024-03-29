% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/customize_comment.R
\name{customize_comment}
\alias{customize_comment}
\title{Customized comments}
\usage{
customize_comment(grade, structure)
}
\arguments{
\item{grade}{Numeric. Grade of the student.}

\item{structure}{Tibble. Variables: threshold, status, color, and comment. Comment is not necessary.}
}
\value{
character. Comments adapted to the grade obtained by the student.
}
\description{
Function selecting the appropriate comment to be addressed to a student based on her or his grade.
}
\author{
Nicolas Mangin
}
