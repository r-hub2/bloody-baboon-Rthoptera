% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spectrogram_app.R
\name{spectrogram_app}
\alias{spectrogram_app}
\title{Spectrogram Shiny App}
\usage{
spectrogram_app(launch.browser = FALSE)
}
\arguments{
\item{launch.browser}{Logical. If TRUE, the app will automatically open in the default web browser. Defaults to FALSE.}
}
\value{
A Shiny app for visualizing and saving spectrograms of audio wave objects.
}
\description{
This Shiny app allows users to generate a standardized-resolution spectrogram from a wave object with options to display the mean spectrum and adjust the noise floor. The user can save the spectrogram as a PNG image.
}
\examples{
 \dontrun{
if (interactive()) {
  spectrogram_app()
}
}
}
