% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_icon_picker.R
\name{input_icon_picker}
\alias{input_icon_picker}
\title{Icon Picker}
\usage{
input_icon_picker(
  inputId,
  label,
  selected = NULL,
  btn_class = "btn-outline-secondary"
)
}
\arguments{
\item{inputId}{chr: the ID of the input element.}

\item{label}{chr: the label of the icon picker.}

\item{selected}{chr: the name of the selected fontawesome icon. Must have the form 'fas fa-name'.}

\item{btn_class}{chr: the class of the button element.}
}
\value{
a dropdown list with icons and a hidden input to hold their values.
}
\description{
Create an icon picker element.
}
