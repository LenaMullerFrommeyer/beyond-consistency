# beyond-consistency
This repository contains the code for the analyses presented in our article,
"Beyond consistency: Contextual dependency of language style in monologue and
conversation" (Müller-Frommeyer, Kauffeld, & Paxton, *accepted*,
  *Cognitive Science*).

# Overview
The repository contains several analysis files, figures, an R markdown file,
and a markdown file.

- `beyond_consistency.Rmd`: R markdown with all data preparation, analysis, and
  visualization presented in our manuscript. Note that it includes some
  visualizations that are not included in the manuscript itself.
- `beyond_consistency.md`: A markdown file generated by the R markdown of the
  same name. We recommend that you open this version to view in your browser.
- `./scripts/bc-libraries_and_functions.R`: Downloads and loads in necessary
  packages and libraries to run the R markdown. (This is included in the R
  markdown file and does not need to be run separately.)
- `./figures`: Contains recurrence plots for all monologues and conversations.
  The code to re-create them is included in the R markdown.

# Related materials
Anonymized LIWC outputs that are the basis for the calculations in the paper
can be found at our OSF project: https://osf.io/3jx2g/

# Notes on running and viewing
For best viewing in a browser, we recommend selecting `beyond_consistency.md`,
rather than the similarly named `.Rmd` file. (Analyses should be run using the
`.Rmd` file of the same name.) For those unfamiliar with R markdown, we
recommend taking a look at
[RStudio's introduction to R markdown](http://rmarkdown.rstudio.com/) before
attempting to run the `.Rmd` file. (Be sure to download
[RStudio](https://www.rstudio.com/) first, if you do not already have it
installed on your machine.)
