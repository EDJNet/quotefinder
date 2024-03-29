% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qf_emm_newsbrief.R
\name{qf_get_emm_newsbrief}
\alias{qf_get_emm_newsbrief}
\title{Get current news in multiple languages from EMM newsbrief}
\usage{
qf_get_emm_newsbrief(
  languages = "en",
  category_id = "rtn",
  keep_xml = TRUE,
  shuffle = TRUE,
  store = TRUE,
  store_only_new = TRUE
)
}
\arguments{
\item{languages}{A character vector of one or more languages as two letter
codes (e.g. "en"). Defaults to "en", as the previous default, "all", is not
any more available. Current available languages are:
`c("ar","bg","cs","da","de","el","en","es", "et","fi","fr","hr", "hu","it",
"lt","lv","nl","pl","pt","ro","ru", "sk","sl","sv","sw","tr","zh")`}

\item{category_id}{A character vector of length 1, defaults to "rtn" (default
category with all contents). The id for separate categories can be derived
from the URL of the given section, e.g. for EC news the id is "ECnews".}

\item{keep_xml}{Logical, defaults to FALSE. If TRUE, removes the xml file
downloaded from EMM newsbrief.}

\item{shuffle}{Logical, defaults to TRUE. If TRUE, order in which languages
are processed is randomised.}

\item{store}{Logical, defaults to TRUE. If TRUE, it stores the data extracted
from the xml as an rds file in a local subfolder, organised by language and
date.}

\item{store_only_new}{Logical, defaults to TRUE. Before storing outputs, it
checks the previous file and keeps only articles published after the most
recent article incldued in the previous post.}
}
\value{
Nothing, used for its side effects.
}
\description{
Imports and cleans RSS files from [http://emm.newsbrief.eu/]
}
\examples{

}
