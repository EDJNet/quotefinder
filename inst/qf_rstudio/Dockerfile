FROM rocker/verse

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev

RUN apt-get update && apt-get install -y cron

RUN apt-get install -y pandoc

RUN R -e 'install.packages("remotes")'

RUN R -e 'remotes::install_cran("rmarkdown")'

RUN R -e 'remotes::install_cran("shiny")'
RUN R -e 'remotes::install_cran("dplyr")'
RUN R -e 'remotes::install_cran("rtweet")'
RUN R -e 'remotes::install_cran("stringr")'
RUN R -e 'remotes::install_cran("fs")'
RUN R -e 'remotes::install_cran("DT")'
RUN R -e 'remotes::install_cran("glue")'
RUN R -e 'remotes::install_cran("shinydashboard")'
RUN R -e 'remotes::install_cran("magrittr")'
RUN R -e 'remotes::install_cran("tidytext")'
RUN R -e 'remotes::install_cran("readr")'

RUN R -e 'remotes::install_github("giocomai/edjnetquotefinder")'
