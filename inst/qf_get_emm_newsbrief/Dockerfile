FROM rocker/r-ver:3.6.1

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev pandoc libxml2-dev 

RUN R -e 'install.packages("remotes")'

RUN R -e 'remotes::install_cran("rmarkdown")'

RUN R -e 'remotes::install_cran("dplyr")'
RUN R -e 'remotes::install_cran("tidyr")'
RUN R -e 'remotes::install_cran("stringr")'
RUN R -e 'remotes::install_cran("fs")'
RUN R -e 'remotes::install_cran("glue")'
RUN R -e 'remotes::install_cran("readr")'
RUN R -e 'remotes::install_cran("tidytext")'
RUN R -e 'remotes::install_cran("xml2")'
RUN R -e 'remotes::install_cran("urltools")'

RUN R -e 'remotes::install_github("edjnet/quotefinder")'

CMD Rscript /home/qf/qf_data/qf_get_emm_newsbrief.R
