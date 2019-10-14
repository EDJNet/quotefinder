FROM rocker/r-ver:3.6.0

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev libxml2-dev

RUN apt-get install -y pandoc

RUN apt-get install -y wget curl lbzip2

RUN R -e 'install.packages("remotes")'

RUN R -e 'remotes::install_cran("dplyr")'
RUN R -e 'remotes::install_cran("tidyr")'
RUN R -e 'remotes::install_cran("purrr")'

RUN R -e 'remotes::install_cran("rmarkdown")'

RUN R -e 'remotes::install_cran("shiny")'
RUN R -e 'remotes::install_cran("rtweet")'
RUN R -e 'remotes::install_cran("stringr")'
RUN R -e 'remotes::install_cran("fs")'
RUN R -e 'remotes::install_cran("DT")'
RUN R -e 'remotes::install_cran("glue")'
RUN R -e 'remotes::install_cran("shinydashboard")'
RUN R -e 'remotes::install_cran("magrittr")'

RUN R -e 'remotes::install_cran("shiny")'
RUN R -e 'remotes::install_cran("tidytext")'
RUN R -e 'remotes::install_cran("wordcloud")'
RUN R -e 'remotes::install_cran("wordcloud2")'
RUN R -e 'remotes::install_cran("reshape2")'
RUN R -e 'remotes::install_cran("stopwords")'
RUN R -e 'remotes::install_cran("syuzhet")'
RUN R -e 'remotes::install_cran("colourpicker")'
RUN R -e 'remotes::install_cran("shinyWidgets")'
RUN R -e 'remotes::install_cran("RColorBrewer")'
RUN R -e 'remotes::install_cran("shinycustomloader")'
RUN R -e 'remotes::install_cran("readr")'
RUN R -e 'remotes::install_cran("curl")'
RUN R -e 'remotes::install_cran("webshot")'

RUN R -e 'webshot::install_phantomjs()'

RUN R -e 'remotes::install_github(repo = "Lchiffon/wordcloud2")'
RUN R -e 'remotes::install_github("giocomai/edjnetquotefinder")'

EXPOSE 3939
CMD R -e "edjnetquotefinder::qf_wordcloud_app(shiny_port = 3939)"
