FROM rocker/r-ver:3.6.1

RUN apt-get update && apt-get install -y \
libcurl4-openssl-dev libssl-dev pandoc libxml2-dev

RUN apt-get install -y wget libappindicator3-1 libatk-bridge2.0-0 libgtk-3-0 lsb-release

RUN wget https://github.com/Medium/phantomjs/releases/download/v2.1.1/phantomjs-2.1.1-linux-x86_64.tar.bz2
RUN tar xvjf phantomjs-2.1.1-linux-x86_64.tar.bz2
RUN mv phantomjs-2.1.1-linux-x86_64 /usr/local/share
RUN ln -sf /usr/local/share/phantomjs-2.1.1-linux-x86_64/bin/phantomjs /usr/local/bin
RUN chmod +x /usr/local/share/phantomjs-2.1.1-linux-x86_64/bin/phantomjs
RUN phantomjs --version

RUN apt-get install -y locales-all

RUN R -e 'install.packages("remotes")'

RUN R -e 'remotes::install_cran("rmarkdown")'

RUN R -e 'remotes::install_cran("dplyr")'
RUN R -e 'remotes::install_cran("tidyr")'

RUN R -e 'remotes::install_cran("xml2")'
RUN R -e 'remotes::install_cran("rvest")'
RUN R -e 'remotes::install_cran("ggplot2")'
RUN R -e 'remotes::install_cran("tidytext")'



RUN R -e 'remotes::install_github("giocomai/castarter")'

CMD Rscript /home/qf/qf_data/qf_get_eu_castarter.R
