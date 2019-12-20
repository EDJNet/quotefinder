FROM rocker/r-ver:3.6.1

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev libxml2-dev

RUN apt-get install -y pandoc wget

RUN apt-get install -y fonts-roboto fonts-roboto-fontface

RUN apt-get install build-essential chrpath libssl-dev libxft-dev -y
RUN apt-get install libfreetype6 libfreetype6-dev -y
RUN apt-get install libfontconfig1 libfontconfig1-dev -y
RUN wget https://github.com/Medium/phantomjs/releases/download/v2.1.1/phantomjs-2.1.1-linux-x86_64.tar.bz2
RUN tar xvjf phantomjs-2.1.1-linux-x86_64.tar.bz2
RUN mv phantomjs-2.1.1-linux-x86_64 /usr/local/share
RUN ln -sf /usr/local/share/phantomjs-2.1.1-linux-x86_64/bin/phantomjs /usr/local/bin
RUN chmod +x /usr/local/share/phantomjs-2.1.1-linux-x86_64/bin/phantomjs
RUN phantomjs --version

RUN R -e 'install.packages("remotes")'

RUN R -e 'remotes::install_cran("rmarkdown")'

RUN R -e 'remotes::install_cran("dplyr")'
RUN R -e 'remotes::install_cran("reshape2")'
RUN R -e 'remotes::install_cran("tidyr")'
RUN R -e 'remotes::install_cran("stringr")'
RUN R -e 'remotes::install_cran("fs")'
RUN R -e 'remotes::install_cran("tidytext")'
RUN R -e 'remotes::install_cran("wordcloud")'
RUN R -e 'remotes::install_cran("extrafont")'

RUN R -e 'extrafont::font_import(prompt=FALSE)'
RUN R -e 'extrafont::loadfonts()'

RUN R -e 'remotes::install_cran("rtweet")'


CMD Rscript /home/qf/qf_data/qf_twitter_bot.R
