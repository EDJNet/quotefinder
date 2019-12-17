FROM rocker/r-ver:3.6.1

RUN apt-get update && apt-get install -y \
sudo \
gdebi-core \
pandoc \
pandoc-citeproc \
libcurl4-gnutls-dev \
libcairo2-dev \
libxt-dev \
xtail \
wget \
fonts-roboto \
fonts-roboto-fontface


# Download and install shiny server
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
VERSION=$(cat version.txt)  && \
wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
gdebi -n ss-latest.deb && \
rm -f version.txt ss-latest.deb && \
. /etc/environment && \
R -e "install.packages(c('shiny', 'rmarkdown'), repos='$MRAN')" && \
cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/ && \
chown shiny:shiny /var/lib/shiny-server

RUN apt-get install build-essential chrpath libssl-dev libxft-dev -y
RUN apt-get install libfreetype6 libfreetype6-dev -y
RUN apt-get install libfontconfig1 libfontconfig1-dev -y
RUN wget https://github.com/Medium/phantomjs/releases/download/v2.1.1/phantomjs-2.1.1-linux-x86_64.tar.bz2
RUN tar xvjf phantomjs-2.1.1-linux-x86_64.tar.bz2
RUN mv phantomjs-2.1.1-linux-x86_64 /usr/local/share
RUN ln -sf /usr/local/share/phantomjs-2.1.1-linux-x86_64/bin/phantomjs /usr/local/bin
RUN chmod +x /usr/local/share/phantomjs-2.1.1-linux-x86_64/bin/phantomjs
RUN phantomjs --version

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev libxml2-dev


RUN R -e 'install.packages("remotes")'

RUN R -e 'remotes::install_cran("tidyverse")'

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

RUN R -e 'remotes::install_cran("shiny")'
RUN R -e 'remotes::install_cran("tidytext")'
RUN R -e 'remotes::install_cran("wordcloud")'
RUN R -e 'remotes::install_cran("reshape2")'
RUN R -e 'remotes::install_cran("stopwords")'
RUN R -e 'remotes::install_cran("syuzhet")'
RUN R -e 'remotes::install_cran("colourpicker")'
RUN R -e 'remotes::install_cran("shinyWidgets")'
RUN R -e 'remotes::install_cran("RColorBrewer")'
RUN R -e 'remotes::install_cran("shinycustomloader")'
RUN R -e 'remotes::install_cran("readr")'

RUN R -e 'remotes::install_github(repo = "Lchiffon/wordcloud2")'

RUN R -e 'remotes::install_cran("webshot")'

RUN R -e 'remotes::install_cran("golem")'

RUN R -e 'remotes::install_cran("extrafont")'
RUN R -e 'extrafont::font_import(prompt=FALSE)'
RUN R -e 'extrafont::loadfonts()'

RUN R -e 'remotes::install_github(repo = "JohnCoene/marker")'
RUN R -e 'remotes::install_cran("waiter")'

RUN R -e 'remotes::install_github(repo = "giocomai/castarter")'
RUN R -e 'remotes::install_github(repo = "edjnet/quotefinder")'

# add .conf file to image/container to preserve log file
COPY  shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY  shiny-server.sh /usr/bin/shiny-server.sh

EXPOSE 3838

CMD ["/usr/bin/shiny-server.sh"]
