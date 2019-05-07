FROM ubuntu:18.04

# set environment variable to supress user interaction
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    apt-utils \ 
    dialog \
    dirmngr \
    gdebi-core \
    gpg-agent \
    less \
    libcurl4-openssl-dev \
    libssh-dev \
    libssl-dev \
    locales \
    nano \
    software-properties-common \
    ssh \
    sudo \
    tar \
    unzip \
    vim \
    wget \
    xtail

RUN apt-get clean

RUN locale-gen en_US.utf8 \
    && /usr/sbin/update-locale LANG=en_US.UTF-8
ENV LANG=en_US.UTF-8

# add user + password
# https://stackoverflow.com/questions/2150882/how-to-automatically-add-user-account-and-password-with-a-bash-script
RUN useradd -ms /bin/bash user
RUN echo user:password | chpasswd 

# Add R apt repository for latest R
# https://cran.r-project.org/bin/linux/ubuntu/
RUN gpg --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN gpg -a --export E298A3A825C0D65DFD57CBB651716619E084DAB9 | sudo apt-key add -
RUN add-apt-repository "deb http://cran.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran35/"

RUN apt-get update && apt-get install -y --no-install-recommends \
    r-base-dev

# install opensource shiny server
# https://www.rstudio.com/products/shiny/download-server/
# https://github.com/rocker-org/shiny/blob/master/Dockerfile
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb

# install necessary r-packages
ARG p_base="shiny \
    shinyjs \
    shinydashboard \
    shinyWidgets \
    DT \
    data.table \
    ggplot2"

RUN for package in $p_base; do \
    R -q -e "p <- \"$package\"; if (isFALSE(p %in% installed.packages()[,\"Package\"])){; cat(paste(\"Installing package:\", p, \"\n\n\")); install.packages(p, repos = \"https://ftp.fau.de/cran/\", quiet=T);} else {;cat(paste(\"Package\", p, \"is already installed\n\n\"));}"; \
    done

ARG p_add="magrittr \
    polynom"

RUN for package in $p_add; do \
    R -q -e "p <- \"$package\"; if (isFALSE(p %in% installed.packages()[,\"Package\"])){; cat(paste(\"Installing package:\", p, \"\n\n\")); install.packages(p, repos = \"https://ftp.fau.de/cran/\", quiet=T);} else {;cat(paste(\"Package\", p, \"is already installed\n\n\"));}"; \
    done

# server configuration
ADD shiny-server.conf /etc/shiny-server/shiny-server.conf

# copy app
ADD addfolder /srv/shiny-server/

# fix permissions
RUN chown -R shiny:shiny /srv/shiny-server/

# make app available at port 80
EXPOSE 3838

# copy server script
# https://github.com/rocker-org/shiny/blob/master/shiny-server.sh
ADD shiny-server.sh /usr/bin/shiny-server.sh 
RUN cd /usr/bin/ && chmod +x shiny-server.sh 

CMD ["/usr/bin/shiny-server.sh"]
