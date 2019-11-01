# https://hub.docker.com/r/rocker/shiny/
FROM rocker/shiny-verse

# install necessary system dependencies for r packages (e.g. devtools, RPostgres)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libxml2-dev \
    apt-utils \
    libcurl4-openssl-dev \
    libssl-dev \
    libpq-dev \
    pandoc \
    pandoc-citeproc \
    gpg-agent \
    git

# set cran repo
RUN echo "options('repos' = 'https://ftp.fau.de/cran/')" >> /usr/local/lib/R/etc/Rprofile.site

ARG p_base="devtools"

RUN for package in $p_base; do \
    R -q -e "p <- \"$package\"; if (isFALSE(p %in% installed.packages()[,\"Package\"])){; cat(paste(\"Installing package:\", p, \"\n\n\")); install.packages(p, repos = \"https://ftp.fau.de/cran/\", quiet=T);} else {;cat(paste(\"Package\", p, \"is already installed\n\n\"));}"; \
    done

# add cloned repo
COPY ./addfolder /home/shiny/

# repository name
ARG REPO_NAME=rBiasCorrection

# install our R-Package
RUN cd /home/shiny/${REPO_NAME}/ && \
    R -q -e "devtools::install('.')"

# repository name
ARG REPO_NAME=BiasCorrector

# install our R-Package
RUN cd /home/shiny/${REPO_NAME}/ && \
    R -q -e "devtools::install('.')"

# add shiny app
RUN cd /home/shiny/${REPO_NAME}/docker && \
    cp app.R /srv/shiny-server/ && \
    # add custom server conf (running shiny as user 'shiny' is more secure than running as 'root')
    cp shiny-server.conf /etc/shiny-server/ && \
    # add log-script
    cp show-log.sh /


# fix permissions of directories, where R/ the app need rights to write
RUN chown -R shiny:shiny /srv/shiny-server/
RUN chmod +x show-log.sh

CMD ["/usr/bin/shiny-server.sh"]
