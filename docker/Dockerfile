# https://hub.docker.com/r/rocker/shiny/
FROM rocker/shiny-verse:latest

# install necessary system dependencies for r packages (e.g. devtools, RPostgres)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libxml2-dev \
    apt-utils \
    libcurl4-openssl-dev \
    libssl-dev \
    libpq-dev \
    gpg-agent \
    git

ENV RSESSION_USER=shiny

# set cran repo
RUN echo "options('repos' = 'https://cloud.r-project.org/')" >> /usr/local/lib/R/etc/Rprofile.site

ARG p_base="devtools \
    ggpubr"

RUN for package in $p_base; do \
    R -q -e "p <- \"$package\"; remotes::update_packages(packages = p, upgrade = \"always\")"; \
    done

# add cloned repo
COPY ./addfolder /home/${RSESSION_USER}/

# repository name
ARG REPO_NAME=rBiasCorrection

# install our R-Package
RUN cd /home/${RSESSION_USER}/${REPO_NAME}/ && \
    R -q -e "devtools::install('.')"

# repository name
ARG REPO_NAME=BiasCorrector

# install our R-Package
RUN cd /home/${RSESSION_USER}/${REPO_NAME}/ && \
    R -q -e "devtools::install('.')"

# add shiny app
RUN cd /home/${RSESSION_USER}/${REPO_NAME}/docker && \
    cp app.R /srv/shiny-server/ && \
    # add custom server conf (running shiny as user 'shiny' is more secure than running as 'root')
    cp shiny-server.conf /etc/shiny-server/ && \
    # add log-script
    cp show-log.sh /


# fix permissions of directories, where R/ the app need rights to write
RUN chown -R ${RSESSION_USER}:${RSESSION_USER} /srv/shiny-server/
RUN chmod +x show-log.sh

USER shiny

# Start shiny server
ENTRYPOINT ["/usr/bin/shiny-server"]
