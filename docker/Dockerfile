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

RUN for package in $p_base; do \
    R -q -e "p <- \"$package\"; if (isFALSE(p %in% installed.packages()[,\"Package\"])){; cat(paste(\"Installing package:\", p, \"\n\n\")); install.packages(p, repos = \"https://ftp.fau.de/cran/\", quiet=T);} else {;cat(paste(\"Package\", p, \"is already installed\n\n\"));}"; \
    done

ARG p_add="DT \
    shiny \
    shinyjs \
    shinydashboard"

RUN for package in $p_add; do \
    R -q -e "p <- \"$package\"; if (isFALSE(p %in% installed.packages()[,\"Package\"])){; cat(paste(\"Installing package:\", p, \"\n\n\")); install.packages(p, repos = \"https://ftp.fau.de/cran/\", quiet=T);} else {;cat(paste(\"Package\", p, \"is already installed\n\n\"));}"; \
done
    
# install PCRBiasCorrection
RUN R -q -e "devtools::install_github('kapsner/PCRBiasCorrection@v0.1.0.9001')"

# clone repo
RUN cd /home/shiny &&\
    git clone https://github.com/kapsner/BiasCorrector.git

ARG REPO_NAME=BiasCorrector

# add shiny app
RUN cd /home/shiny/${REPO_NAME}/docker && \
    cp -r ../BiasCorrector/* /srv/shiny-server/ && \
    # add custom server conf (running shiny as user 'shiny' is more secure than running as 'root')
    cp shiny-server.conf /etc/shiny-server/ && \
    # add log-script
    cp show-log.sh /

# fix permissions of directories, where R/ the app need rights to write
RUN chown -R shiny:shiny /srv/shiny-server/
RUN chmod +x show-log.sh

CMD ["/usr/bin/shiny-server.sh"]
