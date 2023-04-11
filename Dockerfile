FROM rocker/verse:4.2.2
RUN apt-get update && apt-get install -y  libcurl4-openssl-dev libicu-dev libmariadb-dev libssl-dev libxml2-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("tibble",upgrade="never", version = "3.1.8")'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "1.0.6")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.6.2")'
RUN Rscript -e 'remotes::install_version("cli",upgrade="never", version = "3.6.0")'
RUN Rscript -e 'remotes::install_version("R6",upgrade="never", version = "2.5.1")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.4")'
RUN Rscript -e 'remotes::install_version("sass",upgrade="never", version = "0.4.5")'
RUN Rscript -e 'remotes::install_version("fontawesome",upgrade="never", version = "0.5.0")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.4")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("RColorBrewer",upgrade="never", version = "1.1-3")'
RUN Rscript -e 'remotes::install_version("lubridate",upgrade="never", version = "1.9.2")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.1.3")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("fresh",upgrade="never", version = "0.2.0")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.1.6")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.2")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.7.6")'
RUN Rscript -e 'remotes::install_version("RMariaDB",upgrade="never", version = "1.2.2")'
RUN Rscript -e 'remotes::install_version("reactable",upgrade="never", version = "0.4.4")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "1.0.1")'
RUN Rscript -e 'remotes::install_version("ids",upgrade="never", version = "1.0.1")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.4.0")'
RUN Rscript -e 'remotes::install_version("colourpicker",upgrade="never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("bs4Dash",upgrade="never", version = "2.2.1")'
RUN Rscript -e 'remotes::install_github("JohnCoene/firebase@072fd5d8c7c25f52a3bec92c8d160edb164e1698")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 80
USER rstudio
WORKDIR /home/rstudio
ADD .Renviron .Renviron
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');library(appReporteAvance);appReporteAvance::run_app()"
