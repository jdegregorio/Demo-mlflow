FROM rocker/r-ver:3.6.1

RUN R -e "install.packages('packrat')"

COPY ./packrat/packrat.lock packrat/

RUN R -e "packrat::init(restart = FALSE)"

RUN R -e "packrat::restore()"