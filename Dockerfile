FROM rocker/shiny-verse:latest

RUN R <./installLibs.R

RUN rm -rf /srv/shiny-server/*

WORKDIR /srv/shiny-server/

COPY ./*.qmd ./*.RDS ./*.txt ./*.json ./*.yml ./*.css .
RUN mkdir img IntroStats _extensions RIntro
COPY ./img/* ./img
COPY ./IntroStats/* ./IntroStats
COPY ./RInto/* ./RIntro
COPY ./_extensions/* ./_extensions

