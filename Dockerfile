FROM rocker/shiny:latest

RUN R -e 'install.packages("pak"); pak::pak("cimentadaj/lifetableprojection")'

EXPOSE 8180

CMD ["Rscript", "-e", "options(shiny.port=8180, shiny.host='0.0.0.0'); lifetableprojection::run_app()"]
