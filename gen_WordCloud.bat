
@echo off

if exist "%1" (
  "C:\Program Files\R\R-3.5.1\bin\Rscript" --vanilla "gen_WordCloud.R" %1
) else (
  echo Nombre de archivo incorrecto o no existe. ["%1"]
)