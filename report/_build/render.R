source("_build/utils.R")

projectFolder <- file.path(".") 
buildFolder <- file.path("_build")
markupFolder <- file.path("_markup")
publishFolder <- file.path("_publish")
exportFolder <- file.path("_export")
renderFolder <- file.path("_render")
markup_filename <- "markup.Rds"
index_filename <- "index.qmd"
quarto_filename <- "_quarto.yml"

source("_build/preRender.R")
# .encode(language ="EN")
# .decode(language="EN")


# .buildYAML(output_format=c("els-pdf"),output_dir=publishFolder)
# quarto::quarto_render(input = "index.qmd",output_format = "els-pdf")
# 
# .buildYAML(output_format=c("docx"),output_dir=publishFolder)
# quarto::quarto_render(input = "index.qmd",output_format = "docx")

.buildYAML(output_format=c("html"),output_dir=publishFolder)
quarto::quarto_render(input = "index.qmd",output_format = "html")


# OUTPUT_FORMAT <- "els-pdf"
# OUTPUT_FORMAT <- "revealjs"


source("_build/postRender.R")



