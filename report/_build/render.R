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
# .encode(language ="ES")
# .decode(language="EN")


.buildYAML(
    language = "ES",
    output_format = c("html", "docx"),
    output_dir = publishFolder
)

quarto::quarto_render(input = "index.qmd")

source("_build/postRender.R")

system2("netlify", args = c("deploy --prod"))
