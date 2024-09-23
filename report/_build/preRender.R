subdirs <- list.dirs(path = markupFolder, full.names = TRUE, recursive = TRUE)[-1]
unlink(subdirs, recursive = TRUE, force = TRUE)

subdirs <- list.dirs(path = renderFolder, full.names = TRUE, recursive = TRUE)[-1]
unlink(subdirs, recursive = TRUE, force = TRUE)

subdirs <- list.dirs(path = publishFolder, full.names = TRUE, recursive = TRUE)[-1]
unlink(subdirs, recursive = TRUE, force = TRUE)
