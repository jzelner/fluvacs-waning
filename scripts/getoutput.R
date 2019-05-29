#! /usr/bin/env Rscript
require(glue)
cmd <- "rsync -azvh sven:~/repos/projects/bread-and-roses/output"
system(cmd)
