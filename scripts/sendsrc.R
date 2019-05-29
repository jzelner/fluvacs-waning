#! /usr/bin/env Rscript
require(glue)
cmd <- glue("rsync --exclude=.drake -azvh {dir} sven:~/repos/projects", dir = getwd())

system(cmd)
