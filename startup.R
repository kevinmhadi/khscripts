forceload = function() {
    pkgs = gsub("package:", "", grep('package:', search(), value = TRUE))
    for (pkg in pkgs) {
        tryCatch( {
            message("force loading ", pkg)
            invisible(eval(as.list((asNamespace(pkg)))))
            invisible(eval(eapply(asNamespace(pkg), base::force, all.names = TRUE)))
        }, error = function(e) message("could not force load ", pkg))
    }
}


if (!exists("priv_lib")) {
    ## priv_lib = "/gpfs/commons/groups/imielinski_lab/lib/R-3.4.1_alt"
    ## priv_lib = "/gpfs/commons/groups/imielinski_lab/lib/R-3.5.1_KH"
    priv_lib = "/gpfs/commons/groups/imielinski_lab/lib/R-3.6.1_KH"
}
if (!exists("lab_lib")) {
    ## lab_lib = "/gpfs/commons/groups/imielinski_lab/lib/R-3.4.1"  
    ## lab_lib = "/gpfs/commons/groups/imielinski_lab/lib/R-3.5.1"
    lab_lib = "/gpfs/commons/groups/imielinski_lab/lib/R-3.6.1"
}

Sys.setenv(DEFAULT_BSGENOME = "/gpfs/commons/groups/imielinski_lab/DB/GATK/human_g1k_v37_decoy.chrom.sizes")
Sys.setenv(DEFAULT_GENOME = "/gpfs/commons/groups/imielinski_lab/DB/GATK/human_g1k_v37_decoy.chrom.sizes")

Sys.setenv(PATH = paste("~/software/bcftools-1.9", Sys.getenv("PATH"), sep = ":"))
Sys.setenv(BCFTOOLS_PLUGINS = "/gpfs/commons/groups/imielinski_lab/Software/bcftools-1.9/plugins")

options(bitmapType="cairo")
options(stringsAsFactors = FALSE)
## options(na.action = "na.pass")
## options(na.action = "na.exclude")
options(na.action = "na.omit")
library(devtools)
library(withr)
library(tools)
options(device = grDevices::pdf)

suppressWarnings(expr = {
    suppressMessages(expr = {
        suppressPackageStartupMessages(expr = {
            withr::with_libpaths(priv_lib, library(gTrack), action = "prefix")
            withr::with_libpaths(priv_lib, library(igraph), action = "prefix")
            withr::with_libpaths(priv_lib, library(dplyr), action = "prefix")
            withr::with_libpaths(priv_lib, library(Flow), action = "prefix")
            library(gGnome)
            library(gUtils)
            library(colorspace)
            library(MASS)
            library(cowplot)
            library(doParallel)
            library(withr)
            library(rlang)
            library(tools)
            library(plyr)
            library(sinaplot)            
            ## library(dplyr)
            library(tidyverse)
            withr::with_libpaths(lab_lib, library(VariantAnnotation), action = "prefix")
            ## withr::with_libpaths("/gpfs/commons/groups/imielinski_lab/lib/R-3.4.1", library(gGnome))
            library(skitools)
            library(skidb)
            library(rtracklayer)
            ## library(gTrack)
            library(JaBbA)
            library(skidb)
            library(skitools)
            library(stringr)
            library(stringi)
            library(naturalsort)
            library(copynumber)
            library(forcats)
            library(purrr)
            library(egg)
            library(grid)
            library(gridExtra)
            library(scales)
            library(tibble)
            library(ggforce)
############################## topic modeling libs
            library(topicmodels)
            library(tidytext)
            library(broom)
            ## library(ldatuning)
            library(bedr)
            library(ggpubr)
            library(plyranges)
            library(mltools)
#############################
            ## library(stringi)
            ## source('/gpfs/commons/home/khadi/DThelp.R')
            source("/gpfs/commons/groups/imielinski_lab/home/khadi/git/khscripts/DThelp.R")
            ## source('/gpfs/commons/home/khadi/hkev_utils.R')
            source("/gpfs/commons/groups/imielinski_lab/home/khadi/git/khscripts/hkev_utils.R")
            ## source('/gpfs/commons/home/khadi/safety_func.R')
            source('/gpfs/commons/groups/imielinski_lab/home/khadi/git/khscripts/safety_func.R')
        })
    })
})


data.table::setDTthreads(1)

`%$%` = gUtils::`%$%`
`%&%` = gUtils::`%&%`
`%Q%` = gUtils::`%Q%`
`%+%` = gUtils::`%+%`
`%-%` = gUtils::`%-%`
`%^%` = gUtils::`%^%`
reduce = GenomicRanges::reduce
between = data.table::between
setnames = data.table::setnames
select = dplyr::select
update = Flow::update
cache = Flow::cache
set = data.table::set
set_names = rlang::set_names
matches = dplyr::matches
n = dplyr::n


forceload()
