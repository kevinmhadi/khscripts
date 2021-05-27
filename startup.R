.libPaths(unique(c("/gpfs/commons/groups/imielinski_lab/lib/R-4.0.2_KH", .libPaths())))

Sys.setenv(DEFAULT_BSGENOME = "/gpfs/commons/groups/imielinski_lab/DB/GATK/human_g1k_v37_decoy.chrom.sizes")
Sys.setenv(DEFAULT_GENOME = "/gpfs/commons/groups/imielinski_lab/DB/GATK/human_g1k_v37_decoy.chrom.sizes")

Sys.setenv(PATH = paste("~/software/bcftools-1.9", Sys.getenv("PATH"), sep = ":"))
Sys.setenv(BCFTOOLS_PLUGINS = "/gpfs/commons/groups/imielinski_lab/Software/bcftools-1.9/plugins")

startup();
library3(
    withr,
    skitools,
    gGnome,
    Flow,
    dplyr,
    bamUtils,
    skidb,
    naturalsort,
    magrittr,
    khtools
)

## source("~/lab/home/khadi/git/khscripts/patch_gtrack.R")


brewer.master = skitools::brewer.master
`%$%` = gUtils::`%$%`
`%&%` = gUtils::`%&%`
`%Q%` = gUtils::`%Q%`
`%+%` = gUtils::`%+%`
`%-%` = gUtils::`%-%`
`%^%` = gUtils::`%^%`
width = GenomicRanges::width
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
last = dplyr::last
first = dplyr::first
tailf = khtools::tailf
coalesce = khtools::coalesce
ppdf = khtools::ppdf
ppng = khtools::ppng
with_libpaths = withr::with_libpaths
with_options = withr::with_options
melt = data.table::melt
overwritefun(khtools::gr.flipstrand, gUtils::gr.flipstrand); gr.flipstrand = khtools::gr.flipstrand

