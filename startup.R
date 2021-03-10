Sys.setenv(DEFAULT_BSGENOME = "/gpfs/commons/groups/imielinski_lab/DB/GATK/human_g1k_v37_decoy.chrom.sizes")
Sys.setenv(DEFAULT_GENOME = "/gpfs/commons/groups/imielinski_lab/DB/GATK/human_g1k_v37_decoy.chrom.sizes")

Sys.setenv(PATH = paste("~/software/bcftools-1.9", Sys.getenv("PATH"), sep = ":"))
Sys.setenv(BCFTOOLS_PLUGINS = "/gpfs/commons/groups/imielinski_lab/Software/bcftools-1.9/plugins")

startup();
library3(
    skitools,
    gGnome,
    Flow,
    dplyr,
    bamUtils,
    skidb,
    naturalsort,
    khtools
)

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
