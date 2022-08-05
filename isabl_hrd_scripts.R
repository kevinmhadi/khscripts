sv_classify = function(bpe, id = NULL,return.table = FALSE) {
    sv.feats = c("DEL_<1kbp","DEL_1kbp-10kbp","DEL_10kbp-100kbp","DEL_100kbp-1Mbp","DEL_1Mbp-10Mbp","DEL_>10Mbp","DUP_<1kbp","DUP_1kbp-10kbp","DUP_10kbp-100kbp","DUP_100kbp-1Mbp","DUP_1Mbp-10Mbp","DUP_>10Mbp","INV_<1kbp","INV_1kbp-10kbp","INV_10kbp-100kbp","INV_100kbp-1Mbp","INV_1Mbp-10Mbp","INV_>10Mbp","TRA")
    if (is.character(bpe) && file.exists(bpe)) {
        bpe = isv2grl(bpe)
        bpe = grl2bedpe(gr.sort(bpe, ignore.strand = T))
    } else if (inherits(bpe, "GRangesList")) {
        bpe = grl2bedpe(gr.sort(bpe, ignore.strand = T))
    } else if (!inherits(bpe, "data.frame")) {
        stop()
    }
    if (NROW(bpe)) {
        bpe[, jdist := ifelse(chrom1 == chrom2, start2 - start1, Inf)]
        bpe[, class := case_when(!is.infinite(jdist) & strand1 == "+" & strand2 == "-" ~ "DUP",
                                 !is.infinite(jdist) & strand1 == "-" & strand2 == "+" ~ "DEL",
                                 !is.infinite(jdist) & strand1 == strand2 ~ "INV",
                                 TRUE ~ "TRA")]
        bpe[, dist_cat := cut(jdist, c(-1, 1e3, 1e4, 1e5, 1e6, 1e7, Inf), labels = paste0("_", c("<1kbp", "1kbp-10kbp", "10kbp-100kbp", "100kbp-1Mbp", "1Mbp-10Mbp", ">10Mbp")))]
        bpe[, sv_category := factor(ifelse(class != "TRA", paste0(class, dist_cat), "TRA"), sv.feats)]
    } else {
        if (isTRUE(return.table)) {
            return(bpe)
        } else {
            out = as.data.frame(transp(rep(0, length(sv.feats)))[[1]])
            names(out) = sv.feats
            if (!is.null(id)) {
                out$id = id
                setcolorder(out, "id")
            }
            return(out)
        }
    }
    if (isTRUE(return.table)) {
        return(bpe)
    } else {
        mat = as.data.frame(table(bpe$sv_category))
        out = matrify(mat)
        out = as.data.frame(transp(out)[[1]])
        names(out) = mat$Var1
        if (!is.null(id)) {
            out$id = id
            setcolorder(out, "id")
        }
        return(out)
    }
}


read.isabl.mut = function(path) {
    out = fread(cmd = paste0('gunzip -c ', path, ' | grep -v "##"'), fill = T)
    return(out)
}


fitsig = function(catalog, weights, id = NULL, methodFit = "KLD", threshold_percentFit = 5, bootstrapSignatureFit = TRUE, nbootFit = 100, threshold_p.valueFit = 0.05, bootstrapHRDetectScores = FALSE, nparallel = 1, randomSeed = 10) {
    require(signature.tools.lib)
    bootstrap_fit <-
        signature.tools.lib::SignatureFit_withBootstrap(
                                 catalog,
                                 weights,
                                 nboot = nbootFit,
                                 method = methodFit, 
                                 threshold_percent = threshold_percentFit,
                                 threshold_p.value = threshold_p.valueFit, 
                                 verbose = FALSE, nparallel = nparallel, randomSeed = randomSeed
                             )
    exposures <- bootstrap_fit$E_median_filtered
    exposures[is.nan(exposures)] <- 0

    out = as.data.table(transp(exposures)[[1]])
    setnames(out, rownames(exposures))
    if (!is.null(id)) {
        out$id = id
        setcolorder(out, "id")
    }
    return(out)

}


mski2brass = function(bpe) {
    strand.conv = setnames(rbind(
        data.table(cbind("+", "+"), cbind("-", "+")),
        data.table(cbind("-", "-"), cbind("+", "-")),
        data.table(cbind("+", "-"), cbind("-", "-")),
        data.table(cbind("-", "+"), cbind("+", "+"))
    ), c("new.strand1", "new.strand2", "strand1", "strand2"))
    bpe$tmpix = seq_len(NROW(bpe))
    bpe = merge.repl(bpe, strand.conv, by = c("strand1", "strand2"))
    bpe$strand1 = bpe$new.strand1
    bpe$strand2 = bpe$new.strand2
    bpe$new.strand1 = NULL
    bpe$new.strand2 = NULL
    bpe = bpe[order(bpe$tmpix)]
    bpe$tmpix = NULL
    return(bpe)
}

isv2grl = function(sv, flipstrand = TRUE) {
    if (is.character(sv) && file.exists(sv)) {
        sv.path = sv
        vcf = S4Vectors::expand(readVcf(sv))
        sv = rowRanges(vcf)
        mcols(sv) = cbind(mcols(sv), info(vcf))
    }
    if (NROW(sv)) {
        gstrands = transp(strsplit(sv$STRANDS, ""), c)
        if (flipstrand) {
            gstrands[[1]] = unname(c("+" = "-", "-" = "+")[gstrands[1][[1]]])
            gstrands[[2]] = unname(c("+" = "-", "-" = "+")[gstrands[2][[1]]])
        }
        strand(sv) = gstrands[[1]]
        sv$ALT = unlist(sv$ALT)
        sv$alt = gsub("[\\[N\\]]", "", sv$ALT, perl = T)
        gr.2 = GRanges(sv$alt)
        strand(gr.2) = gstrands[[2]]
        grl = GRangesList(unname(gr.noval(sv)), unname(gr.2))
        grl = grl.pivot(grl)
    } else {
        grl = GRangesList()
    }
    mcols(grl) = mcols(sv)
    return(grl)
}


fit.hrd.input.sigs = function(snv.path, indel.path, sv.path, cnv.path, id = NULL) {
    
    if (!(file.exists(snv.path) &&
          file.exists(indel.path) &&
          file.exists(sv.path) &&
          file.exists(cnv.path))) {
        stop("need snv.path, indel.path, and sv.path")
    }
    indel.tmp = read.isabl.mut(indel.path)
    indel.tmp$chr = indel.tmp$CHR
    indel.tmp$position = indel.tmp$START

    goodindels = indel.tmp[indel.tmp$NUMBER_OF_CALLERS == 3 & indel.tmp$FLAGS_ALL == "PASS"]
    indel.class = signature.tools.lib::tabToIndelsClassification(goodindels, goodindels$TARGET_NAME[1])

    snv.tmp = read.isabl.mut(snv.path)

    snv.tmp$chr = snv.tmp$CHR
    snv.tmp$position = snv.tmp$START

    goodsnvs = snv.tmp[snv.tmp$NUMBER_OF_CALLERS == 3 & snv.tmp$FLAGS_ALL == "PASS"]
    snv.class = signature.tools.lib::tabToSNVcatalogue(goodsnvs, goodsnvs$TARGET_NAME[1])

    bpe = isv2grl(sv.path)
    bpe = grl2bedpe(gr.sort(bpe, ignore.strand = T))
    bpe$sample = "dummy"

    goodbpe = mski2brass(bpe)

    goodbpe = as.data.frame(goodbpe)

    sv.class = bedpeToRearrCatalogue(goodbpe)

    sv.sigs = fitsig(sv.class$rearr_catalog, sigstofit_rearr, id)
    snv.sigs = fitsig(snv.class$catalogue, sigstofit_subs, id)
    indel.sigs = indel.class$count_proportion
    indel.sigs$id = id

    batseg = fread(cnv.path)
    
    ascat.tmp = batseg[, .(
        seg_no = V1,
        Chromosome = chr,
        chromStart = startpos,
        chromEnd = endpos,
        total.copy.number.inNormal = 2,
        minor.copy.number.inNormal = 1,
        total.copy.number.inTumour = nMaj1_A + nMin1_A,
        minor.copy.number.inTumour = nMin1_A
    )] %>% na.omit

    cnv.sig = enframe(ascatToHRDLOH(ascat.tmp, this$system_id), "system_id", "hrd_loh_raw")

    
    out = Reduce(function(x,y) merge.repl(x,y, by = "id"),
                 list(snv.sigs, indel.sigs, sv.sigs, cnv.sig))
    return(out)
}


fit.hrd.input.sigs = function(snv.path, indel.path, sv.path, cnv.path, id = NULL, verbose = TRUE) {
    cosmic_siglist <- 1:30
    sigstofit_subs <- signature.tools.lib:::cosmic30[, cosmic_siglist, drop = FALSE]
    sigstofit_rearr <- signature.tools.lib:::RS.Breast560
    
    if (!(file.exists(snv.path) &&
          file.exists(indel.path) &&
          file.exists(sv.path) &&
          file.exists(cnv.path))) {
        stop("need snv.path, indel.path, and sv.path")
    }
    indel.tmp = read.isabl.mut(indel.path)
    indel.tmp$chr = indel.tmp$CHR
    indel.tmp$position = indel.tmp$START

    goodindels = indel.tmp[indel.tmp$NUMBER_OF_CALLERS == 3 & indel.tmp$FLAGS_ALL == "PASS"]

    if (verbose)
        message("Classifying InDels")
    indel.class = signature.tools.lib::tabToIndelsClassification(goodindels, goodindels$TARGET_NAME[1])

    snv.tmp = read.isabl.mut(snv.path)

    snv.tmp$chr = snv.tmp$CHR
    snv.tmp$position = snv.tmp$START

    goodsnvs = snv.tmp[snv.tmp$NUMBER_OF_CALLERS == 3 & snv.tmp$FLAGS_ALL == "PASS"]
    
    if (verbose)
        message("Classifying SNVs")
    snv.class = signature.tools.lib::tabToSNVcatalogue(goodsnvs, goodsnvs$TARGET_NAME[1])

    bpe = isv2grl(sv.path)
    bpe = grl2bedpe(gr.sort(bpe, ignore.strand = T))
    bpe$sample = "dummy"

    goodbpe = mski2brass(bpe)

    goodbpe = as.data.frame(goodbpe)

    if (verbose)
        message("Classifying SVs")
    sv.class = bedpeToRearrCatalogue(goodbpe)

    if (verbose)
        message("Fitting Signatures")
    
    sv.sigs = fitsig(sv.class$rearr_catalog, sigstofit_rearr, id)
    snv.sigs = fitsig(snv.class$catalogue, sigstofit_subs, id)
    indel.sigs = indel.class$count_proportion
    indel.sigs$id = id
    indel.sigs$sample = NULL

    batseg = fread(cnv.path)
    
    ascat.tmp = batseg[, .(
        seg_no = V1,
        Chromosome = chr,
        chromStart = startpos,
        chromEnd = endpos,
        total.copy.number.inNormal = 2,
        minor.copy.number.inNormal = 1,
        total.copy.number.inTumour = nMaj1_A + nMin1_A,
        minor.copy.number.inTumour = nMin1_A
    )] %>% na.omit

    cnv.sig = enframe(ascatToHRDLOH(ascat.tmp, id), "id", "hrd_loh")

    
    out = Reduce(function(x,y) merge.repl(x,y, by = "id"),
                 list(snv.sigs, indel.sigs, sv.sigs, cnv.sig))
    return(out)
}


calculate_loh = function(asc, id = NULL) {
    if (is.character(asc) && file.exists(asc)) {
        asc = read.table(asc, header = T, fill = F, sep = "\t")
        asc = bat2asc(asc, id = id)
    } else if (!inherits(asc, "data.frame")) {
        stop("input must be a tabular object")
    }
    if (is.null(id)) id = "1"
    asc$ix = seq_len(NROW(asc))
    asc = asc[order(asc$Chromosome, asc$Start, asc$End),]
    ix = rleseq(asc$Chromosome, use.data.table = FALSE, clump = T)$idx
    asc$ignore = unlist(by(asc, ix, function(x) {
        rep_len(all(x$nB == 0), length(x$nB))
    }))
    ## asc$ignore = asdt(asc)[, ix := .I][, .(ix, ignore = all(nB == 0)), by = Chromosome][order(ix)]$ignore
    asc$width = asc$End - asc$Start + 1
    ## loh_score = sum(asc$nB == 0 & asc$width > 1.5e7 & !asc$ignore %in% TRUE)
    gr = GenomicRanges::makeGRangesFromDataFrame(asc, seqnames.field = "Chromosome", start.field = "Start", end.field = "End", keep.extra.columns = TRUE)
    grloh = gr[gr$nB == 0 & gr$ignore == FALSE]
    grloh = GenomicRanges::reduce(grloh + 1e5) - 1e5
    loh.score = sum(width(grloh) > 1.5e7)
    return(loh.score)
}



calculate_lst <- function(asc, centros, id = NULL) {
    reduce = GenomicRanges::reduce
    if (is.character(asc) && file.exists(asc)) {
        asc = read.table(asc, header = T, fill = F, sep = "\t")
        asc = bat2asc(asc, id = id)
    } else if (!inherits(asc, "data.frame")) {
        stop("input must be a tabular object")
    }
    if (is.character(centros) && file.exists(centros)) {
        centros = read.table(centros, header = T, sep = "\t")
    } else if (!inherits(centros, "data.frame")) {
        stop("centros must be a data frame")
    }
    if (is.null(id)) id = "1"
    gr.out = merge(asc, centros, by.x = "Chromosome", by.y = "chrom")
    
    gr.out$arm = ifelse(gr.out$End <= gr.out$centromere_start, "p", ifelse(gr.out$Start >= gr.out$centromere_end, "q", NA_character_))
    gr.out = gr.out[!is.na(gr.out$arm),]

    gr.out2 = GenomicRanges::makeGRangesFromDataFrame(gr.out, seqnames.field = "Chromosome", start.field = "Start", end.field = "End", keep.extra.columns = TRUE)

    gr.out2$big.filt1 = GenomicRanges::width(gr.out2) > 3000000

    tmp = gr_construct_by(gr.out2, c("arm", "nA", "nB", "big.filt1"))
    tmp = reduce(tmp + 3e6)
    tmp = tmp - 3e6
    tmp = gr_deconstruct_by(tmp, by = c("arm", "nA", "nB", "big.filt1"), meta = TRUE)
    ## gr.out3 = sort(sortSeqlevels(tmp))
    gr.out3 = tmp
    
    dt.out = as.data.frame(gr.out3)
    dt.out = dt.out[dt.out$big.filt1 == TRUE,,drop = F]
    dt.out$is_big = dt.out$width >= 10000000
    dt.out = dt.out[order(dt.out$seqnames, dt.out$start, dt.out$end),]
    ## dt.out = dt.out[order(dt.out$seqnames, dt.out$end, dt.out$start),]

    ix = rleseq(dt.out$seqnames, dt.out$arm, use.data.table = FALSE, clump = T)$idx
    dt.out$run = unlist(by(dt.out, ix, function(x) {label.runs(x$is_big)})) # get the runs of large scale CN changes
    dt.out = dt.out[!is.na(dt.out$run),,drop=F]
    ix = rleseq(dt.out$seqnames, dt.out$arm, dt.out$run, use.data.table = FALSE, clump = T)$idx
    state_changes = unlist(by(dt.out, ix, function(x) nrow(x) - 1)) ## per run - tabulate how many state changes there are; should be V2 = total run size - 1; if V2 = 0 that means that the run does not correspond to large scale state change
    return(sum(state_changes, na.rm = T))

}
## calculate_lst(pairs.22[5]$battenberg, "~/Dropbox/Isabl/DB/hg19_centromeres.tsv")

calculate_ai = function(basc, centros, id = NULL) {
    if (is.character(basc) && file.exists(basc)) {
        basc = read.table(basc, header = T, fill = F, sep = "\t")
        basc = bat2asc(basc, id = id)
    } else if (!inherits(basc, "data.frame")) {
        stop("input must be a tabular object")
    }
    if (is.character(centros) && file.exists(centros)) {
        centros = read.table(centros, header = T, sep = "\t")
    } else if (!inherits(centros, "data.frame")) {
        stop("centros must be a data frame")
    }
    if (is.null(id)) id = "1"
    basc = basc[order(basc$Chromosome, basc$Start, basc$End),]
    basc$tmpix = seq_len(NROW(basc))
    ## get chromosome ploidy and telomeric annotations (isfirst or islast)
    lst.out = lapply(unique(basc$Chromosome), function(chr) {
        thischr = basc[basc$Chromosome == chr,,drop=F]
        cn = thischr$totalCN
        wid = thischr$End - thischr$Start
        chrploidy = round(sum(wid * cn, na.rm = TRUE) / sum(wid))
        data.frame(ploidy = rep_len(chrploidy, NROW(thischr)), chrom_is_segmented = NROW(thischr) > 1, isfirst = thischr$tmpix == min(thischr$tmpix), islast = thischr$tmpix == max(thischr$tmpix))
    })
    
    basc = do.assign(basc, do.call(rbind, lst.out))
    basc.df = merge.data.frame(basc, centros[, c("chrom", "centromere_start", "centromere_end")], by.x = "Chromosome", by.y = "chrom")
    basc.df$evenpl = basc.df$ploidy %% 2 == 0
    basc.df$oddpl = basc.df$ploidy %% 2 != 0

    
    basc.df$goodend = ((basc.df$isfirst %in% TRUE & basc.df$End < basc.df$centromere_start) |
                       (basc.df$islast %in% TRUE & basc.df$Start > basc.df$centromere_end)) & basc.df$chrom_is_segmented %in% TRUE
    
    basc.df$ai = ifelse((basc.df$isfirst %in% TRUE | basc.df$islast %in% TRUE) & basc.df$goodend == TRUE,
                 ifelse(basc.df$evenpl %in% TRUE,
                 ifelse(basc.df$nA == basc.df$nB, "balanced", "tai"),
                 ifelse(basc.df$oddpl %in% TRUE,
                 ifelse(basc.df$nA + basc.df$nB == basc.df$ploidy & basc.df$nB != 0, "balanced", "tai"),
                 NA_character_)),
                 NA_character_)
    
    sum(basc.df$ai == "tai", na.rm = T)
}
## calculate_ai(pairs.22$battenberg[5], "~/Dropbox/Isabl/DB/hg19_centromeres.tsv", pairs.22[5]$system_id)
