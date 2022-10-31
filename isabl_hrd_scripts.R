classify_indels = function(indels, max.mh.len = 5, max.other.len = 5) {
    
    indels = indels[(nchar(indels$REF) > 1 & nchar(indels$ALT) == 1) |
                    (nchar(indels$ALT) > 1 & nchar(indels$REF) == 1),]

    indel_sig_lvl <- c(
        paste0("del.rep.", 1:max.other.len), 
        paste0("ins.rep.", 1:max.other.len),
        paste0("del.mh.", 1:max.mh.len),
        paste0("ins.mh.", 1:max.mh.len), 
        paste0("del.none.", 1:max.other.len),
        paste0("ins.none.", 1:max.other.len)
    )

    if (NROW(indels)) {    
        indels$indel_type = ifelse(nchar(indels$REF) > 1 & nchar(indels$ALT) == 1, "del",
                            ifelse(nchar(indels$ALT) > 1 & nchar(indels$REF) == 1, "ins", NA_character_))
        indel.seq = change = indels$CHANGE 
        fivep = indels$CONTEXT_5
        flank.seq = threep = indels$CONTEXT_3
        numreps = count_added_repeats(change, threep)
        indels$n_repeats = unlist(numreps)    
        mh.5p = count_mh(stringreverse(change), stringreverse(fivep))
        mh.3p = count_mh(change, threep)
        mh = pmax(mh.5p, mh.3p)
        indels$mh_len = mh
        indels$indel_len = nchar(indels$CHANGE)

        indels$indel_context = ifelse(indels$n_repeats >= 1,
                               ifelse(indels$indel_len < 50, "rep", "mh"),
                               ifelse(indels$n_repeats >= 0 & indels$mh_len >= 2, "mh",
                               ifelse(indels$mh_len >= 1 & indels$n_repeats >= 0 & indels$indel_len > 3, "mh", "none")))

        indels$indel_signature = ifelse(
            indels$indel_context == "mh",
            paste(indels$indel_type,
                  indels$indel_context,
                  pmin(indels$mh_len, max.mh.len),
                  sep = "."),
            paste(indels$indel_type,
                  indels$indel_context,
                  pmin(indels$indel_len, max.other.len),
                  sep = ".")
        )
        return(table(factor(indels$indel_signature, indel_sig_lvl)))
    } else {
        return(structure(rep(0, length(indel_sig_lvl)), 
                         names = indel_sig_lvl))
    }
}

count_added_repeats = function(change, threep) {
    indel.seq = change
    flank.seq = threep
    Map(function(indel_s, flank_s) {
        nc_indel_s = nchar(indel_s)
        nc_flank_s = nchar(flank_s)
        strstarts = seq(1, nc_flank_s, by = nc_indel_s)
        strends = c(seq(0, nc_flank_s - 1, by = nc_indel_s)[-1], nchar(flank_s))
        strings = substring(flank_s, strstarts, strends)
        matches = indel_s == strings
        nrepeats = 0
        for (match in matches) {
            if (!match) break
            nrepeats = nrepeats + 1
        }
        nrepeats
    }, indel.seq, flank.seq)
}



stringreverse = function(x) {
    out = unlist(lapply(x, function(y) {
        intToUtf8(rev(utf8ToInt(y)))
    }))
    return(out)
}

count_mh = function(change, flank) {
    unname(unlist(Map(function(indel_s, flank_s) {
        indel_s
        maxflank = substring(flank_s, 1, nchar(indel_s))
        char_flank = unlist(strsplit(maxflank, NULL))
        char_indel = unlist(strsplit(indel_s, NULL))
        matches = char_flank == char_indel
        nmh = 0
        for (match in matches) {
            if (!match) break
            nmh = nmh + 1
        }
        nmh
    }, change, flank)))
}



## sv_classify = function(bpe, id = NULL,return.table = FALSE) {
##     sv.feats = c("deletion <1kb","deletion 1kb-10kb","deletion 10kb-100kb","deletion 100kb-1Mb","deletion >1Mb","tandem-duplication <1kb","tandem-duplication 1kb-10kb","tandem-duplication 10kb-100kb","tandem-duplication 100kb-1Mb","tandem-duplication >1Mb", "inversion <1kb","inversion 1kb-10kb","inversion 10kb-100kb","inversion 100kb-1Mb","inversion >1Mb", "translocation")
##     if (is.character(bpe) && file.exists(bpe)) {
##         bpe = isv2grl(bpe)
##         bpe = grl2bedpe(gr.sort(bpe, ignore.strand = T))
##     } else if (inherits(bpe, "GRangesList")) {
##         bpe = grl2bedpe(gr.sort(bpe, ignore.strand = T))
##     } else if (!inherits(bpe, "data.frame")) {
##         stop()
##     }
##     if (NROW(bpe)) {
##         bpe[, jdist := ifelse(chrom1 == chrom2, start2 - start1, Inf)]
##         bpe[, class := ifelse(!is.infinite(jdist) & strand1 == "+" & strand2 == "-", "tandem-duplication",
##                        ifelse(!is.infinite(jdist) & strand1 == "-" & strand2 == "+", "deletion",
##                        ifelse(!is.infinite(jdist) & strand1 == strand2, "inversion", "translocation")))]
##         bpe[, dist_cat := cut(jdist, c(-1, 1e3, 1e4, 1e5, 1e6, Inf), labels = c("<1kb", "1kb-10kb", "10kb-100kb", "100kb-1Mb", ">1Mb"))]
##         bpe[, sv_category := factor(ifelse(class != "translocation", paste(class, dist_cat), "translocation"), sv.feats)]
##     } else {
##         if (isTRUE(return.table)) {
##             return(bpe)
##         } else {
##             out = as.data.frame(transp(rep(0, length(sv.feats)))[[1]])
##             names(out) = sv.feats
##             if (!is.null(id)) {
##                 out$id = id
##                 setcolorder(out, "id")
##             }
##             return(out)
##         }
##     }
##     if (isTRUE(return.table)) {
##         return(bpe)
##     } else {
##         mat = as.data.frame(table(bpe$sv_category))
##         out = matrify(mat)
##         out = as.data.frame(transp(out)[[1]])
##         names(out) = mat$Var1
##         if (!is.null(id)) {
##             out$id = id
##             setcolorder(out, "id")
##         }
##         return(out)
##     }
## }


sv_classify_19 = function(bpe, id = NULL,return.table = FALSE) {
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
        bpe$jdist = ifelse(bpe$chrom1 == bpe$chrom2, bpe$start2 - bpe$start1, Inf)
        bpe$class = ifelse(!is.infinite(bpe$jdist) & bpe$strand1 == "+" & bpe$strand2 == "-", "DUP",
                    ifelse(!is.infinite(bpe$jdist) & bpe$strand1 == "-" & bpe$strand2 == "+", "DEL",
                    ifelse(!is.infinite(bpe$jdist) & bpe$strand1 == bpe$strand2, "INV", "TRA")))
        ## bpe[, jdist := ifelse(chrom1 == chrom2, start2 - start1, Inf)]
        ## bpe[, class := ifelse(!is.infinite(jdist) & strand1 == "+" & strand2 == "-", "DUP",
        ##                ifelse(!is.infinite(jdist) & strand1 == "-" & strand2 == "+", "DEL",
        ##                ifelse(!is.infinite(jdist) & strand1 == strand2, "INV", "TRA")))]
        
        bpe$dist_cat = cut(
            bpe$jdist, c(-1, 1e3, 1e4, 1e5, 1e6, 1e7, Inf),
            labels = paste0("_", c("<1kbp", "1kbp-10kbp", "10kbp-100kbp", "100kbp-1Mbp", "1Mbp-10Mbp", ">10Mbp"))
        )
        bpe$sv_category = factor(ifelse(bpe$class != "TRA", paste0(bpe$class, bpe$dist_cat), "TRA"), sv.feats)
        ## bpe[, sv_category := factor(ifelse(class != "TRA", paste0(class, dist_cat), "TRA"), sv.feats)]
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
    out = read.table(file = pipe(paste0('gunzip -c ', path, ' | grep -v "##"')), sep = "\t", header = TRUE)
    ## out = fread(cmd = paste0('gunzip -c ', path, ' | grep -v "##"'), fill = T)
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

isv2grl = function(sv, flipstrand = TRUE, genome = "hg19") {
    if (is.character(sv) && file.exists(sv)) {
        sv.path = sv
        vcf = readVcf(sv, genome = genome)
        vcf = IRanges::expand(vcf)
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
        sspl = strsplit(sv$alt, ":")
        lst = split(unlist(sspl), unlist(lapply(base::lengths(sspl), seq_len)))
        gr.2 = GRanges(paste0(lst[[1]], ":", lst[[2]], "-", lst[[2]]))
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



calculate_lst = function(asc, centros, id = NULL) {
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








annotCluster = function(rearrs, utils_dir = "~/git/click_wgspost/click_wgspost/data/rearr_utils") {
    pfmap = c("DUP" = 4, "DEL" = 2, "TRA" = 32, "INV" = 8)
    svtypemap = c(
        "DUP" = "tandem-duplication",
        "DEL" = "deletion",
        "TRA" =  "translocation",
        "INV" = "inversion"
    )

    require(VariantAnnotation) # for checking if genome regions are mappable
    require(GenomicFeatures)
    require(BSgenome.Hsapiens.UCSC.hg19)

    source(file.path(utils_dir,"calcIntermutDist.R"))
    ##source(file.path(utils_dir,"plotScatterCirco.R"))
    source(file.path(utils_dir,"rearrangement.clustering.demo.R"))
    source(file.path(utils_dir,"fastPCF.R"))
    source(file.path(utils_dir,"extract.kat.regions.R"))

    cat(paste('preparing rearrangements ... \n'))
    if (is.character(rearrs) && file.exists(rearrs)) {
        rearrs <- read.table(rearrs, header=TRUE, sep='\t')
    }
    
    if (!inherits(rearrs, "data.frame")) {
        stop("bedpe is not in a tabular format")
    }
    if (inherits(rearrs, "data.table")) rearrs = as.data.frame(rearrs)

    if (!all(c("chromosome.1", "chromosome.2", "pos.1.min", "pos.2.min", "ID", "svclass",
               "nts", "mh") %in% colnames(rearrs))) {
        ## assuming standard bedpe columns
        if (!all(c("chrom1", "chrom2", "start1", "start2", "strand1", "strand2") %in% colnames(rearrs))) {
            stop("bedpe is not formatted properly?")
        }
        rearrs = bedpe2grl(rearrs)
        rearrs = grl2bedpe(rearrs, as.data.table = FALSE)
        rearrs$svclass = ifelse(
            rearrs$chrom1 == rearrs$chrom2,
                 ifelse(rearrs$strand1 == "+" & rearrs$strand2 == "-", "DUP",
                 ifelse(rearrs$strand1 == "-" & rearrs$strand2 == "+", "DEL", "INV")),
            "TRA"
        )
        rearrs$svclass = svtypemap[rearrs$svclass]
        rearrs$chromosome.1 = rearrs$chrom1
        rearrs$chromosome.2 = rearrs$chrom2
        rearrs$pos.1.min = rearrs$start1
        rearrs$pos.2.min = rearrs$start2
        rearrs$ID = rearrs$name
        rearrs$pf = pfmap[rearrs$svclass]
        rearrs$sample = rep_len("1", NROW(rearrs))
        rearrs$nts = rep_len("-", NROW(rearrs))
        rearrs$mh = rep_len("-", NROW(rearrs))
    }

    rearrs$Chromosome.1 <- rearrs$chromosome.1
    rearrs$Chromosome.2 <- rearrs$chromosome.2
    rearrs$pos.1 <- rearrs$pos.1.min
    rearrs$pos.2 <- rearrs$pos.2.min
    rearrs$id <- rearrs$ID
    rearrs$pf <- rearrs$svclass
    sample.rearrs <- rearrs

    rearrs.left <- rearrs[,c('chromosome.1','pos.1.min', 'svclass','pf', 'sample', 'ID','nts', 'mh')]; names(rearrs.left ) <- NA_character_
    rearrs.right <- rearrs[,c('chromosome.2','pos.2.min', 'svclass', 'pf', 'sample', 'ID','nts', 'mh')];names(rearrs.right ) <- NA_character_
    rearrs.cncd <- rbind(rearrs.left , rearrs.right  );
    rearrs.cncd$isLeft <- c(rep(TRUE, nrow(rearrs.left)), rep(FALSE, nrow(rearrs.left)))
    colnames(rearrs.cncd) <- c('chr', 'position', 'svclass', 'pf', 'sample', 'id', 'nts', 'mh', 'isLeft')
    sample.bps <- rearrs.cncd
    cat('pcf: rearrangements \n')
    if (nrow(sample.bps)>0) { # if there are any rearrangements
        ## annotate each rearrangement whether it falls into repeats
        ##sample.rearrs <- annotateRearrsMh(sample.rearrs, metadata)
        sample.bps <- sample.bps[order(sample.bps$chr, sample.bps$position),]

        clustering.result <- rearrangement.clustering(sample.bps,
                                                      plot.path = NA,
                                                      kmin=10,
                                                      kmin.samples=1,
                                                      gamma.sdev=25,
                                                      PEAK.FACTOR=10,
                                                      thresh.dist=NA)

        sample.bps <- clustering.result$sample.bps
        ## mark both breakpoints of a rearrangement as clustered if any is
        sample.rearrs$is.clustered <- sample.rearrs$id %in% sample.bps$id[sample.bps$is.clustered.single]
    } else {
        sample.rearrs$is.clustered <- character(0)
    }
    return(sample.rearrs)

}



bat2asc = function(bat, id) {
    x = bat
    ## asc = bat[, .(SampleID = "1", Chromosome = chr, Start = startpos,
    ##               End = endpos, nProbes = 1, totalCN = nMaj1_A + nMin1_A, nA = nMaj1_A,
    ##               nB = nMin1_A, Ploidy = rep(NA_real_), AberrantCellFraction = 0.0)]
    asc = data.frame(SampleID = id, Chromosome = x$chr, Start = x$startpos,
               End = x$endpos, nProbes = 1, totalCN = x$nMaj1_A + x$nMin1_A,
               nA = x$nMaj1_A, nB = x$nMin1_A, Ploidy = NA_real_, AberrantCellFraction = 0.0)
    return(asc)
}

label.runs = function (x) 
{
    if (!is.logical(x)) {
        cumsum(abs(diff(as.numeric(c(0, as.integer(factor(x)))))) > 
               0)
    }
    else {
        as.integer(ifelse(x, cumsum(diff(as.numeric(c(FALSE, 
                                                      x))) > 0), NA))
    }
}


wgspost_indel = function(indel.path) {
    if (!file.exists(indel.path))
        stop("Isabl indel path not found!")
    indels = read.isabl.mut(indel.path)
    indelclass = t(classify_indels(indels[indels$ANY2_LCC > 0,,drop=F]))
    smalldels = indelclass[,grepl("del", colnames(indelclass)),drop=F]
    smalldelfrac = nan2zero(smalldels / rowSums(smalldels))
    colnames(smalldelfrac) = paste0(colnames(smalldelfrac), ".frac")
    indelclass = cbind(indelclass, smalldelfrac)
    return(indelclass)
}



wgspost_sv = function(sv.path) {
    if (!file.exists(sv.path))
        stop("Isabl SV path not found!")
    bedpe = grl2bedpe(gr.sort(isv2grl(sv.path)))
    bedpe = sv_classify_19(bedpe, return.table = TRUE)
    bedpe$is.clustered = annotCluster(bedpe)$is.clustered
    lvs = c("DEL_<1kbp","DEL_1kbp-10kbp","DEL_10kbp-100kbp","DEL_100kbp-1Mbp","DEL_1Mbp-10Mbp","DEL_>10Mbp","DUP_<1kbp","DUP_1kbp-10kbp","DUP_10kbp-100kbp","DUP_100kbp-1Mbp","DUP_1Mbp-10Mbp","DUP_>10Mbp","INV_<1kbp","INV_1kbp-10kbp","INV_10kbp-100kbp","INV_100kbp-1Mbp","INV_1Mbp-10Mbp","INV_>10Mbp","TRA")
    svtb = as.data.frame(table(factor(bedpe$is.clustered, c(TRUE, FALSE), c("clust", "disp")), factor(bedpe$sv_category, lvs)))
    svtb = svtb[order(svtb$Var1, svtb$Var2),]
    svtb$cat = paste(svtb$Var1, svtb$Var2, sep = "_")
    svclass = t(setNames(svtb$Freq, svtb$cat))
    return(svclass)
}

snv_contexts = function(snvs, pad = 1) {
    require(Biostrings)
    
    ## ordconts = c(
    ##     "A[C>A]A","A[C>A]C","A[C>A]G","A[C>A]T","C[C>A]A","C[C>A]C","C[C>A]G",
    ##     "C[C>A]T","G[C>A]A","G[C>A]C","G[C>A]G","G[C>A]T","T[C>A]A","T[C>A]C",
    ##     "T[C>A]G","T[C>A]T","A[C>G]A","A[C>G]C","A[C>G]G","A[C>G]T","C[C>G]A",
    ##     "C[C>G]C","C[C>G]G","C[C>G]T","G[C>G]A","G[C>G]C","G[C>G]G","G[C>G]T",
    ##     "T[C>G]A","T[C>G]C","T[C>G]G","T[C>G]T","A[C>T]A","A[C>T]C","A[C>T]G",
    ##     "A[C>T]T","C[C>T]A","C[C>T]C","C[C>T]G","C[C>T]T","G[C>T]A","G[C>T]C",
    ##     "G[C>T]G","G[C>T]T","T[C>T]A","T[C>T]C","T[C>T]G","T[C>T]T","A[T>A]A",
    ##     "A[T>A]C","A[T>A]G","A[T>A]T","C[T>A]A","C[T>A]C","C[T>A]G","C[T>A]T",
    ##     "G[T>A]A","G[T>A]C","G[T>A]G","G[T>A]T","T[T>A]A","T[T>A]C","T[T>A]G",
    ##     "T[T>A]T","A[T>C]A","A[T>C]C","A[T>C]G","A[T>C]T","C[T>C]A","C[T>C]C",
    ##     "C[T>C]G","C[T>C]T","G[T>C]A","G[T>C]C","G[T>C]G","G[T>C]T","T[T>C]A",
    ##     "T[T>C]C","T[T>C]G","T[T>C]T","A[T>G]A","A[T>G]C","A[T>G]G","A[T>G]T",
    ##     "C[T>G]A","C[T>G]C","C[T>G]G","C[T>G]T","G[T>G]A","G[T>G]C","G[T>G]G",
    ##     "G[T>G]T","T[T>G]A","T[T>G]C","T[T>G]G","T[T>G]T"
    ## )
    fiveplst = list()
    threeplst = list()
    bases = c("A", "C", "G", "T")
    for (i in seq_len(pad)) {
        fiveplst = c(fiveplst, list(bases))
        threeplst = c(threeplst, list(bases))
    }
    lst = c(fiveplst, ref = list(c("C", "T")), alt = list(bases), threeplst)
    
    ## combos = expand.grid(c("A", "C", "G", "T"), c("C", "T"), c("A", "C", "G", "T"), c("A", "C", "G", "T"))
    combos = do.call(expand.grid, lst)
    combos = subset(combos, as.character(combos$ref) != as.character(combos$alt))
    nuc5p = head(colnames(combos), pad)
    nuc3p = tail(colnames(combos), pad)
    ord = do.call(order, subset(combos, select = c(nuc5p, "ref", "alt", nuc3p)))
    combos = combos[ord,,drop=F]
    ordconts = paste0(
        do.call(paste0, subset(combos, select = nuc5p)),
        "[", combos$ref, ">", combos$alt, "]",
        do.call(paste0, subset(combos, select = nuc3p))
    )
    refix = which(colnames(combos) == "ref")
    altix = which(colnames(combos) == "alt")
    ## for later
    threepix = which(colnames(combos) %in% nuc3p) - 1
    fivepix = which(colnames(combos) %in% nuc5p)
    rm(combos)

    genomeSeq <- BSgenome.Hsapiens.1000genomes.hs37d5::BSgenome.Hsapiens.1000genomes.hs37d5

    if (NROW(snvs)) {
        seqcol = "^chr$|^chromosome$|^chrom$|^seqnames$"
        seqcol = grep(seqcol, colnames(snvs), T, value = T)[1]
        startcol = "^start$|^chromstart$|^chrstart$"
        startcol = grep(startcol, colnames(snvs), T, value = T)[1]
        endcol = "^end$|^chromend$|^chrend$"
        endcol = grep(endcol, colnames(snvs), T, value = T)[1]
        strandcol = "^strand$|^chromend$|^chrend$"
        strandcol = grep(strandcol, colnames(snvs), T, value = T)[1]
        vecselect = na.omit(c(seqcol, startcol, endcol, strandcol, "REF", "ALT"))
        snvs = subset(snvs, select = vecselect)
        grsnvs = df2gr(snvs, seqcol, startcol, endcol, strandcol)
        ## grsnvs = df2gr(
        ##     subset(snvs, select = colnames(snvs) %in% c("seqnames", "start", "end", "REF", "ALT"))
        ## )
        strand(grsnvs) = c(C = "+", G = "-", T = "+", A = "-")[grsnvs$REF]
        grsnvs$trinuc = as.character(getSeq(genomeSeq, grsnvs + pad))
        grsnvs$ref_collapse = substr(grsnvs$trinuc, refix, refix)
        ## getSeq(genomeSeq, gr.stripstrand(grsnvs + 1))
        altd = DNAStringSet(grsnvs$ALT)
        grsnvs$alt_collapse = ifelse(as.logical(strand(grsnvs) == "+"), as.character(altd), as.character(reverseComplement(altd)))

        grsnvs$fivep = substr(grsnvs$trinuc, min(fivepix), max(fivepix))
        grsnvs$threep = substr(grsnvs$trinuc, min(threepix), max(threepix))

        grsnvs$context = factor(
            paste0(grsnvs$fivep, "[", grsnvs$ref_collapse, ">", grsnvs$alt_collapse, "]", grsnvs$threep),
            ordconts
        )
        return(table(grsnvs$context))
    } else {
        return(table(factor(character(0), levels = ordconts)))
    }
}
