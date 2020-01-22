ra.merge = function (..., pad = 0, ind = FALSE, ignore.strand = FALSE) {
    ra = list(...)
    ra = ra[which(!sapply(ra, is.null))]
    nm = names(ra)
    if (is.null(nm)) {
        nm = paste("ra", 1:length(ra), sep = "")
    }
    nm = paste("seen.by", nm, sep = ".")
    if (length(nm) == 0) {
        return(NULL)
    }
    out = ra[[1]]
    values(out) = cbind(as.data.frame(matrix(FALSE, nrow = length(out), 
                                             ncol = length(nm), dimnames = list(NULL, nm))), values(out))
    if (!ind) {
        values(out)[, nm[1]] = TRUE
    }
    else {
        values(out)[, nm[1]] = 1:length(out)
    }
    if (length(ra) > 1) {
        for (i in 2:length(ra)) {
            this.ra = ra[[i]]
            if (length(this.ra) > 0) {
                values(this.ra) = cbind(as.data.frame(matrix(FALSE, 
                                                             nrow = length(this.ra), ncol = length(nm), 
                                                             dimnames = list(NULL, nm))), values(this.ra))
                ovix = ra.overlaps(out, this.ra, pad = pad, ignore.strand = ignore.strand)
                if (!ind) {
                    values(this.ra)[[nm[i]]] = TRUE
                }
                else {
                    values(this.ra)[[nm[i]]] = 1:length(this.ra)
                }
                if (!ind) {
                    if (!all(is.na(ovix))) {
                        values(out)[, nm[i]][ovix[, 1]] = TRUE
                    }
                }
                else {
                    values(out)[, nm[i]] = NA
                    if (!all(is.na(ovix))) {
                        values(out)[, nm[i]][ovix[, 1]] = ovix[, 
                                                               1]
                    }
                }
                if (!all(is.na(ovix))) {
                    nix = setdiff(1:length(this.ra), ovix[, 2])
                }
                else {
                    nix = 1:length(this.ra)
                }
                if (length(nix) > 0) {
                    val1 = values(out)
                    val2 = values(this.ra)
                    if (ind) {
                        val2[, nm[1:(i - 1)]] = NA
                    }
                    else {
                        val2[, nm[1:(i - 1)]] = FALSE
                    }
                    values(out) = NULL
                    values(this.ra) = NULL
                    out = grl.bind(out, this.ra[nix])
                    values(out) = rrbind(val1, val2[nix, ])
                } else if (length(setxor(1:length(this.ra), ovix[, 2])) == 0) { ## fix if there is perfect overlap... this case was previously not dealt with
                    merge_cols = setdiff(colnames(values(this.ra)), nm)
                    merge_cols = setdiff(merge_cols, colnames(mcols(out)))
                    if (length(merge_cols) > 0) {
                        tmp_field = "out_ix_50169346127375946273"
                        val1 = values(out)
                        val2 = values(this.ra)
                        val1[[tmp_field]] = seq_along(out)
                        val2[ovix[,2],tmp_field] = ovix[,1]
                        new_val = merge(val1, val2[,c(tmp_field, merge_cols)], by = tmp_field, all.x = TRUE)
                        new_val = new_val[order(new_val[[tmp_field]]),]
                        new_val[[tmp_field]] = NULL
                        values(out) = new_val
                    }
                }
            }
        }
    }
    return(out)
}


ra_breaks = function (rafile, keep.features = T, seqlengths = hg_seqlengths(),
    chr.convert = T, geno = NULL, flipstrand = FALSE, swap.header = NULL,
    breakpointer = FALSE, seqlevels = NULL, force.bnd = FALSE,
    skip = NA, get.loose = FALSE, dedup = TRUE,  pad = 500)
{
    if (is.character(rafile)) {
        if (grepl(".rds$", rafile)) {
            ra = readRDS(rafile)
            return(junctions(ra))
        }
        else if (grepl("(.bedpe$)", rafile)) {
            ra.path = rafile
            cols = c("chr1", "start1", "end1", "chr2", "start2",
                "end2", "name", "score", "str1", "str2")
            ln = readLines(ra.path)
            if (is.na(skip)) {
                nh = min(c(Inf, which(!grepl("^((#)|(chrom))",
                  ln)))) - 1
                if (is.infinite(nh)) {
                  nh = 1
                }
            }
            else {
                nh = skip
            }
            if ((length(ln) - nh) == 0) {
                return(GRangesList(GRanges(seqlengths = seqlengths))[c()])
            }
            if (nh == 0) {
                rafile = fread(rafile, header = FALSE)
            }
            else {
                rafile = tryCatch(fread(ra.path, header = FALSE,
                  skip = nh), error = function(e) NULL)
                if (is.null(rafile)) {
                  rafile = tryCatch(fread(ra.path, header = FALSE,
                    skip = nh, sep = "\\t"), error = function(e) NULL)
                }
                if (is.null(rafile)) {
                  rafile = tryCatch(fread(ra.path, header = FALSE,
                    skip = nh, sep = ","), error = function(e) NULL)
                }
                if (is.null(rafile)) {
                  stop("Error reading bedpe")
                }
            }
            setnames(rafile, 1:length(cols), cols)
            rafile[, `:=`(str1, ifelse(str1 %in% c("+", "-"),
                str1, "*"))]
            rafile[, `:=`(str2, ifelse(str2 %in% c("+", "-"),
                str2, "*"))]
        }
        else if (grepl("(vcf$)|(vcf.gz$)", rafile)) {
            require(VariantAnnotation)
            vcf = readVcf(rafile, Seqinfo(seqnames = names(seqlengths),
                                          seqlengths = seqlengths))
            vgr = read_vcf(rafile, swap.header = swap.header,
                           geno = geno)
            ## browser()
            mc = data.table(as.data.frame(mcols(vgr)))
            if (!("SVTYPE" %in% colnames(mc))) {
                warning("Vcf not in proper format.  Is this a rearrangement vcf?")
                return(GRangesList())
            }
            if (any(w.0 <- (width(vgr) < 1))) {
                warning("Some breakpoint width==0.")
                vgr[which(w.0)] = gr.start(vgr[which(w.0)]) %-%
                  1
            }
            if (any(duplicated(names(vgr))))
                names(vgr) = NULL
            if (length(vgr) == 0)
                return(GRangesList())
            .vcf2bnd = function(vgr) {
                if (!"END" %in% colnames(values(vgr)))
                  stop("Non BND SV should have the second breakpoint coor in END columns!")
                if (!"CHR2" %in% colnames(values(vgr)) | any(is.na(vgr$CHR2)))
                  vgr$CHR2 = as.character(seqnames(vgr))
                bp2 = data.table(as.data.frame(mcols(vgr)))
                bp2[, `:=`(seqnames = CHR2, start = as.numeric(END),
                  end = as.numeric(END))]
                bp2.gr = dt2gr(bp2)
                mcols(bp2.gr) = mcols(vgr)
                if (!is.null(names(vgr)) & !anyDuplicated(names(vgr))) {
                  jid = names(vgr)
                }
                else {
                  jid = seq_along(vgr)
                }
                names(vgr) = paste(paste0("exp", jid), "1", sep = ":")
                names(bp2.gr) = paste(paste0("exp", jid), "2",
                  sep = ":")
                vgr = resize(c(vgr, bp2.gr), 1)
                if (all(grepl("[_:][12]$", names(vgr)))) {
                  nm <- vgr$MATEID <- names(vgr)
                  ix <- grepl("1$", nm)
                  vgr$MATEID[ix] = gsub("(.*?)(1)$", "\\12",
                    nm[ix])
                  vgr$MATEID[!ix] = gsub("(.*?)(2)$", "\\11",
                    nm[!ix])
                  vgr$SVTYPE = "BND"
                }
                return(vgr)
            }
            if (!"MATEID" %in% colnames(mcols(vgr))) {
                if (length(fake.bix <- which(values(vgr)$SVTYPE ==
                  "BND")) != 0) {
                  values(vgr[fake.bix])$SVTYPE = "TRA"
                }
                if (all(names(vgr) == "N" | is.null(names(vgr)) |
                  all(grepl("^DEL|DUP|INV|BND", names(vgr))))) {
                  vgr = .vcf2bnd(vgr)
                }
            }
            else if (any(is.na(mid <- as.character(vgr$MATEID)))) {
                if (is.null(vgr$CHR2)) {
                  vgr$CHR2 = as.character(NA)
                }
                names(vgr) = gsub("_", ":", names(vgr))
                vgr$MATEID = sapply(vgr$MATEID, function(x) gsub("_",
                  ":", x))
                values(vgr) = data.table(as.data.frame(values(vgr)))
                if ("STRANDS" %in% colnames(mc) & any(ns <- sapply(vgr$STRANDS,
                  length) > 1)) {
                  if (any(fuix <- sapply(vgr[which(!ns)]$STRANDS,
                    str_count, ":") > 1)) {
                    tofix <- which(!ns)[fuix]
                    vgr$STRANDS[tofix] = lapply(vgr$STRANDS[tofix],
                      function(x) {
                        strsplit(gsub("(\\d)([\\+\\-])", "\\1,\\2",
                          x), ",")[[1]]
                      })
                    ns[tofix] = TRUE
                  }
                  vgr.double = vgr[which(ns)]
                  j1 = j2 = vgr.double
                  st1 = lapply(vgr.double$STRANDS, function(x) x[1])
                  st2 = lapply(vgr.double$STRANDS, function(x) x[2])
                  j1$STRANDS = st1
                  j2$STRANDS = st2
                  vgr.double = c(j1, j2)
                  names(vgr.double) = dedup(names(vgr.double))
                  vgr = c(vgr[which(!ns)], vgr.double)
                }
                mid <- as.logical(sapply(vgr$MATEID, length))
                vgr.bnd = vgr[which(mid)]
                vgr.nonbnd = vgr[which(!mid)]
                vgr.nonbnd = .vcf2bnd(vgr.nonbnd)
                mc.bnd = data.table(as.data.frame(values(vgr.bnd)))
                mc.nonbnd = data.table(as.data.frame(values(vgr.nonbnd)))
                mc.bnd$MATEID = as.character(mc.bnd$MATEID)
                vgr = c(vgr.bnd[, c()], vgr.nonbnd[, c()])
                values(vgr) = rbind(mc.bnd, mc.nonbnd)
            }
            if (!any(c("MATEID", "SVTYPE") %in% colnames(mcols(vgr))))
                stop("MATEID or SVTYPE not included. Required")
            vgr$mateid = vgr$MATEID
            vgr$svtype = vgr$SVTYPE
            if (!is.null(info(vcf)$SCTG))
                vgr$SCTG = info(vcf)$SCTG
            if (force.bnd)
                vgr$svtype = "BND"
            if (sum(vgr$svtype == "BND") == 0)
                warning("Vcf not in proper format.  Will treat rearrangements as if in BND format")
            if (!all(vgr$svtype == "BND")) {
                warning(sprintf("%s rows of vcf do not have svtype BND, treat them as non-BND!",
                  sum(vgr$svtype != "BND")))
            }
            bix = which(vgr$svtype == "BND")
            vgr = vgr[bix]
            alt <- sapply(vgr$ALT, function(x) x[1])
            ## browser()
            if ("CT" %in% colnames(mcols(vgr))) {
                message("CT INFO field found.")
                if ("SVLEN" %in% colnames(values(vgr))) {
                  del.ix = which(vgr$SVTYPE == "DEL")
                  dup.ix = which(vgr$SVTYPE == "DUP")
                  vgr$CT[del.ix] = "3to5"
                  vgr$CT[dup.ix] = "5to3"
                }
                ori = strsplit(vgr$CT, "to")
                iid = sapply(strsplit(names(vgr), ":"), function(x) as.numeric(x[2]))
                orimap = setNames(c("+", "-"), c("5", "3"))
                strd = orimap[sapply(seq_along(ori), function(i) ori[[i]][iid[i]])]
                strand(vgr) = strd
                vgr.pair1 = vgr[which(iid == 1)]
                vgr.pair2 = vgr[which(iid == 2)]
            }
            else if ("STRANDS" %in% colnames(mcols(vgr))) {
                message("STRANDS INFO field found.")
                ## browser()
                iid = sapply(strsplit(names(vgr), ":"), function(x) as.numeric(x[2]))
                vgr$iid = iid
                vgr = vgr[order(names(vgr))]
                iid = vgr$iid
                ori = strsplit(substr(unlist(vgr$STRANDS), 1,
                  2), character(0))
                orimap = setNames(c("+", "-"), c("-", "+"))
                strd = orimap[sapply(seq_along(ori), function(i) ori[[i]][iid[i]])]
                strand(vgr) = setNames(strd, NULL)
                vgr.pair1 = vgr[which(iid == 1)]
                vgr.pair2 = vgr[which(iid == 2)]
            }
            else if (any(grepl("\\[", alt))) {
                message("ALT field format like BND")
                vgr$first = !grepl("^(\\]|\\[)", alt)
                vgr$right = grepl("\\[", alt)
                vgr$coord = as.character(paste(seqnames(vgr),
                  ":", start(vgr), sep = ""))
                vgr$mcoord = as.character(gsub(".*(\\[|\\])(.*\\:.*)(\\[|\\]).*",
                  "\\2", alt))
                vgr$mcoord = gsub("chr", "", vgr$mcoord)
                geno(vcf)
                values(vgr)
                if (all(is.na(vgr$mateid)))
                  if (!is.null(names(vgr)) & !any(duplicated(names(vgr)))) {
                    warning("MATEID tag missing, guessing BND partner by parsing names of vgr")
                    vgr$mateid = paste(gsub("::\\d$", "", names(vgr)),
                      (sapply(strsplit(names(vgr), "\\:\\:"),
                        function(x) as.numeric(x[length(x)])))%%2 +
                        1, sep = "::")
                  }
                  else if (!is.null(vgr$SCTG)) {
                    warning("MATEID tag missing, guessing BND partner from coordinates and SCTG")
                    require(igraph)
                    ucoord = unique(c(vgr$coord, vgr$mcoord))
                    vgr$mateid = paste(vgr$SCTG, vgr$mcoord,
                      sep = "_")
                    if (any(duplicated(vgr$mateid))) {
                      warning("DOUBLE WARNING! inferred mateids not unique, check VCF")
                      bix = bix[!duplicated(vgr$mateid)]
                      vgr = vgr[!duplicated(vgr$mateid)]
                    }
                  }
                  else {
                    stop("Error: MATEID tag missing")
                  }
                vgr$mix = as.numeric(match(vgr$mateid, names(vgr)))
                pix = which(!is.na(vgr$mix))
                vgr.pair = vgr[pix]
                if (length(vgr.pair) == 0) {
                  stop("Error: No mates found despite nonzero number of BND rows in VCF")
                }
                vgr.pair$mix = match(vgr.pair$mix, pix)
                vix = which(1:length(vgr.pair) < vgr.pair$mix)
                vgr.pair1 = vgr.pair[vix]
                vgr.pair2 = vgr.pair[vgr.pair1$mix]
                tmpix = vgr.pair1$first & vgr.pair1$right
                if (any(tmpix)) {
                  strand(vgr.pair1)[tmpix] = "-"
                  strand(vgr.pair2)[tmpix] = "+"
                }
                tmpix = vgr.pair1$first & !vgr.pair1$right
                if (any(tmpix)) {
                  strand(vgr.pair1)[tmpix] = "-"
                  strand(vgr.pair2)[tmpix] = "-"
                }
                tmpix = !vgr.pair1$first & !vgr.pair1$right
                if (any(tmpix)) {
                  strand(vgr.pair1)[tmpix] = "+"
                  strand(vgr.pair2)[tmpix] = "-"
                }
                tmpix = !vgr.pair1$first & vgr.pair1$right
                if (any(tmpix)) {
                  strand(vgr.pair1)[tmpix] = "+"
                  strand(vgr.pair2)[tmpix] = "+"
                }
                pos1 = as.logical(strand(vgr.pair1) == "+")
                if (any(pos1)) {
                  start(vgr.pair1)[pos1] = start(vgr.pair1)[pos1] -
                    1
                  end(vgr.pair1)[pos1] = end(vgr.pair1)[pos1] -
                    1
                }
                pos2 = as.logical(strand(vgr.pair2) == "+")
                if (any(pos2)) {
                  start(vgr.pair2)[pos2] = start(vgr.pair2)[pos2] -
                    1
                  end(vgr.pair2)[pos2] = end(vgr.pair2)[pos2] -
                    1
                }
            }
            ra = grl.pivot(GRangesList(vgr.pair1[, c()], vgr.pair2[,
                c()]))
            if (exists("pix") & exists("vix"))
                this.inf = values(vgr)[pix[vix], ]
            if (exists("iid"))
                this.inf = values(vgr[which(iid == 1)])
            if (is.null(this.inf$POS)) {
                this.inf = cbind(data.frame(POS = ""), this.inf)
            }
            if (is.null(this.inf$CHROM)) {
                this.inf = cbind(data.frame(CHROM = ""), this.inf)
            }
            if (is.null(this.inf$MATL)) {
                this.inf = cbind(data.frame(MALT = ""), this.inf)
            }
            this.inf$CHROM = seqnames(vgr.pair1)
            this.inf$POS = start(vgr.pair1)
            this.inf$MATECHROM = seqnames(vgr.pair2)
            this.inf$MATEPOS = start(vgr.pair2)
            this.inf$MALT = vgr.pair2$AL
            values(ra) = this.inf
            if (is.null(values(ra)$TIER)) {
                values(ra)$tier = ifelse(values(ra)$FILTER %in%
                  c(".", "PASS"), 2, 3)
            }
            else {
                values(ra)$tier = values(ra)$TIER
            }
            if (dedup) {
                ra = ra.dedup(ra, pad = pad)
            }
            if (!get.loose | is.null(vgr$mix)) {
                return(ra)
            }
            else {
                npix = is.na(vgr$mix)
                vgr.loose = vgr[npix, c()]
                tmp = tryCatch(values(vgr)[bix[npix], ], error = function(e) NULL)
                if (!is.null(tmp)) {
                  values(vgr.loose) = tmp
                }
                else {
                  values(vgr.loose) = cbind(vcf@fixed[bix[npix],
                    ], info(vcf)[bix[npix], ])
                }
                return(list(junctions = ra, loose.ends = vgr.loose))
            }
        }
        else {
            rafile = read.delim(rafile)
        }
    }
    if (is.data.table(rafile)) {
        require(data.table)
        rafile = as.data.frame(rafile)
    }
    if (nrow(rafile) == 0) {
        out = GRangesList()
        values(out) = rafile
        return(out)
    }
    if (flipstrand) {
        rafile$str1 = ifelse(rafile$strand1 == "+", "-", "+")
        rafile$str2 = ifelse(rafile$strand2 == "+", "-", "+")
    }
    if (!is.null(seqlevels)) {
        rafile$chr1 = seqlevels[rafile$chr1]
        rafile$chr2 = seqlevels[rafile$chr2]
    }
    if (is.null(rafile$str1)) {
        rafile$str1 = rafile$strand1
    }
    if (is.null(rafile$str2)) {
        rafile$str2 = rafile$strand2
    }
    if (!is.null(rafile$pos1) & !is.null(rafile$pos2)) {
        if (breakpointer) {
            rafile$pos1 = rafile$T_BPpos1
            rafile$pos2 = rafile$T_BPpos2
        }
        if (!is.numeric(rafile$pos1)) {
            rafile$pos1 = as.numeric(rafile$pos1)
        }
        if (!is.numeric(rafile$pos2)) {
            rafile$pos2 = as.numeric(rafile$pos2)
        }
        rafile$str1 <- gsub("[()]", "", rafile$str1)
        rafile$str2 <- gsub("[()]", "", rafile$str2)
        if (is.character(rafile$str1) | is.factor(rafile$str1)) {
            rafile$str1 = gsub("0", "-", gsub("1", "+", gsub("\\-",
                "1", gsub("\\+", "0", rafile$str1))))
        }
        if (is.character(rafile$str2) | is.factor(rafile$str2)) {
            rafile$str2 = gsub("0", "-", gsub("1", "+", gsub("\\-",
                "1", gsub("\\+", "0", rafile$str2))))
        }
        if (is.numeric(rafile$str1)) {
            rafile$str1 = ifelse(rafile$str1 > 0, "+", "-")
        }
        if (is.numeric(rafile$str2)) {
            rafile$str2 = ifelse(rafile$str2 > 0, "+", "-")
        }
        rafile$rowid = 1:nrow(rafile)
        bad.ix = is.na(rafile$chr1) | is.na(rafile$chr2) | is.na(rafile$pos1) |
            is.na(rafile$pos2) | is.na(rafile$str1) | is.na(rafile$str2) |
            rafile$str1 == "*" | rafile$str2 == "*" | rafile$pos1 <
            0 | rafile$pos2 < 0
        rafile = rafile[which(!bad.ix), ]
        if (nrow(rafile) == 0) {
            return(GRanges())
        }
        seg = rbind(data.frame(chr = rafile$chr1, pos1 = rafile$pos1,
            pos2 = rafile$pos1, strand = rafile$str1, ra.index = rafile$rowid,
            ra.which = 1, stringsAsFactors = F), data.frame(chr = rafile$chr2,
            pos1 = rafile$pos2, pos2 = rafile$pos2, strand = rafile$str2,
            ra.index = rafile$rowid, ra.which = 2, stringsAsFactors = F))
        if (chr.convert) {
            seg$chr = gsub("chr", "", gsub("25", "M", gsub("24",
                "Y", gsub("23", "X", seg$chr))))
        }
        out = seg2gr(seg, seqlengths = seqlengths)[, c("ra.index",
            "ra.which")]
        out = split(out, out$ra.index)
    }
    else if (!is.null(rafile$start1) & !is.null(rafile$start2) &
        !is.null(rafile$end1) & !is.null(rafile$end2)) {
        ra1 = gr.flipstrand(GRanges(rafile$chr1, IRanges(rafile$start1,
            rafile$end1), strand = rafile$str1))
        ra2 = gr.flipstrand(GRanges(rafile$chr2, IRanges(rafile$start2,
            rafile$end2), strand = rafile$str2))
        out = grl.pivot(GRangesList(ra1, ra2))
    }
    if (keep.features) {
        values(out) = rafile[, ]
    }

    ## if (!is.null(pad)) {
    ##     out = ra.dedup(out, pad = pad)
    ## }
    if (!get.loose) {
        return(out)
    }
    else {
        return(list(junctions = out, loose.ends = GRanges()))
    }
    return(new("junctions", out))
}
