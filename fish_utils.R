castrod = function(mods, nm = c(names(grl.feat), names(grl.feat.ch), names(grl.feat.ov), names(grl.feat.mes), "super", "openchr_h1hesc")) {
    ## out = rbindlist(mods)[event %nin% "qrp"][trimws(name) %in% nm][, estimate := replace2(estimate, list(p.adjust(dg(p), "BH") > 0.10, dg(estimate) < log(1.25)), list(NA, NA))]
    out = rbindlist(mods, fill = TRUE)[event %nin% "qrp"][trimws(name) %in% nm][, estimate := normv_sep(estimate), by = name][, estimate := replace2(estimate, p.adjust(dg(p), "BH") > 0.25, NA)]
    ## out = rbindlist(mods,fill = TRUE)[event %nin% "qrp"][trimws(name) %in% nm][, estimate := replace2(estimate, p.adjust(dg(p), "BH") > 0.10, NA)]
    ## out = rbindlist(mods)[event %nin% "qrp"][trimws(name) %in% nm]
    return(dcast.wrap(out, lh = "event", rh = "name", value.var = "estimate"))
}

model.enr = function(x, enr = "pos") {
    if (identical(enr, "pos"))
        summ_glm(x$model)[p.adjust(p, "bonferroni") < 0.05 & estimate > 0]
    else if (identical(enr, "neg"))
        summ_glm(x$model)[p.adjust(p, "bonferroni") < 0.05 & estimate < 0]
    else if (is.null(enr) || is.na(enr) || !nzchar(enr) || identical(enr, "both"))
        summ_glm(x$model)[p.adjust(p, "bonferroni") < 0.05]
}


plot_qqp = function(fish, color.sig = FALSE, sig.thresh = 0.25, ...) {
    if (color.sig == TRUE) {
        col = with(fish$res, ifelse(fdr < sig.thresh, "red", alpha("black", 0.05)))
        ppng(qqp(fish$res, plotly = FALSE, bottomrighttext = paste0("alpha = ", round(fish$model$theta, 2)),
                 col = col, ...), res = 200, h = 5, w = 5)
    } else {
        ppng(qqp(fish$res, plotly = FALSE, bottomrighttext = paste0("alpha = ", round(fish$model$theta, 2)),
                 ...), res = 200, h = 5, w = 5)
    }
    ## ppng(fish$qqp(plotly = FALSE, ...), res = 200, h = 5, w = 5)
}


annotate.tiles.grl = function(tiles, grl, pad, nm) {
    bind.cols = name.fc(process.grl.fish(tiles, grl, pad = 2.5e4), nm)
    mcols(tiles) = cbind(mcols(tiles), bind.cols)
    tiles
}

annotate.tiles.gr = function(tiles) {
    tiles$maxwid = log(gr.eval(tiles, pge, max(width) + 1, fill = 0) + 1)
    tiles$rept = (tiles %$% rept[, 'timing'])$timing %>% round
    tiles$super = tiles %O% super %>% pmax(0.01) %>% pmin(0.99)
    tiles$fragile = (tiles %^% fragile) %>% sign
    tiles$attract = pinch.frac(tiles %O% (reduce(at.tracts + 1e2) - 1e2))
    tiles
}

sigfish = function(x, thresh = 0.25) {
    if (inherits(x, "FishHook"))
        x = x$res
    x %Q% (fdr < thresh)
}


insigfish = function(x, thresh = 0.25) {
    if (inherits(x, "FishHook"))
        x = x$res
    x %Q% (fdr > thresh)
}

.topr = function(fishres, pad = 1e5, sigth = 0.25) (((((fishres %Q% (fdr< sigth) %Q% (order(p)))+pad) %>% gr.reduce)-pad) %$% pge.cgc[, "cgc"] %$%  super[, 'tissue'] %Q% order(fdr))


report_fish = function(fish) {
    if (inherits(fish, "FishHook")) {
        fish = fish$res
        out = fish %>% .topr %>% as.data.table %>% print
    } else if (inherits(fish, "GRanges"))
        out = fish %>% .topr %>% as.data.table %>% print
    else if (inherits(fish, "data.frame"))
        out = fish %>% .topr %>% as.data.table %>% print
    ## out = fish %>% .top() %>% as.data.table %>% print
    with(fish, {
        message("numsig (fdr < 0.25): ", length(which(fdr < 0.25)))
        message("numhyp: ", length(start))
        message("summary width stats: ")
        print(summary(width))
    })
    invisible(out)
}
