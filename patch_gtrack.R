'plot.gTrack' =  function(x,  ##pplot  (for easy search)
                          y,
                          windows = si2gr(seqinfo(x)), ## windows to plot can be Granges or GRangesList
                          links = NULL, ## GRangesList of pairs of signed locations,
                          gap = NULL,  ## spacing betwen windows (in bp)
                          y.heights = NULL, # should be scalar or length(windows) if windows is a GRangesList
                          y.gaps = NULL, # relative heights of gaps below and above stacks (xaxes will be drawn here)
                          cex.xlabel = 1,
                          cex.ylabel = 1,
                          max.ranges = NA, # parameter for max ranges to draw on canvas in each track (overrides formatting)
                          links.feat = NULL, # links features override for links (must be nrow 1 or length(links) data frame
                          verbose=FALSE,
                          legend.params = list(),
                          ... ## additional args to draw.grl OR last minute formatting changes to gTrack object
                          ) {

    if (!missing(y))
        windows = y
    
    .Object = x
    if (!missing(y))
        windows = y

    if (is(windows, 'numeric') | is(windows, 'integer'))
        windows = as.character(windows)

    if (is(windows, 'character'))
        windows = unlist(parse.grl(windows, seqlengths(seqinfo(.Object))))

    ## make sure we have min legend data
    if (!"xpos" %in% names(legend.params))
        legend.params$xpos = 0
    if (!"ypos" %in% names(legend.params))
        legend.params$ypos = 1
    if (!"plot" %in% names(legend.params))
        legend.params$plot = TRUE

    win.gap = gap ## PATCH: recasting some variable names
    new.plot = TRUE
    window.segs = list();
    dotdot.args = list(...);

    ## parse the wind<ows into GRanges
    windows = format_windows(windows, .Object)

    windows = windows[width(windows)>0]

    ## if totally empty, plot blank and leave
    if(!length(windows)) {
        plot.blank(bg.col = bg.col)
        return()
    }

    ## make sure gTrack has all fields that are expected later
    .Object <- prep_defaults_for_plotting(.Object)

    if (length(dotdot.args)>0) {
        format_name <- which(names(dotdot.args) == "name")
        if (length(format_name) > 0) {
            formatting(.Object)[, format_name] = dotdot.args[[format_name]]
        }
    }

    if (is.null(formatting(.Object)$legend))
        formatting(.Object)$legend = TRUE
    else (any(is.na(formatting(.Object)$legend)))
    formatting(.Object)$legend[is.na(formatting(.Object)$legend)] = TRUE


    if (is.null(formatting(.Object)$legend.title))
        formatting(.Object)$legend.title = NA

    ## name vs track.name saga
    if (is.null(formatting(.Object)$track.name))
    {
        if (!is.null(formatting(.Object)$name))
            formatting(.Object)$track.name = formatting(.Object)$name
        else if (!is.null(formatting(.Object)$y.field))
            formatting(.Object)$track.name = formatting(.Object)$y.field
        else
            formatting(.Object)$track.name = NA
    }

    has.colormap = sapply(colormap(.Object), length)>0
    has.colorfield = !is.na(formatting(.Object)$gr.colorfield)
    formatting(.Object)$legend = ifelse(is.na(formatting(.Object)$legend), formatting(.Object)$legend == TRUE & (has.colormap | has.colorfield), formatting(.Object)$legend)
    numlegends = sum(formatting(.Object)$legend)
    which.legend = which(formatting(.Object)$legend)
    .Object$legend.title = ifelse(!is.na(.Object$legend.title), .Object$legend.title,
                           ifelse(has.colormap, names(colormap(.Object))[1:length(.Object)],
                           ifelse(has.colorfield, .Object$gr.colorfield,
                           ifelse(!is.na(.Object$track.name), .Object$track.name, ''))))

    ## add last minute formatting changes to gTrack
    if (length(dotdot.args)>0)
        for (f in intersect(names(dotdot.args), names(formatting(.Object))))
            formatting(.Object)[, f] = dotdot.args[[f]]

    ## set the window gap .. so that windows don't collide
    if (is.null(win.gap))
        win.gap = sum(as.numeric(width(windows)))/30

    ## get the height of the stacks
    if (is.null(y.heights) | length(y.heights) != length(windows))
        ##y.heights = rep(1, length(windows)) ## old from when we had windows as GRangesList
        y.heights <- 1

    ## set the gaps between the gTracks
    if (is.null(y.gaps) )
        y.gaps = y.heights*0.8
    else if (length(y.gaps) != length(windows))
        y.gaps = rep(y.gaps[1], length(windows))

    ## ensure that we don't plot too much
    if (!is.na(max.ranges))
        formatting(.Object)$max.ranges = pmin(max.ranges, formatting(.Object)$max.ranges, na.rm = TRUE)

    oth.ix = 1:(length(windows)-1);
    top.gaps = 0.5*y.gaps
    bottom.gaps = 0.5*y.gaps
    if (length(windows)==1)
        oth.ix = c()
    ylim.stacks = data.frame(start = c(bottom.gaps[1], bottom.gaps[1] + cumsum(y.heights[oth.ix] + top.gaps[oth.ix] + bottom.gaps[oth.ix+1])),
                             end = cumsum(y.heights + top.gaps + bottom.gaps) - top.gaps)

    oth.ix = 1:(length(.Object)-1);

    if (length(.Object)==1)
        oth.ix = c()

    tmp.top.gaps = 0.5 * formatting(.Object)$ygap
    tmp.bottom.gaps = 0.5 * formatting(.Object)$ygap
    tmp.ylim.subplot = data.frame(start = c(tmp.bottom.gaps[1], tmp.bottom.gaps[1] +

cumsum(formatting(.Object)$height[oth.ix] + tmp.top.gaps[oth.ix] + tmp.bottom.gaps[oth.ix+1])),
end = cumsum(formatting(.Object)$height + tmp.top.gaps + tmp.bottom.gaps) - tmp.top.gaps)

    ylim = c(0, max(ylim.stacks$end)+top.gaps[length(top.gaps)])
    ylim.parent=ylim
    window.ylims = data.frame(start = rep(NA, length(windows)), end = NA);

    new.axis = TRUE;

    this.windows = windows
                                        #end(this.windows) <- end(this.windows) + 1 ## +1 added
                                        #this.windows = gUtils::streduce(windows[[i]]) ##gr.stripstrand(GenomicRanges::trim(windows[[i]]))
    ##if (!inherits(this.windows, 'GRanges'))
    ##  this.windows = gUtils::si2gr(this.windows)
    i=1
    this.ylim.subplot = tmp.ylim.subplot;
    this.ylim.subplot$start = affine.map(pmin(1, this.ylim.subplot$start), ylim = unlist(ylim.stacks[i, c('start', 'end')]), xlim = c(0, 1))
    this.ylim.subplot$end = affine.map(pmin(1, this.ylim.subplot$end), ylim = unlist(ylim.stacks[i, c('start', 'end')]), xlim = c(0, 1))
    this.tmp.bottom.gap = tmp.bottom.gaps[1]*(ylim.stacks$end[i]-ylim.stacks$start[i])

    this.xaxis.pos = this.ylim.subplot$start[1]-bottom.gaps[i]*0-this.tmp.bottom.gap
    this.xaxis.pos.label = this.ylim.subplot$start[1]-5*bottom.gaps[i]/6-this.tmp.bottom.gap
    ylim.stacks[i, 'xaxis.pos'] = this.xaxis.pos

    ## loop through the gTracks
    for (j in 1:length(.Object))
    {
        par(xpd = NA);
        cmap = colormap(.Object)[[j]];
        cfield = names(colormap(.Object))[j]

        if (is.na(cfield))
            cfield = formatting(.Object)$gr.colorfield[j]

        if (length(cmap)==0)
            cmap = NA

        ## get the data into GRanges or GRangesList format

        pre.filtered = FALSE;
        if (.Object@formatting$triangle[j])
            pre.filtered = TRUE


        tt <- extract_data_from_tmp_dat(.Object, j, this.windows)
        .Object = tt$o
        tmp.dat = tt$t
        this.windows = tt$w

        ## subsample if we need to for enforcing max.ranges
        if (!is.na(formatting(.Object)$max.ranges[j]) && formatting(.Object)$max.ranges[j] > 0) {
            tt <- enforce_max_ranges(.Object, pre.filtered, j, tmp.dat, this.windows)
            tmp.dat = tt$t
            pre.filtered = tt$p
        }

        ## flag to tell us whether data is pre-filtered to window (ie in fftrack or rlelist)

        ## adjust y0 .bar
        if (is.null((formatting(.Object)$y0.bar[j])) || is.na((formatting(.Object)$y0.bar[j])))
            formatting(.Object)$y0.bar[j] = NA

        ## smooth the y.field data
        if (!is.na(formatting(.Object)$y.field[j]) && is(tmp.dat, 'GRanges') && !is.na(formatting(.Object)$smooth[j]))
            tmp.dat <- smooth_yfield(.Object, j, tmp.dat)

        ## fix y limits and apply log transform if needed
        if (!is.na(formatting(.Object)$y.field[j]) && (is.na(formatting(.Object)$y0[j]) || is.na(formatting(.Object)$y1[j])))
            .Object <- format_yfield_limits(.Object, j, tmp.dat, pre.filtered, this.windows)

                                        # if (formatting(.Object[j])$format != 'ranges')
                                        #   stop("violated assumption. need to fix")

        all.args = as.list(formatting(.Object[j]))
        all.args = c(dotdot.args, all.args[!names(all.args) %in% names(dotdot.args)])

                                        #                     all.args = list(
                                        #   col = formatting(.Object)$col[j],
                                        #   ywid = formatting(.Object)$ywid[j],
                                        #   border = formatting(.Object)$border[j],
                                        #   lwd.border = formatting(.Object)$lwd.border[j],
                                        #   adj.label = c(formatting(.Object)$hadj.label[j],
                                        #     formatting(.Object)$vadj.label[j]),
                                        #   gr.adj.label = c(0.5,
                                        #     formatting(.Object)$vadj.label[j]),
                                        #   angle = formatting(.Object)$angle[j],
                                        #   y.pad = formatting(.Object)$ypad[j],
                                        #   circles = formatting(.Object)$circles[j],
                                        #   lines = formatting(.Object)$lines[j],
                                        #   bars = formatting(.Object)$bars[j],
                                        #   y.grid.cex = formatting(.Object)$yaxis.cex[j],
                                        #   edges = edgs(.Object)[[j]],
                                        #   y0.bar = formatting(.Object)$y0.bar[j],
                                        #   stack.gap = formatting(.Object)$stack.gap[j])
                                        # na.fields = names(formatting(.Object))[sapply(1:ncol(formatting(.Object)), function(field) is.na(formatting(.Object)[j, field]))]
                                        # other.fields = setdiff(names(formatting(.Object)), c('name', 'height', 'ygap', 'stack.gap', 'lift', 'split', 'angle', 'format', 'lwd.border', 'source.file', 'source.file.chrsub', 'ypad', 'ywid', 'border', 'col', 'hadj.label', 'vadj.label', 'y.field', 'round', 'cex.ylabel', 'y.quantile', 'max.ranges', 'yaxis', 'yaxis.cex', 'is.null', 'yaxis.pretty', names(all.args))) ## remove na fields and anything else that might mess up draw.grl
                                        #
                                        # other.formats = structure(names = other.fields,
                                        #   lapply(other.fields, function(x) formatting(.Object)[j, x]))
                                        # all.args[names(other.formats)] = other.formats;
                                        #
                                        # all.args = all.args[setdiff(names(all.args), setdiff(na.fields, c('col')))]

        this.y.field = formatting(.Object)$y.field[j]
        this.y.grid = NA;

        if (is.na(this.y.field) | !(this.y.field %in% names(values(tmp.dat))))
            this.y = this.ylim.subplot[j, ]
        else
        {
            if (is.null(formatting(.Object)$log))
                formatting(.Object)$log = NA

            if (!is.na(formatting(.Object)$log[j]))
                if (formatting(.Object)$log[j])
                {
                    if (!is.null(tmp.dat$ywid))
                        tmp.dat$ywid = log10(tmp.dat$ywid)
                    values(tmp.dat)[, this.y.field] = log10(values(tmp.dat)[, this.y.field])
                    formatting(.Object)[j, 'y0'] = log10(formatting(.Object)[j, 'y0'])
                    formatting(.Object)[j, 'y1'] = log10(formatting(.Object)[j, 'y1'])

                }
            range.y = NULL;
            if (all(c('y0', 'y1') %in% names(formatting(.Object))))
            {
                if (!is.na(formatting(.Object)[j, 'y0']) & !is.na(formatting(.Object)[j, 'y1']))
                    range.y = c(formatting(.Object)[j, 'y0'], formatting(.Object)[j, 'y1'])
                else if (!is.na(formatting(.Object)[j, 'y0']) & is.na(formatting(.Object)[j, 'y1']))
                    range.y = c(formatting(.Object)[j, 'y0'], max(setdiff(values(tmp.dat)[, this.y.field], c(Inf, -Inf)), na.rm = T))
                else if (is.na(formatting(.Object)[j, 'y0']) & !is.na(formatting(.Object)[j, 'y1']))
                    range.y = c(min(setdiff(values(tmp.dat)[, this.y.field], c(Inf, -Inf)), na.rm = T), formatting(.Object)[j, 'y1'])
            }

            if (length(tmp.dat)>0)
            {
                if (!is.null(tmp.dat$ywid)) ## remove any weird infinite ywids
                {

                    if (any(ix <- is.infinite(values(tmp.dat)$ywid)))
                        values(tmp.dat)$ywid[ix] = NA
                }
                else
                    values(tmp.dat)$ywid = NA
                if (is.null(range.y)) ## if y range is empty then pull from data
                    range.y = range(setdiff(values(tmp.dat)[, this.y.field], c(Inf, -Inf)), na.rm = T);
                                        #                              range.y = range(setdiff(values(dat(.Object)[[j]])[, this.y.field], c(Inf, -Inf)), na.rm = T);
            }
            ## however if there is a single data value, then we need to scale appropriately
            if (diff(range.y)==0)
            {

                if (any(ix <- !is.na(values(tmp.dat)$ywid))) ## use ywid
                    range.y = range.y + 5*max(values(tmp.dat)$ywid[ix])*c(-1, 1)
                else # otherwise use some arbitrary proportion around the value
                    range.y = range.y + abs(range.y)*0.2*c(-1, 1)
            }

            this.y.ticks = pretty(range.y, formatting(.Object)$yaxis.pretty[j])

            if (is.null(formatting(.Object)$y.cap))
                formatting(.Object)$y.cap = NA

            if (!is.na(formatting(.Object)$y.cap[j])) ## cap values from top and bottom
                this.y = affine.map(values(tmp.dat)[, this.y.field], ylim = unlist(this.ylim.subplot[j, ]), xlim = range(this.y.ticks), cap = formatting(.Object)$y.cap[j])
            else
                this.y = affine.map(values(tmp.dat)[, this.y.field], ylim = unlist(this.ylim.subplot[j, ]), xlim = range(this.y.ticks), cap = TRUE)
                                        #                            this.y = affine.map(values(dat(.Object)[[j]])[, this.y.field], ylim = unlist(this.ylim.subplot[j, ]), xlim = range(this.y.ticks))

            ## if need, bump the range to include ybar base
                                        # if (formatting(.Object)$y0.bar[j] < min(unlist(this.ylim.subplot[j, ])))
                                        #   this.ylim.subplot[j,'start'] <- formatting(.Object)$y0.bar[j]

            if (is.na(formatting(.Object)$y0.bar[j]))
                formatting(.Object)$y0.bar[j] = 0

            all.args$y0.bar = affine.map(formatting(.Object)$y0.bar[j], ylim = unlist(this.ylim.subplot[j, ]), xlim = range(this.y.ticks))
            if (formatting(.Object)$yaxis[j])
            {
                                        # make pretty grid in range.y
                this.y.grid = structure(affine.map(this.y.ticks, ylim = unlist(this.ylim.subplot[j, ]), xlim = range(this.y.ticks)), names = this.y.ticks)

                if (!is.na(formatting(.Object)$log[j]))
                    if (formatting(.Object)$log[j])
                        names(this.y.grid) = signif(10^this.y.ticks)
            }
        }

        if (is.null(.Object[j]$bars))
            formatting(.Object)$bars[j] = FALSE
        else if (is.na(.Object[j]$bars))
            formatting(.Object)$bars[j] = FALSE


        if (.Object[j]$bars && is.na(all.args$y0.bar))
            all.args$y0.bar = this.ylim.subplot[j, 1]

        if (is.null(.Object$chr.sub))
            .Object$chr.sub = FALSE

        if (is.na(.Object$chr.sub[j]))
            .Object$chr.sub[j] = FALSE

        if (.Object[j]$chr.sub)
            tmp.windows = gr.sub(windows, 'chr', '')
        else
            tmp.windows = this.windows

        ## fix legend params
        this.legend.params = legend.params
        if (!formatting(.Object)$legend[j])
            this.legend.params$plot = FALSE
        else
        {
            this.legend.params$xpos = NA
            if (!is.null(formatting(.Object)$legend.xpos))
                this.legend.params$xpos = is.formatting(.Object)$legend.xpos[j]
            if (!is.null(formatting(.Object)$legend.ypos))
                this.legend.params$ypos = formatting(.Object)$legend.ypos[j]

            jj = match(j, which.legend)
            if (is.na(this.legend.params$xpos))
                this.legend.params$xpos = seq(0, 1, length.out = numlegends)[jj]

            this.legend.params$xpos = seq(0, 1, length.out = numlegends)[jj]
            this.legend.params$xjust = c(0, 0.5, 1)[as.integer(cut(this.legend.params$xpos, c(-0.01, 0.2, 0.8, 1)))]
            this.legend.params$title = .Object$legend.title[j]
        }


        ## remove field "y" from tmp.dat if it exists
        if (is(tmp.dat, 'GRangesList'))
        {
            values(tmp.dat)$y = NULL
        }
        else
        {
            if (!is.null(tmp.dat$y))
                tmp.dat$y = NULL
        }

        main.args <- list(grl=tmp.dat,y = this.y, ylim = ylim,
                          xaxis.pos = this.xaxis.pos,xaxis.pos.label = this.xaxis.pos.label,
                          win.gap = win.gap[i],windows = tmp.windows,
                          new.plot = new.plot, new.axis = new.axis,
                          gr.colorfield = cfield,gr.colormap = cmap,
                          legend = formatting(.Object)$legend[j],
                          y.grid = this.y.grid, verbose=verbose,
                          ylim.parent=ylim.parent,mdata=.Object@mdata[[j]],
                          leg.params = this.legend.params,
                          adj.label = c(formatting(.Object)$hadj.label[j],
                                        formatting(.Object)$vadj.label[j]),
                          gr.adj.label = c(0.5,
                                           formatting(.Object)$vadj.label[j]),
                          y.pad = formatting(.Object)$ypad[j],
                          y.grid.cex = formatting(.Object)$yaxis.cex[j],
                          edges = edgs(.Object)[[j]])
        all.args <- c(main.args, all.args[!names(all.args) %in% names(main.args)])

        ## clear out na.args to make default setting simpler downstream
        na.args = sapply(all.args, function(x) if(is.vector(x)) all(is.na(x)) else FALSE)
        if (any(na.args))
            all.args = all.args[!na.args]

                                        # main.args = c(main.args, all.args[setdiff(names(all.args), names(main.args))]);
                                        #
                                        # other.args = dotdot.args
                                        # other.args = other.args[setdiff(names(other.args), names(main.args))]
        ## make empty plot

        if (new.plot)
        {
            blank.main.args <- all.args
###blank.main.args = main.args;
###blank.main.args[[1]] = blank.main.args[[1]][c()]
                                        #blank.main.args$y = list(start = min(this.ylim.subplot$start), end = max(this.ylim.subplot$end))

                                        # if (any(is.na(.Object@formatting$triangle)) & any(.Object@formatting$triangle))
                                        #   blank.main.args$y = list(start = min(this.ylim.subplot$start[is.na(.Object@formatting$triangle)]), end = max(this.ylim.subplot$end[is.na(.Object@formatting$triangle)])) ## JEREMIAH
                                        # else
            blank.main.args$grl <- GRanges()
            blank.main.args$y = list(start=min(this.ylim.subplot$start), end = max(this.ylim.subplot$end))
            blank.main.args$triangle=FALSE
            blank.main.args$sep.draw=FALSE

            do.call('draw.grl', blank.main.args)
            ##do.call('draw.grl', c(blank.main.args, other.args))
            all.args$new.plot = FALSE
            all.args$new.axis = FALSE
        }

        new.plot = FALSE
        new.axis = FALSE
        ##arrg <- c(main.args, other.args)
        ##form <- as.list(formatting(.Object[j]))
        ##arrg <- c(arrg, form[!names(form) %in% names(arrg)]) ## add in remaining args

        window.segs = list();
        ##window.segs[[i]] = do.call('draw.grl', c(main.args, other.args))

        if (formatting(.Object[j])$triangle)
        {
            all.args$sigma= all.args$smooth
            window.segs[[i]] <- do.call('draw.triangle', all.args[names(all.args) %in% c("grl","y","mdata","ylim.parent","windows","win.gap","sigma",
                                                                                         "cmap.min","cmap.max", "m.sep.lwd","m.bg.col","legend","leg.params",
                                                                                         "islog","gr.colormap")])
        } else {
            window.segs[[i]] <- do.call('draw.grl', all.args)
        }

        this.tname = formatting(.Object[j])$track.name

        if (!is.na(this.tname))
        {
            ylab.las = if (is.null(dotdot.args[["ylab.las"]])) 0 else dotdot.args[["ylab.las"]]
            ylab.adj = if (is.null(dotdot.args[["ylab.adj"]])) c(0.5, 1) else dotdot.args[["ylab.adj"]]
            if (ylab.las %in% c(0,3)) {
                ylab.angle = -90
            } else if (ylab.las %in% c(1,2)) {
                ylab.angle = 0
            }
            this.cex.ylabel = ifelse(!is.null(formatting(.Object[j])$cex.ylabel), formatting(.Object[j])$cex.ylabel, cex.ylabel)
            text(par('usr')[2], mean(unlist(this.ylim.subplot[j, c('start', 'end')])),
                 this.tname, srt = ylab.angle, adj = ylab.adj, cex = this.cex.ylabel) ### ARG TO ROTATE
        }

    }

    if (is.null(links))
        links = GenomicRanges::GRangesList()

    if (length(links)>0) # draw rearrangement links
    {
                                        # first map rearrangements to various windows>
        win.u = this.windows
        win.u$grl.ix = 1  ##holdover from grangeslist windows
        ##win.u = gr.stripstrand(grl.unlist(windows))
        window.segs.u = do.call(rbind, window.segs)
        window.segs.u$width = window.segs.u$end - window.segs.u$start + 1
        window.segs.xlim = do.call('rbind', lapply(window.segs, function(x) data.frame(start = min(x$start), end = max(x$end))))

        links.u = grl.unlist(links)

        if (any(table(links.u$grl.ix)!=2))
            stop('Links should be GRangesList of range pairs.')

        links.p = grl.pivot(links)

        ## flip strands to conform to connectors convention (- connection to left side and + is connection to right side)
        ## with ra specification convention (- refers to segment to left of breakpoint and + refers to segment to right)
        GenomicRanges::strand(links.p[[1]]) = c("-" = "+", "+" = "-")[as.character(GenomicRanges::strand(links.p[[1]]))]
        GenomicRanges::strand(links.p[[2]]) = c("-" = "+", "+" = "-")[as.character(GenomicRanges::strand(links.p[[2]]))]

        ## find overlaps with windows and calculate their window specific coordinates
        l1 = gr.findoverlaps(links.p[[1]], win.u)
        values(l1) = cbind(as.data.frame(values(l1)), as.data.frame(values(links)[l1$query.id, , drop = FALSE]))
        GenomicRanges::strand(l1) = GenomicRanges::strand(links.p[[1]])[l1$query.id]
        l1$stack.id = win.u$grl.ix[l1$subject.id]
        l1$y.pos = ylim.stacks$xaxis.pos[l1$stack.id]
        l1$x.pos = mapply(function(x,y,z,a) (y-z)*a + x, x = window.segs.u[l1$subject.id,]$start, y = start(l1),
                          z = start(win.u[l1$subject.id]), a = window.segs.u$width[l1$subject.id] / width(win.u)[l1$subject.id])

        l2 = gr.findoverlaps(links.p[[2]], win.u)
        values(l2) = cbind(as.data.frame(values(l2)), as.data.frame(values(links)[l2$query.id, , drop = FALSE]))
        GenomicRanges::strand(l2) = GenomicRanges::strand(links.p[[2]])[l2$query.id]
        l2$stack.id = win.u$grl.ix[l2$subject.id]
        l2$y.pos = ylim.stacks$xaxis.pos[l2$stack.id]
        l2$x.pos = mapply(function(x,y,z,a) (y-z)*a + x, x = window.segs.u[l2$subject.id,]$start, y = start(l2),
                          z = start(win.u[l2$subject.id]), a = window.segs.u$width[l2$subject.id] / width(win.u)[l2$subject.id])


        .fix.l = function(ll) {
            if (!is.null(links.feat)) {
                for (col in names(links.feat)) {
                    if (nrow(links.feat) == 1) {
                        values(ll)[, col] = links.feat[, col]
                    } else {
                        values(ll)[, col] = links.feat[ll$query.id, col]
                    }
                }
            }
            

                                        # set up connectors
            if (is.null(ll$v))
                ll$v = y.gaps[ll$stack.id]/4
            else
                ll$v = y.gaps[ll$stack.id]*ll$v/2

            if (is.null(ll$h))
                ll$h = (window.segs.xlim$end[ll$stack.id] - window.segs.xlim$start[ll$stack.id])/20
            else
                ll$h = (window.segs.xlim$end[ll$stack.id] - window.segs.xlim$start[ll$stack.id])*ll$h

            if (is.null(ll$arrow))
                ll$arrow = TRUE

            if (is.null(ll$cex.arrow))
                ll$cex.arrow = 1

            if (is.null(ll$lwd))
                ll$lwd = 1


            if (is.null(ll$lty))
                ll$lty = 3


            if (is.null(ll$col))
                ll$col = 'red'

            if (is.null(ll$col.arrow))
                ll$col.arrow = ll$col

            return(ll)
        }

        if (length(l1)>0)
            l1 = .fix.l(l1)

        if (length(l2)>0)
            l2 = .fix.l(l2)


        ## now pair up / merge l1 and l2 using query.id as primary key
        if (length(l1)>0 & length(l2)>0)
        {
            pairs = merge(data.frame(l1.id = 1:length(l1), query.id = l1$query.id), data.frame(l2.id = 1:length(l2), query.id = l2$query.id))[, c('l1.id', 'l2.id')]
            l1.paired = GenomicRanges::as.data.frame(l1)[pairs[,1], ]
            l2.paired = GenomicRanges::as.data.frame(l2)[pairs[,2], ]
        }
        else
        {
            l2.paired = l1.paired = data.frame()
            pairs = data.frame()
        }


        ## some l1 and l2 will be unpaired
        l.unpaired = GRanges(seqlengths = GenomeInfoDb::seqlengths(links));
        p1 = p2 = c();
        if (nrow(pairs)>0)
        {
            p1 = pairs[,1]
            p2 = pairs[,2]
        }

        if (length(l1)>0)
            l.unpaired = grbind(l.unpaired, l1[setdiff(1:length(l1), p1)])

        if (length(l2)>0)
            l.unpaired = grbind(l.unpaired, l2[setdiff(1:length(l2), p2)])

        if (length(l.unpaired)>0)
        {
            l.unpaired$v = l.unpaired$v/4
            l.unpaired$h = l.unpaired$h/2
            l.unpaired$col = alpha(l.unpaired$col, 0.5)
            ## unpaired will get a "bridge to nowhere" in the proper direction (eg down)
            l.unpaired$y.pos = ylim.stacks$end[l.unpaired$stack.id]
                                        #                    l.unpaired$y.pos2 = l.unpaired$y.pos + top.gaps[l.unpaired$stack.id]
            l.unpaired$y.pos2 = l.unpaired$y.pos + l.unpaired$v

            connectors(l.unpaired$x.pos, l.unpaired$y.pos, as.character(GenomicRanges::strand(l.unpaired)),
                       l.unpaired$x.pos, l.unpaired$y.pos2,
                       as.character(GenomicRanges::strand(l.unpaired)),
                       v = abs(l.unpaired$v), h = l.unpaired$h, type = 'S',
                       f.arrow = l.unpaired$arrow, b.arrow = l.unpaired$arrow,
                       cex.arrow = 0.2*l.unpaired$cex.arrow,
                       col.arrow = l.unpaired$col.arrow,
                       lwd = l.unpaired$lwd, lty = l.unpaired$lty, col = l.unpaired$col)

            if (!is.null(l.unpaired$label))
            {
                cex = 0.5;
                if (!is.null(l.unpaired$cex.label))
                    cex = l.unpaired$cex.label

                                        #                        text(l.unpaired$x.pos, l.unpaired$y.pos2, l.unpaired$label, adj = c(0.5, 0.5), cex = cex)
                l.unpaired$text.y.pos =l.unpaired$y.pos - (ylim.stacks$end[l.unpaired$stack.id]-ylim.stacks$start[l.unpaired$stack.id])/100
                text(l.unpaired$x.pos, l.unpaired$text.y.pos, l.unpaired$label, adj = c(0.5, 1), cex = cex)
            }
        }

        ## now draw connectors for paired links
        ## pairs on the same level will get a "U" link,
        ## pairs on different levels will get "S" links

        ## fix y distances so that connections go from topmost part of bottom most connection to the bottom most part of
        ## top connection (ie the xaxis position)

        if (nrow(l1.paired)>0)
        {
            l1.paired$bottom = l1.paired$y.pos < l2.paired$y.pos

            if (any(l1.paired$bottom))
                l1.paired$y.pos[l1.paired$bottom] = ylim.stacks$end[l1.paired$stack.id[l1.paired$bottom]]

            if (any(!l1.paired$bottom))
                l2.paired$y.pos[!l1.paired$bottom] = ylim.stacks$end[l2.paired$stack.id[!l1.paired$bottom]]

                                        # also make all c type connections top connections with positive v
            ctype = ifelse(l1.paired$stack.id == l2.paired$stack.id, 'U', 'S')
            l1.paired$y.pos[ctype == 'U'] = ylim.stacks$end[l1.paired$stack.id[ctype == 'U']]
            l2.paired$y.pos[ctype == 'U'] = ylim.stacks$end[l2.paired$stack.id[ctype == 'U']]
            l1.paired$v[ctype == 'U'] = abs(l1.paired$v[ctype == 'U'])

            win.width = diff(par('usr')[1:2])
            l1.paired$v = l1.paired$v * affine.map(abs(l2.paired$x.pos - l1.paired$x.pos)/diff(par('usr')[1:2]), xlim = c(0, 1), ylim = c(0.5, 1)) ## make longer links taller
            connectors(l1.paired$x.pos, l1.paired$y.pos, l1.paired$strand, l2.paired$x.pos, l2.paired$y.pos, l2.paired$strand,
                       v = l1.paired$v, h = l1.paired$h, type = ctype,
                       f.arrow = l1.paired$arrow, b.arrow = l1.paired$arrow, cex.arrow = 0.2*l1.paired$cex.arrow, col.arrow = l1.paired$col.arrow,
                       lwd = l1.paired$lwd, lty = l1.paired$lty, col = l1.paired$col)

            if (!is.null(l1.paired$label))
            {
                cex = 0.5;
                if (!is.null(l1.paired$cex.label))
                    cex = l1.paired$cex.label

                ##                         text(l1.paired$x.pos, l1.paired$y.pos+l1.paired$v/2, l1.paired$label, adj = c(0.5, 0.5), cex = cex)
                ##                         text(l2.paired$x.pos, l2.paired$y.pos+l1.paired$v/2, l2.paired$label, adj = c(0.5, 0.5), cex = cex)

                l1.paired$text.y.pos = l1.paired$y.pos - (ylim.stacks$end[l1.paired$stack.id]-ylim.stacks$start[l1.paired$stack.id])/100
                l2.paired$text.y.pos = l2.paired$y.pos - (ylim.stacks$end[l2.paired$stack.id]-ylim.stacks$start[l2.paired$stack.id])/100

                text(l1.paired$x.pos, l1.paired$text.y.pos, l1.paired$label, adj = c(0.5, 1), cex = cex)
                text(l2.paired$x.pos, l2.paired$text.y.pos, l2.paired$label, adj = c(0.5, 1), cex = cex)
            }
        }

    }
}

oldgt <- get("plot.gTrack", envir = asNamespace("gTrack"))
environment(plot.gTrack) <- environment(oldgt)
attributes(plot.gTrack) <- attributes(oldgt)  # don't know if this is really needed
assignInNamespace("plot.gTrack", plot.gTrack, ns="gTrack")
plot.gTrack = plot.gTrack


draw.grl = function(grl,
                    y = NULL,  # can be either vector of length grl, or data.frame row / list with fields $start and $end
                    # specifying y coordinates to "pack" the granges into (or just length 2 list)
                    # note this is different from ylim, which determines the size of the canvas
                    ylim = NULL,  # if null and y provided, will find range of y and plot
                    ylim.subplot = NULL,
                    ywid = NULL,
                    edges = NULL, ## data.frame specifying edges connecting items of grl, with fields $from $to indexing grl and optional fields $lwd, $col, $lty specifying formatting options for connectors, for gr the from arrow will leave the right side of a + range and left side of a - range, and enter the left side of a + range and right side of a - range.   For grl, from arrows will be drawn from the last range in the "from" list item to the first range in the "to" list item
                    draw.paths = F, # if draw.paths = T will treat grl's as paths / contigs,
                    # connecting intervals joined by arrowed curved arcs using alternate stacking algo (ignoring y information)

                    draw.var = F, # if true, then varbase will be called on grl or unlist(grl)
                    var = NULL, # optional GRangesList of same length as grl specifying variant bases with value cols $type, $varbase (ie output of varbase)
                    # corresponding to each grl item
                    var.col = NULL, # named vector with variant colors to override - valid names are XA, XT, XG, XC, S, D, I
                    var.soft = T, ## whether to draw soft clips for varbase
                    windows,  # windows specifies windows, can have optional meta data features $col and $border
                    win.gap = NULL,
                    stack.gap,
                    min.gapwidth = 1, # only applies if windows is not specified
                    col = NULL, border = NA, # all of these variables can be scalar or vectors of length(grl),
                    # can be over-ridden by values / columns in grl
                    col.backbone = alpha('gray', 0.8),
                    gr.colorfield = NULL, ## values field in the gr from which colors can be mapped
                    gr.colormap = NULL, ## named vector mapping fields in the gr.colorfield to colors, if unspecified brewer.master() will be applied
                    gr.labelfield = NULL, ## field of gr labels to draw.
                    grl.labelfield = NULL, ## field of grl to draw as label
                    leg.params,
                    labels = NULL, # vector of length(grl)
                    labels.suppress = F,
                    labels.suppress.grl = labels.suppress,
                    labels.suppress.gr = labels.suppress,
                    spc.label = 0.05, # number between 0 and 1 indicating spacing of label
                    adj.label = c(0, 1),
                    cex.label = 1,
                    cex.edge.label = cex.label,
                    gr.cex.label = 0.8 * cex.label,
                    gr.srt.label = 0,
                    gr.adj.label = c(0,0.5),
                    new.plot, new.axis,
                    sep.lty = 2,
                    sep.lwd = 1,
                    sep.bg.col = 'gray95',
                    sep.draw = TRUE,
                    y.pad,  # this is the fractional padding to put on top and bottom of ranges if y is specified as $start and $end pair (def was 0.05)
                    xaxis.prefix = '', xaxis.suffix = 'MB', xaxis.unit = 1, xaxis.round = 3,
                    xaxis.interval = 'auto', xaxis.pos = 1,
                    xaxis.pos.label, xaxis.cex.label,
                    xaxis.newline = FALSE,
                    xaxis.chronly = FALSE,
                    xaxis.ticklen = 1,
                    xaxis.width = TRUE,
                    xaxis.label.angle = 0,
                    xaxis.cex.tick = 1,
                    ylim.grid = ylim, # can be also specified directly for plots with multiple axes and/or non-numeric tracks
                    y.grid = NA, # if non NA, then the number specifies the spacing between y grid (drawn from ylim[1] to ylim[2]), can also be named vector mapping grid.tick labels to plot locations
                    ylab = NULL,
                    y.grid.col = alpha('gray', 0.5),
                    y.grid.pretty = 5,
                    y.grid.cex = 1,
                    y.grid.lty = 2,
                    y.grid.lwd = 1,
                    path.col = 'black',
                    path.lwd = 1, 
                    path.col.arrow = path.col,
                    path.cex.arrow = 1,
                    path.stack.y.gap = 1,
                    path.stack.x.gap = 0,
                    path.cex.v = 1,
                    path.cex.h = 1,
                    draw.backbone = NULL,
                    xlim = c(0, 20000), # xlim of canvas
                    points = NA, ## if non NA then will draw a given point with pch style
                    circles = FALSE, ## only one of these should be true, however if multiple are true then they will be interpreted in this order
                    bars = FALSE,
                    y0.bar = NULL,
                    lines = F,
                    angle, # angle of barbs to indicate directionality of ranges
                    verbose=FALSE,
                    triangle=FALSE, # trigger a triangle matrix plot
                    ylim.parent=NULL, ## ylim of the full thing. This is importat for angle preseveration
                    legend.params = list(plot=TRUE),
                    bg.col = NA, ## background of whole plot
                    ylab.las = 3,
                    ylab.adj = c(0.5, 1),
                    ...)
{
    now = Sys.time();
    ## PATCH: we are forgetting about any ylim.subplot settings done above .. WHY?
#    ylim.subplot = NULL
    empty.plot = FALSE

  ## PATCH: last minute defaults
  if (is.na(xaxis.width))
      xaxis.width = TRUE

  if (is.na(xaxis.chronly))
      xaxis.chronly = FALSE

  if (is.na(xaxis.label.angle))
      xaxis.label.angle = 0



  if (length(grl)>0)
  {
    if (is.null(draw.backbone))
      draw.backbone = TRUE


    # if (inherits(grl, 'GappedAlignments'))
    #   grl = ga2gr(grl)

    if ((inherits(grl, 'GRanges')))
    {
      if (!is.null(windows)) ## hack to get over stupid GRanges speed issues when we have a giant GRanges input (eg coverage)
      {
        strand(windows) <- rep("*", length(windows)) ## don't need strand on windows, mess up intersections

        ix <- gUtils::gr.in(grl, windows)
        grl = grl[ix]

        if (!is.null(col))
          if (length(col)!=1)
            col = col[ix]

        if (!is.null(border))
          if (length(border)!=1)
            border = border[ix]

        if (!is.null(ywid))
          if (length(ywid)!=1)
            ywid = ywid[ix]

        if (!is.null(y))
          if (!is.list(y))
            if (length(y)!=1)
              y = y[ix]

        if (!is.null(labels))
          if (!is.list(labels))
            if (length(labels)!=1)
              labels = labels[ix]

        if (!is.null(edges))
          if (nrow(edges)>0 & all(c('from', 'to') %in% colnames(edges)))
          {
            if (data.table::is.data.table(edges))
              edges = as.data.frame(edges)

            ix.i = which(ix)
            edges = edges[edges$from %in% ix.i | edges$to %in% ix.i,];
            edges$from = match(edges$from, ix.i)
            edges$to = match(edges$to, ix.i)
          }
      }
      gr.names = names(grl);
      names(grl) = NULL;

      if (length(grl)>0)
        grl = GenomicRanges::split(grl, 1:length(grl))
      else
        grl = GenomicRanges::GRangesList(grl)

      names(grl) = gr.names;
    }


    if (is.null(labels))
      if (is.null(values(grl)$labels))
        labels = names(grl)
      else
        labels = values(grl)$labels

      # make sure names are unique
      names(grl) = 1:length(grl);

      if (is.na(labels.suppress.grl))
        labels.suppress.grl = FALSE

      if (is.na(labels.suppress.gr))
        labels.suppress.gr = FALSE

      if (is.na(draw.var))
        draw.var = F

      if (is.na(draw.paths))
        draw.paths = F

      if (!is.null(gr.colorfield))
          if (all(is.na(gr.colorfield)))
              gr.colorfield = NULL


      if (!is.null(gr.colormap))
        if (all(is.na(gr.colormap)))
          gr.colormap = NULL
      else if (is.null(names(gr.colormap)))
        gr.colormap = NULL

      if (!is.null(col))
        if (all(is.na(col)))
          col = NULL


    ## postpone further color processing until later
      if (is.null(col))
          if (!is.null(values(grl)$col))
              col = values(grl)$col
          else if (is.null(gr.colorfield) & is.null(gr.colormap))
              col = NA # 'black'
          else
              col = NA

      if (is.na(col.backbone))
        col.backbone = NULL

      if (is.null(col.backbone))
        col.backbone = col

#      if (is.na(border))
#        border = NULL

      if (is.null(border))
          border = NA

      grl.props = cbind(data.frame(group = names(grl), col = col, stringsAsFactors = F), as.data.frame(values(grl)))
      grl.props$border = border;
      grl.props$ywid = ywid;

      if (!is.null(y) & !is.list(y) & length(y)>0) ## if y coordinate is specified for the ranges
      {
        grl.props$y = y;
        bad = is.na(y)
        bad[which(is.infinite(y))] = TRUE
        if (any(bad))
        {
          grl.props = grl.props[!bad, ]
          grl = grl[!bad]

          if (!is.null(labels))
              labels = labels[!bad]

          y = grl.props$y
        }
      }
      if (!is.null(grl.labelfield))
      {
        if (!is.na(grl.labelfield))
            {
                if (grl.labelfield %in% names(grl.props))
                    grl.props$grl.labels = grl.props[, grl.labelfield]
            }
        else if (!is.null(labels))
            grl.props$grl.labels = labels ## use $grl.labels to allow labeling of individual grs
      }
      else if (!is.null(labels))
        if (!is.na(labels[1])) ## will only be null if labels is NULL and names(grl) was NULL
          grl.props$grl.labels = labels ## use $grl.labels to allow labeling of individual grs

      gr = tryCatch(grl.unlist(grl), error = function(e)
      {
          ## ghetto solution if we get GRanges names snafu
          gr = unlist(grl);
          if (length(gr)>0)
              {
                  tmpc = textConnection(names(gr));
                  cat('budget .. \n')
                  gr$grl.ix = read.delim(tmpc, sep = '.', header = F)[,1];
                                        #            gr$grl.iix = levapply(rep(1, length(gr)), gr$grl.ix, FUN = function(x) 1:length(x))
                  gr$grl.iix = data.table::data.table(ix = gr$grl.ix)[, iix := 1:length(ix), by = ix][, iix]
                  close(tmpc)
              }
        return(gr)
      })

      gr$group = grl.props$group[gr$grl.ix]
      gr$group.ord = gr$grl.iix
      gr$first = gr$grl.iix == 1

      last = iix = NULL ## NOTE fix
      if (length(gr)>0)
        gr$last = data.table::data.table(iix = as.numeric(gr$grl.iix), ix = gr$grl.ix)[, last := iix == max(iix), by = ix][, last]

      #          gr$last = levapply(gr$grl.iix, gr$grl.ix, FUN = function(x) x == max(x))

      grl.props$group = as.character(grl.props$group)

      S4Vectors::values(gr) = cbind(as.data.frame(values(gr)),
                                    grl.props[match(values(gr)$group, grl.props$group), setdiff(colnames(grl.props), c(colnames(values(gr)), 'group', 'labels')), drop = FALSE])


  #####
  ## variant drawing
  ####

    if (draw.var & is.null(var) & length(gr)>0)
        {
                                        #        var = bamUtils::varbase(gr[gr.in(gr, windows)], soft = var.soft)
            gix = which(gr.in(gr, windows))
            var = varbase(gr[gix], soft = var.soft)
            if (any(iix <- var$type == 'I'))
                end(var[ix]) = end(var[ix])+1
            names(var) = gix
        }
    else
        gix = NULL


    if (!is.null(var))
        if (inherits(var, 'GRangesList'))
        {
        VAR.COL = get.varcol()

        if (!is.null(var.col))
            VAR.COL[names(var.col)] = var.col;

        names(var) = NULL

        if (!is.null(gix))
            names(var) = gix
        else
            names(var) = 1:length(var)

                                        #        var.group = as.numeric(as.data.frame(var)$element)
        var.gr = grl.unlist(var)
        if (length(var.gr)>0)
            {
                var.group = names(var)[var.gr$grl.ix]
                                        #            var.gr = gr.stripstrand(unlist(var))


                                        # inherit properties from gr

                values(var.gr) = cbind(as.data.frame(values(var.gr)),
                          as.data.frame(values(gr)[as.numeric(var.group), setdiff(names(values(gr)), c('labels'))]))

                if (!is.null(gr.labelfield))
                    if (!is.na(gr.labelfield))
                        values(var.gr)[, gr.labelfield] = NA

                var.gr$col.sig = as.character(var.gr$type);
                xix = var.gr$type == 'X'
                var.gr$col.sig[xix] = paste(var.gr$col.sig[xix], var.gr$varbase[xix], sep = '')
                var.gr$col = VAR.COL[var.gr$col.sig]
                var.gr$border = var.gr$col
                var.gr$first = FALSE
                var.gr$last = FALSE

                if (draw.paths) ## if we are drawing paths, then need to setdiff variant vs non variant parts of edges and re-order
                    {
                        ## remove soft clips
                        var.gr = var.gr[!(var.gr$type %in% c('H',  'S'))]
                        gr2 = gr;
                        GenomicRanges::strand(var.gr) == GenomicRanges::strand(gr)[var.gr$grl.ix]

                        gr$grl.iix = as.numeric(gr$grl.iix)

                        ## now doing some ranges acrobatics to find all the pieces of gr that are not in var.gr
                        ## TODO: speed up, remove awkawardness
                        ir = IRanges::ranges(gr)
                        var.ir = IRanges::ranges(var.gr)
                        tmp.ix = split(1:length(var.gr), var.gr$grl.ix)
                        tmp.l = lapply(names(tmp.ix), function(i) IRanges::disjoin(c(ir[as.numeric(i)], var.ir[tmp.ix[[i]]])))
                        tmp.ogix = rep(as.numeric(names(tmp.ix)), sapply(tmp.l, length))
                        tmp.ir = do.call(c, tmp.l)
                        tmp.gr = GenomicRanges::GRanges(seqnames(gr)[tmp.ogix], tmp.ir, seqlengths = GenomeInfoDb::seqlengths(gr), og.ix = tmp.ogix)
                        tmp.ov = gr.findoverlaps(tmp.gr, var.gr)
                        tmp.ov = tmp.ov[tmp.gr$og.ix[tmp.ov$query.id] == var.gr$grl.ix[tmp.ov$subject.id] ]
                        new.gr = tmp.gr[!(1:length(tmp.gr) %in% tmp.ov$query.id), ] ## only keep the non variant pieces
                        GenomicRanges::strand(new.gr) = GenomicRanges::strand(gr)[new.gr$og.ix]
                        values(new.gr) = cbind(as.data.frame(values(gr)[new.gr$og.ix, ]) , og.ix = new.gr$og.ix)
                        var.gr$og.ix = var.gr$grl.ix
                        GenomicRanges::strand(var.gr) = GenomicRanges::strand(gr)[var.gr$og.ix]
                                        #          var.gr$group = as.numeric(as.character(gr$group[var.gr$og.ix]))

                        new.gr = grbind(new.gr, var.gr, gr[setdiff(1:length(gr), var.gr$grl.ix)])
                        new.gr$grl.iix = as.numeric(gr$grl.iix[new.gr$og.ix])
                        new.ord = mapply(function(x, y, z) if (y[1]) x[order(z)] else rev(x[order(z)]),
                            split(1:length(new.gr), new.gr$og.ix), split(as.logical(GenomicRanges::strand(new.gr)=='+'), new.gr$og.ix), split(start(new.gr), new.gr$og.ix))
                        new.ix = unlist(lapply(new.ord, function(x) ((1:length(x))-1)/length(x)))
                        new.gr$grl.iix[unlist(new.ord)] = new.gr$grl.iix[unlist(new.ord)] + new.ix

                        gr = new.gr[order(new.gr$group, new.gr$grl.iix), ]
                    }
                gr = grbind(gr, var.gr)
            }

        else
        {
          gr = grbind(gr, var.gr)
        }
      }

      if (length(gr)>0)
        names(gr) = 1:length(gr)

      if (is.null(windows)) ## find covered windows in provided grl
      {
        seqlevels(gr) = seqlevels(gr)[seqlevels(gr) %in% as.character(seqnames(gr))]
        windows = as(coverage(gr), 'GRanges');
        windows = windows[values(windows)$score!=0]
        windows = reduce(windows, min.gapwidth = min.gapwidth);
      }

      else if (!is(windows, 'GRanges'))
        if (is(windows, 'GRangesList'))
          windows = unlist(windows)
      else  ## assume it's a seqinfo object or an object that has a seq
        windows = si2gr(windows)

      if (is.null(win.gap))
        win.gap = mean(width(windows))*0.2

      if (sum(as.numeric(width(windows)))==0)
        stop('Windows have width 0')

      if (verbose) {
        print('Before flatmap')
        print(Sys.time() - now)
    }

    ## add 1 bp to end for visualization .. ranges avoids weird width < 0 error
    if (length(gr)>0)
        {
            IRanges::ranges(gr) = IRanges::IRanges(start(gr), pmax(end(gr), pmin(end(gr)+1, GenomeInfoDb::seqlengths(gr)[as.character(seqnames(gr))], na.rm = T), na.rm = T)) ## jeremiah commented
                                        #        end(gr) = pmax(end(gr), pmin(end(gr)+1, seqlengths(gr)[as.character(seqnames(gr))], na.rm = T), na.rm = T)
        }

      suppressWarnings(end(windows) <- end(windows) + 1) ## shift one needed bc gr.flatmap has continuous convention, we have categorical (e.g. 2 bases is width 2, not 1)
      mapped = gr.flatmap(gr, windows, win.gap);

      grl.segs = mapped$grl.segs;
      window.segs = mapped$window.segs;

      if (verbose) {
        print('After flatmap')
        print(Sys.time() - now)
      }

      grl.segs$border = as.character(grl.segs$border)
      grl.segs$col = as.character(grl.segs$col)
      grl.segs$group = as.character(grl.segs$group)
      grl.segs$strand = as.character(grl.segs$strand)

      if (!is.null(gr.labelfield))
        if (!is.na(gr.labelfield))
          if (gr.labelfield %in% names(grl.segs))
            grl.segs$label = grl.segs[, gr.labelfield]

      if (nrow(grl.segs)==0)
      {
        #           warning('No ranges intersecting window');
        #          return()
      }

    if (is.list(y))
    {
        if (all(c('start', 'end') %in% names(y)))
            ylim.subplot = c(y$start[1], y$end[1])
        else
            ylim.subplot = c(y[[1]], y[[length(y)]])
    }

      if (nrow(grl.segs)>0)
      {
        if (!draw.paths)
        {
          if (is.null(y) | !is.null(ylim.subplot))
          {
            pos1  = aggregate(formula = pos1 ~ group, data = grl.segs, FUN = min);
            pos2  = aggregate(formula = pos2 ~ group, data = grl.segs, FUN = max);
            pos1 = structure(pos1[,2], names = pos1[,1]) - round(stack.gap/2);
            pos2 = structure(pos2[,2]-1, names = pos2[,1]) + round(stack.gap/2);

            ix = order(as.numeric(names(pos1)))
            pos1 = pos1[ix]
            pos2 = pos2[ix]

            ## FIX HERE .. avoid using disjointBins
            ## these gymnastics allow us to use disjointBins (which works on IRanges)
            ## without getting integer overflow
            if (max(c(pos1, pos2))>2e9)
            {
              r = range(c(pos1, pos2))
              pos1 = affine.map(pos1, xlim = r, ylim = c(0, 2e9))
              pos2 = affine.map(pos2, xlim = r, ylim = c(0, 2e9))
              ## m = max(c(pos1,pos2));
              ## pos1 = ceiling(pos1/m*2e9)
              ## pos2 = floor(pos2/m*2e9);
            }

            # bin ranges
            y.bin = IRanges::disjointBins(IRanges::IRanges(pos1, pos2))

            m.y.bin = max(y.bin)
            if (is.null(ylim))
              ylim = c(1, m.y.bin) + c(-0.5*m.y.bin, 0.5*m.y.bin)

            ## squeeze y coordinates into provided (or inferred) ylim
            if (!is.null(ylim.subplot))
              tmp.ylim = ylim.subplot
            else
              tmp.ylim = ylim

            ## provide bottom and top padding of y.bin
            y.pad = max(c(0, min(0.49, y.pad)));
            #                    tmp.ylim = tmp.ylim + c(1, -1)*y.pad*diff(tmp.ylim);
            y.pad = pmin(1/(m.y.bin+1)/2, 0.125)
            tmp.ylim = tmp.ylim + c(1, -1)*y.pad*diff(tmp.ylim);

            y = structure(affine.map(y.bin, tmp.ylim), names = names(pos1));

            grl.segs$y = y[grl.segs$group]

          }
          else ## data is numeric, i.e. has some kind of y data
          {

              if (is.null(ylim))
                  if (any(!is.na(y[!is.infinite(y)])))
                  {
                      tmp.ylim = range(y[!is.infinite(y)], na.rm = T)
                      ylim = tmp.ylim + c(-1, 1)*0.2*diff(tmp.ylim) + c(-1, 0.2)*y.pad*diff(tmp.ylim)
                  }
                  else
                      ylim = c(0,10)
          }
        }
        else  ## draw.paths = T -->  will treat each grl as a sequence, which will be joined by connectors
        {
          ix.l = lapply(split(1:nrow(grl.segs), grl.segs$group), function(x) x[order(grl.segs$group.ord[x])])
          grl.segs$y.relbin = NA

          ## we want to layout paths so that we prevent collisions between different paths

          grl.segs$y.relbin[unlist(ix.l)] = unlist(lapply(ix.l, function(ix)
          {
            # find runs where start[i+1]>end[i] and strand[i] == strand[i+1] = '+'
                                        # and end[i+1]<start[i] and strand[i] == strand[i+1] = '-'
            if (length(ix)>1)
            {
              iix = 1:(length(ix)-1)
              concordant = ((grl.segs$pos1[ix[iix+1]] >= grl.segs$pos2[ix[iix]]
                             & grl.segs$strand[ix[iix+1]] != '-' & grl.segs$strand[ix[iix]] != '-') |
                              (grl.segs$pos2[ix[iix+1]] <= grl.segs$pos1[ix[iix]]
                                & grl.segs$strand[ix[iix+1]] == '-' & grl.segs$strand[ix[iix]] == '-'))
              return(c(0, cumsum(!concordant)))
            }
            else
              return(0)
          }))

          contig.lim = data.frame(
            group = names(vaggregate(formula = y.relbin ~ group, data = grl.segs, FUN = max)),
            pos1  = vaggregate(formula = pos1 ~ group, data = grl.segs, FUN = min) - round(stack.gap)/2,
            pos2  = vaggregate(formula = pos2 ~ group, data = grl.segs, FUN = max) + round(stack.gap)/2,
            height = vaggregate(formula = y.relbin ~ group, data = grl.segs, FUN = max)
          );
          contig.lim$width = contig.lim$pos2 - contig.lim$pos1
          contig.lim$y.bin = 0;

          contig.lim = contig.lim[order(-contig.lim$width), ]

          if (nrow(contig.lim)>1)
            for (i in 2:nrow(contig.lim))
            {
              # find lowest level at which there is no clash with this and previously stacked segments
                if (max(c(contig.lim$pos1, contig.lim$pos2))>2e9)
                {
                    m = max(c(contig.lim$pos1,contig.lim$pos2));
                    contig.lim$pos1 = ceiling(contig.lim$pos1/m*2e9)
                    contig.lim$pos2 = floor(contig.lim$pos2/m*2e9);
                }
              ir1 = IRanges::IRanges(contig.lim[1:(i-1), 'pos1'], contig.lim[1:(i-1), 'pos2'])
              ir2 = IRanges::IRanges(contig.lim[i, 'pos1'], contig.lim[i, 'pos2'])
#              clash = which(gUtils::gr.in(ir1, ir2 + path.stack.x.gap))
#              clash = which(gUtils::gr.in(ir1, ir2 + path.stack.x.gap))
              clash = which(ir1 %over% (ir2 + path.stack.x.gap))
              pick = clash[which.max(contig.lim$y.bin[clash] + contig.lim$height[clash])]
              contig.lim$y.bin[i] = c(contig.lim$y.bin[pick] + contig.lim$height[pick] + path.stack.y.gap, 0)[1]
            }

          grl.segs$y.bin = contig.lim$y.bin[match(grl.segs$group, contig.lim$group)] + grl.segs$y.relbin + 1

          m.y.bin = max(grl.segs$y.bin)
          if (is.null(ylim))
            ylim = c(1, m.y.bin) + c(-0.5*m.y.bin, 0.5*m.y.bin)

          ## squeeze y coordinates into provided (or inferred) ylim
          if (!is.null(ylim.subplot))
            tmp.ylim = ylim.subplot
          else
            tmp.ylim = ylim

          ## provide bottom and top padding of y.bin
          #
          y.pad = 1/(m.y.bin+1)/2
          y.pad = pmin(1/(m.y.bin+1)/2, 0.125)
          tmp.ylim = tmp.ylim + c(1, -1)*y.pad*diff(tmp.ylim);

          ## make final y coordinates by squeezing y.bin into tmp.ylim

          if (is.null(grl.segs$y))
            grl.segs$y = NA

          if (all(is.na(grl.segs$y)))
          {
            grl.segs$y = affine.map(grl.segs$y.bin, tmp.ylim)
          }
        }

        if (verbose) {
          print('After agg')
          print(Sys.time() - now)
        }              #        xlim = c(min(window.segs$start), max(window.segs$end));
      }
      else
        empty.plot = TRUE

      #window.segs$end <- window.segs$end + 1 ##debug
      ## now collapse everything to 0, 1 based on windows.segs
      winlim = range(c(window.segs$start, window.segs$end))

      ## xlim here is arbitrary, just needs to be > 1, helps us scale overlayed plots to window.segs boundaries (if many plots with different
      ## windows are drawn on the same canvas
      grl.segs$pos1 = affine.map(grl.segs$pos1, winlim, ylim = xlim)
      grl.segs$pos2 = affine.map(grl.segs$pos2, winlim, ylim = xlim)
      window.segs$start = affine.map(window.segs$start, winlim, ylim = xlim)
      window.segs$end = affine.map(window.segs$end, winlim, ylim = xlim)
  }
  else
    empty.plot = TRUE

  if (empty.plot)
  {
    if (is.null(windows))
      stop('Either nonempty range data or windows must be provided')

    mapped = gr.flatmap(GRanges(), windows, win.gap)
    window.segs = mapped$window.segs
    winlim = range(c(window.segs$start, window.segs$end))
    window.segs$start = affine.map(window.segs$start, winlim, ylim = xlim)
    window.segs$end = affine.map(window.segs$end, winlim, ylim = xlim)
    #        xlim = c(min(window.segs$start), max(window.segs$end));

    if (is.null(ylim))
      ylim = c(0, 1)

    if (is.list(y) & is.null(ylim.subplot))
      if (all(c('start', 'end') %in% names(y)))
        ylim.subplot = c(y$start[1], y$end[1])
    else
      ylim.subplot = c(y[[1]], y[[2]])

    if (is.null(ylim.subplot))
      ylim.subplot = ylim
  }

  # if new plot will add (optional) axes and vertical lines separating windows
  if (new.plot)
  {
    if (verbose) {
      print('Before axis draw')
      print(Sys.time() - now)
    }
    plot.blank(xlim = xlim, ylim = ylim, bg.col = bg.col);
    new.axis = TRUE
  }

  if (is.na(sep.draw))
      sep.draw = FALSE

  if (sep.draw && length(windows)>1)
  {
    ## rect(window.segs$end[1:(nrow(window.segs)-1)], rep(ylim[1], nrow(window.segs)-1),
    ## window.segs$start[2:(nrow(window.segs))], rep(ylim[2], nrow(window.segs)-1), border = 'white', col = sep.col)
    if (any(width(windows)<=0))
      warning('Some windows are width 0')

    sep.loc = c(window.segs$start, window.segs$end);

    if (!is.null(y.grid) && !all(is.na(y.grid)))
      yrange = range(y.grid)
    else if (!is.null(ylim.subplot))
      yrange = ylim.subplot
    else
      yrange = range(grl.segs$y, na.rm = TRUE)

    if (is.null(window.segs$border))
      window.segs$border = 'white'
    sep.x0 = window.segs$start[1:(nrow(window.segs))]
    sep.x1 = window.segs$end[1:(nrow(window.segs))]
    sep.y0 = rep(yrange[1], nrow(window.segs))
    bgcol.l <- as.character(window.segs$col) ## JEREMIAH
    bgborder.l <- as.character(window.segs$border) ## JEREMIAH
                                        #rep(min(xaxis.pos.label, xaxis.pos, yrange[1]), nrow(window.segs))
    sep.y1 = rep(yrange[2], nrow(window.segs))

    rect(sep.x0, sep.y0, sep.x1, sep.y1, border = bgborder.l, col = bgcol.l) ## JEREMIAH added bgcol.l
    
    segments(sep.x0, sep.y0, sep.x0, sep.y1, lty = sep.lty, lwd = sep.lwd)
    segments(sep.x1, sep.y0, sep.x1, sep.y1, lty = sep.lty, lwd = sep.lwd)

##       else
##       {
## #          rect(window.segs$start[1:(nrow(window.segs))], rep(ylim[1], nrow(window.segs)),
##                                         #window.segs$end[1:(nrow(window.segs))], rep(ylim[2], nrow(window.segs)), border = 'white', col = sep.bg.col) ## MARCIN
##  #              window.segs$end[1:(nrow(window.segs))], rep(ylim[2], nrow(window.segs)), border = 'white', col = as.character(window.segs$col)) ## JEREMIAH
## #          abline(v = sep.loc, col = 'gray', lty = sep.lty, lwd = sep.lwd);
      ## }
  }

  if (new.plot || new.axis)
  {
    if (is.null(xaxis.pos)) {
      if (!is.null(ylim.subplot))
        xaxis.pos = ylim.subplot[1]-0.05*diff(ylim.subplot)
      else
        xaxis.pos = ylim[1]+0.12*diff(ylim)
    }

    if (is.null(window.segs$col))
      window.segs$col = sep.bg.col

    if (is.null(xaxis.pos.label)) {
      if (!is.null(ylim.subplot))
        xaxis.pos.label = xaxis.pos - 0.04*diff(ylim.subplot)
      else
        xaxis.pos.label = xaxis.pos - 0.04*diff(ylim)
    }

    if (new.axis)
    {
        nwin = length(windows);

        ## draw the actual x axis
        segments(window.segs$start, rep(xaxis.pos[1], nwin), window.segs$end, rep(xaxis.pos[1], nwin));

        # if (!is.null(xaxis.suffix))
        #   if (is.na(xaxis.suffix) | nchar(xaxis.suffix)==0)
        #     xaxis.suffix = NULL

        draw_x_ticks(xaxis.interval, windows, mapped, winlim, xlim, ylim, xaxis.pos, xaxis.suffix, xaxis.unit, xaxis.cex.tick, xaxis.ticklen, xaxis.round)


          # then (label) text
        newline <- ifelse(xaxis.newline, '\n', '')

        width.text = ''
        if (xaxis.width)
        {
            if (!is.null(xaxis.suffix))
                width.text = paste('(', paste(prettyNum(ifelse(rep(xaxis.unit == 1, length(windows)),
                                                               width(windows), round(width(windows)/xaxis.unit, 2)), big.mark = ','), xaxis.suffix),  ')', sep = '')
            else

                width.text = paste('(', prettyNum(ifelse(rep(xaxis.unit == 1, length(windows)),
                                                         width(windows), round(width(windows)/xaxis.unit, 2)), big.mark = ','),  ')', sep = '')
        }

        begin.text = prettyNum(pmax(floor(1/xaxis.unit),
                                    ifelse(rep(xaxis.unit == 1, length(windows)), start(windows), round(start(windows)/xaxis.unit, xaxis.round))),
                               big.mark = ',')

        end.text = prettyNum(ifelse(rep(xaxis.unit == 1, length(windows)), end(windows),
                                    round(end(windows)/xaxis.unit, xaxis.round)), big.mark = ',')

        if (!xaxis.chronly) {
            text(rowMeans(window.segs[, c('start', 'end')]), rep(xaxis.pos.label, nwin),
                 paste(xaxis.prefix, ' ',  seqnames(windows), ':',newline,
                       begin.text,'-', newline,
                       end.text, ' ', xaxis.suffix, newline, width.text, sep = ''),

                 cex = xaxis.cex.label*0.7, srt = 0, adj = c(0.5, 0), srt=xaxis.label.angle)
        } else {
            text(rowMeans(window.segs[, c('start', 'end')]), rep(xaxis.pos.label, nwin),
                 paste(xaxis.prefix, ' ',  seqnames(windows),
                       sep = ''),
                 cex = xaxis.cex.label*0.7, srt = 0, adj = c(0.5, 0), srt=xaxis.label.angle)
        }
    }
  }
  
  if (empty.plot)
  {
    if (verbose) {
      print('Returning ..')
      print(Sys.time() - now)
    }
    return(window.segs)
  }

    line.loc = NULL

  if (!is.na(y.grid[1]))
  {
    if (is.logical(y.grid))
      line.loc = pretty(ylim.grid,y.grid.pretty)
    else if (length(y.grid)==1) ## only interval is specified
      line.loc = seq(floor(ylim.grid[1]/y.grid)*y.grid, ceiling(ylim.grid[2]/y.grid)*y.grid, y.grid)
    else ## specific grid lines are specified
      line.loc = y.grid

    if (is.null(names(line.loc)))
      names(line.loc) = line.loc;

    #        abline(h = line.loc, col = y.grid.col, lty = y.grid.lty, lwd = y.grid.lwd)
    segments(xlim[1], line.loc,  xlim[2], line.loc, col = y.grid.col, lty = y.grid.lty, lwd = y.grid.lwd)

    if (is.null(y.grid.cex))
      y.grid.cex = NA

    if (is.na(y.grid.cex))
      y.grid.cex = 1

    axis(2, at = line.loc, labels = names(line.loc), tick = TRUE, pos = line.loc[1], las = 2, cex.axis = y.grid.cex)

    if (!is.null(ylab))

        mtext(ylab, side = 2, at = mean(c(line.loc[1], line.loc[length(line.loc)])), line = 2, cex.lab = xaxis.cex.label,
              las = ylab.las, adj = ylab.adj[1], padj = ylab.adj[2])
  }

  if (length(grl)>0)
  {
    if (is.null(grl.segs$ywid))
      grl.segs$ywid = 1

    if (any(nix <- is.na(grl.segs$ywid)))
      grl.segs$ywid[nix] = 1

    if (!is.null(ylim.subplot))
      tmp.ydiff = diff(ylim.subplot)*(1-2*y.pad)
    else if (!is.null(line.loc))
      tmp.ydiff = diff(range(line.loc))
    else
      tmp.ydiff = diff(range(grl.segs$y, na.rm = T))

    if (tmp.ydiff==0)
      tmp.ydiff = ylim

    fact = 1.5*(1+length(unique(grl.segs$y)))
    if (!is.null(line.loc))
      fact = pmax(10, pmin(1000, fact))

    ## if ywid not provided then create from tmp.ydiff and then scale to new axis
    if (is.null(ywid) || is.na(ywid))
      {
        ywid = tmp.ydiff / fact
        if (!is.null(line.loc))
          ywid = pmin(ywid, min(c(1, diff(sort(unique(grl.segs$y))))*2)) ## want to be able to see a minimal difference between data points
        
        if (!is.null(line.loc)) ## don't want segments to be fatter than a grid unit
          ywid = pmin(min(diff(line.loc))/2, ywid)        
      }

    grl.segs$ywid  = ywid * grl.segs$ywid


    #########################################
    ## border / color logic finally
    #########################################


    if (is.null(gr.colorfield))
        gr.colorfield = NA

    if (gr.colorfield %in% names(grl.segs))
        {
            if (is.null(gr.colormap))
                {
                    uval = unique(as.character(grl.segs[, gr.colorfield]))
                    gr.colormap = structure(alpha(brewer.master(length(uval)), 0.5), names = uval)
                }

            cols = gr.colormap[as.character(grl.segs[, gr.colorfield])];
            grl.segs$col[!is.na(cols)] = cols[!is.na(cols)]
        }
    else
        {
            if (any(ix <- is.na(grl.segs$col)))
                grl.segs$col[ix] = alpha('black', 0.5)
        }

    ## border if unspecified will be a darker and less transparent version of the color
    if (any(ix <- is.na(grl.segs$border)))
        {
            rgb = col2rgb(grl.segs$col[ix], alpha = TRUE)
            grl.segs$border[ix] = rgb(rgb['red', ]/255, rgb['green', ]/255, rgb['blue', ]/255, alpha = 0.9)
        }

    if (leg.params$plot && length(gr.colormap)>0)
        {
            leg.params$x = leg.params$xpos * diff(xlim) + xlim[1]
            leg.params$y = leg.params$ypos * diff(par('usr')[3:4]) + par('usr')[3]
            leg.params$legend = names(gr.colormap)
            if (circles) {
                leg.params$col = gr.colormap
                leg.params$pch = 16
            }
            else
                leg.params$fill = gr.colormap
            leg.params$border = gr.colormap
            leg.params$xpos = leg.params$ypos = NULL

                                        # if (length(gr.colormap)>legend.maxitems & legend.maxitems > 0)
                                        #   gr.colormap = gr.colormap[intersect(names(sort(table(grl.segs[, gr.colorfield]), decreasing = T)[1:legend.maxitems]),
                                        #     names(gr.colormap))]

            do.call(graphics::legend, leg.params)
                                        #if (circles)
                                        #    graphics::legend(legend.pos[1]*diff(xlim) + xlim[1], legend.pos[2]*diff(par('usr')[3:4]) + par('usr')[3], legend = names(gr.colormap), col = gr.colormap, border = gr.colormap, cex = legend.cex * 0.5, ncol = legend.ncol, xjust = legend.xjust, pch = 16, yjust = legend.yjust)
                                        #else
                                        #    graphics::legend(legend.pos[1]*diff(xlim) + xlim[1], legend.pos[2]*diff(par('usr')[3:4]) + par('usr')[3], legend = names(gr.colormap), fill = gr.colormap, border = gr.colormap, cex = legend.cex * 0.5, ncol = legend.ncol, xjust = legend.xjust, yjust = legend.yjust)
        }

    if (draw.paths)
      draw.backbone = FALSE

    if (labels.suppress.gr)
      grl.segs$label = NULL

    if (!is.na(lines))
      if (lines)
        grl.segs = grl.segs[order(grl.segs$pos1), ]

    draw.ranges(grl.segs, y = grl.segs$y, group = grl.segs$group, col = grl.segs$col, border = grl.segs$border, ylim = ylim, xlim = xlim, lwd = grl.segs$ywid, draw.backbone = draw.backbone, angle = angle, col.backbone = col.backbone, points = points, circles = circles, bars = bars, y0.bar = y0.bar, lines = lines, cex.label = gr.cex.label, srt.label = gr.srt.label, adj.label = gr.adj.label, ...)

    ## if draw.paths, will now draw connectors
    ##
    if (draw.paths)
    {
      grl.segs = grl.segs[
        order(grl.segs$grl.ix, # walk id
              grl.segs$grl.iix, # walk segment
              ifelse(grl.segs$strand=='+', 1, -1)*grl.segs$start) ## break ties in strand aware manner if walk segment spread across multiple (window) segments
      , ]
      ix.l = split(1:nrow(grl.segs), grl.segs$group)
      grl.segs$ctype = NA;  ## connector type

      # keep track to see if next index is missing --> will allow us to draw dotted connectors for these ..
      # "missing" indices occur when we are focusing on windows and potentially removing some of the
      # ranges in the contig
      grl.segs$next.missing = !((grl.segs$query.id+1) %in% grl.segs$query.id) & !grl.segs$last
      grl.segs$prev.missing = !((grl.segs$query.id-1) %in% grl.segs$query.id) & !grl.segs$first

      if (is.null(grl.segs$is.cycle))
        grl.segs$is.cycle = FALSE;

      connector.args = do.call('rbind', lapply(ix.l, function(ix)
      {
        # find runs where start[i+1]>end[i] and strand[i] == strand[i+1] = '+'
        # and end[i+1]<start[i] and strand[i] == strand[i+1] = '-'
        out = NULL;
        if (length(ix)>1)
        {
          out = data.frame(ix0 = ix[-length(ix)], ix1 = ix[-1], type = 'U', sign = '+', cyclic = F, stringsAsFactors = F);
          discordant = grl.segs$y.bin[ix[-length(ix)]] != grl.segs$y.bin[ix[-1]]
          out$type[discordant & grl.segs$strand[ix[-1]] == grl.segs$strand[ix[-length(ix)]]] = 'S'
          out$type[discordant & grl.segs$strand[ix[-1]] != grl.segs$strand[ix[-length(ix)]]] = 'S'
        }

        if (grl.segs$next.missing[ix[length(ix)]]) ## make bridge to nowhere
          out = rbind(out, data.frame(ix0 = ix[length(ix)], ix1 = NA, type = 'S', cyclic = F,
                                      sign = grl.segs$strand[ix[length(ix)]], stringsAsFactors = F))
        if (grl.segs$prev.missing[ix[1]]) ## make bridge from nowhere
          out = rbind(out, data.frame(ix0 = NA, ix1 = ix[1], type = 'S', cyclic = F,
                                      sign = grl.segs$strand[ix[1]], stringsAsFactors = F))

        if (grl.segs$is.cycle[ix[length(ix)]])
        {
          if (grl.segs$y.bin[ix[length(ix)]] == grl.segs$y.bin[ix[1]])
            out = rbind(out, data.frame(ix0 = ix[length(ix)], ix1 = ix[1], type = 'U', cyclic = T, sign = '-', stringsAsFactors = F))
          else if (grl.segs$strand[ix[length(ix)]] == '-' & grl.segs$strand[ix[1]] == '+')
            out = rbind(out, data.frame(ix0 = ix[length(ix)], ix1 = ix[1], type = 'S', cyclic = T, sign = '+', stringsAsFactors = F))
          else if (grl.segs$strand[ix[length(ix)]] == '+' & grl.segs$strand[ix[1]] == '-')
            out = rbind(out, data.frame(ix0 = ix[length(ix)], ix1 = ix[1], type = 'S', cyclic = T, sign = '-', stringsAsFactors = F))
          else if (grl.segs$strand[ix[length(ix)]] == '-' & grl.segs$strand[ix[1]] == '-')
            out = rbind(out, data.frame(ix0 = ix[length(ix)], ix1 = ix[1], type = 'S', cyclic = T, sign = '+', stringsAsFactors = F))
          else if (grl.segs$strand[ix[length(ix)]] == '+' & grl.segs$strand[ix[1]] == '+')
            out = rbind(out, data.frame(ix0 = ix[length(ix)], ix1 = ix[1], type = 'S', cyclic = T, sign = '-', stringsAsFactors = F))
        }

        return(out)
      }))

      if (!is.null(connector.args))
      {
        path.h = path.cex.h * rep(diff(par('usr')[1:2])/50, nrow(connector.args))
        if (any(connector.args$type == 'S'))
          path.h[connector.args$type == 'S'] = 2*path.h[connector.args$type == 'S']

        path.v = rep(path.cex.v, nrow(connector.args))
        path.v[is.na(connector.args$ix0) | is.na(connector.args$ix1)] = path.cex.v*2*grl.segs$ywid[connector.args$ix0[is.na(connector.args$ix0) | is.na(connector.args$ix1)]]
#        path.v[is.na(connector.args$ix0) | is.na(connector.args$ix1)] = path.cex.v*2*grl.segs$ywid[connector.args$ix0[is.na(connector.args$ix0) | is.na(connector.args$ix1)]]

        #                path.v[!is.na(connector.args$ix0)] = path.cex.v*2*grl.segs$ywid[connector.args$ix0[!is.na(connector.args$ix0)]]
        #               path.v[!is.na(connector.args$ix1)] = pmax(path.v[!is.na(connector.args$ix1)],
        #                      path.cex.v*2*grl.segs$ywid[connector.args$ix1[!is.na(connector.args$ix1)]])

        connector.args$path.col = path.col
        connector.args$path.lwd = path.lwd

        ## override path.col and path.lwd if provided in grl.segs
        if (!is.null(grl.segs$path.col))
          connector.args$path.col[!is.na(connector.args$ix0)] = grl.segs$path.col[connector.args$ix0[!is.na(connector.args$ix0)]]

        if (!is.null(grl.segs$path.lwd))
          connector.args$path.lwd[!is.na(connector.args$ix0)] = grl.segs$path.lwd[connector.args$ix0[!is.na(connector.args$ix0)]]

        connector.args$strand0[!is.na(connector.args$ix0)] = grl.segs$strand[connector.args$ix0[!is.na(connector.args$ix0)]]
        connector.args$strand1[!is.na(connector.args$ix1)] = grl.segs$strand[connector.args$ix1[!is.na(connector.args$ix1)]]
        connector.args$strand0[is.na(connector.args$ix0)] = grl.segs$strand[connector.args$ix1[is.na(connector.args$ix0)]]
        connector.args$strand1[is.na(connector.args$ix1)] = grl.segs$strand[connector.args$ix0[is.na(connector.args$ix1)]]

        connector.args$strand0 = ifelse(connector.args$strand0 == '*', '+', connector.args$strand0)
        connector.args$strand1 = ifelse(connector.args$strand1 == '*', '+', connector.args$strand1)

        connector.args$x0[!is.na(connector.args$ix0) & connector.args$strand0=='+'] =
          grl.segs$pos2[connector.args$ix0[!is.na(connector.args$ix0) & connector.args$strand0=='+']]
        connector.args$x0[!is.na(connector.args$ix0) & connector.args$strand0=='-'] =
          grl.segs$pos1[connector.args$ix0[!is.na(connector.args$ix0) & connector.args$strand0=='-']]
        connector.args$x1[!is.na(connector.args$ix1) & connector.args$strand1=='+'] =
          grl.segs$pos1[connector.args$ix1[!is.na(connector.args$ix1) & connector.args$strand1=='+']]
        connector.args$x1[!is.na(connector.args$ix1) & connector.args$strand1=='-'] =
          grl.segs$pos2[connector.args$ix1[!is.na(connector.args$ix1) & connector.args$strand1=='-']]

        # bridges from nowhere
        connector.args$x0[is.na(connector.args$ix0) & connector.args$strand1=='+'] =
          grl.segs$pos1[connector.args$ix1[is.na(connector.args$ix0) & connector.args$strand1=='+']] - path.h[is.na(connector.args$ix0) & connector.args$strand1=='+']
        connector.args$x0[is.na(connector.args$ix0) & connector.args$strand1=='-'] =
          grl.segs$pos2[connector.args$ix1[is.na(connector.args$ix0) & connector.args$strand1=='-']] + path.h[is.na(connector.args$ix0) & connector.args$strand1=='-']

        # bridges to nowhere
        connector.args$x1[is.na(connector.args$ix1) & connector.args$strand0=='+'] =
          grl.segs$pos2[connector.args$ix0[is.na(connector.args$ix1) & connector.args$strand0=='+']] + path.h[is.na(connector.args$ix1) & connector.args$strand0=='+']
        connector.args$x1[is.na(connector.args$ix1) & connector.args$strand0=='-'] =
          grl.segs$pos1[connector.args$ix0[is.na(connector.args$ix1) & connector.args$strand0=='-']] - path.h[is.na(connector.args$ix1) & connector.args$strand0=='-']

        connector.args$y0[!is.na(connector.args$ix0)] = grl.segs$y[connector.args$ix0[!is.na(connector.args$ix0)]]
        connector.args$y1[!is.na(connector.args$ix1)] = grl.segs$y[connector.args$ix1[!is.na(connector.args$ix1)]]
        connector.args$y0[is.na(connector.args$ix0)] = grl.segs$y[connector.args$ix1[is.na(connector.args$ix0)]] - 0.25*path.v[is.na(connector.args$ix0)]
        connector.args$y1[is.na(connector.args$ix1)] = grl.segs$y[connector.args$ix0[is.na(connector.args$ix1)]] + 0.25*path.v[is.na(connector.args$ix1)]

        connector.args$lty = 1;

        connector.args$lty[grl.segs$next.missing[connector.args$ix0[!is.na(connector.args$ix0)]] &
                             !connector.args$cyclic[!is.na(connector.args$ix0)]] = 3
        connector.args$lty[grl.segs$prev.missing[connector.args$ix1[!is.na(connector.args$ix1)]] &
                             connector.args$cyclic[!is.na(connector.args$ix1)]] = 3
        connector.args$lty[is.na(connector.args$ix0) | is.na(connector.args$ix1)] = 3; ## label all bridge to / from nowhere with dotted line

        ##
        path.v[connector.args$y0 == connector.args$y1] = 0
        path.h[connector.args$y0 == connector.args$y1] = 0
        path.v[connector.args$cyclic] = path.cex.v*2*grl.segs$ywid[connector.args$ix1[connector.args$cyclic]]
        path.h[connector.args$cyclic] = path.cex.h * diff(par('usr')[1:2])/20

        ## workaround current lines() limitation in connectors
        if (any(lty3 <- connector.args$lty == 3))
          connectors(connector.args$x0[lty3], connector.args$y0[lty3], connector.args$strand0[lty3],
                     connector.args$x1[lty3], connector.args$y1[lty3], ifelse(connector.args$strand1[lty3] == '+', '-', '+'),
                     type = connector.args$type[lty3],
                     h = path.h[lty3], v = path.v[lty3],
                     lwd = connector.args$path.lwd[lty3],
                     lty = connector.args$lty[lty3],
                     col = connector.args$path.col[lty3],
                     col.arrow = path.col.arrow,
                     cex.arrow = grl.segs$ywid[1]*path.cex.arrow, f.arrow = T)

        if (any(lty1 <- connector.args$lty == 1))
          connectors(connector.args$x0[lty1], connector.args$y0[lty1], connector.args$strand0[lty1],
                     connector.args$x1[lty1], connector.args$y1[lty1], ifelse(connector.args$strand1[lty1] == '+', '-', '+'),
                     type = connector.args$type[lty1],
                     h = path.h[lty1], v = path.v[lty1],
                     lty = connector.args$lty[lty1],
                     lwd = connector.args$path.lwd[lty1],
                     col = connector.args$path.col[lty1],                     
                     col.arrow = path.col.arrow,
                     cex.arrow = grl.segs$ywid[1]*path.cex.arrow, f.arrow = T)


        #text(connector.args$x1, connector.args$y1, paste(connector.args$ix0, ' (', grl.segs$group[connector.args$ix0], '), ', connector.args$ix1, ' (', grl.segs$group[connector.args$ix1], '), ', connector.args$type, ' ', connector.args$sign, sep = ''))
        #            text(connector.args$x1, connector.args$y1-path.v, paste(grl.segs$group[connector.args$ix0], connector.args$type, ' ', connector.args$sign, sep = ''), )
      }
    }

    if (!is.null(edges))
      if (nrow(edges)>0 & all(c('from', 'to') %in% colnames(edges)))
      {
        if (data.table::is.data.table(edges))
          edges = as.data.frame(edges)

        if (is.null(edges$col))
          edges$col = NA

        if (is.null(edges$lty))
          edges$lty = NA

        if (is.null(edges$lwd))
          edges$lwd = NA

        if (is.null(edges$h))
          edges$h = 1

        if (is.null(edges$cex.arrow))
          edges$cex.arrow = 1

        edges.og = edges
        edges = edges[edges$to %in% grl.segs$group | edges$from %in% grl.segs$group, ]

        if (nrow(edges)>0)
        {
          if (any(ix <- is.na(edges$col)))
            edges$col[ix] = 'black'

          if (any(ix <- is.na(edges$lwd)))
            edges$lwd[ix] = 1

          if (any(ix <- is.na(edges$lty)))
            edges$lty[ix] = 1

          ## now translate $from $to from group to gr id, and choosing last range in group for "from" indices
          ## and first range in group for "to" indices putting NA's when gr not in grl.segs

          first.ix = which(grl.segs$first)
          last.ix = which(grl.segs$last)

          grl.segs$to.gr = grl.segs$from.gr = 1:nrow(grl.segs)
          edges$edge.id = 1:nrow(edges)
          edges = merge(merge(edges, grl.segs[last.ix, c('group', 'from.gr')], by.x = 'from', by.y = 'group', all.x = T),
                        grl.segs[first.ix, c('group', 'to.gr')], by.x = 'to', by.y = 'group', all.x = T)


          #                  edges$to.gr = first.ix[match(edges$to, grl.segs[first.ix, ]$group)]
          #                  edges$from.gr = last.ix[match(edges$from, grl.segs[last.ix, ]$group)]

          ## replicate edges that connect input gr's that have multiple instantiations

          ## figure out which grs are clipped
          grl.segs$clipped.start = !(start(gr)[grl.segs$query.id] == grl.segs$start)
          grl.segs$clipped.end = !(end(gr)[grl.segs$query.id] == grl.segs$end)

          clipped.to = ((grl.segs$clipped.start[edges$to.gr] & grl.segs$strand[edges$to.gr] == '+') |
                          (grl.segs$clipped.end[edges$to.gr] & grl.segs$strand[edges$to.gr] == '-'))
          clipped.from = ((grl.segs$clipped.start[edges$from.gr] & grl.segs$strand[edges$from.gr] == '-') |
                            (grl.segs$clipped.end[edges$from.gr] & grl.segs$strand[edges$from.gr] == '+'))

          clipped.to[is.na(clipped.to)] = F
          clipped.from[is.na(clipped.from)] = F
          ## now determine start and end points based on signs of connectors
          to.ix = !is.na(edges$to.gr) & !clipped.to
          from.ix = !is.na(edges$from.gr) & !clipped.from
          edges$x.pos.from = edges$x.pos.to = edges$y.pos.from = edges$y.pos.to = edges$dir.from = edges$dir.to =  NA

          #                  from.ix = !is.na(edges$fromx.gr)
          #                  to.ix = !is.na(edges$to.gr)

          if (any(to.ix)) ## connect to left end if + and right end if -
          {
            edges$x.pos.to[to.ix] = ifelse(grl.segs$strand[edges$to.gr[to.ix]] != '-',
                                           grl.segs$pos1[edges$to.gr[to.ix]], grl.segs$pos2[edges$to.gr[to.ix]])
            edges$y.pos.to[to.ix] = grl.segs$y[edges$to.gr[to.ix]]
            edges$dir.to[to.ix] = ifelse(grl.segs$strand[edges$to.gr[to.ix]] != '-',
                                         '-', '+')
          }

          if (any(from.ix)) ## connect from right end if + and left end if -
          {
            edges$x.pos.from[from.ix] = ifelse(grl.segs$strand[edges$from.gr[from.ix]] != '-',
                                               grl.segs$pos2[edges$from.gr[from.ix]], grl.segs$pos1[edges$from.gr[from.ix]])
            edges$y.pos.from[from.ix] = grl.segs$y[edges$from.gr[from.ix]]
            edges$dir.from[from.ix] = ifelse(grl.segs$strand[edges$from.gr[from.ix]] != '-',
                                             '+', '-')
          }


          ## now let NA endpoints "dangle"
          dangle.w = diff(par('usr')[1:2])/100
          edges$dangle = F

          if (is.null(edges$dangle.w))
            edges$dangle.w = 1

          edges$dangle.w  = edges$dangle.w * dangle.w

          if (any(!from.ix))
          {
              edges$x.pos.from[!from.ix] =
                  edges$x.pos.to[!from.ix] + ifelse(grl.segs$strand[edges$to.gr[!from.ix]] != '-', -1, 1)*edges$dangle.w[!from.ix]
            edges$y.pos.from[!from.ix] = edges$y.pos.to[!from.ix]
            edges$from.gr[!from.ix] =  edges$from.gr[!from.ix]
            edges$dir.from[!from.ix] =  ifelse(edges$dir.to[!from.ix] != '-', '-', '+')
            edges$dangle[!from.ix] = T
          }

          if (any(!to.ix))
          {
              edges$x.pos.to[!to.ix] =
                  edges$x.pos.from[!to.ix] + ifelse(grl.segs$strand[edges$from.gr[!to.ix]] != '-', 1, -1)*edges$dangle.w[!to.ix]
            edges$y.pos.to[!to.ix] = edges$y.pos.from[!to.ix]
            edges$to.gr[!to.ix] =  edges$to.gr[!to.ix]
            edges$dir.to[!to.ix] =  ifelse(edges$dir.from[!to.ix] != '-', '-', '+')
            edges$dangle[!to.ix] = T
          }

          edges$from.ix = from.ix
          edges$to.ix = to.ix
          edges = edges[order(edges$dangle), ]; ## hack to make sure we prefer non-dangling versions of edges when we have a choice (TODO: MAKE NON HACKY)
          dup = duplicated(cbind(edges$edge.id, edges$from.gr)) | duplicated(cbind(edges$edge.id, edges$to.gr))
          edges = edges[(edges$from.ix | edges$to.ix) & !dup, ]

          if (nrow(edges)>0)
          {

            if (!is.null(ylim.subplot))
              tmp.ydiff = diff(ylim.subplot)*(1-2*y.pad)
            else
              tmp.ydiff = diff(range(grl.segs$y, na.rm = T))

            #                      edges$type = ifelse(edges$y.pos.from == edges$y.pos.to, 'U', 'S')
            uthresh = max(grl.segs$ywid)
            edges$type = ifelse(abs(edges$y.pos.from - edges$y.pos.to) <= uthresh, 'U', 'S')

            edges$f.arrow = T
            edges$b.arrow = F
            if (is.null(edges$v))
              edges$v = 1


            edges$v = ifelse(abs(edges$y.pos.from - edges$y.pos.to)<=uthresh,
                             2*uthresh*affine.map(abs(edges$x.pos.to-edges$x.pos.from), xlim = c(0, diff(par('usr')[1:2])), ylim = c(0.5, 1.0)),
                             abs(edges$y.pos.from-edges$y.pos.to)/2)*edges$v
            #                  edges$v[is.na(edges$v)] = 0
            edges$h = dangle.w*edges$h * affine.map(abs(edges$x.pos.to-edges$x.pos.from), xlim = c(0, diff(par('usr')[1:2])), ylim = c(0.5, 1.0))
            make.flat = edges$type == 'U' & edges$dir.to != edges$dir.from

            if (!is.null(edges$not.flat)) ## allow edges specified as $not.flat to have height, unless dangling
              if (length(which(ix <- edges$not.flat & !edges$dangle))>0)
                make.flat[ix] = F

            if (any( ix <- make.flat ))
            {
              edges$v[ix] = 0
              edges$h[ix] = 0
            }

            if (!is.null(edges$v.override))
              if (any(ix <- (!is.na(edges$v.override) & edges$to.ix & edges$from.ix)))
                edges$v[ix] = edges$v.override[ix]

            if (!is.null(edges$h.override))
              if (any(ix <- (!is.na(edges$h.override) & edges$to.ix & edges$from.ix)))
                edges$h[ix] = dangle.w*edges$h.override[ix]


            connectors(edges$x.pos.from, edges$y.pos.from, edges$dir.from,
                       edges$x.pos.to, edges$y.pos.to, edges$dir.to,
                       v = edges$v, h = edges$h, type = edges$type,
                       f.arrow = T, b.arrow = F,
                       cex.arrow = 0.2*edges$cex.arrow,
                       col.arrow = edges$col,
                       lwd = edges$lwd, lty = edges$lty, col = edges$col)

            if (!is.null(edges$label))
            {
              ## text((edges$x.pos.from + edges$x.pos.to)/2, edges$y.pos.from + 0.5*edges$v*sign(edges$y.pos.to - edges$y.pos.from + 0.01),
              ##      edges$label, adj = c(0.5, 0.5), col = 'black', cex = cex.edge.label)
              text((edges$x.pos.from + edges$x.pos.to)/2, edges$y.pos.from + 0.5*edges$v*sign(edges$y.pos.to - edges$y.pos.from + 0.01),
                   edges$label, adj = c(0.5, -0.25), col = 'black', cex = cex.edge.label)
            }
          }
        }
      }

    if (verbose) {
      print('After draw')
      print(Sys.time() - now)
    }

    if (nrow(grl.segs)>0 & !is.null(grl.props$grl.labels) & !labels.suppress.grl)
    {
      if (!draw.paths)
     {
        pos1  = vaggregate(formula = pos1 ~ group, data = grl.segs, FUN = min);
        pos2  = vaggregate(formula = pos2 ~ group, data = grl.segs, FUN = max);
        ywid  = vaggregate(formula = ywid ~ group, data = grl.segs, FUN = max);
        y  = vaggregate(formula = y ~ group, data = grl.segs, FUN = mean);
        grl.segs.u = data.frame(group = names(pos1), pos1, pos2, y, ywid);
        grl.segs.u$grl.labels = grl.props$grl.labels[match(grl.segs.u$group, grl.props$group)]
        rownames(grl.segs.u) = grl.segs.u$group;

        if (adj.label[2] == 0)
          text(grl.segs.u$pos1, grl.segs.u$y+grl.segs.u$ywid + 0.005*diff(ylim),
               grl.segs.u$grl.labels, adj = adj.label, cex = cex.label)
        if  (adj.label[2] == 0.5)
          text(grl.segs.u$pos1-0.005*diff(xlim), grl.segs.u$y,
               grl.segs.u$grl.labels, adj = adj.label, cex = cex.label)
        else
          text(grl.segs.u$pos1, grl.segs.u$y-grl.segs.u$ywid - 0.005*diff(ylim),
               grl.segs.u$grl.labels, adj = adj.label, cex = cex.label)
      }
      else
      {
        pos1  = vaggregate(formula = pos1 ~ group, data = grl.segs, FUN = min);
        pos2  = vaggregate(formula = pos2 ~ group, data = grl.segs, FUN = max);
        y0  = vaggregate(formula = y ~ group, data = grl.segs, FUN = min);
        y1  = vaggregate(formula = y ~ group, data = grl.segs, FUN = max);
        grl.segs.u = data.frame(group = names(pos1), pos1, pos2, y0, y1);
        grl.segs.u$grl.labels = grl.props$grl.labels[match(grl.segs.u$group, grl.props$group)]

        text(adj.label[1]*grl.segs.u$pos1 + (1-adj.label[1])*grl.segs.u$pos2, adj.label[2]*grl.segs.u$y0 + (1-adj.label[2])*grl.segs.u$y1,
             grl.segs.u$grl.labels, adj = adj.label, cex = cex.label)

      }

    }
  }

  return(window.segs)
}

olddg <- get("draw.grl", envir = asNamespace("gTrack"))
environment(draw.grl) <- environment(olddg)
attributes(draw.grl) <- attributes(olddg)  # don't know if this is really needed
assignInNamespace("draw.grl", draw.grl, ns="gTrack")
draw.grl = draw.grl
