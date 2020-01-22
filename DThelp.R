### DATA TABLE HELPERS

fullSD = function(.SD, .BY)
{
    cbind(copy(.SD), data.table(t(unlist(.BY))))
}

fullSD2 = function(p_env = parent.env(environment())) {
    expr = expression(cbind(.SD, data.table(t(unlist(.BY)))))
    eval(expr, envir = parent.env(environment()))
}



v = function (expr) {
    base::eval(expr, sys.frame(4), sys.frame(1))
}



d_evc = function(string_vec, calling_env = parent.frame()) {
    eval(dc(string_vec), envir = calling_env)
}

d_evl = function(string_vec, calling_env = parent.frame()) {
    eval(dl(string_vec), envir = calling_env)
}




dc = function(string_vec, q = T) {
    ez_eval(string_vec, c = T, q = T)
}

dl = function(string_vec, c = F,  q = F) {
    ez_eval(string_vec, c = F, q = F)
}


## allows so if you have a bunch of data table columns to parse
ez_eval = function(string_vec, c = T, list = !c, quotes = T) {
    op_string_c = "c("
    op_string_l = "list("
    if (quotes) {
        q  = "\""
    }
    else
        q = NULL
    ## cat('current frame is', sys.nframe(), "\n")
    ## cat('parent frame is', sys.parent(), "\n")
    c_cmd = parse(text = (paste0(op_string_c, paste0(q, string_vec, q, collapse = ", "), ")")))
    list_cmd = parse(text = (paste0(op_string_l, paste0(q, string_vec, q, collapse = ", "), ")")))
    lst_args = as.list(match.call())
    c_arg = eval(lst_args$c)
    l_arg = eval(lst_args$list)
    c_arg_cond = tryCatch(! is.null(c_arg) & c_arg, error = function(e) FALSE)
    l_arg_cond = tryCatch(! is.null(l_arg) & l_arg, error = function(e) FALSE)
    if (length(l_arg_cond) == 0) {
        l_arg_cond = FALSE
    }
    ## if (is.null(l_arg)) {
    ##     if (is.null(c_arg)) {
    ##         is_c = TRUE
    ##         is_l = FALSE
    ##     } else {
    ##         if (c_arg) {
    ##             is_c = TRUE
    ##             is_l = FALSE
    ##         } else {
    ##             is_c = FALSE
    ##             is_l = TRUE
    ##         }
    ##     }
    ## } else {
    ##     if (l_arg) {
    ##         is_l = TRUE
    ##     }
    ##     if (!is.null(c_arg)) {
    ##         if (c_arg) {
    ##             is_c = TRUE
    ##         } else {
    ##             is_c = FALSE
    ##         }
    ##     } else {
    ##         is_c = FALSE
    ##     }
    ## }
    ## if (is_c) {
    ##     if (is_l) {
    ##         (c_cmd)
    ##         (list_cmd)
    ##     }
    ##     c_cmd
    ## } else if (is_l) {
    ##     (list_cmd)
    ## }

    ## browser()
    if (all(is.null(c(c_arg, l_arg))))
        (c_cmd)
    else if(c_arg_cond & l_arg_cond) {
        (c_cmd)
        (list_cmd)
    }
    else if (l_arg_cond)
        (list_cmd)
    else if (is.null(c_arg) & ! l_arg_cond)
        (c_cmd)
    else if (! c_arg_cond) {
        ## eval((list_cmd), parent.frame(1), parent.frame(2))
        (list_cmd)
    }
    else
        (c_cmd)
}


ez_string = function(string_vec, c = T, list = !c, quotes = T, ws = "\n") {
    ws = paste0(",", ws)
    op_string_c = "c("
    op_string_l = "list("
    if (quotes) {
        q = "\""
    }
    else
        q = NULL
    c_cmd = expression(cat(paste0(op_string_c, paste0(q, string_vec, q, collapse = ws), ")\n")))
    list_cmd = expression(cat(paste0(op_string_l, paste0(q, string_vec, q, collapse = ws), ")\n")))
    lst_args = as.list(match.call())
    c_arg = eval(lst_args$c)
    l_arg = eval(lst_args$list)
    c_arg_cond = tryCatch(! is.null(c_arg) & c_arg, error = function(e) FALSE)
    l_arg_cond = tryCatch(! is.null(l_arg) & l_arg, error = function(e) FALSE)
    if (all(is.null(c(c_arg, l_arg))))
        eval(c_cmd)
    else if(c_arg_cond & l_arg_cond) {
        eval(c_cmd)
        eval(list_cmd)
    }
    else if (l_arg_cond)
        eval(list_cmd)
    else if (is.null(c_arg) & ! l_arg_cond)
        eval(c_cmd)
    else if (! c_arg_cond)
        eval(list_cmd)
    else
        eval(c_cmd)
}


#' Data table merging wrappers

### replace
merge.repl = function(dt.x,
                      dt.y,
                      replace_in_x = TRUE,
                      suffix = NULL,
                      sep = "_",
                      replace_NA = TRUE,
                      force_y = TRUE,
                      overwrite_x = FALSE,
                      keep_order = FALSE,
                      ...)
{
    is_forcats = require(forcats)
    if (!is_forcats) {
        warning("forcats package not installed, if variables are factors, this merge operation will return error")
    }
    arg_lst = as.list(match.call())
    by.y = eval(arg_lst$by.y)
    by.x = eval(arg_lst$by.x)
    by = eval(arg_lst$by)
    all.x = eval(arg_lst$all.x)
    all.y = eval(arg_lst$all.y)
    all = eval(arg_lst$all)
    allow.cartesian = eval(arg_lst$allow.cartesian)
    key_x = key(dt.x)
    if (is.null(all.x)) {
        all.x = TRUE
    }
    if (is.null(all.y)) {
        all.y = FALSE
    }
    if (!is.null(all) && all) {
        all.y = TRUE
        all.x = TRUE
    }
    if (is.null(allow.cartesian)) {
        allow.cartesian = FALSE
    }
    if (!inherits(dt.x, "data.table")) {
        dt.x = as.data.table(dt.x)
    }
    if (!inherits(dt.y, "data.table")) {
        dt.y = as.data.table(dt.y)
    }
    ## data.table::set(dt.x, j = "tmp.2345098712340987", value = seq_len(nrow(dt.x)))
    ## data.table::set(dt.x, j = "in.x.2345098712340987", value = TRUE)
    ## data.table::set(dt.y, j = "in.y.2345098712340987", value = TRUE)
    if (keep_order == TRUE) {
        dt.x$tmp.2345098712340987 = seq_len(nrow(dt.x))
    }
    dt.x$in.x.2345098712340987 = TRUE
    dt.y$in.y.2345098712340987 = TRUE
    new_ddd_args = list(by = by, by.x = by.x, by.y = by.y, all.x = all.x, all.y = all.y, allow.cartesian = allow.cartesian)
    if (is.null(by.y) & is.null(by.x) & is.null(by)) {
        k.x = key(dt.x)
        k.y = key(dt.y)
        if (is.null(k.x) | is.null(k.y) || (k.x != k.y)) {
            stop("neither by.x/by.y  nor by are supplied, keys of dt.x and dt.y must be identical and non NULL")
        }
        x.cols = setdiff(names(dt.x), k.x)
        y.cols = setdiff(names(dt.y), k.y)
    } else if (!is.null(by.x) & !is.null(by.y)) {
        x.cols = setdiff(names(dt.x), by.x)
        y.cols = setdiff(names(dt.y), by.y)
        new_ddd_args = new_ddd_args[setdiff(names(new_ddd_args), c("by"))]
    } else if (!is.null(by)) {
        x.cols = setdiff(names(dt.x), by)
        y.cols = setdiff(names(dt.y), by)
        ## if (length(x.cols) == 0 | length(y.cols) == 0) {
        if (! all(by %in% colnames(dt.x)) | ! all(by %in% colnames(dt.y))) {
            stop("column ", by, " does not exist in one of the tables supplied \nCheck the column names")
        }
        new_ddd_args = new_ddd_args[setdiff(names(new_ddd_args), c("by.y", "by.x"))]

    }
    these_cols = intersect(x.cols, y.cols)
    if (replace_in_x) {
        if (!replace_NA) {
            ## dt.x.tmp = copy(dt.x)[, eval(dc(these_cols)) := NULL]
            dt.x.tmp = copy(dt.x)
            for (this_col in these_cols) {
                data.table::set(dt.x.tmp, i = NULL, j = this_col, value = NULL)
            }
            ## dt.repl = merge(dt.x.tmp, dt.y, all.x = all.x, ...)
            dt.repl = do.call("merge", args = c(list(x = dt.x.tmp, y = dt.y), new_ddd_args))
            dt_na2false(dt.repl, c("in.x.2345098712340987", "in.y.2345098712340987"))
        } else {
            ## dt.repl = merge(dt.x, dt.y, all.x = all.x, ...)
            dt.repl = do.call("merge", args = c(list(x = dt.x, y = dt.y), new_ddd_args))
            dt_na2false(dt.repl, c("in.x.2345098712340987", "in.y.2345098712340987"))
            this_env = environment()
            for (this_col in these_cols) {
                x_cname = paste0(this_col, ".x")
                y_cname = paste0(this_col, ".y")
                ## x_col = as.data.frame(dt.repl)[, x_cname]
                x_col = dt.repl[[x_cname]]
                ## y_col = as.data.frame(dt.repl)[, y_cname]
                y_col = dt.repl[[y_cname]]
                if (force_y) {
                    if (!overwrite_x) {
                        if (inherits(x_col, "factor") & inherits(y_col, "factor")) {
                            new_col = factor(y_col, forcats::lvls_union(list(y_col, x_col)))
                            new_col[is.na(new_col)] = x_col[is.na(new_col)]
                        } else {
                            new_col = ifelse(!is.na(y_col), y_col, x_col)
                        }
                    } else {
                        if (inherits(x_col, "factor") & inherits(y_col, "factor")) {
                            new_col = factor(x_col, forcats::lvls_union(list(y_col, x_col)))
                        } else {
                            new_col = x_col
                        }
                        new_col[dt.repl$in.y.2345098712340987] = y_col[dt.repl$in.y.2345098712340987]
                        ## new_col = y_col
                    }
                } else {
                    if (inherits(x_col, "factor") & inherits(y_col, "factor")) {
                        new_col = factor(x_col, forcats::lvls_union(list(x_col, y_col)))
                        new_col[is.na(new_col) & !is.na(y_col)] = y_col[is.na(new_col) & !is.na(y_col)]
                    } else {
                        new_col = ifelse(is.na(x_col) & !is.na(y_col), y_col, x_col)
                    }
                }
                ## dt.repl[, eval(dc(c(x_cname, y_cname))) := NULL]
                data.table::set(dt.repl, j = c(x_cname, y_cname, this_col), value = list(NULL, NULL, this_env[["new_col"]]))
            }
            ## lapply(these_cols, function(this_col)
            ## {
            ##     this_env = environment()
            ##     x_cname = paste0(this_col, ".x")
            ##     y_cname = paste0(this_col, ".y")
            ##     ## x_col = as.data.frame(dt.repl)[, x_cname]
            ##     x_col = dt.repl[[x_cname]]
            ##     ## y_col = as.data.frame(dt.repl)[, y_cname]
            ##     y_col = dt.repl[[y_cname]]
            ##     if (force_y) {
            ##         if (!overwrite_x) {
            ##             if (inherits(x_col, "factor") & inherits(y_col, "factor")) {
            ##                 new_col = factor(y_col, forcats::lvls_union(list(y_col, x_col)))
            ##                 new_col[is.na(new_col)] = x_col[is.na(new_col)]
            ##             } else {
            ##                 new_col = ifelse(!is.na(y_col), y_col, x_col)
            ##             }
            ##         } else {
            ##             new_col = x_col
            ##             new_col[dt.repl$in.y.2345098712340987] = y_col[dt.repl$in.y.2345098712340987]
            ##             ## new_col = y_col
            ##         }
            ##     } else {
            ##         if (inherits(x_col, "factor") & inherits(y_col, "factor")) {
            ##             new_col = factor(x_col, forcats::lvls_union(list(x_col, y_col)))
            ##             new_col[is.na(new_col) & !is.na(y_col)] = y_col[is.na(new_col) & !is.na(y_col)]
            ##         } else {
            ##             new_col = ifelse(is.na(x_col) & !is.na(y_col), y_col, x_col)
            ##         }
            ##     }
            ##     ## dt.repl[, eval(dc(c(x_cname, y_cname))) := NULL]
            ##     data.table::set(dt.repl, j = c(x_cname, y_cname, this_col), value = list(NULL, NULL, this_env[["new_col"]]))
            ##     return(NULL)
            ##     ## dt.repl[, eval(dc(this_col)) := new_col]
            ## })
            ## dt.x.tmp = copy(dt.x)[, eval(dc(these_cols)) := NULL]
            ## lapply(these_cols, function(this_col)
            ## {
            ##     eval_x = paste0("x = ", this_col)
            ##     ## tmp_x = copy(dt.x)[, eval(dl(eval_x))]
            ##     eval_y = paste0("y = ", this_col)
            ##     ## tmp_y = copy(dt.y)[, eval(dl(eval_y))]
            ##     tmp = cbind(copy(dt.x)[, eval(dl(eval_x))],
            ##                 copy(dt.y)[, eval(dl(eval_y))])
            ##     new_val = tmp[, ifelse(is.na(x) & !is.na(y),
            ##                  y,
            ##                  x)]
            ##     dt.x.tmp[, eval(dc(this_col)) := new_val]
            ## })
        }
    } else if (!replace_in_x & !is.null(suffix)) {
        y.suff.cols = paste0(y.cols, sep, suffix)
        ## dt.y.tmp = copy(dt.y)[, eval(dc(y.suff.cols)) := eval(dl(y.cols))][, eval(dc(y.cols)) := NULL]
        dt.y.tmp = copy(dt.y)
        data.table::set(dt.y, j = y.suff.cols, value = dt.y[, y.cols, with = FALSE])
        data.table::set(dt.y, j = y.cols, value = NULL)
        ## dt.repl = merge(dt.x, dt.y.tmp, all.x = TRUE, ...)
        dt.repl = do.call("merge", args = c(list(x = dt.x, y = dt.y.tmp), new_ddd_args))
    }
    if (keep_order == TRUE) {
        data.table::setorderv(dt.repl, "tmp.2345098712340987")
        dt.repl$tmp.2345098712340987 = NULL
    }
    data.table::set(dt.repl, j = c("in.y.2345098712340987", "in.x.2345098712340987"),
                    value = list(NULL, NULL))
    ## dt.repl$in.y.2345098712340987 = NULL
    ## dt.repl$in.x.2345098712340987 = NULL
    invisible(dt.repl)
}

merge.suff = function(dt.x, dt.y, suffix = NULL, replace_in_x = FALSE,  sep = "_") {
    return(merge.repl(dt.x, dt.y, replace_in_x = replace_in_x, suffix = suffix, sep = sep))
}

lg2int = function(dt) {
    these_cols = which(sapply(dt, class) == "logical")
    for (this_col in these_cols) {
        this_val = as.data.frame(dt[, this_col, with = FALSE])[,1]
        data.table::set(dt, j = this_col, value = as.integer(this_val))
    }
    return(dt)
}


dt_lg2int = function(dt) {
    these_cols = which(sapply(dt, class) == "logical")
    for (this_col in these_cols) {
        this_val = as.data.frame(dt[, this_col, with = FALSE])[,1]
        data.table::set(dt, j = this_col, value = as.integer(this_val))
    }
    return(dt)
}


dt_na2false = function(dt, these_cols = NULL) {
    na2false = function(v)
    {
        ## v = ifelse(is.na(v), v, FALSE)
        v[is.na(v)] = FALSE
        as.logical(v)
    }
    if (is.null(these_cols)) {
        these_cols = which(sapply(dt, class) == "logical")
    }
    for (this_col in these_cols) {
        ## this_val = as.data.frame(dt[, this_col, with = FALSE])[,1]
        this_val = dt[[this_col]]
        data.table::set(dt, j = this_col, value = na2false(this_val))
    }
    return(dt)
}


dt_na2true = function(dt, these_cols = NULL) {
    if (is.null(these_cols)) {
        these_cols = which(sapply(dt, class) == "logical")
    }
    for (this_col in these_cols) {
        ## this_val = as.data.frame(dt[, this_col, with = FALSE])[,1]
        this_val = dt[[this_col]]
        data.table::set(dt, j = this_col, value = na2true(this_val))
    }
    return(dt)
}


dt_na2zero = function(dt, these_cols = NULL) {
    if (is.null(these_cols)) {
        these_cols = which(sapply(dt, class) %in% c("numeric", "integer"))
    }
    if (!inherits(dt, "data.table")) {
        setDT(dt)
    }
    for (this_col in these_cols) {
        this_val = dt[[this_col]]
        ## this_val = as.data.frame(dt)[, this_col]
        this_val[is.na(this_val)] = 0
        data.table::set(dt, j = this_col, value = this_val)
        ## dt[, this_col] = this_val
    }
    return(dt)
}


dt_na2empty = function(dt) {
    these_cols = which(sapply(dt, class) == "character")
    for (this_col in these_cols) {
        ## this_val = as.data.frame(dt[, this_col, with = FALSE])[,1]
        this_val = dt[, this_col, with = FALSE][[1]]
        data.table::set(dt, j = this_col, value = na2empty(this_val))
    }
    return(dt)
}

dt_empty2na = function(dt) {
    these_cols = which(sapply(dt, class) == "character")
    for (this_col in these_cols) {
        ## this_val = as.data.frame(dt[, this_col, with = FALSE])[,1]
        this_val = dt[, this_col, with = FALSE][[1]]
        ## browser()
        data.table::set(dt, j = this_col, value = empty2na(this_val))
    }
    return(dt)
}


dt_setnull = function(dt, cols) {
    for (this_col in cols) {
        data.table::set(dt, j = this_col, value = NULL)
    }
    return(dt)
}


dt_setint = function(dt, cols = NULL) {
    if (is.null(cols)) {
        cols = names(dt)[which(sapply(dt, class) %in% c("numeric"))]
    }
    for (this_col in cols) {
        data.table::set(dt, j = this_col, value = as.integer(dt[[this_col]]))
    }
    return(dt)
}



dt_setallna = function(dt, cols = NULL, na_type = NA_integer_) {
    if (is.null(cols)) {
        cols = colnames(dt)
    }
    for (this_col in cols) {
        data.table::set(dt, j = this_col, value = NULL)
        data.table::set(dt, j = this_col, value = na_type)
    }
    return(dt)
}


dt_setchar = function(dt, cols = NULL) {
    if (is.null(cols)) {
        cols = names(dt)[which(!sapply(dt, class) == "character")]
    }
    for (this_col in cols) {
        data.table::set(dt, j = this_col, value = as.character(dt[[this_col]]))
    }
    return(dt)
}


dt_any2lg = function(dt, cols = NULL) {
    if (is.null(cols)) {
        ## cols = names(dt)[which(!unlist(lapply(dt, class)) == "character")]
        cols = colnames(dt)
    }
    for (this_col in cols) {
        data.table::set(dt, j = this_col, value = as.logical(dt[[this_col]]))
    }
    return(dt)
}


dt_f2char = function(dt, cols = NULL) {
    if (is.null(cols)) {
        ## cols = names(dt)[which(!unlist(lapply(dt, class)) == "character")]
        cols = colnames(dt)
    } else {
        cols = names(dt)[which(!unlist(lapply(dt, class)) == "factor")]
    }
    for (this_col in cols) {
        data.table::set(dt, j = this_col, value = as.character(dt[[this_col]]))
    }
    return(dt)
}
