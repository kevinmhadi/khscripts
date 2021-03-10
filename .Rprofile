{
    options(stringsAsFactors = FALSE)
    options(bitmapType="cairo")
    options(device = grDevices::pdf)

    names2 = function(x) {
        nm = names(x)
        if (is.null(nm))
            return(rep_len("", length(x)))
        else
            return(nm)
    }

    `names2<-` = function(x, value, useempty = FALSE) {
        names(x) = if (!is.null(value))
                       rep_len(value, length(x))
                   else {
                       if (useempty)
                           rep_len("", length(x))
                   }
        return(x)
    }

    forceload = function(envir = globalenv(), .force = FALSE) {
        if (!exists("envload_34507213048974")) {
            envload_34507213048974 = new.env(parent = globalenv())
            globasn(envload_34507213048974)
            sesh =  sessionInfo()
            pkgs = c(sesh$basePkgs,
                     names(sesh$otherPkgs),
                     names(sesh$loadedOnly))
            pkvec = rep(FALSE, length(pkgs))
            names(pkvec) = pkgs
            envload_34507213048974$pkvec = pkvec
        }
        force = function(x) x
        ## pkgs = gsub("package:", "", grep('package:', search(), value = TRUE))
        ## pkgs = c(pkgs, names(sessionInfo()$loadedOnly))
        if (!exists("sesh")) sesh =  sessionInfo()
        if (!exists("pkgs")) {
            pkgs = c(sesh$basePkgs,
                     names(sesh$otherPkgs),
                     names(sesh$loadedOnly))
        }
        pkvec = envload_34507213048974$pkvec
        notloaded_firsttime = setdiff(pkgs, names(pkvec))
        pkvec = c(pkvec, setNames(rep_len(FALSE, length(notloaded_firsttime)), notloaded_firsttime))
        if (.force)
            notloaded = names(pkvec)
        else
            notloaded = names(which(pkvec == FALSE))
        if (length(notloaded)) {
            for (pkg in notloaded) {
                tryCatch( {
                    message("force loading ", pkg)
                    invisible(eval(as.list((asNamespace(pkg))), envir = envir))
                    invisible(eval(eapply(asNamespace(pkg), force, all.names = TRUE), envir = envir))
                    pkvec[pkg] = TRUE
                }, error = function(e) message("could not force load ", pkg))
            }
            envload_34507213048974$pkvec = pkvec
        } else {
            message("nothing to forceload")
        }
    }

    forcefun = function(envir = globalenv(), evalenvir = globalenv()) {
        funnames = as.character(lsf.str(envir = envir))
        for (fun in funnames) {
            tryCatch( {
                message("force loading ", fun)
                eval(force(get(fun, envir = envir)), envir = evalenvir)
            }, error = function(e) message("could not force load ", fun))
        }
    }



    relib2 = function(lib = 'Flow', force = TRUE, unload = TRUE)
    {
        suppressMessages(forceload())
        if (sprintf("package:%s", lib) %in% search())
        {
            expr = sprintf("detach(package:%s, force = force, unload = unload)", lib)
            eval(parse(text = expr))
            ## tryCatch(unload(lib), error = function(e) NULL) ## DO NOT use this line...
            ## it will break re-librarying
        }
        txt = sprintf("library2(%s)", lib)
        eval(parse(text = txt))
        suppressMessages(forceload())
    }

    relib3 = function(..., force = TRUE, unload = TRUE)
    {
        if (!exists("envload_34507213048974")) {
            envload_34507213048974 = new.env(parent = globalenv())
            globasn(envload_34507213048974)
            sesh =  sessionInfo()
            pkgs = c(sesh$basePkgs,
                     names(sesh$otherPkgs),
                     names(sesh$loadedOnly))
            pkvec = rep(FALSE, length(pkgs))
            names(pkvec) = pkgs
            envload_34507213048974$pkvec = pkvec
        }
        suppressMessages(forceload(.force = T))
        names2 = function(x) {
            nm = names(x)
            if (is.null(nm))
                return(rep_len("", length(x)))
            else
                return(nm)
        }
        lst.arg = as.list(match.call(expand.dots = F))$`...`
        nm = names2(lst.arg)
        otherarg = lst.arg[nzchar(nm)]
        pkgarg = lst.arg[!nzchar(nm)]
        pkgarg = pkgarg[sapply(pkgarg, function(x) is.call(x) || is.character(x))]
        charvec = as.character(all.vars(match.call()))
        if (length(charvec)) {
            notfound= { set.seed(10); paste0("notfound_", round(runif(1) * 1e9)); }
            vars = mget(charvec, ifnotfound=notfound, mode = "character", inherits = T)
            ## charvec = unlist(strsplit(toString(vars[[1]]), ", "))
            charvec = unique(c(names2(vars[vars == notfound]), unlist(vars[vars != notfound])))
        }
        charvec = c(charvec, unlist(as.vector(sapply(pkgarg,
                                                     function(x) tryCatch(eval(x), error = function(e) NULL)))))
        for (lib in charvec) {
            if (sprintf("package:%s", lib) %in% search())
            {
                expr = sprintf("detach(package:%s, force = force, unload = unload)", lib)
                eval(parse(text = expr))
                ## tryCatch(unload(lib), error = function(e) NULL) ## DO NOT use this line...
                ## it will break re-librarying
            }
            pkvec = envload_34507213048974$pkvec
            if (lib %in% names(pkvec)) {
                pkvec = pkvec[!names(pkvec) %in% lib]
                envload_34507213048974$pkvec = pkvec
            } else {
                pev = packageEvent(lib, "onLoad")
                gh = getHook(pev)
                if (length(gh) == 0 || is.null(gh$forceall12340987)) {
                    setHook(pev,
                            list("forceall12340987" = function(...) forceall(envir = asNamespace(lib))))
                }
            }
            library(lib, character.only = T)
        }
        suppressMessages(forceload(.force = T))
    }

    detach2 = function(lib = "Flow", force = TRUE, unload = TRUE) {
        suppressMessages(forceload())
        if (sprintf("package:%s", lib) %in% search())
        {
            expr = sprintf("detach(package:%s, force = force, unload = unload)", lib)
            suppressMessages(eval(parse(text = expr)))
            tryCatch(unload(lib), error = function(e) NULL)
        }
        suppressMessages(forceload())
    }

    library2 = function(x, ...) {
        suppressMessages(forceload())
        arg = as.list(match.call())[["x"]]
        if (is.symbol(arg)) {
            lib = tryCatch(as.character(eval(arg)), error = function(e) arg)
            if (!is.character(lib)) {
                lib = toString(lib)
            }
        } else {
            lib = x
        }
        library(lib, character.only = T, ...)
        suppressMessages(forceload())
    }

    library3 = function (...)
    {
        suppressMessages(forceload())
        names2 = function(x) {
            nm = names(x)
            if (is.null(nm))
                return(rep_len("", length(x)))
            else
                return(nm)
        }
        suppressMessages(forceload())
        lst.arg = as.list(match.call(expand.dots = F))$`...`
        nm = names2(lst.arg)
        otherarg = lst.arg[nzchar(nm)]
        pkgarg = lst.arg[!nzchar(nm)]
        pkgarg = pkgarg[sapply(pkgarg, function(x) is.call(x) || is.character(x))]
        charvec = as.character(all.vars(match.call()))
        if (length(charvec)) {
            notfound= { set.seed(10); paste0("notfound_", round(runif(1) * 1e9)); }
            vars = mget(charvec, ifnotfound=notfound, mode = "character", inherits = T)
            ## charvec = unlist(strsplit(toString(vars[[1]]), ", "))
            charvec = unique(c(names2(vars[vars == notfound]), unlist(vars[vars != notfound])))
        }
        charvec = c(charvec, unlist(as.vector(sapply(pkgarg,
                                                     function(x) tryCatch(eval(x), error = function(e) NULL)))))
        for (lib in charvec) {
            pev = packageEvent(lib, "onLoad")
            gh = getHook(pev)
            if (length(gh) == 0 || is.null(gh$forceall12340987)) {
                setHook(pev,
                        list("forceall12340987" = function(...) forceall(envir = asNamespace(lib))))
            }
            do.call(library, c(alist(package = lib, character.only = T),
                               otherarg))
        }
        suppressMessages(forceload())
    }

    require3 = function (...)
    {
        suppressMessages(forceload())
        names2 = function(x) {
            nm = names(x)
            if (is.null(nm))
                return(rep_len("", length(x)))
            else
                return(nm)
        }
        suppressMessages(forceload())
        lst.arg = as.list(match.call(expand.dots = F))$`...`
        nm = names2(lst.arg)
        otherarg = lst.arg[nzchar(nm)]
        pkgarg = lst.arg[!nzchar(nm)]
        pkgarg = pkgarg[sapply(pkgarg, function(x) is.call(x) || is.character(x))]
        charvec = as.character(all.vars(match.call()))
        if (length(charvec)) {
            notfound= { set.seed(10); paste0("notfound_", round(runif(1) * 1e9)); }
            vars = mget(charvec, ifnotfound=notfound, mode = "character", inherits = T)
            ## charvec = unlist(strsplit(toString(vars[[1]]), ", "))
            charvec = unique(c(names2(vars[vars == notfound]), unlist(vars[vars != notfound])))
        }
        charvec = c(charvec, unlist(as.vector(sapply(pkgarg,
                                                     function(x) tryCatch(eval(x), error = function(e) NULL)))))
        for (lib in charvec) {
            pev = packageEvent(lib, "onLoad")
            gh = getHook(pev)
            if (length(gh) == 0 || is.null(gh$forceall12340987)) {
                setHook(pev,
                        list("forceall12340987" = function(...) forceall(envir = asNamespace(lib))))
            }
            do.call(require, c(alist(package = lib, character.only = T),
                               otherarg))
        }
        suppressMessages(forceload())
    }


    force2 = function(x)
        tryCatch(x, error = function(e) NULL)


    forceall = function(invisible = TRUE, envir = parent.frame(), evalenvir = parent.frame()) {
        if (!exists("envload_34507213048974")) {
            envload_34507213048974 = new.env(parent = globalenv())
            globasn(envload_34507213048974)
            sesh =  sessionInfo()
            pkgs = c(sesh$basePkgs,
                     names(sesh$otherPkgs),
                     names(sesh$loadedOnly))
            pkvec = rep(FALSE, length(pkgs))
            names(pkvec) = pkgs
            envload_34507213048974$pkvec = pkvec
        }
        pkg = environmentName(envir)
        pkvec = envload_34507213048974$pkvec
        if ( { pkg %in% names(pkvec) && isFALSE(pkvec[pkg]); } ||
             { ! pkg %in% names(pkvec); } ) {
            if (invisible == TRUE)  {
                invisible(eval(as.list(envir), envir = evalenvir))
                invisible(eval(eapply(envir, force, all.names = TRUE), envir = evalenvir))
            } else {
                print(eval(as.list(envir), envir = evalenvir))
                print(eval(eapply(envir, force, all.names = TRUE), envir = evalenvir))
            }
            addon = TRUE
            names(addon) = pkg
            envload_34507213048974$pkvec = c(pkvec, addon)
        } else {
            message("nothing to load")
        }
    }

    overwriteR6 = function (newfun, oldfun, r6gen, meth = "public_methods", package = NULL,
                            envir = globalenv())
    {
        meth = ifelse(grepl("^pub", meth), "public_methods", ifelse(grepl("^pri",
                                                                          meth), "private_methods", ifelse(grepl("^act", meth),
                                                                                                           "active", NA_character_)))
        if (is.na(meth))
            stop("method must refer to public, private, or active method")
        if (!is.null(package)) {
            if (is.character(package))
                envpkg = asNamespace(package)
            else if (isNamespace(package))
                envpkg = package
            nmpkg = environmentName(envpkg)
        }
        r6 = get(r6gen)
        tmpfun = r6[[meth]][[oldfun]]
        .newfun = get(newfun)
        environment(.newfun) = environment(tmpfun)
        attributes(.newfun) = attributes(tmpfun)
        r6[[meth]][[oldfun]] = .newfun
        NULL
    }

    globasn = function (obj, var = NULL, return_obj = TRUE, envir = .GlobalEnv,
                        verbose = TRUE, vareval = F)
    {
        var = as.list(match.call())$var
        if (is.null(var)) {
            globx = as.character(substitute(obj))
        }
        else {
            if (is.name(var)) {
                if (isFALSE(vareval))
                    var = as.character(var)
                else var = eval(var, parent.frame())
            }
            else if (!is.character(var)) {
                stop("var must be coercible to a character")
            }
            if (inherits(var, "character")) {
                globx = var
            }
            else {
                globx = as.character(substitute(var))
            }
        }
        if (verbose)
            message("variable being assigned to ", globx)
        assign(globx, value = obj, envir = envir)
        if (return_obj) {
            invisible(obj)
        }
        else {
            NULL
        }
    }

    overwritefun = function (newfun, oldfun, package, envir = globalenv())
    {
        if (is.character(package))
            envpkg = asNamespace(package)
        else if (isNamespace(package))
            envpkg = package
        nmpkg = environmentName(envpkg)
        tmpfun = get(oldfun, envir = envpkg)
        .newfun = get(newfun, envir = parent.frame())
        environment(.newfun) = environment(tmpfun)
        attributes(.newfun) = attributes(tmpfun)
        eval(asn2(oldfun, .newfun, ns = nmpkg), globalenv())
        globasn(.newfun, oldfun, vareval = T)
    }

    asn2 = function (x, value, ns, pos = -1, envir = as.environment(pos))
    {
        nf <- sys.nframe()
        if (missing(ns)) {
            nm <- attr(envir, "name", exact = TRUE)
            if (is.null(nm) || substr(nm, 1L, 8L) != "package:")
                stop("environment specified is not a package")
            ns <- asNamespace(substring(nm, 9L))
        }
        else ns <- asNamespace(ns)
        ns_name <- getNamespaceName(ns)
        if (bindingIsLocked(x, ns)) {
            in_load <- Sys.getenv("_R_NS_LOAD_")
            if (nzchar(in_load)) {
                if (in_load != ns_name) {
                    msg <- gettextf("changing locked binding for %s in %s whilst loading %s",
                                    sQuote(x), sQuote(ns_name), sQuote(in_load))
                    if (!in_load %in% c("Matrix", "SparseM"))
                        warning(msg, call. = FALSE, domain = NA, immediate. = TRUE)
                }
            }
            else if (nzchar(Sys.getenv("_R_WARN_ON_LOCKED_BINDINGS_"))) {
                warning(gettextf("changing locked binding for %s in %s",
                                 sQuote(x), sQuote(ns_name)), call. = FALSE, domain = NA,
                        immediate. = TRUE)
            }
            unlockBinding(x, ns)
            assign(x, value, envir = ns, inherits = FALSE)
            w <- options("warn")
            on.exit(options(w))
            options(warn = -1)
            lockBinding(x, ns)
        }
        else {
            assign(x, value, envir = ns, inherits = FALSE)
        }
        if (!isBaseNamespace(ns)) {
            S3 <- .getNamespaceInfo(ns, "S3methods")
            if (!length(S3))
                return(invisible(NULL))
            S3names <- S3[, 3L]
            if (x %in% S3names) {
                i <- match(x, S3names)
                genfun <- get(S3[i, 1L], mode = "function", envir = parent.frame())
                if (.isMethodsDispatchOn() && methods::is(genfun,
                                                          "genericFunction"))
                    genfun <- methods::slot(genfun, "default")@methods$ANY
                defenv <- if (typeof(genfun) == "closure")
                              environment(genfun)
                          else .BaseNamespaceEnv
                S3Table <- get(".__S3MethodsTable__.", envir = defenv)
                remappedName <- paste(S3[i, 1L], S3[i, 2L], sep = ".")
                if (exists(remappedName, envir = S3Table, inherits = FALSE))
                    assign(remappedName, value, S3Table)
            }
        }
        invisible(NULL)
    }

    saveRDS = function (object, file = "", ascii = FALSE, version = NULL, compress = TRUE,
                        refhook = NULL) {
        if (is.character(file)) {
            if (file == "")
                stop("'file' must be non-empty string")
            if (!dir.exists(dirname(file)))
                system2("mkdir", c("-p", dirname(file)))
            object <- object
            mode <- if (ascii %in% FALSE)
                        "wb"
                    else "w"
            con <- if (is.logical(compress))
                       if (compress)
                           gzfile(file, mode)
                       else file(file, mode)
                   else switch(compress, bzip2 = bzfile(file, mode), xz = xzfile(file,
                                                                                 mode), gzip = gzfile(file, mode), stop("invalid 'compress' argument: ",
                                                                                                                        compress))
            on.exit(close(con))
        }
        else if (inherits(file, "connection")) {
            if (!missing(compress))
                warning("'compress' is ignored unless 'file' is a file name")
            con <- file
        }
        else stop("bad 'file' argument")
        .Internal(serializeToConn(object, con, ascii, version, refhook))
    }
    overwritefun('saveRDS','saveRDS', package = "base")

    ## proceed with caution
#######################
#######################
#######################
    ## if you don't wrap this in a function,
    ## and just run it manually after startup,
    ## everything goes bollocks...
    startup = function() {
        `:::.new` = function (pkg, name) {
            pkg <- as.character(substitute(pkg))
            name <- as.character(substitute(name))
            pev = packageEvent(pkg, "onLoad")
            gh = getHook(pev)
            if (length(gh) == 0 || is.null(gh$forceall12340987)) {
                setHook(pev,
                        list("forceall12340987" = function(...) forceall(envir = asNamespace(pkg))))
            }
            out = get(name, envir = asNamespace(pkg), inherits = FALSE)
            ## forceall(envir = asNamespace(pkg), evalenvir = globalenv())
            return(out)
        }

        `::.new` = function (pkg, name) {
            pkg <- as.character(substitute(pkg))
            name <- as.character(substitute(name))
            pev = packageEvent(pkg, "onLoad")
            gh = getHook(pev)
            if (length(gh) == 0 || is.null(gh$forceall12340987)) {
                setHook(pev,
                        list("forceall12340987" = function(...) forceall(envir = asNamespace(pkg))))
            }
            out = getExportedValue(pkg, name)
            ## forceall(envir = asNamespace(pkg), evalenvir = globalenv())
            return(out)
        }

        overwritefun("::.new", "::", package = asNamespace("base"))
        overwritefun(":::.new", ":::", package = asNamespace("base"))
    }

#######################
#######################
#######################

    Sys.setenv(R_DATATABLE_NUM_THREADS = 1)
    Sys.setenv("GENCODE_DIR" = "~/DB/GENCODE")

    Sys.setenv("BASH_FUNC_blip()" = "() { echo \"hoohah\"; }")

    Sys.setenv(DEFAULT_GENOME = "~/DB/references/hg19/human_g1k_v37_decoy.chrom.sizes")
    Sys.setenv(DEFAULT_BSGENOME = "~/DB/references/hg19/human_g1k_v37_decoy.chrom.sizes")

    ww = with
    wn = within

    go.R = function() {
        evalq(source("~/startup.R"), globalenv())
    }
}
