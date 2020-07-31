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

forceload = function(envir = globalenv()) {
    pkgs = gsub("package:", "", grep('package:', search(), value = TRUE))
    pkgs = c(pkgs, names(sessionInfo()$loadedOnly))
    for (pkg in pkgs) {
        tryCatch( {
            message("force loading ", pkg)
            invisible(eval(as.list((asNamespace(pkg))), envir = envir))
            invisible(eval(eapply(asNamespace(pkg), base::force, all.names = TRUE), envir = envir))
        }, error = function(e) message("could not force load ", pkg))
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
    pkgarg = pkgarg[sapply(pkgarg, is.call)]
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
        do.call(library, c(alist(package = lib, character.only = T),
                           otherarg))
    }
    suppressMessages(forceload())
}


force2 = function(x)
    tryCatch(x, error = function(e) NULL)


forceall = function(invisible = TRUE, envir = parent.frame(), evalenvir = parent.frame()) {
    if (invisible)  {
        invisible(eval(as.list(envir), envir = evalenvir))
        invisible(eval(eapply(envir, force, all.names = TRUE), envir = evalenvir))
    } else {
        print(eval(as.list(envir), envir = evalenvir))
        print(eval(eapply(envir, force, all.names = TRUE), envir = evalenvir))
    }
}    
