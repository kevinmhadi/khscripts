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
    if (sprintf("package:%s", lib) %in% search())
    {
        expr = sprintf("detach(package:%s, force = force, unload = unload)", lib)
        eval(parse(text = expr))
    }
    txt = sprintf("library(%s)", lib)
    eval(parse(text = txt))
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
