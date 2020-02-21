forceload = function(envir = globalenv()) {
    pkgs = gsub("package:", "", grep('package:', search(), value = TRUE))
    for (pkg in pkgs) {
        tryCatch( {
            message("force loading ", pkg)
            invisible(eval(as.list((asNamespace(pkg))), envir = envir))
            invisible(eval(eapply(asNamespace(pkg), base::force, all.names = TRUE), envir = envir))
        }, error = function(e) message("could not force load ", pkg))
    }
}

forcefun = function(envir = globalenv()) {
    funnames = as.character(lsf.str(envir = envir))
    for (fun in funnames) {
        tryCatch( {
            message("force loading ", fun)
            eval(force(get(fun, envir = envir)), envir = envir)
        }, error = function(e) message("could not force load ", fun))
    }
}



.relib = function(lib = 'Flow', force = TRUE, unload = TRUE)
{    
    if (sprintf("package:%s", lib) %in% search())
    {
        expr = sprintf("detach(package:%s, force = force, unload = unload)", lib)
        eval(parse(text = expr))
    }
    txt = sprintf("library(%s)", lib)
    eval(parse(text = txt))
}
