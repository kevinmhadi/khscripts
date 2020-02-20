forceload = function() {
    pkgs = gsub("package:", "", grep('package:', search(), value = TRUE))
    for (pkg in pkgs) {
        tryCatch( {
            message("force loading ", pkg)
            invisible(eval(as.list((asNamespace(pkg)))))
            invisible(eval(eapply(asNamespace(pkg), base::force, all.names = TRUE)))
        }, error = function(e) message("could not force load ", pkg))
    }
}

relib = function(lib = 'Flow', force = TRUE, unload = TRUE)
{    
    if (sprintf("package:%s", lib) %in% search())
    {
        expr = sprintf("detach(package:%s, force = force, unload = unload)", lib)
        eval(parse(text = expr))
    }
    txt = sprintf("library(%s)", lib)
    eval(parse(text = txt))
}
