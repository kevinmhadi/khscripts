source("/gpfs/commons/groups/imielinski_lab/home/khadi/git/khscripts/.Rprofile")
require3(khtools, glmnet, randomForest, caret, dplyr, signature.tools.lib, pROC)


## robust.scale <- function(x, qlow = 0.1, qhigh = 0.9) {
##   out = x
##   ql = quantile(out, qlow)
##   qh = quantile(out, qhigh)
##   out = pmax(out, ql)
##   out = pmin(out, qh)
##   return(out)
## }

robust.scale <- function(x, qlow = 0.1, qhigh = 0.9) {
    MEDIAN =  median(x, na.rm = T)
    (x - MEDIAN) / quantile(x, qlow)
}



mystats = function(x, na.rm = TRUE) {
    if (is.null(dim(x))) {
        x = as.data.frame(x)
    }
    fun = function(x) {
        if (is.numeric(x)) {
            c(
                mean = mean(x, na.rm = na.rm),
                median = median(x, na.rm = na.rm),
                sd = sd(x, na.rm = na.rm),
                mad = mad(x, na.rm = na.rm),
                var = stats::var(x, na.rm = na.rm),
                min = min(x, na.rm = na.rm),
                max = max(x, na.rm = na.rm),
                q25 = unname(quantile(x, 0.25, na.rm = na.rm)),
                q75 = unname(quantile(x, 0.75, na.rm = na.rm)),
                q10 = unname(quantile(x, 0.10, na.rm = na.rm)),
                q90 = unname(quantile(x, 0.90, na.rm = na.rm))
            )
        } else {
            c(
                mean = NA_real_,
                median = NA_real_,
                sd = NA_real_,
                mad = NA_real_,
                var = NA_real_,
                min = NA_real_,
                max = NA_real_,
                q25 = NA_real_,
                q75 = NA_real_,
                q10 = NA_real_,
                q90 = NA_real_
            )
        }
    }
    cn = colnames(x)
    out.lst = lapply(x, fun)
    out.lst
    ## transp(out.lst, c)
}


my_preprocess = function(dat) {
    lapply(dat, function(x) {
        if (inherits(x, c("numeric", "integer"))) {
            unlist(mystats(x))
        }
    })
}

my_predict = function(mypre, newdat, method = "robust") {
    goods = sapply(mypre, Negate(is.null))
    nm = names(goods[goods])
    qm = qmat(newdat,,nm)
    if (identical(method, "robust")) {
        mp = mapply(function(x,y) {
            (x - y["x.median"]) / (y["x.q90"] - y["x.q10"])
        }, qm, mypre[nm], SIMPLIFY = F)
    }
    do.assign(newdat, setColnames(as.data.frame(mp), nm))
}


do.pp = function(dat, vars, method = c("center", "scale"), prefun = NULL, rangeBounds = c(0, 1), verbose = TRUE) {
    require(caret)
    require(dplyr)
    odat = copy3(dat)
    if (!is.null(prefun) && is.function(prefun)) {
        for (v in vars)
            dat[[v]] = prefun(dat[[v]])
    } else {
        prefun = NULL
    }
    if (!is.null(method) && is.character(method)){
        if (identical(method, "robust")) {            
            pp = dat %>% select(!!vars) %>% my_preprocess
            dat = my_predict(pp, dat)
        } else {
            pp = preProcess(dat %>% select(!!vars), method = method, rangeBounds = rangeBounds, verbose = verbose)
            dat = predict(pp, dat)
        }
    } else {
        pp = NULL
    }
    return(structure(list(pp = pp, dat = dat, prefun = prefun, vars = vars, method = method, orig_dat = odat), class = "carprep"))
}

predict.pp = function(pp.res, newdat, apply.prefun = TRUE) {
    require(caret)
    require(dplyr)
    odat = copy3(newdat)
    if (is.function(apply.prefun) || (!is.null(pp.res$prefun) && isTRUE(apply.prefun) && is.function(pp.res$prefun))) {
      if (is.function(apply.prefun)) pp.res$prefun = apply.prefun
      for (v in pp.res$vars)
        newdat[[v]] = pp.res$prefun(newdat[[v]])
    } else {
      pp.res$prefun = NULL
    }
    if (!is.null(pp.res$pp) && !identical(pp.res$method, "robust")) {
        newdat = predict(pp.res$pp, newdat)
    } else if (identical(pp.res$method, "robust")) {
        newdat = my_predict(pp.res$pp, newdat)
    }
    return(structure(list(pp = pp.res$pp, newdat = newdat, prefun = pp.res$prefun, vars = pp.res$vars, method = pp.res$method, orig_newdat = odat), class = "carpost"))
}


## nysv = c("tib", "ihdel", "qrdup", "qrdel")
## uksv = c("RS3", "RS5")
## fullmod = c("RS3", "RS5", "del.mh.prop", "hrd", "SNV8", "SNV3")

## fitglmnet = function(var, lambda = NULL, family = "multinomial", ycol, dat) {
##   if (missing(ycol)) ycol = "fmut"
##   if (missing(dat)) {
##     message("attempting to grab variable 'fulldat' from env stack")
##     fulldat = dg(fulldat)
##   } else {
##     fulldat = dat
##   }
##   source("/gpfs/commons/groups/imielinski_lab/home/khadi/git/khscripts/.Rprofile")
##   require3(glmnet)
##   set.seed(10);
##   x = select(fulldat, !!var) %>% asm
##   ## x.train = model.matrix(~., (select(traind, !!vars)))
##   y = fulldat[[ycol]]
##   if (family == "multinomial") {
##       y = fulldat[[ycol]]
##       modglm = glmnet(x = x,
##                       y = y, family = family, type.multinomial = "grouped",
##                       lambda = lambda, lower.limits = rep_len(0, NCOL(x)),
##                       n_lambda = 1000)
##   } else if (family == "binomial") {
##       y = refactor(fulldat[[ycol]], "WT") %>% fct_recode("HRD" = "OTHER") %>% relevel("WT")
##       modglm = glmnet(x = x,
##                       y = y, family = family,
##                       lambda = lambda, lower.limits = rep_len(0, NCOL(x)),
##                       n_lambda = 1000)
##   }
##   return(modglm)
## }

fitglmnet = function(var, lambda = NULL, family = "multinomial", ycol, dat) {
  if (missing(ycol)) ycol = "fmut"
  if (missing(dat)) {
    message("attempting to grab variable 'fulldat' from env stack")
    fulldat = dg(fulldat)
  } else {
    fulldat = dat
  }
  ## source("/gpfs/commons/groups/imielinski_lab/home/khadi/git/khscripts/.Rprofile")
  ## require3(glmnet)
  set.seed(10);
  x = select(fulldat, !!var) %>% asm
  ## x.train = model.matrix(~., (select(traind, !!vars)))
  y = fulldat[[ycol]]
  if (family == "multinomial") {
      y = fulldat[[ycol]]
      modglm = cv.glmnet(x = x,
                      y = y, family = family, type.multinomial = "grouped",
                      lower.limits = rep_len(0, NCOL(x)))
  } else if (family == "binomial") {
      y = refactor(fulldat[[ycol]], "WT") %>% fct_recode("HRD" = "OTHER") %>% relevel("WT")
      modglm = cv.glmnet(x = x,
                      y = y, family = family,
                      lower.limits = rep_len(0, NCOL(x)))
  }
  return(modglm)
}


# mylambdas <- function(M = 2, N = 10, step = 0.005) {
#     return(10^(seq(M, -N, -abs(step))))
# }

mylambdas <- function(M = 2, N = 10, step = 0.1) {
    return(10^(seq(M, -N, -abs(step))))
}


fitcvglmnet = function(var, lambda = mylambdas(), family = "multinomial", ycol, dat,
                       only_positive = TRUE, nlambda = 100, maxit = 1e5, thresh = 1e-07,
                       type.multinomial = "grouped",
                       standardize = FALSE, standardize.response = FALSE,
                       type.measure = "default", alpha = 1,
                       ...
                       ) {
  if (isFALSE(standardize)) {
    message("standardize is FALSE")
    message("you must standardize the features yourself")
  }
  if (isTRUE(standardize)) message("standardize is TRUE")
  if (missing(ycol)) ycol = "fmut"
  if (missing(dat)) {
    message("attempting to grab variable 'fulldat' from env stack")
    fulldat = dg(fulldat)
  } else {
    fulldat = dat
  }
  ## source("/gpfs/commons/groups/imielinski_lab/home/khadi/git/khscripts/.Rprofile")
  ## require3(glmnet)
  set.seed(10);
  x = select(fulldat, !!var) %>% asm
  ## x.train = model.matrix(~., (select(traind, !!vars)))
  y = fulldat[[ycol]]
  if (only_positive)
      llim = rep_len(0.0, NCOL(x))
  else
      llim = rep_len(-Inf, NCOL(x))
  if (family == "multinomial") {
      ## y = fulldat[[ycol]]
      modglm = cv.glmnet(x = x,
                      y = y, family = family, type.multinomial = type.multinomial,
                      lower.limits = llim,
                      lambda = lambda,
                      nlambda = nlambda,
                      maxit = maxit,
                      thresh = thresh,
                      standardize = standardize,
                      standardize.response = standardize.response,
                      type.measure = type.measure, alpha = alpha, ...
                      )
  } else if (family == "binomial") {
      ## y = refactor(fulldat[[ycol]], "WT") %>% fct_recode("HRD" = "OTHER") %>% relevel("WT")
      modglm = cv.glmnet(x = x,
                      y = y, family = family,
                      lower.limits = llim,
                      lambda = lambda,
                      nlambda = nlambda,
                      maxit = maxit,
                      thresh = thresh,
                      standardize = standardize,
                      standardize.response = standardize.response,
                      type.measure = type.measure, alpha = alpha, ...
                      )
  }
  return(modglm)
}


fitglmnet = function(var, lambda = 0, family = "multinomial", ycol, dat,
                     only_positive = TRUE, maxit = 1e5, thresh = 1e-07, standardize = FALSE) {
  if (isFALSE(standardize)) {
    message("standardize is FALSE")
    message("you must standardize the features yourself")
  }
  if (isTRUE(standardize)) message("standardize is TRUE")
  if (missing(ycol)) ycol = "fmut"
  if (missing(dat)) {
    message("attempting to grab variable 'fulldat' from env stack")
    fulldat = dg(fulldat)
  } else {
    fulldat = dat
  }
  set.seed(10);
  x = select(fulldat, !!var) %>% asm
  y = fulldat[[ycol]]
  if (only_positive)
      llim = rep_len(0.0, NCOL(x))
  else
      llim = rep_len(-Inf, NCOL(x))
  if (family == "multinomial")
  {
      modglm = glmnet(
          x = x,
          y = y, family = family, 
          lower.limits = llim,
          lambda = lambda, ## thresh = 1e-14, maxit = 10e6,
          trace.it = 10,
          maxit = maxit,
          thresh = thresh,
          standardize = standardize
      )
  } else if (family == "binomial") {
      modglm = glmnet(
          x = x,
          y = y, family = family, 
          lower.limits = llim,
          lambda = lambda, ## thresh = 1e-14, maxit = 10e6,
          trace.it = 10,
          maxit = maxit,
          thresh = thresh,
          standardize = standardize
      )
  }
  return(modglm)
}


fitrandomforest <- function(var, ycol, dat, ...) {
  if (missing(ycol)) ycol = "fmut"
  if (missing(dat)) {
    message("attempting to grab variable 'fulldat' from env stack")
    fulldat = dg(fulldat)
  } else {
    fulldat = dat
  }
  ## source("/gpfs/commons/groups/imielinski_lab/home/khadi/git/khscripts/.Rprofile")
  ## require3(randomForest)
  set.seed(10);
  x = select(fulldat, !!var) %>% asm
  ## x.train = model.matrix(~., (select(traind, !!vars)))
  y = fulldat[[ycol]]
  rfdat = et(sprintf("cbind(asdf(x), %s = y)", ycol))
  ## rfdat = cbind(asdf(x), mutstat = y)
  form = formula(sprintf("%s ~ %s", ycol, paste(var, collapse = "+")))
  message("formula: ", deparse(form))
  rfmod = randomForest(form, data = rfdat, ntree = 1000, importance = TRUE, ...)
  return(rfmod)
}



fit_classifier <- function(mod, data, s = 0, alpha = 1, exact = FALSE) {
  
  data = copy(data)
  
  cls.mod = class(mod)[1]
  if (identical(cls.mod, "cv.glmnet")) {
    gmod = mod$glmnet.fit
  } else {
    gmod = mod
  }
  classnames = gmod$classnames

  for (thisc in c(classnames, "NRSUM"))
    data[[thisc]] = NULL

  
  ## debugonce(predict.multnet)
  if (inherits(gmod$beta, "list"))
    vars = rownames2(gmod$beta[[1]])
  else
    vars = rownames2(gmod$beta)
  
  scores = get_pred(mod, data, vars, type = "response", s = s, alpha = alpha, exact = exact)  
  

  classes = factor(
    get_pred(mod, data, vars, type = "class", s = s, alpha = alpha, exact = exact),
    classnames
  )

  data$classes = classes
  scores = asdf(scores)

  if (NCOL(scores) == 1)
    colnames(scores) = classnames[2]
  
  for (i in 1:NCOL(scores))
    data[[colnames(scores)[[i]]]] = scores[[i]]

  data$NRSUM = rowSums(qmat(data,,classnames[-1]))

  return(data)

}



fit_rforest <- function (mod, data) {
    data = copy(data)
    classnames = mod$classes
    scores = as.data.frame.matrix(predict(mod, data, type = "prob"))
    out = copy(data)
    for (i in 1:NCOL(scores))
        out[[colnames(scores)[[i]]]] = scores[[i]]
    ## out = out %>% mutate(scores)
    out$classes = predict(mod, data, type = "class")
    out$NRSUM = rowSums(qmat(out,,classnames[-1]))
    
    ## data$NRSUM = rowSums(qmat(data, , classnames[-1]))
    return(out)
}



glmnet_aic = function(fit) {
  tLL <- fit$nulldev - deviance(fit)
  k <- fit$df
  n <- fit$nobs
  AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
  AICc
}


get_pred <- function(mod, newdata, feats, type = "response", s = 0, alpha = 1, exact = FALSE) {
  cls.mod = class(mod)
  if (any(cls.mod == "cv.glmnet")) cls.mod = class(mod$glmnet.fit)[1]
  if (any(cls.mod == "multnet")) {
    prd = predict(mod, select(newdata, !!feats) %>% asm, type = type, s = s, alpha = alpha, type.multinomial = "grouped", exact = exact)
    if (length(dim(prd)) == 3) {
      dnm2 = dimnames(prd)[[2]]
      dim(prd) = dim(prd)[1:2]
      dimnames(prd) = list(NULL, dnm2)
    }
  } else if (any(cls.mod == "lognet")) {
    prd = predict(mod, select(newdata, !!feats) %>% asm, type = type, s = s, alpha = alpha, exact = exact) %>%
      setColnames(mod$classnames[2])
  } else if (any(cls.mod == "randomForest")) {
      ## type means different things for random forest
      if (identical(type, "class"))
          type = "response"
      else if (identical(type, "response"))
          type = "prob"
      prd = predict(mod, newdata, type = type)
  }
  return(prd)
}


make_roc <- function(dat, lab = "lab", score = "HRD", include_group = FALSE) {
  mrlab = mroclab(dat[[lab]])
  mrpred = mrocpred(et(sprintf("dat[, %s, drop=F]", mkst(paste0("'", score, "'")))))
  colslab = gsub("(\\w+)[ ](\\w+)", "\\1", colnames(mrlab))
  colspred = gsub("(\\w+)[ ](\\w+)", "\\1", colnames(mrpred))
  goodcols = intersect(colslab, colspred)
  mrlab = asdf(et(sprintf("mrlab[, %s,drop=F]", mkst(colslab %in% goodcols))))
  mrpred = asdf(et(sprintf("mrpred[, %s,drop=F]", mkst(colspred %in% goodcols))))

  mrocdat = function (lbl, prd) {
    prd00 = rbind(0, asm(prd))
    mg = setcols(with((melt(prd00)), g2()[order(Var2, value), 
      ]), c("Var2", "value"), c("Group", "prd"))[, c("Group", 
        "prd"), drop = F]
    cb = cbind(lbl, prd)
    roc_res = multiROC::multi_roc(cb, force_diag = T)
    plot_roc_df <- multiROC::plot_roc_data(roc_res)
    if (!include_group) {
      gdat = asdt(plot_roc_df)[Group %nin% c("Macro", "Micro")]## [, 
        ## `:=`(prd, rev(mg$prd))]
    } else {
      gdat = asdt(plot_roc_df)## [Group %nin% c("Macro", "Micro"), 
        ## `:=`(prd, rev(mg$prd))]
    }
    gdat[, `:=`(Group, trimws(Group))]
    return(gdat)
  }
  
  mrdat = suppressWarnings(mrocdat(
    mrlab,
    mrpred
  ))
  return(withAutoprint(mrdat, echo = F)$value)
}



score_glmnet = function(data, features, family, labels = "lab", lambda = 0, mod) {
  if (missing(mod))
    mod = fitglmnet(features, family = family, dat = data, ycol = labels, lambda = lambda)
  if (class(mod)[1] == "lognet")
    ## data[[levels(data[[labels]])[-1]]] = get_pred(mod, newdata = data, features)
    data[[mod$classnames[-1]]] = as.vector(get_pred(mod, newdata = data, features))
  else if (class(mod)[1] == "multnet") {
    scores = get_pred(mod, newdata = data, features)
    ## lvs = levels(data[[labels]])
    lvs = mod$classnames[-1]
    for (lv in lvs) {
      data[[lv]] = scores[,lv]
    }
  }
  list(data = data, model = mod, features = features)
}


score_randomforest = function(data, features, labels = "lab", lambda = 0, mod) {
  if (missing(mod)) {
    mod = fitrandomforest(features, dat = data, ycol = labels)
  }
  if (class(mod)[1] == "lognet")
    ## data[[levels(data[[labels]])[-1]]] = get_pred(mod, newdata = data, features)
    data[[mod$classnames[-1]]] = as.vector(get_pred(mod, newdata = data, features))
  else if (class(mod)[1] == "multnet") {
    scores = get_pred(mod, newdata = data, features)
    ## lvs = levels(data[[labels]])
    lvs = mod$classnames[-1]
    for (lv in lvs) {
      data[[lv]] = scores[,lv]
    }
  } else if (any(grepl("randomForest", class(mod)))) {
    scores = predict(mod, newdata = asdf(data), type = "prob")
    lab = predict(mod, newdata = asdf(data))
    if (length(mod$classes) == 2)
      lvs = mod$classes[-1]
    else
      lvs = mod$classes
    for (lv in lvs) {
      data[[lv]] = scores[,lv]
    }
    data[["class_lab"]] = lab
  }
  list(data = data, model = mod, features = features)
}

feature_importance <- function(ixfold, xfolds, dat, lambda = 0, vars, seed = 10, ycol, debug = FALSE,
                               rpart = FALSE, permute_varlist = NULL, added_testd = NULL,
                               caret_preprocess = do.pp(traind, vars = vars, method = "range", prefun = log1p),
                               only_positive = FALSE, standardize = FALSE, model.type = c("glm", "glmnet", "randomforest"),
                               s = 0, alpha = 1
                               ) {
    tryCatch({
        if (debug) browser()
        nm = names(xfolds[ixfold])
        xfold = xfolds[[ixfold]]
        message("testing split: ", ixfold, " ", nm)
        traind = copy(dat[xfold$train,])
        testd = copy(dat[xfold$test,])

        if (!is.null(added_testd)) {
            added_testd = copy(added_testd[pair %in% setdiff(pair, c(traind$pair, testd$pair))])
            if (NROW(added_testd) > 0)
                testd = rbind(testd[, added := FALSE], added_testd[, added := TRUE], fill = T)
        } else {
            testd$added = FALSE
        }

        set.seed(seed);

        traind = copy3(traind)
        traind$fmut = traind[[ycol]]
        testd = copy3(testd)
        testd$fmut = testd[[ycol]]

        if (!is.null(caret_preprocess)) {
            if (is.character(caret_preprocess) && caret_preprocess[1] %in% c("range", "scale")) {
                pp = preProcess(select(traind, !!vars), method = caret_preprocess)
                traind = predict(pp, traind)
                testd = predict(pp, testd)
            } else if (is.function(caret_preprocess)) {
                traind = traind %>% mutate_at(vars(!!vars), caret_preprocess)
                testd = testd %>% mutate_at(vars(!!vars), caret_preprocess)
            } else if (inherits(caret_preprocess, "carprep")) {
                traind = caret_preprocess$dat
                prep.res = predict.pp(caret_preprocess, testd)
                testd = prep.res$newdat
            } else if (inherits(caret_preprocess, c("expression", "call"))) {
                caret_preprocess = eval(caret_preprocess)
                traind = caret_preprocess$dat
                prep.res = predict.pp(caret_preprocess, testd)
                testd = prep.res$newdat
            }
        }


        if (length(levels(traind$fmut)) > 2) {
            family = "multinomial"
        } else {
            family = "binomial"
        }

        model.type = intersect(model.type, c("glm", "glmnet", "randomforest"))

        if (length(model.type) > 1) {
            model.type = model.type[1]
        }



        if (identical(model.type, "glm")) {
            trainglm = fitcvglmnet(vars, family = family, dat = traind, ycol = ycol, only_positive = only_positive, standardize = standardize, lambda = mylambdas())
            testd = fit_classifier(trainglm, testd, s = s, alpha = alpha)
        } else if (identical(model.type, "glmnet")) {
            trainglm = fitglmnet(vars, family = family, dat = traind, ycol = ycol, only_positive = only_positive, standardize = standardize, lambda = mylambdas())
            testd = fit_classifier(trainglm, testd, s = s, alpha = alpha)
        } else if (identical(model.type, "randomforest")) {
            trainglm = fitrandomforest(var = vars, dat = traind, ycol = ycol)
            testd = fit_rforest(trainglm, testd)
        }
        
        if (any(class(trainglm) %in% c("cv.glmnet", "glmnet"))) {
            co = as.data.frame(as.matrix(coef(trainglm, s = s))) %>% rn2col("feat") %>% as.data.table
            co$nm = nm
            # co$xfold = xfold
        } else {
            co = NULL
        }


        cls.mod = class(trainglm)[1]
        if (identical(cls.mod, "cv.glmnet")) {
            gmod = trainglm$glmnet.fit
        } else {
            gmod = trainglm
        }
        if (any(class(gmod) == "glmnet"))
            classnames = gmod$classnames
        else if (any(class(gmod) == "randomForest"))
            classnames = gmod$classes

        if (length(classnames) == 2) {
            classnames = classnames[2]
        }
        scores = qmat(testd,,classnames)


        testd$fold_id = nm
        testd$Method = "all"

        off_diag = function(x) {
            diag(x) = NA
            as.vector(x) %>% na.omit
        }

        get_accuracy = function(confus_mat) {
            correct = diag(confus_mat)
            sum(correct) / sum(off_diag(confus_mat), correct)
        }

        indiv.levels = levels(testd[[ycol]])[-1]
        full_mod_indiv_accuracy = c()
        if (length(indiv.levels) > 1) {
            for (i in indiv.levels) {
                indiv.mat = as.matrix(table(refactor(testd[[ycol]], i),
                  refactor(factor(testd$classes, levels = levels(testd[[ycol]])), i)))
                full_mod_indiv_accuracy = c(full_mod_indiv_accuracy, setNames(get_accuracy(indiv.mat), i))
            }
        }

        confus_mat = as.matrix(table(testd[[ycol]], factor(testd$classes, levels = levels(testd[[ycol]]))))
        full_mod_accuracy = c(FULL = get_accuracy(confus_mat))

        lvars = as.list(vars)

        if (!is.null(permute_varlist)) {
            ## .NotYetImplemented()
            if (is.character(permute_varlist))
                pvar = list(permute_varlist)
            else if (is.list(permute_varlist)) {
                pvar = permute_varlist
            } else if (! is.list(permute_varlist)) {
                warning("ignoring permute_list")
                pvar = list()
            }
            lvars = c(lvars, pvar)
        }

        permute_lst = purrr::transpose(lapply(lvars, function(v, seed = 10) {
            testd_permute = copy(testd)
            testd_permute$fold_id = nm
            set.seed(seed)
            for (i in 1:NROW(v))
                testd_permute[[v[i]]] = sample(testd_permute[[v[i]]])
            testd_permute$Method = paste(v, collapse = ", ")
            permute_classes = get_pred(trainglm, testd_permute, vars, type = "class")
            permut_s = asdf(get_pred(trainglm, testd_permute, vars, type = "response"))
            if (NCOL(permut_s) == 1)
                colnames(permut_s) = levels(testd[[ycol]])[2]
            for (i in seq_len(NCOL(permut_s)))
                testd_permute[[names(permut_s)[i]]] = permut_s[[i]]
            permute_roc = make_roc(testd_permute, ycol, colnames(permut_s))[
               ,Method := testd_permute$Method[1]][
               ,fold_id := nm]

            indiv.levels = levels(testd[[ycol]])[-1]
            indiv.accuracy = c()
            if (length(indiv.levels) > 1) {
                for (i in indiv.levels) {
                    indiv.mat = as.matrix(table(refactor(testd[[ycol]], i),
                  refactor(factor(permute_classes, levels = levels(testd[[ycol]])), i)))
                    indiv.accuracy = c(indiv.accuracy, setNames(get_accuracy(indiv.mat), i))
                }
            }
            indiv.dec.accuracy = full_mod_indiv_accuracy[names(indiv.accuracy)] - indiv.accuracy
            confus_mat_permute = as.matrix(table(testd[[ycol]], factor(permute_classes, levels = levels(testd[[ycol]]))))
            dec_accuracy = data.table(feature = paste(v, collapse = ", "), decrease_accuracy = full_mod_accuracy - get_accuracy(confus_mat_permute))[, which := "FULL"]
            dec_accuracy = dec_accuracy[rep_len(1:NROW(dec_accuracy), NROW(dec_accuracy) + NROW(indiv.accuracy))]
            dec_accuracy[-1, decrease_accuracy := indiv.dec.accuracy]
            dec_accuracy[-1, which := names(indiv.dec.accuracy)]
            list(dec_accuracy = dec_accuracy,
                 roc = permute_roc,
                 testd = testd_permute)
        }))

        dec_accuracy = rbindlist(permute_lst$dec_accuracy)
        dec_accuracy$fold_id = nm

        bigroc = rbind(rbindlist(permute_lst$roc),
                       make_roc(testd, ycol, classnames)[, Method := "all"][, fold_id := nm])


        ## mlab = mroclab(testd[[ycol]])
        ## mpred = mrocpred(get_pred(trainglm, testd, vars))

        ## if (class(trainglm)[1] == "lognet")
        ##     mlab = mlab[,2,drop=F]

        outd = rbind(rbindlist(permute_lst$testd), testd)

        ret = list(
            dec_accuracy = dec_accuracy,
            ## lab = mlab, mpred = mpred,
            roc = bigroc,
            classes = testd$classes,
            scores = scores,
            outd = outd,
            coef = co
        )
        return(ret)
    }, error = function(e) printerr(ixfold))
};  ifun <- feature_importance


## feature_importance <- function(ixfold, xfolds, dat, lambda = 0, vars, seed = 10, ycol, debug = FALSE, rpart = FALSE, permute_varlist = NULL, added_testd = NULL, caret_preprocess = NULL, only_positive = TRUE) {
##     tryCatch({
##     if (debug) browser()
##     nm = names(xfolds[ixfold])
##     xfold = xfolds[[ixfold]]
##     traind = copy(dat[xfold$train,])
##     testd = copy(dat[xfold$test,])

##     if (!is.null(added_testd)) {
##         added_testd = copy(added_testd[pair %in% setdiff(pair, c(traind$pair, testd$pair))])
##         if (NROW(added_testd) > 0)
##             testd = rbind(testd[, added := FALSE], added_testd[, added := TRUE])
##     } else {
##         testd$added = FALSE
##     }

##     set.seed(seed);

##     traind = copy3(traind)
##     traind$fmut = traind[[ycol]]
##     testd = copy3(testd)
##     testd$fmut = testd[[ycol]]

##     if (!is.null(caret_preprocess)) {
##         if (is.character(caret_preprocess) && caret_preprocess[1] %in% c("range", "scale")) {
##             pp = preProcess(select(traind, !!vars), method = caret_preprocess)
##             traind = predict(pp, traind)
##             testd = predict(pp, testd)
##         } else if (is.function(caret_preprocess)) {
##             traind = traind %>% mutate_at(vars(!!vars), caret_preprocess)
##             testd = testd %>% mutate_at(vars(!!vars), caret_preprocess)
##         }        
##     }
 

##     if (length(levels(traind$fmut)) > 2) {
##         family = "multinomial"
##     } else {
##         family = "binomial"
##     }

##     trainglm = fitcvglmnet(vars, family = family, dat = traind, ycol = ycol, only_positive = only_positive)

##     classes = get_pred(trainglm, testd, vars, type = "class")
##     scores = get_pred(trainglm, testd, vars, type = "response")

##     testd$fold_id = nm
##     testd$Method = "all"



##     testd$classes = classes
##     if (!inherits(scores, "data.frame"))
##         scores = asdf(scores)

##     if (NCOL(scores) == 1)
##         colnames(scores) = levels(testd[[ycol]])[2]

##     for (i in seq_len(NCOL(scores)))
##         testd[[names(scores)[i]]] = scores[[i]]

##     off_diag = function(x) {
##         diag(x) = NA
##         as.vector(x) %>% na.omit
##     }

##     get_accuracy = function(confus_mat) {
##         correct = diag(confus_mat)
##         sum(correct) / sum(off_diag(confus_mat), correct)
##     }

##     indiv.levels = levels(testd[[ycol]])[-1]
##     full_mod_indiv_accuracy = c()
##     if (length(indiv.levels) > 1) {
##         for (i in indiv.levels) {
##             indiv.mat = as.matrix(table(refactor(testd[[ycol]], i),
##               refactor(factor(testd$classes, levels = levels(testd[[ycol]])), i)))
##             full_mod_indiv_accuracy = c(full_mod_indiv_accuracy, setNames(get_accuracy(indiv.mat), i))
##         }
##     }

##     confus_mat = as.matrix(table(testd[[ycol]], factor(testd$classes, levels = levels(testd[[ycol]]))))
##     full_mod_accuracy = c(FULL = get_accuracy(confus_mat))

##     lvars = as.list(vars)

##     if (!is.null(permute_varlist)) {
##         ## .NotYetImplemented()
##         if (is.character(permute_varlist))
##             pvar = list(permute_varlist)
##         else if (is.list(permute_varlist)) {
##             pvar = permute_varlist
##         } else if (! is.list(permute_varlist)) {
##             warning("ignoring permute_list")
##             pvar = list()
##         }
##         lvars = c(lvars, pvar)
##     }

##     permute_lst = purrr::transpose(lapply(lvars, function(v, seed = 10) {
##         testd_permute = copy(testd)
##         testd_permute$fold_id = nm
##         set.seed(seed)
##         for (i in 1:NROW(v))
##             testd_permute[[v[i]]] = sample(testd_permute[[v[i]]])
##         testd_permute$Method = paste(v, collapse = ", ")
##         permute_classes = get_pred(trainglm, testd_permute, vars, type = "class")
##         permut_s = asdf(get_pred(trainglm, testd_permute, vars, type = "response"))
##         if (NCOL(permut_s) == 1)
##             colnames(permut_s) = levels(testd[[ycol]])[2]
##         for (i in seq_len(NCOL(permut_s)))
##             testd_permute[[names(permut_s)[i]]] = permut_s[[i]]
##         permute_roc = make_roc(testd_permute, ycol, colnames(permut_s))[
##            ,Method := testd_permute$Method[1]][
##            ,fold_id := nm]

##         indiv.levels = levels(testd[[ycol]])[-1]
##         indiv.accuracy = c()
##         if (length(indiv.levels) > 1) {
##             for (i in indiv.levels) {
##                 indiv.mat = as.matrix(table(refactor(testd[[ycol]], i),
##               refactor(factor(permute_classes, levels = levels(testd[[ycol]])), i)))
##                 indiv.accuracy = c(indiv.accuracy, setNames(get_accuracy(indiv.mat), i))
##             }
##         }
##         indiv.dec.accuracy = full_mod_indiv_accuracy[names(indiv.accuracy)] - indiv.accuracy
##         confus_mat_permute = as.matrix(table(testd[[ycol]], factor(permute_classes, levels = levels(testd[[ycol]]))))
##         dec_accuracy = data.table(feature = paste(v, collapse = ", "), decrease_accuracy = full_mod_accuracy - get_accuracy(confus_mat_permute))[, which := "FULL"]
##         dec_accuracy = dec_accuracy[rep_len(1:NROW(dec_accuracy), NROW(dec_accuracy) + NROW(indiv.accuracy))]
##         dec_accuracy[-1, decrease_accuracy := indiv.dec.accuracy]
##         dec_accuracy[-1, which := names(indiv.dec.accuracy)]
##         list(dec_accuracy = dec_accuracy,
##              roc = permute_roc,
##              testd = testd_permute)
##     }))

##     dec_accuracy = rbindlist(permute_lst$dec_accuracy)
##     dec_accuracy$fold_id = nm

##     bigroc = rbind(rbindlist(permute_lst$roc),
##                    make_roc(testd, ycol, colnames(scores))[, Method := "all"][, fold_id := nm])


##     ## mlab = mroclab(testd[[ycol]])
##     ## mpred = mrocpred(get_pred(trainglm, testd, vars))

##     ## if (class(trainglm)[1] == "lognet")
##     ##     mlab = mlab[,2,drop=F]

##     outd = rbind(rbindlist(permute_lst$testd), testd)

##     ret = list(dec_accuracy = dec_accuracy,
##          ## lab = mlab, mpred = mpred,
##          roc = bigroc,
##          classes = classes,
##          scores = scores,
##          outd = outd)
##     return(ret)
##     }, error = function(e) printerr(ixfold))
## };  ifun <- feature_importance




































## ifun = function(ixfold, xfolds, dat, lambda = 0, vars, seed = 10, ycol, debug = FALSE, rpart = FALSE, permute_varlist = NULL, added_testd = NULL) {
##     if (debug) browser()
##     nm = names(xfolds[ixfold])
##     xfold = xfolds[[ixfold]]
##     traind = copy(dat[xfold$train,])
##     testd = copy(dat[xfold$test,])

##     if (!is.null(added_testd)) {
##         added_testd = copy(added_testd[pair %in% setdiff(pair, c(traind$pair, testd$pair))])
##         if (NROW(added_testd) > 0)
##             testd = rbind(testd[, added := FALSE], added_testd[, added := TRUE])
##     }
    
##     set.seed(seed);

##     traind = copy3(traind)
##     traind$fmut = traind[[ycol]]
##     testd = copy3(testd)
##     testd$fmut = testd[[ycol]]


    
##     if (length(levels(traind$fmut)) > 2) {
##         family = "multinomial"
##     } else {
##         family = "binomial"
##     }
    
##     trainglm = fitcvglmnet(vars, family = family, dat = traind, ycol = ycol)

##     classes = get_pred(trainglm, testd, vars, type = "class")
##     scores = get_pred(trainglm, testd, vars, type = "response")

##     testd$fold_id = nm
##     testd$Method = "all"


    
##     testd$classes = classes
##     if (!inherits(scores, "data.frame"))
##         scores = asdf(scores)

##     if (NCOL(scores) == 1)
##         colnames(scores) = levels(testd[[ycol]])[2]

##     for (i in seq_len(NCOL(scores)))
##         testd[[names(scores)[i]]] = scores[[i]]

##     off_diag = function(x) {
##         diag(x) = NA
##         as.vector(x) %>% na.omit
##     }

##     get_accuracy = function(confus_mat) {
##         correct = diag(confus_mat)
##         sum(correct) / sum(off_diag(confus_mat), correct)
##     }

##     confus_mat = as.matrix(table(testd[[ycol]], factor(testd$classes, levels = levels(testd[[ycol]]))))
##     full_mod_accuracy = get_accuracy(confus_mat)

##     lvars = as.list(vars)
    
##     if (!is.null(permute_varlist)) {
##         ## .NotYetImplemented()
##         if (is.character(permute_varlist))
##             pvar = list(permute_varlist)
##         else if (is.list(permute_varlist)) {
##             pvar = permute_varlist
##         } else if (! is.list(permute_varlist)) {
##             warning("ignoring permute_list")
##             pvar = list()
##         }
##         lvars = c(lvars, pvar)
##     }

##     permute_lst = purrr::transpose(lapply(lvars, function(v, seed = 10) {
##         testd_permute = copy(testd)
##         testd_permute$fold_id = nm
##         set.seed(seed)
##         for (i in 1:NROW(v))
##             testd_permute[[v[i]]] = sample(testd_permute[[v[i]]])
##         testd_permute$Method = paste(v, collapse = ", ")
##         permute_classes = get_pred(trainglm, testd_permute, vars, type = "class")
##         permut_s = asdf(get_pred(trainglm, testd_permute, vars, type = "response"))
##         if (NCOL(permut_s) == 1)
##             colnames(permut_s) = levels(testd[[ycol]])[2]
##         for (i in seq_len(NCOL(permut_s)))
##             testd_permute[[names(permut_s)[i]]] = permut_s[[i]]
##         permute_roc = make_roc(testd_permute, ycol, colnames(permut_s))[
##            ,Method := testd_permute$Method[1]][
##            ,fold_id := nm]
##         confus_mat_permute = as.matrix(table(testd[[ycol]], factor(permute_classes, levels = levels(testd[[ycol]]))))
##         list(dec_accuracy = data.table(feature = paste(v, collapse = ", "), decrease_accuracy = full_mod_accuracy - get_accuracy(confus_mat_permute)),
##              roc = permute_roc,
##              testd = testd_permute)
##     }))
    
##     dec_accuracy = rbindlist(permute_lst$dec_accuracy)
##     dec_accuracy$fold_id = nm

##     bigroc = rbind(rbindlist(permute_lst$roc),
##                    make_roc(testd, ycol, colnames(scores))[, Method := "all"][, fold_id := nm])
    

##     ## mlab = mroclab(testd[[ycol]])
##     ## mpred = mrocpred(get_pred(trainglm, testd, vars))
    
##     ## if (class(trainglm)[1] == "lognet")
##     ##     mlab = mlab[,2,drop=F]

##     outd = rbind(rbindlist(permute_lst$testd), testd)

##     ret = list(dec_accuracy = dec_accuracy,
##          ## lab = mlab, mpred = mpred,
##          roc = bigroc,
##          classes = classes,
##          scores = scores,
##          outd = outd)
##     return(ret)
## }
