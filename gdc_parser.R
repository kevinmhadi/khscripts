library(RCurl)
library(jsonlite)
library(XML)

gdc_end = function(endpoint, legacy = FALSE, version = NULL) {
    if (!is.null(version)) {
        version = paste0("", version, "/")
    } else {
        version = ""
    }
    if (legacy) {
        paste0("https://api.gdc.cancer.gov/", version, "legacy/", endpoint)
    } else  {
        paste0("https://api.gdc.cancer.gov/", version, endpoint)
    }
}


gdc_filt = function(filters, auto_unbox = TRUE, pretty = FALSE) {
    library(RCurl)
    library(jsonlite)
    curlEscape(toJSON(filters, auto_unbox = auto_unbox, pretty = FALSE))
}




gdc_url = function(end = "cases", legacy = FALSE, filters = NULL, cust_ins = NULL, extras = NULL, size = NULL, fields = NULL) {
    if (length(cust_ins) > 1) {
        cust_ins = paste0(cust_ins, collapse = ",")
    }
    if (length(extras) > 1) {
        cust_ins = paste0(cust_ins, extra)
    }
    url = gdc_end(endpoint = end, legacy = legacy)
    if (!is.null(filters) | !is.null(cust_ins) | !is.null(extras) | !is.null(fields) | !is.null(size)) {
        url = paste0(url, "?")
    }
    if (end %in% c("data", "manifest")) {        
        url = paste0(url, "/", cust_ins)
        ## return(paste0(gdc_end(end, legacy = legacy), "/", cust_ins))
    }
    if (!is.null(filters)) {
        if (is.null(extras) || nchar(extras) == 0) {
            ## return(paste0(gdc_end(end, legacy = legacy), "?filters=", gdc_filt(filters), "&pretty=true"))
            ## return(paste0(gdc_end(end, legacy = legacy), "?filters=", gdc_filt(filters)))
            url = paste0(url, "filters=", gdc_filt(filters))
        } else {
            ## return(paste0(gdc_end(end, legacy = legacy), "?filters=", gdc_filt(filters), "&pretty=true", extras))
            ## return(paste0(gdc_end(end, legacy = legacy), "?filters=", gdc_filt(filters), extras))
            url = paste0(url, "filters=", gdc_filt(filters), extras)
        }
    } else {
        if (is.null(extras) || nchar(extras) == 0) {
            ## return(paste0(gdc_end(end, legacy = legacy), "/", cust_ins, "&pretty=true"))
            ## return(paste0(gdc_end(end, legacy = legacy), "/", cust_ins))
            ## url = paste0(gdc_end(end, legacy = legacy), "/", cust_ins)
            url = paste(c(url, cust_ins), collapse = "/")
        } else {
            ## return(paste0(gdc_end(end, legacy = legacy), "/", cust_ins, extras, "&pretty=true"))
            ## return(paste0(gdc_end(end, legacy = legacy), "/", cust_ins, extras))
            ## url = paste0(gdc_end(end, legacy = legacy), "/", cust_ins, extra)
            url = paste(c(url, cust_ins), collapse = "/")
        }
    }
    if (!is.null(size)) {
        url = sprintf("%s&size=%s", url, size)
    }
    if (!is.null(fields)) {
        url = sprintf("%s&fields=%s", url, fields)
    }
    url = sub("(\\?)\\&", "\\1", url)
    return(url)
}


## fields_example = "files.experimental_strategy,files.file_name,files.file_id,files.center.short_name,files.data_type,files.data_category,files.metadata_files.file_name"
fields_example = "disease_type,primary_site,files.experimental_strategy,files.file_name,files.file_id,files.center.short_name,files.data_type,files.data_category,files.metadata_files.data_format,files.metadata_files.data_type,files.metadata_files.data_category,files.metadata_files.file_name,files.metadata_files.file_id,files.metadata_files.submitter_id"


gdccurl = function(url, fields = "", download = FALSE, download_dir = NULL, size = NULL) {
    if (!download) {
        cmd = paste0("curl ", "'", url)
        if (nchar(fields) > 0) {
            cmd = paste0(cmd, "&fields=", fields)
        }
        if (!is.null(size)) {
            cmd = paste0(cmd, sprintf("&size=%s", size))
        }
        ## cmd = paste0(cmd, "'")
        ## return(fromJSON(system(cmd, intern = T)))
        json_input = RCurl::getURL(url)
        return(fromJSON(json_input))
    } else if (grepl("\\/data\\/|manifest", url)) {
        if (!is.null(download_dir)) {
            tmp.env123987  = new.env()
            assign("orig_wd", getwd(), envir = tmp.env123987)
            setwd(download_dir)
        }
        return(system(paste0("curl --remote-name --remote-header-name ", "'",  url, "'")))
        try(setwd(tmp.env123987$orig_wd))
    }

}



analysis_grab = function(xmllist) {
    dt = as.data.table(t(xmllist$ANALYSIS_SET$ANALYSIS$.attrs))
    aset = xmllist$ANALYSIS_SET$ANALYSIS
    label = paste(xmllist$ANALYSIS_SET$ANALYSIS$TITLE, xmllist$ANALYSIS_SET$ANALYSIS$DESCRIPTION)
    ## assembly = xmllist$ANALYSIS_SET$ANALYSIS$ANALYSIS_TYPE$REFERENCE_ALIGNMENT$ASSEMBLY[[1]]
    assembly = unlist(aset$ANALYSIS_TYPE$REFERENCE_ALIGNMENT$ASSEMBLY)
    dt[, label := label]
    dt[, assembly := assembly]
    lst_of_pipelines = xmllist$ANALYSIS_SET$ANALYSIS$ANALYSIS_TYPE$REFERENCE_ALIGNMENT$PROCESSING$PIPELINE
    set(dt, i = 1L, j = "lst_of_pipelines", value = list(list(lst_of_pipelines)))
    starts = regexpr("TCGA\\-", label)
    clean_ident = unlist(lapply(strsplit(substring(label, starts), "\\:"), function(x) x[1]))
    legacy_sample_id = substring(clean_ident, 1, 28)
    sample = substring(legacy_sample_id, 1, 25)
    ## low_passes = grepl("(Low pass)|(Low Pass)", label) ## original
    low_passes = any(grepl("(low pass)|(lowpass)", tolower(unlist(xmllist))))
    tss_id = substring(legacy_sample_id,first = 6, last = 7)
    sample_type = substring(legacy_sample_id,first = 14, last = 15)
    analyte_code = substring(legacy_sample_id,first = 20, last = 20)
    dt[, legacy_sample_id := legacy_sample_id]
    dt[, sample := sample]
    dt[, low_passes := low_passes]
    dt[, tss_id := tss_id]
    dt[, sample_type := sample_type]
    dt[, analyte_code := analyte_code]
    #' to access each entry of lst_of_pipelines dt$lst_of_pipelines[[1]]
    return(dt)
}


grab_xml = function(these_xml, mc.cores = 1, mc.preschedule = FALSE) {
    mclapply(these_xml, function(this_xml)
    {
        this = try({
            tmp = xmlToList(this_xml)
            message("read in: ", this_xml, "\n")
            dt = analysis_grab(tmp)
            dt[, analysis_file_name := basename(this_xml)]
            dt[, analysis_local_path := this_xml]
            return(dt)
        })
        ## if (this == "try-error") {
        ##     warning(this_xml, " has produced an error")
        ## }
        this
    }, mc.cores = mc.cores, mc.preschedule = mc.preschedule)
}


grab_by_file_id = function(file_ids, mc.cores = 1, mc.preschedule = TRUE, size = 1)
{
    mclapply(file_ids, function(ix)
    {
        tryCatch(
        {
            filters = list(op = "in", content = list(field = "files.file_id", value = ix))
            url = gdc_url("files", legacy = TRUE, filters)
            bla = gdccurl(url, size = size)
            dt = as.data.table(bla$data$hits)
            bla2 = gdccurl(url, fields = "cases.case_id,cases.submitter_id", size = size)
            dt2 = as.data.table(bla2$data$hits$cases)
            setnames(dt2, paste0("cases.", names(dt2)))
            build_q = cbind(dt, dt2)
            file_nm = build_q$file_name
            filters2 = list(op = "in", content = list(field = "files.file_name", value = file_nm))
            url2 = gdc_url("files", TRUE, filters2)
            bla2 = gdccurl(url, fields = "metadata_files.file_id,metadata_files.file_name,metadata_files.data_format,metadata_files.data_category,metadata_files.data_type", size = size)
            analysis_file = as.data.table(bla2$data$hits$metadata_files[[1]])[data_type == "Analysis Metadata"]
            setnames(analysis_file, paste0("analysis_", names(analysis_file)))
            analysis_file[, analysis_id := sub("\\_analysis.xml", "", analysis_file_name)]
            build_q = cbind(build_q, analysis_file)
            build_q[, id := NULL]
            return(build_q)
        }, error = function(e) "try-error")
    }, mc.cores = mc.cores, mc.preschedule = mc.preschedule)
}

## make_chunks = function(vec, num_per_chunk = 100) {
##         len_to = ceiling(length(vec)/num_per_chunk)
##         ids = split(1:length(vec), rep(1:len_to, length.out = length(vec)))
##         return(lapply(ids, function(i) vec[i]))
## }

grab_by_file_id2 = function(file_ids, max_per_chunk = 100) { ## more than 100 crashes
    ## make_chunks = function(vec, num_per_chunk = 100) {
    ##     len_to = ceiling(length(vec)/num_per_chunk)
    ##     ids = split(1:length(vec), rep(1:len_to, length.out = length(vec)))
    ##     return(lapply(ids, function(i) vec[i]))
    ## }
    make_chunks = function(vec, num_per_chunk = 100) {
        require(S4Vectors)
        require(parallel)
        ind = parallel::splitIndices(length(case_id), max(length(case_id) / max_per_chunk))
        split(case_id, rep(seq_along(ind), times = elementNROWS(ind)))
    }
    chunked_ids = make_chunks(file_ids, max_per_chunk)
    ret_lst = lapply(seq_along(chunked_ids), function(i) {
        ## browser(expr = {i == 2})
        size = length(chunked_ids[[i]])
        filters = list(op = "in", content = list(field = "files.file_id", value = chunked_ids[[i]]))
        fields = "files.file_id,file_id,cases.case_id,cases.submitter_id,cases.samples.sample_id,cases.files.submitter_id,metadata_files.file_id,metadata_files.file_name,metadata_files.data_format,metadata_files.data_category,metadata_files.data_type"
        url = gdc_url(end = "files", legacy = TRUE, filters = filters, size = size, fields = NULL)
        file_meta1 = gdccurl(url = url)
        dt = as.data.table(file_meta1$data$hits)
        dt$file_id = file_meta1$data$hits$id
        ## url = gdc_url(end = "files", legacy = TRUE, filters = filters, size = size, fields = "cases.case_id,cases.submitter_id")
        url = gdc_url(end = "files", legacy = TRUE, filters = filters, size = size, fields = fields)
        file_meta2 = gdccurl(url = url)
        dt2 = as.data.table(rbindlist(file_meta2$data$hits$cases))
        s_df = dt2[, samples]
        s_df = as.data.table(do.call("rbind", s_df))
        dt2[, samples := NULL]
        dt2 = cbind(dt2, s_df)
        ## dt2[, file_id := file_meta2$data$hits$file_id]
        dt2$file_id = file_meta2$data$hits$id
        setnames(dt2, paste0("cases.", names(dt2)))
        dt2$file_id = file_meta2$data$hits$id
        ## dt2[, file_id := file_meta2$data$hits$file_id]
        build_q = dplyr::left_join(dt, dt2, by = "file_id") %>% setDT()
        ## build_q = cbind(dt, dt2)
        file_nm = build_q$file_name
        filters2 = list(op = "in", content = list(field = "files.file_name", value = file_nm))
        url2 = gdc_url(end = "files", legacy = TRUE, filters = filters2, size = size, fields = "metadata_files.file_id,metadata_files.file_name,metadata_files.data_format,metadata_files.data_category,metadata_files.data_type")
        file_meta3 = gdccurl(url2)
        analysis_file = mapply(function(fl, id) {if (!is.null(fl)) fl$orig_file_id = id; fl}, file_meta3$data$hits$metadata_files, file_meta3$data$hits$id, SIMPLIFY = FALSE) %>% lapply(function(df) df[df[["data_type"]] == "Analysis Metadata",]) %>% rbindlist() %>% as.data.table()
        ## analysis_file = as.data.table(rbindlist(lapply(file_meta3$data$hits$metadata_files, function(df) df[df[["data_type"]] == "Analysis Metadata",])))
        ## setnames(analysis_file, paste0("analysis_", names(analysis_file)))
        setnames(analysis_file, names(analysis_file) %>% {c(paste0("analysis_", head(., -1)), tail(., 1))})
        analysis_file[, analysis_id := sub("\\_analysis.xml", "", analysis_file_name)]
        ## build_q = cbind(build_q, analysis_file)
        build_q = dplyr::left_join(build_q, analysis_file, by = c("file_id" = "orig_file_id")) %>% setDT()
        build_q[, id := NULL]
    })
    ## browser()
    return(rbindlist(ret_lst))
}

grab_by_cases_submitter_id = function(case_id, legacy = FALSE, max_per_chunk = 100) { ## more than 100 crashes
    ## make_chunks = function(vec, num_per_chunk = 100) {
    ##     len_to = ceiling(length(vec)/num_per_chunk)
    ##     ids = split(1:length(vec), rep(1:len_to, length.out = length(vec)))
    ##     return(lapply(ids, function(i) vec[i]))
    ## }
    make_chunks = function(vec, num_per_chunk = 100) {
        require(S4Vectors)
        require(parallel)
        ind = parallel::splitIndices(length(case_id), max(length(case_id) / max_per_chunk))
        split(case_id, rep(seq_along(ind), times = elementNROWS(ind)))
    }
    chunked_ids = make_chunks(case_id, max_per_chunk)
    ret_lst = lapply(seq_along(chunked_ids), function(i) {
        ## browser(expr = {i == 2})
        size = length(chunked_ids[[i]])
        filters = list(op = "in", content = list(field = "cases.submitter_id", value = chunked_ids[[i]]))
        url = gdc_url(end = "cases", legacy = legacy, filters = filters, size = size, fields = NULL)
        file_meta1 = gdccurl(url = url, fields = fields_example)
        dt = as.data.table(file_meta1$data$hits)
        return(dt[,sapply(dt, function(x) !inherits(x, c("list", "List", "AsIs"))),with = FALSE])
    })
    ## browser()
    return(rbindlist(ret_lst))
}


grab_by_file_name = function(file_names, mc.cores = 1, mc.preschedule = TRUE, size = 1)
{
    mclapply(file_names, function(ix)
    {
        tryCatch(
        {
            filters2 = list(op = "in", content = list(field = "files.file_name", value = ix))
            url2 = gdc_url("files", TRUE, filters2)
            bla2 = gdccurl(url2, fields = "metadata_files.file_id,metadata_files.file_name,metadata_files.data_format,metadata_files.data_category,metadata_files.data_type", size = size)
            analysis_file = as.data.table(bla2$data$hits$metadata_files[[1]])[data_type == "Analysis Metadata"]
            setnames(analysis_file, paste0("analysis_", names(analysis_file)))
            analysis_file[, analysis_id := sub("\\_analysis.xml", "", analysis_file_name)]
            file_id = bla2$data$hits$id

            filters = list(op = "in", content = list(field = "files.file_id", value = file_id))
            url = gdc_url("files", legacy = TRUE, filters)
            bla = gdccurl(url, size = size)
            dt = as.data.table(bla$data$hits)

            bla2 = gdccurl(url, fields = "cases.case_id,cases.submitter_id", size = size)
            dt2 = as.data.table(bla2$data$hits$cases)
            setnames(dt2, paste0("cases.", names(dt2)))

            build_q = cbind(dt, dt2)
            build_q = cbind(build_q, analysis_file)
            build_q[, id := NULL]
            return(build_q)
        }, error = function(e) "try-error")
    }, mc.cores = mc.cores, mc.preschedule = mc.preschedule)
}


library(httr)
library(jsonlite)
getBarcode <- function(uuid, legacy = TRUE){
    # Get manifest using the API
    uuid <- tolower(uuid)    
    baseURL <- ifelse(legacy,"https://api.gdc.cancer.gov/legacy/files/?","https://api.gdc.cancer.gov/files/?")
    options.pretty <- "pretty=true"
    options.expand <- "expand=cases.samples.portions.analytes.aliquots"
    options.field <- "fields=cases.samples.portions.analytes.aliquots.submitter_id"
    option.size <- paste0("size=",length(uuid))
    option.format <- paste0("format=JSON")
    options.filter <- paste0("filters=",
                             URLencode('{"op":"and","content":[{"op":"in","content":{"field":"files.file_id","value":['),
                             paste0('"',paste(uuid,collapse = '","')),
                             URLencode('"]}}]}'))
    
    url <- paste0(baseURL,paste(options.pretty, options.expand,option.size, 
                                options.filter, options.field,
                                option.format, sep = "&"))
    json  <- tryCatch(
        fromJSON(url, simplifyDataFrame = TRUE),
        error = function(e) {
            fromJSON(content(GET(url), as = "text", encoding = "UTF-8"), simplifyDataFrame = TRUE)
        }
    )
    dt = rrrepeated(json$data$hits$cases, 8, function(x) unlist(x, recursive = FALSE)) %>% do.call(what = "rbind") %>% setRownames(nm = NULL) %>% as.data.table()
    dt = dt[, list(file_id = json$data$hits$id, aliquot_id, legacy_sample_barcode = submitter_id)]
    ## df <- stack(unlist(json$data$hits))
    ## barcode <- df[grep("TCGA",df[,1]),1]
    ## df <- data.frame(uuid = uuid, barcode = barcode)
    return(dt)
}


grab_barcode_by_file_id = function(file_ids, max_per_chunk = 100, legacy = TRUE) { ## more than 100 crashes
    make_chunks = function(vec, num_per_chunk = 100) {
        len_to = ceiling(length(vec)/num_per_chunk)
        ids = split(1:length(vec), rep(1:len_to, length.out = length(vec)))
        return(lapply(ids, function(i) vec[i]))
    }
    chunked_ids = make_chunks(file_ids, max_per_chunk)
    lst = lapply(seq_along(chunked_ids), function(i) {
        getBarcode(chunked_ids[[i]], legacy = legacy)
    })
    return(rbindlist(lst))
}
