# function updates "treatment" and "structure" in a user's query to match the
# appropriate columns in the database.

#' @importFrom stringr str_extract_all 
#' @noRd

call_update <- function(query){
  
  # query is some user's input (i.e. a query) to the pplr_browse() function
  
  # if the query is null, don't do anything; just return the query
  if(is.null(query)){ 
    return(query) 
  }
  
  # define some regex strings to:
  f_logic <- "[|]+[!]|[&]+[!]|[&|]+"  # ...find logical operators
  f_compn <- "![=]|[<=>]=|<|>|%in%"   # ...find comparison operators
  
  # defne replacement strings for treatment and structure
  r_trt <- "(treatment_type_1 @@ @ @@@ 
  treatment_type_2 @@ @ @@@ 
  treatment_type_3 @@ @)"
  
  r_str <- "(structured_type_1 @@ @ @@@ 
  structured_type_2 @@ @ @@@ 
  structured_type_3 @@ @ @@@ 
  structured_type_4 @@ @)"
  
  # convert query to character string and remove spaces
  query_str <- gsub(" ", "", paste0(deparse(query), collapse = ""))
  
  # split query_str on logical operators to get individual arguments
  query_arg <- unlist(strsplit(query_str, f_logic))
  
  # make a matrix where col1 is the LHS of each query_arg and col2 is the RHS
  LHS_RHS <- matrix(unlist(strsplit(query_arg, f_compn)),
                    ncol = 2,
                    byrow = TRUE)
  
  # find logical operators in the query by searching query_str
  logic <- c(stringr::str_extract_all(query_str, f_logic)[[1]],"")
  
  # find comparison operators in each argument by searching query_arg
  comps <- unlist(stringr::str_extract_all(query_arg, f_compn))
  
  # make a list where each element is an argument in the query, updating LHS of
  # an argument when necessary
  query_list = list()
  for(i in seq_len(nrow(LHS_RHS))){
    
    # determine operator in case treatment or structure strings need to change
    op <- ifelse(comps[i] == "!=", "&", "|")
    
    if(LHS_RHS[i, 1] == "treatment"){
      
      # update "treatment" string if necessary, then put query back together
      query_list[i] <- paste(gsub("@", string_eval_local(LHS_RHS[i,2]), 
                                  gsub("@@", comps[i], 
                                       gsub("@@@", op, r_trt))),
                             logic[i])
      
    } else if(LHS_RHS[i,1] == "structure"){
      
      # update "structure" string if necessary, then put query back together
      query_list[i] <- paste(gsub("@", string_eval_local(LHS_RHS[i, 2]),
                                  gsub("@@", comps[i],
                                       gsub("@@@", op, r_str))),
                             logic[i])
      
    } else{
      
      # for any other LHS, just put the query back together
      query_list[i] = paste(LHS_RHS[i,1], comps[i], 
                            string_eval_local(LHS_RHS[i,2]), logic[i])
      
    }
  }
  # slight rewriting to avoid using the "." placeholder from magrittr.
  # this should alleviate NOTES in check, even if it is slightly
  # more verbose
  QueryOut <- paste0(unlist(query_list), collapse = "") 
  TextToParse <- paste0("substitute(", QueryOut ,")", collapse = "")
  
  # collapse query_list to a single string, convert to expression, and return it
  return(eval(parse(text = TextToParse)))
}


#' @noRd
# given a browse() object or a get_data() object, returns an identical browse
# object with full_tbl=TRUE
rebrowse <- function(input, ...){
  UseMethod("rebrowse")
}


#' @noRd 
rebrowse.browse <- function(input, ...) {
  pmk <- paste0(input$proj_metadata_key, collapse=",")
  return(eval(parse(text = paste0("pplr_browse(proj_metadata_key %in% c(",
                                  pmk,
                                  "), full_tbl=TRUE)"))))
}

#' @noRd
rebrowse.get_data <- function(input, ...) {
  pmk <- paste0(attributes(input)$unique_projects, collapse=",")
  return(eval(parse(text = paste0("pplr_browse(proj_metadata_key %in% c(", 
                                  pmk,
                                  "), full_tbl=TRUE)"))))
}


#' @noRd
# Converts factor columns into character format
factor_to_character <- function(x, full_tbl = FALSE){
  
  for(i in 1:ncol(x)){
    if(class(x[,i])=="factor") x[ ,i] <- as.character(x[ ,i])
  }
  return(x)
  
}

# service functions that 'talk' to API -----------------------------------------

popler_base <- "https://popler.space"
cc          <- function(l) Filter(Negate(is.null), l)

#' @importFrom crul HttpClient
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @noRd
# get actual data from the API
pop_GET <- function(path, args, ...) {
  cli <- crul::HttpClient$new(url = popler_base, opts = list(...))
  res <- cli$get(path = path, query = cc(args))
  res$raise_for_status()
  txt <- res$parse("UTF-8")
  dat <- jsonlite::fromJSON(txt)
  dat$data <- tibble::as_tibble(dat$data)
  return(dat)
}

#' search 
#' 
#' @export
#' @param limit number of records to return, default: 10
#' @param offset record number to start at, default: first record
#' @param ... curl options passed on to [crul::HttpClient]
#' @examples
#' # basic example
#' pplr_summary()
#' # pass in curl options for debugging, seeing http request details
#' pplr_summary(verbose = TRUE)
pplr_summary <- function(limit = 10, offset = 0, ...) {
  args <- list(limit = limit, offset = offset)
  pop_GET("summary", args, ...)
}

#' search 
#' 
#' @export
#' @param proj_metadata_key project metadata key
#' @param limit number of records to return, default: 10
#' @param offset record number to start at, default: first record
#' @param ... curl options passed on to [crul::HttpClient]
#' @examples
#' # basic example
#' pplr_search(proj_metadata_key = 13)
#' # pass in curl options for debugging, seeing http request details
#' pplr_search(proj_metadata_key = 13, verbose = TRUE)
pplr_search <- function(proj_metadata_key, limit = 10, offset = 0, ...) {
  args <- list(proj_metadata_key = proj_metadata_key, 
               limit = limit, offset = offset)
  
  # get the data
  ds_out <- pop_GET("search", args, ...)
  
  # format structure_type_ columns
  ds_out$data$structure_type_1 <- as.character(ds_out$data$structure_type_1)
  ds_out$data$structure_type_2 <- as.character(ds_out$data$structure_type_2)
  ds_out$data$structure_type_3 <- as.character(ds_out$data$structure_type_3)
  ds_out$data$structure_type_4 <- as.character(ds_out$data$structure_type_4)
  
  ds_out
  
}

#' @noRd
# set offsets and limits to download summary table through the API
# (The API only downloads 1000 rows at a time)
offset_limit_summary <- function( ){

  # count the number of rows in the dataset
  count_summ  <- pplr_summary( limit = 10, offset = 0 )$count  
    
  # total number of "offsets" and "limits"
  n_download  <- count_summ %/% 1000
  rest        <- count_summ %% 1000
  
  # "offsets" and "limits"
  download_v  <- 1000 * c(0:n_download )
  limit_v     <- rep(1000, length(download_v) )

  if( rest != 0 ) limit_v[length(download_v)] <- rest
  
  list(offset_v   = download_v,
       limit_v    = limit_v )

}

#' @noRd
# set offsets and limits to download whole datasets through the API
# (The API only downloads 1000 rows at a time)
offset_limit_search <- function( proj_id ){

  # count the number of rows in the dataset
  count_summ  <- pplr_search( proj_id, limit = 10, offset = 0 )$count  
    
  # total number of "offsets" and "limits"
  n_download  <- count_summ %/% 1000
  rest        <- count_summ %% 1000
  
  # "offsets" and "limits"
  download_v  <- as.integer( 1000 * c(0:n_download ) )
  limit_v     <- rep(1000L, length(download_v) ) 

  # update limit
  if( rest != 0 ) limit_v[length(download_v)] <- rest
  
  # IMPORTANT: if rest == 0, then # rows is perfectly round.
  if( rest == 0 ){
    true_length <- length(download_v) - 1
    download_v  <- download_v[1:true_length]
    limit_v     <- limit_v[1:true_length]
  } 
  
  list(offset_v   = download_v,
       limit_v    = limit_v )

}

# generate main data table summary ---------------------------------------------

#' @title Update \code{popler}'s summary table
#' 
#' @description Automatically retrieve most up to date version of \code{popler}
#' summary table
#' 
#' @return This function is called for its side effect and does not return 
#' anything
#' 
#' @note The \code{summary_table} is often called internally by popler functions,
#'  but can also be accessed directly by calling \code{pplr_summary_table_import()}. 
#' 
#' @export
#' @importFrom utils txtProgressBar setTxtProgressBar

pplr_summary_table_update <- function(){
  
  message("Please wait while popler updates its summary table... this may take several minutes.")
  
  # set up list of variables to retain
  
  # list all columns
  taxa_cols <- c("taxa_table_key","site_in_project_taxa_key", "sppcode",                 
                "kingdom", "subkingdom", "infrakingdom",            
                "superdivision", "division", "subdivision",             
                "superphylum", "phylum", "subphylum",               
                "clss", "subclass", "ordr",                    
                "family", "genus", "species",
                "common_name", "authority", "metadata_taxa_key")

  lter_cols <- c( "lterid", "lter_name", "lat_lter",                      
                  "lng_lter", "currently_funded", "current_principle_investigator",
                  "current_contact_email", "alt_contact_email", "homepage" )

  proj_cols <- c( "proj_metadata_key",                                
                   "lter_project_fkey",                                
                   "title",                                            
                   "samplingunits",                                    
                   "datatype",                                         
                   "structured_type_1",                                
                   "structured_type_1_units",                         
                   "structured_type_2",                          
                   "structured_type_2_units",
                   "structured_type_3",                                
                   "structured_type_3_units",                          
                   "studystartyr",                                     
                   "studyendyr",                                       
                   "samplefreq",                                       
                   "studytype",                                        
                   "community",                                        
                   "spatial_replication_level_1_extent",               
                   "spatial_replication_level_1_extent_units",         
                   "spatial_replication_level_1_label",                
                   "spatial_replication_level_1_number_of_unique_reps",
                   "spatial_replication_level_2_extent",               
                   "spatial_replication_level_2_extent_units",         
                   "spatial_replication_level_2_label",                
                   "spatial_replication_level_2_number_of_unique_reps",
                   "spatial_replication_level_3_extent",               
                   "spatial_replication_level_3_extent_units",         
                   "spatial_replication_level_3_label",                
                   "spatial_replication_level_3_number_of_unique_reps",
                   "spatial_replication_level_4_extent",              
                   "spatial_replication_level_4_extent_units",
                   "spatial_replication_level_4_label",                
                   "spatial_replication_level_4_number_of_unique_reps",
                   "spatial_replication_level_5_extent",               
                   "spatial_replication_level_5_extent_units",
                   "spatial_replication_level_5_label",        
                   "spatial_replication_level_5_number_of_unique_reps",
                   "treatment_type_1",
                   "treatment_type_2",                                 
                   "treatment_type_3",                                 
                   "control_group",                                    
                   "derived",                                          
                   "authors",                                          
                   "authors_contact",
                   "metalink",                                  
                   "knbid",                                        
                   "structured_type_4",                                
                   "structured_type_4_units",
                   "duration_years",
                   "doi",                                              
                   "doi_citation",                                     
                   "structured_data") 
  
  search_cols <- paste( c(proj_cols,lter_cols,taxa_cols), collapse = ", ")
  
  search_cols[!search_cols %in% c("currently_funded",
                                  "homepage",
                                  "current_principle_investigator")]
  
  # set limits 
  query_in <- offset_limit_summary( )
  
  # set up progress bar
  total    <- length( query_in$limit_v )
  prog_bar <- utils::txtProgressBar(min = 0, max = total, style = 3)
  
  # actually download summary table
  downld_summary <- function(lim,off,i){
                      utils::setTxtProgressBar(prog_bar, i)
                      pplr_summary( limit = lim, offset = off )$data
                    }
  
  # read summary table piecewise
  out_l  <- Map( downld_summary,
                 query_in$limit_v,
                 query_in$offset_v,
                 1:length(query_in$limit_v) )
  
  # notify that the brunt of the wait is over
  message("Download complete, just a few moments to format the summary table!")
  
  # put it all together
  out <- Reduce( function(...) rbind(...), out_l ) %>% as.data.frame 

  # formatting -----------------------------------------------------------
  
  # Select project-specific information 
  proj_info             <- out[,c(proj_cols,lter_cols)]
  
  # Substitute "NA" for NAs introduced by rbind
  replace_na            <- function(x) replace(x, is.na(x), 'NA')
  
  # introduce "NA" only in character columns
  chr_cols              <- Filter(function(x) x == 'character',
                                  sapply(proj_info, class)) %>% names
  chr_ids               <- which( c(proj_cols,lter_cols) %in% chr_cols )

  # ugly loop to change one column at a time (if needed)
  for(ii in 1:length(chr_ids) ){

    if( sum( is.na(proj_info[,chr_ids[ii]]) ) > 0 ){
      proj_info[,chr_ids[ii]] = replace_na( proj_info[,chr_ids[ii]] )
    }

  }
  
  out_proj              <- unique(proj_info)
  
  # strings for spatial_replication_level_X_number_of_unique_reps
  sr_colnames <- paste0("spatial_replication_level_",1:5,"_number_of_unique_reps")
  
  # change any spatial replication -99999 to NA
  out_proj[,sr_colnames][out_proj[,sr_colnames] == -99999] <- NA
  
  # add column for total spatial replicates
  out_proj$tot_spat_rep <- apply(out_proj[,sr_colnames], 1, prod, na.rm=TRUE)
  
  # add column for number of spatial levels 
  out_proj$n_spat_levs  <- apply(!is.na(out_proj[sr_colnames]), 1, sum) 
  
  # reorder out_proj columns
  # out_proj  <- out_proj[c(1:11,46:47,12:36,58:59,37:45,48:57)]
  out_proj  <- out_proj[c(1:11, 46:47, 12:36, 61:62, 37:45, 48, 52:60, 49:51)]
  
  # return unique taxonomic information for each proj_metadata_key
  taxas    <- taxa_cols[! taxa_cols %in% c("taxa_table_key", 
                                           "site_in_project_taxa_key",
                                           "metadata_taxa_key")]
  
  out_taxa <- unique(out[,c("proj_metadata_key",taxas)])
  
  # merge project and species data
  summary_table <- merge(out_proj, out_taxa)
  
  # convert factors to characters
  summary_table <- factor_to_character(summary_table)
  
  # Case insensitive matching ("lower" everything)
  names(summary_table) <- tolower( names(summary_table) )
  
  # convert columns "ordr" to "order" and "clss" to "class"
  summary_table <- colname_change("clss", "class", summary_table)
  summary_table <- colname_change("ordr", "order", summary_table)
  
  # remove symbol "%" from doi_citation, because problematic when creating citation
  summary_table$doi_citation <- gsub('%',
                                     'percent',
                                     summary_table$doi_citation)

  # variables that need be numeric
  summary_table$lat_lter     <- summary_table$lat_lter %>% as.numeric
  summary_table$lng_lter     <- summary_table$lng_lter %>% as.numeric
  
  # store main data table--------------------------------------------------
  st_file <- paste0(system.file("extdata", package = "popler"),"/summary_table.rda")
  save(summary_table, file = st_file)
  
  message("Finished.")
}



#' @noRd
# Check whether the summary table has been recently updated
pplr_summary_table_check = function(){
  
  wks_passed <- floor(as.numeric(difftime(Sys.time(),
                                          file.mtime(system.file("extdata", 
                                                                 "summary_table.rda",
                                                                 package = "popler")), 
                                          units=c("weeks"))))
  # if summary_table.rda does exist, but was created more than 6 weeeks ago,
  # prompt user to update the table.
  if(wks_passed >= 6){
    message("It's been ", wks_passed, " weeks since popler's summary table has been updated.\nWe recommend running 'pplr_summary_table_update()' to make sure your summary table is up to date with the latest database changes.")
  }
  
}


#' @noRd
# evaluate a string using the local environment, return the evaluation as string
string_eval_local <- function(x){
  paste0(deparse(eval(parse(text = paste0("local(",x,")")))),collapse="")
}

#' @noRd
# changes a column name from one name to another
colname_change = function(from, to, x){
  names(x) <- gsub(from, to, names(x))
  return(x)
}


# Source for idea
# https://stackoverflow.com/questions/30357330/r-cmd-check-no-visible-binding-for-global-variable-mypkgdata
#' @noRd
pplr_summary_table_import <- function() {
  
  # create empty environment for loading
  pkgEnv <- new.env(parent = emptyenv())
  
  # if this has not been called
  if(!exists('summary_table', pkgEnv)) {
    load(system.file("extdata", 
                     "summary_table.rda",
                     package = "popler"), envir = pkgEnv)
  }
  
  # allow assignment so that R CMD check find global binding
  summary_table <- pkgEnv[['summary_table']]
  
  return(summary_table)
}

#' @noRd
# gets DOI or, if not present, URL.
links_get = function( sum_tab_df ){
  
  links   <- sum_tab_df$doi
  no_doi  <- which( links == 'NA' )
  links   <- replace( links, 
                      no_doi, 
                      sum_tab_df$metalink[no_doi] )
  
  # return multiple links if '; ' present
  id_smcol <- grep('; ', links)
  
  # if we find "; ", return many links per study
  if( length(id_smcol) > 0 ){
    
    # split links string into separate links
    split_links <- function(x) strsplit(x, '; ') %>% unlist(recursive = FALSE)

    links_out   <- split_links(links)

    # total number of URLs
    tot_url     <- length(links_out) - length(links)
    
    # warn user that they will get a lot of links
    if( length(id_smcol) > 1 ){
      message(paste0("NOTE! Studies ",
                   sum_tab_df$proj_metadata_key[id_smcol],
                   " are linked to a total of", 
                   tot_url,
                   " URLs") 
              )
    }else{
      message(paste0("NOTE! Study ",
                     sum_tab_df$proj_metadata_key[id_smcol],
                     " is linked to ",
                     tot_url,
                     " URLs")
             )
    }
    
         
  }else{
    links_out   <- links
  }
    
  return(links_out)
  
}
