
#' `replace_hard_indices` replace expressions of the form `variable[,j]` by
#'  the corresponding `variable$jth_name`, where `jth_name <- name_list[j]`
#'
#' @param srcfile
#' @param variable
#' @param cnames
replace_hard_indices <- function(srcfile, variable, name_list = NULL, type = 'vec'){

  stopifnot(is.character(variable))
  src  <- readLines(srcfile)

  if ( is.null(name_list) ) {
    name_list <- switch (type,
                         'vec' =  names(get(variable)),
                         'col' =  colnames(get(variable)),
                         'row' =  rownames(get(variable)))
  }
  stopifnot(!is.null(name_list))

  isValidName <- function(string) {
    grepl("^((([[:alpha:]]|[.][._[:alpha:]])[._[:alnum:]]*)|[.])$", string)
  }

  for ( j in seq_along(name_list) ){

    if ( !isValidName(name_list[j]) ) { name_list[j] <- paste0("`",name_list[j],"`") }

    pat <- switch (type,
                   'vec' = paste0(variable,"[",j,"]"),
                   'col' = paste0(variable,"[,",j,"]"),
                   'row' = paste0(variable,"[",j,",]"),
    )
    rep <- switch (type,
                   'vec' = paste0(variable,"$",name_list[j]),
                   'col' = paste0(variable,"$",name_list[j]),
                   'row' = paste0(variable,"[",name_list[j],",]")
    )
    src  <- gsub(pattern = pat, replace = rep, x = src, fixed = TRUE)

    if ( type != 'vec' ){
      pat <- switch (type,
                     'col' = paste0(variable,"\\[([^,]*),",j,"\\]"),
                     'row' = paste0(variable,"\\[",j,",([^,]*)\\]"),
      )
      rep <- switch (type,
                     'col' = paste0(variable,"$",name_list[j],"\\[\\1\\]"),
                     'row' = paste0(variable,"\\[",name_list[j],",\\1\\]")
      )
      src  <- gsub(pattern = pat, replace = rep, x = src)
    }

  }

  writeLines(src, con=srcfile)
}

