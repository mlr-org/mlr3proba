check_subsetpattern = function(x, choices, empty.ok = TRUE){
  if(all(grepl(paste0(choices, collapse="|"), x)))
    return(TRUE)
  else
    return(sprintf("Must be a subset of %s, but is %s",
                   paste0("{", paste0(choices, collapse = ", "), "}"),
                   paste0("{", paste0(x, collapse = ", "), "}")))
}
