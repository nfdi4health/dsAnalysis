#### keeps what the analyst wrote in an earlier variables file
#' @title Merge new variable descriptions with an earlier file
#' @description Combines the variables read from the servers with those of an existing config/variables.yml, so a re-run keeps the analyst's work.
#' @details Per variable (matched by name): a label or category label the analyst filled in is kept over the newly read one, and any other fields the analyst added to the old entry stay; all other fields (type, range, categories not manually filled, missing codes, studies) come from `new`. Variables present in `old` but absent from `new` are dropped and their names returned, so the caller can report them. This is a local, in-memory merge of two R lists already obtained by the caller; it does not itself query or contact any server.
#' @param new A list of variable entries as written by initVariables() (each a list with name, label, type, …).
#' @param old The variables list of the earlier file, or NULL.
#' @return A list with variables (the merged entries, in the order of new) and dropped (names of earlier variables that are gone).
internal_variables_merge <- function(new, old){

  if (length(old) == 0) {
    return(list(variables = new, dropped = character(0)))
  }
  old_names <- vapply(old, function(v) if (is.null(v$name)) "" else as.character(v$name), character(1))
  new_names <- vapply(new, function(v) v$name, character(1))
  filled <- function(x) !is.null(x) && nzchar(as.character(x))

  merged <- lapply(new, function(v){
    position <- match(v$name, old_names)
    if (is.na(position)) {
      return(v)
    }
    before <- old[[position]]
    if (filled(before$label)) {
      v$label <- before$label
    }
    if (!is.null(v$categories) && is.list(before$categories)) {
      for (code in names(v$categories)) {
        if (filled(before$categories[[code]])) {
          v$categories[[code]] <- before$categories[[code]]
        }
      }
    }
    extra <- setdiff(names(before), c(names(v), "name", "label", "type", "range", "categories", "missing_codes", "studies"))
    c(v, before[extra])
  })

  list(variables = merged, dropped = setdiff(old_names[nzchar(old_names)], new_names))
}
