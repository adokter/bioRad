#' Format references for printing
#'
#' Helper function to format   list of bibentry objects as character strings
#' in "Author (Year)" format, with "et al." for multiple authors.
#'
#' @param refs A list of bibentry objects.
#' @returns A character vector of formatted reference strings.
#' @noRd
format_references <- function(refs) {
  ref_strings <- character(length(refs))
  for (i in seq_along(refs)) {
    ref <- refs[[i]]
    first_author <- ref$author[[1]]
    initial <- if (length(first_author$given) > 0) {
      paste0(substr(first_author$given[1], 1, 1), ".")
    } else {
      ""
    }
    author_str <- paste0(
      first_author$family,
      if (initial != "") paste0(", ", initial) else ""
    )
    if (length(ref$author) > 1) {
      author_str <- paste0(author_str, " et al.")
    }
    ref_strings[i] <- paste0(author_str, " (", ref$year, ")")
  }
  return(ref_strings)
}

#' Print references for an object
#'
#' Helper function to print references from an object's attributes$references,
#' respecting the current width option and adding   reminder about get_bibliography().
#'
#' @param x An object with attributes$references.
#' @param prefix Character string to prefix the reference line (e.g., "     references: ").
#' @noRd
print_references <- function(x, prefix = "      references: ") {
  if (!is.null(x$attributes) && !is.null(x$attributes$references)) {
    ref_strings <- format_references(x$attributes$references)
    ref_text <- paste(ref_strings, collapse = "; ")
    # Truncate if too long, respecting getOption("width")
    width <- getOption("width")
    available_width <- max(10, width - nchar(prefix) - 21) # Reserve space for " (use get_bibliography())"
    if (nchar(ref_text) > available_width) {
      cat(
        prefix,
        sep = "",
        substr(ref_text, 1, available_width),
        "... (use get_bibliography())\n"
      )
    } else {
      cat(prefix, ref_text, " (use get_bibliography())\n")
    }
  }
}

#' Get bibliography from an object
#'
#' Retrieve the bibliography/references stored in an object's attributes.
#' Works with vpts, pvol, and other objects that have an `attributes$references` field.
#'
#' @param x An object (e.g., vpts, pvol) with `attributes$references`.
#' @param format Character string indicating the output format. Either "R" (default)
#'   for R bibentry objects, or "bibtex" for BibTeX format.
#' @returns If `format = "R"`,   list of bibentry objects from the object's
#'   `attributes$references`. If `format = "bibtex"`,   character vector of
#'   BibTeX entries. Returns `NULL` if no references exist or if the object
#'   doesn't have the expected structure.
#' @export
#' @examples
#' # Get bibliography as R objects (default) from   vpts object
#' bibliography <- get_bibliography(example_vpts)
#'
#' # Get bibliography as BibTeX from   vpts object
#' bibtex_bibliography <- get_bibliography(example_vpts, format = "bibtex")
get_bibliography <- function(x, format = c("R", "bibtex")) {
  stopifnot(
    is.list(x) &&
      !is.null(x$attributes) &&
      inherits(x, c("vpts", "pvol"))
  )

  format <- match.arg(format)

  if (is.null(x$attributes$references)) {
    return(NULL)
  }

  refs <- x$attributes$references

  if (format == "R") {
    return(refs)
  } else {
    # Convert to BibTeX format
    return(utils::toBibtex(refs))
  }
}
