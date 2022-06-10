disclose_repo <- function(x) {
  if (class(x) %in% paste0("git_", c("commit", "branch", "tag"))) {
    path <- x$repo$path
  } else if (class(x) == "git_repository") {
    path <- x$path
  } else if (is_string(x)) {
    path <- git2r::repository(x)
  } else {
    stop("`x` must be a git2r object or a path string.")
  }

  sub(".*/([^/]+)/.git", "\\1", path)
}


inform_head <- function(repo = ".") {
  if (!is_string(repo) && !class(repo) == "git_repository") {
    stop("`repo` must be a git2r git_repository or a path string.")
  }
  head_info <- git2r::repository_head(repo)
  repo_name <- disclose_repo(head_info)
  head_type <- sub("git_", "", class(head_info))
  head_id <- if ("name" %in% names(head_info)) head_info$name else head_info$sha

  msg <- paste0(repo_name, "@", head_type, ":", head_id)
  message(msg)

  invisible(repo)
}


is_string <- function(x) {
  is.character(x) && length(x) == 1
}
