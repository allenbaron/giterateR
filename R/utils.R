
# Informational -----------------------------------------------------------

disclose_git_path <- function(x) {
  if (is_git2r_ref(x)) {
    path <- x$repo$path
  } else if (is_git2r_repo(x)) {
    path <- x$path
  } else if (is_string(x)) {
    path <- git2r::repository(x)$path
  } else {
    stop("`x` must be a git2r object or a path string.")
  }
  path
}


disclose_repo <- function(x) {
  git_path <- disclose_git_path(x)
  sub(".*/([^/]+)/.git", "\\1", git_path)
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


# Type Predicates ---------------------------------------------------------

is_string <- function(x) {
  is.character(x) && length(x) == 1
}

is_git2r_ref <- function(x) {
  class(x) %in% paste0("git_", c("commit", "branch", "tag"))
}

is_git2r_repo <- function(x) {
  class(x) == "git_repository"
}


# State Restoration -------------------------------------------------------

anchor_head <- function(repo, envir = parent.frame()) {
  .head <- git2r::repository_head(repo)
  withr::defer(git2r::checkout(.head), envir = envir)
}
