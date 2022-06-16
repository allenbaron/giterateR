
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


disclose_ref <- function(x) {
  UseMethod("disclose_ref")
}

#' @export
disclose_ref.git_commit <- function(x) {
  paste(sub("git_", "", class(x)), x$sha, sep = ":")
}

#' @export
disclose_ref.git_tag <- function(x) {
  paste("commit", x$target, sep = ":")
}

#' @export
disclose_ref.git_branch <- function(x) {
  paste(sub("git_", "", class(x)), x$name, sep = ":")
}


inform_head <- function(repo = ".") {
  if (!is_string(repo) && !class(repo) == "git_repository") {
    stop("`repo` must be a git2r git_repository or a path string.")
  }
  head_info <- git2r::repository_head(repo)
  repo_name <- disclose_repo(head_info)
  obj_id <- disclose_ref(head_info)

  msg <- paste0(repo_name, "@", obj_id)
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

is_boolean <- function(x) {
  is.logical(x) && length(x) == 1
}

# State Restoration -------------------------------------------------------

anchor_head <- function(repo, envir = parent.frame()) {
  .head <- git2r::repository_head(repo)
  withr::defer(git2r::checkout(.head), envir = envir)
}
