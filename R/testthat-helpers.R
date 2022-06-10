create_test_repo <- function(setup_df, repo_name) {
  validate_setup_df(setup_df)
  dir.create(repo_name)

  repo <- git2r::init(repo_name)
  git2r::config(repo, user.name = "giterateR", user.email = "fake@example.org")

  commit_info <- lapply(
    1:nrow(setup_df),
    create_test_repo_ref, path = repo_name, setup_df = setup_df
  )

  repo_name
}


create_test_repo_ref <- function(path, setup_df, row) {
  df1 <- setup_df[row, ]
  current_branch <- git2r::repository_head(path)$name
  branches <- names(git2r::branches(path))

  if (is.null(current_branch) || !df1$branch %in% branches) {
    git2r::checkout(path, df1$branch, create = TRUE)
  } else if (df1$branch != current_branch) {
      git2r::checkout(path, df1$branch)
  }

  utils::write.csv(df1, file.path(path, "test_file.csv"), row.names = FALSE)
  git2r::add(path, "test_file.csv")
  commit_msg <- paste0(df1$branch, " commit ", df1$commit, " msg")
  git2r::commit(path, commit_msg)

  if (df1$tag) {
    tag_id <- paste0(df1$branch, df1$commit)
    tag_msg <- paste0(tag_id, "tag msg")
    git2r::tag(path, tag_id, tag_msg)
  }

  invisible(git2r::repository_head(path))
}


validate_setup_df <- function(setup_df) {
  stopifnot(
    all(c("branch", "commit", "tag") %in% names(setup_df))
  )
  stopifnot(
    with(setup_df, all(!is.na(branch) & !is.na(commit)))
  )
}
