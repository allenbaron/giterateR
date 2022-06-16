withr::with_tempdir({ # wrap all to ensure removal of test repository

# Set up test repository --------------------------------------------------
  df <- data.frame(
    branch = c("main", "dev"),
    commit = c(1, 2),
    tag = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  repo_name <- "giterateR_test"
  path_string <- create_test_repo(df, repo_name = repo_name, return_path = TRUE)
  repo <- git2r::repository(path_string)
  commit_list <- git2r::commits(path_string)
  branch_list <- git2r::branches(path_string)
  tag_list <- git2r::tags(path_string)


# Execute tests -----------------------------------------------------------
  test_that("disclose_git_path() works with path string and git2r objects", {
    expected <- normalizePath(
      file.path(path_string, ".git"),
      mustWork = TRUE
    )

    expect_equal(disclose_git_path(repo_name), expected)
    expect_equal(disclose_git_path(path_string), expected)
    expect_equal(disclose_git_path(repo), expected)
    expect_equal(disclose_git_path(commit_list[[1]]), expected)
    expect_equal(disclose_git_path(branch_list[[1]]), expected)
    expect_equal(disclose_git_path(tag_list[[1]]), expected)
  })

  test_that("disclose_repo() works with path string and git2r objects", {
    expect_equal(disclose_repo(repo_name), repo_name)
    expect_equal(disclose_repo(path_string), repo_name)
    expect_equal(disclose_repo(repo), repo_name)
    expect_equal(disclose_repo(commit_list[[1]]), repo_name)
    expect_equal(disclose_repo(branch_list[[1]]), repo_name)
    expect_equal(disclose_repo(tag_list[[1]]), repo_name)
  })

  test_that("inform_head() works on branches", {
    git2r::checkout(branch_list[[1]])
    expected <- paste0(repo_name, "@branch:", branch_list[[1]]$name)

    expect_message(inform_head(repo_name), expected)
    expect_message(inform_head(path_string), expected)
    expect_message(inform_head(repo), expected)
  })

  test_that("inform_head() works on commits", {
    git2r::checkout(commit_list[[2]])
    expected <- paste0(repo_name, "@commit:", commit_list[[2]]$sha)

    expect_message(inform_head(repo_name), expected)
    expect_message(inform_head(path_string), expected)
    expect_message(inform_head(repo), expected)
  })

  test_that("inform_head() works on tags", {
    git2r::checkout(tag_list[[1]])
    expected <- paste0(repo_name, "@commit:", tag_list[[1]]$target)

    expect_message(inform_head(repo_name), expected)
    expect_message(inform_head(path_string), expected)
    expect_message(inform_head(repo), expected)
  })

  test_that("anchor_head() works", {
    # custom test functions
    expect_checkout_restore <- function(repo, git2r_obj, expected) {
      expect_message(
        expect_equal(test_anchor(repo, git2r_obj), expected),
        paste0(repo_name, "@", disclose_ref(git2r_obj))
      )
    }
    anchor_msg_checkout <- function(repo, git2r_obj) {
      anchor_head(repo)
      git2r::checkout(git2r_obj)
      inform_head(repo)
    }
    test_anchor <- function(repo, git2r_obj) {
      anchor_msg_checkout(repo, git2r_obj)
      git2r::repository_head(repo)
    }

    git2r::checkout(branch_list[[2]])
    expected <- git2r::repository_head(repo)

    expect_checkout_restore(repo, branch_list[[1]], expected)
    expect_checkout_restore(path_string, tag_list[[1]], expected)
    expect_checkout_restore(repo, commit_list[[1]], expected)
  })

})
