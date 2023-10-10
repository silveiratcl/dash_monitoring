## Define the path to your Git repository
repo_path <- "C:/Users/silve/OneDrive/Documentos/Academico/POS-DOC_UFSC/@Karon Coral Sol/dash_monitoring/dash_monitoring"  # Replace with the path to your Git repository

# Use system2 to run a Git command to get the commit SHA of the latest commit on "HEAD"
commit_sha <- trimws(system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = TRUE))
commit_sha <- as.character(commit_sha)
commit_sha <-substr(commit_sha, 1, 8)

# Use another Git command to get the commit date
commit_date <- trimws(system2("git", c("log", "-1", "--format=%ci", "HEAD"), stdout = TRUE, stderr = TRUE))
commit_date <- as.character(commit_date)
commit_date <-substr(commit_date, 1, 10)
