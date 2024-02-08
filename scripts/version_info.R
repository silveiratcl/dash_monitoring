## Define the path to your Git repository
repo_path <- "C:/Users/silve/OneDrive/Documentos/Academico/POS-DOC_UFSC/@Karon Coral Sol/dash_monitoring/dash_monitoring"  # Replace with the path to your Git repository

# Use system2 to run a Git command to get the commit SHA of the latest commit on "HEAD"
commit_sha <- system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = TRUE) %>%
  trimws() %>%
  as.character() %>%
  substr(1, 8)

# Use another Git command to get the commit date
commit_date <- system2("git", c("log", "-1", "--format=%ci", "HEAD"), stdout = TRUE, stderr = TRUE) %>%
  trimws() %>%
  as.character() %>%
  substr(1, 10)


commit_sha =  "d744714d"

commit_date = "2024-01-09"
