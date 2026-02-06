# Script to install required packages for the financial analysis agent

# List of required packages
required_packages <- c(
  "conflicted",
  "dplyr",
  "magrittr",
  "lubridate",
  "tidyquant",
  "ecb",
  "emayili",
  "httr"
)

# Install packages if not already installed
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    cat("Installing packages:", paste(new_packages, collapse = ", "), "\n")
    install.packages(new_packages, dependencies = TRUE)
  } else {
    cat("All required packages already installed.\n")
  }
}

# Install required packages
install_if_missing(required_packages)

# If ellmer is needed, install from GitHub
# Uncomment the following lines if you want to use ellmer package instead of direct API calls
# if(!("ellmer" %in% installed.packages()[,"Package"])) {
#   if(!require(remotes)) install.packages("remotes")
#   remotes::install_github("rpkgs/ellmer")
# }

cat("Package installation complete!\n")