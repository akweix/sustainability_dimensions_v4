
# List of required packages
required_packages <- c("plotly", "readxl")

# Function to check and install packages
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
    }
  }
}

# Install missing packages
install_if_missing(required_packages)
