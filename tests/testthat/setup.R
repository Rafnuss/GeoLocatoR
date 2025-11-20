# This file runs once before all tests
# It creates shared pkg objects that can be used across all test files

# Download and save the first package once
pkg_shared <- read_gldp(
  "https://zenodo.org/records/17367320/files/datapackage.json"
)

# Download and save the second package once (used in test-merge_gldp.R)
pkg2_shared <- read_gldp(
  "https://zenodo.org/records/15259763/files/datapackage.json"
)
