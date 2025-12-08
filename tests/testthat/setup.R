# This file runs once before all tests
# It creates shared pkg objects that can be used across all test files

# Download and save the first package once
pkg_shared <- read_gldp("17367320")

# Download and save the second package once (used in test-merge_gldp.R)
pkg2_shared <- read_gldp("15259763")
