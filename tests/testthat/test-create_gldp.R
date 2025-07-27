library(testthat)
library(glue)
library(GeoLocatoR)

# Test for create_gldp function
test_that("create_gldp creates a valid GeoLocator Data Package", {
  # Test minimal input
  package <- create_gldp(
    title = "Geolocator Data Package example",
    contributors = list(
      list(
        title = "Raphaël Nussbaumer",
        roles = c("ContactPerson", "DataCurator", "ProjectLeader")
      )
    )
  )

  # Test with more detailed metadata
  package_full <- create_gldp(
    title = "Geolocator Data Package example",
    contributors = list(
      list(
        title = "Raphaël Nussbaumer",
        roles = c("ContactPerson", "DataCurator", "ProjectLeader")
      ),
      list(
        title = "Yann Rime",
        roles = c("Researcher")
      )
    ),
    id = "https://doi.org/10.5281/zenodo.13829929",
    licenses = list(list(
      name = "CC-BY-4.0", title = "Creative Commons Attribution 4.0",
      path = "https://creativecommons.org/licenses/by/4.0/"
    )),
    description = "test",
    version = "1.0.1",
    embargo = "2025-01-01",
    keywords = c("Woodland Kingfisher", "intra-african", "multi-sensor geolocator"),
    bibliographicCitation = "Nussbaumer, R.(2024). Woodland Kingfisher (v1.1). Zenodo",
    grants = c("Swiss National Fundation grant no. 354251"),
    relatedIdentifiers = list(
      list(
        relationType = "IsPartOf",
        relatedIdentifier = "10.5281/zenodo.11207081",
        relatedIdentifierType = "DOI"
      )
    )
  )
})

test_that("create_gldp handles invalid inputs", {
  # Test invalid title input
  expect_error(create_gldp(
    title = 123, # Invalid type
    contributors = list(list(title = "Raphaël Nussbaumer"))
  ))

  # Test invalid contributor input (missing title)
  expect_error(create_gldp(
    title = "Valid title",
    contributors = list(list(roles = c("ContactPerson")))
  ))

  # Test invalid schema URL
  expect_error(create_gldp(
    title = "Valid title",
    contributors = list(list(title = "Raphaël Nussbaumer")),
    schema = "invalid_schema_url"
  ))
})
