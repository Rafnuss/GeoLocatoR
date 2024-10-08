#' Read Geolocator Data and Orders
#'
#' This set of functions facilitates the reading and combining of Geolocator Data (GDL) and
#' associated order information from various sources.
#' The main function, `read_gdl()`, allows for reading data from an Access file or directly from
#' separate CSV files containing GDL data and order information.
#' It returns a data frame that integrates GDL data with summarized order details.
#'
#' @param access_file A string specifying the path to an Access file containing both the GDL data
#' and order information.
#' If provided, it takes precedence over `file_data` and `file_order`. Defaults to `NA`.  §
#' @param file_data A string specifying the path to the GDL data file. Required if `access_file`
#' is not provided. Defaults to `NA`.
#' @param file_order A string specifying the path to the GDL order file. Required if `access_file`
#' is not provided. Defaults to `NA`.
#' @param filter_col A logical value or a character vector. If `TRUE`, only a predefined set of
#' columns is selected.
#' If `FALSE`, all columns are included. Alternatively, a character vector can be passed to specify
#' which columns to select. Defaults to `TRUE`.
#'
#' @return A data frame combining GDL data and summarized order information. The data frame
#' includes columns such as `OrderName`, `Client`, `NumberDelivered`, `GDL_ID`, `Species`, and more,
#'  depending on the value of `filter_col`.
#'
#' @details
#' The `read_gdl()` function is the primary interface for reading and combining GDL data with
#' orders. It can read from an Access file or from separate data and order files.
#' If an Access file is provided, the `read_gdl_access()` function is used to extract the GDL data
#' and order information. Otherwise, the `read_gdl_data()` and `read_gdl_orders()`
#' functions are employed to read the data from specified CSV files.
#'
#' - **`read_gdl()`**: Reads GDL and order data, either from an Access file or separate CSV files,
#' and returns a combined data frame.
#' - **`read_gdl_orders()`**: Reads GDL order information from a CSV file, including columns like
#' `OrderID`, `OrderName`, `Species`, `Client`, and `NumberDelivered`.
#' - **`read_gdl_data()`**: Reads GDL data from a CSV file, including columns like `DataID`,
#' `OrderName`, `GDL_ID`, `Species`, and spatial and temporal information.
#' - **`read_gdl_access()`**: Extracts GDL data and order information from an Access database file
#' and exports them to temporary CSV files.
#'
#' @export
read_gdl <- function(access_file = NA,
                     file_data = NA,
                     file_order = NA,
                     filter_col = TRUE) {
  if (!is.na(access_file)) {
    data_file_order <- read_gdl_access(access_file)
    file_data <- data_file_order[[1]]
    file_order <- data_file_order[[2]]
  } else {
    if (is.na(file_data) || is.na(file_order)) {
      cli_abort("Either {.var access_file} or both {.var file_data} and {.var file_order} need\
                     to be provided.")
    }
  }

  d <- read_gdl_data(file_data)

  o <- read_gdl_orders(file_order) %>%
    group_by(.data$OrderName) %>%
    summarize(
      NumberOrdered = sum(.data$NumberOrdered),
      NumberDelivered = sum(.data$NumberDelivered),
      across(
        where(~ !is.numeric(.)),
        ~ paste(unique(.), collapse = ", ") # Concatenate unique values for non-numeric columns
      )
    ) %>%
    select(-c("GDL_Type", "Species"))

  gdl <- left_join(d, o,
    by = c("OrderName"),
    suffix = c("_data", "_order")
  )

  if (is.logical(filter_col)) {
    if (filter_col) {
      gdl <- select(
        gdl,
        c(
          # Order
          "OrderName",
          "Client",
          "ResponsibleP3",
          "ClientCategory",
          "NumberDelivered",
          "RemarksProduction",
          "RemarksOrder",

          # GDL
          "GDL_ID",
          "GDL_Type",
          "Species",
          "UTC_Attached",
          "LongitudeAttached",
          "LatitudeAttached",
          "UTC_Removed",
          "LongitudeRemoved",
          "LongitudeRemoved",
          "LongitudeAttached"
        )
      )
    }
  } else {
    gdl <- select(gdl, "filter_col")
  }

  return(gdl)
}

#' @rdname read_gdl
#' @export
read_gdl_orders <- function(file_order) {
  o <- readr::read_csv(
    file_order,
    col_types = readr::cols(
      OrderID = readr::col_integer(),
      OrderName = readr::col_character(),
      Species = readr::col_character(),
      Client = readr::col_character(),
      ResponsibleP3 = readr::col_character(),
      ClientCategory = readr::col_factor(c(
        "own", "cooperation", "coop", "purchase"
      )),
      GDL_Type = readr::col_character(),
      # readr::col_factor(c("GDL1", "GDL2", "GDL3pam", "uTag")),
      NumberOrdered = readr::col_integer(),
      OrderStatus = readr::col_factor(c("ordered", "delivered", "request")),
      NumberDelivered = readr::col_integer(),
      DeliveryDate = readr::col_datetime(format = "%m/%d/%y %H:%M:%S"),
      DateDelivered = readr::col_datetime(format = "%m/%d/%y %H:%M:%S"),
      UnitPrice = readr::col_double(),
      Currency = readr::col_factor(c("CHF")),
      InvoiceDate = readr::col_datetime(format = "%m/%d/%y %H:%M:%S"),
      RemarksProduction = readr::col_character(),
      RemarksOrder = readr::col_character(),
      IntervalBlue = readr::col_integer(),
      LoggingPeriodEnabled = readr::col_logical(),
      StartPeriod1 = readr::col_datetime("%m/%d/%y %H:%M:%S"),
      IntervalRGC = readr::col_integer(),
      GreenEnabled = readr::col_logical(),
      RedEnabled = readr::col_logical(),
      ClearEnabled = readr::col_logical(),
      LightLevelThreshold = readr::col_integer(),
      BaseLoggingInterval = readr::col_integer(),
      IntervalFactorLight = readr::col_integer(),
      IntervalFactorAirTemperature = readr::col_integer(),
      IntervalFactorBodyTemperature = readr::col_integer(),
      IntervalFactorPressure = readr::col_integer(),
      IntervalFactorAcceleration = readr::col_integer(),
      IntervalFactorMagnetic = readr::col_integer(),
      LWL = readr::col_logical(),
      LWL_Length = readr::col_integer(),
      LWL_Diameter = readr::col_integer(),
      Harness = readr::col_character(),
      HarnessMaterial = readr::col_character(),
      HarnessAttachement = readr::col_character(),
      HarnessThickness = readr::col_double(),
      LegHarnessDiameter = readr::col_double(),
      BreastHarnessDiameterHead = readr::col_double(),
      BreastHarnessDiameterTail = readr::col_double(),
      HarnessDescription = readr::col_character(),
      AccSamplingRate = readr::col_logical(),
      AccXaxisEnabled = readr::col_logical(),
      AccYaxisEnabled = readr::col_logical(),
      AccZaxisEnabled = readr::col_logical(),
      DataloggerEnabled = readr::col_logical(),
      RadioEnabled = readr::col_logical(),
      Frequencies = readr::col_character(),
      TransmitPower = readr::col_double(),
      WakeUpTrigger = readr::col_double(),
      PatternID = readr::col_double(),
      PulseWidth = readr::col_double(),
      PulseInterval = readr::col_double(),
      PatternInterval = readr::col_double(),
      PulsePattern = readr::col_character(),
      TRSchedule = readr::col_character(),
      LoggingSchedule = readr::col_character(),
      ActiveAfterWakeUp = readr::col_double(),
      PrioritySensor = readr::col_logical(),
      PriorityMemory = readr::col_double()
    )
  ) %>% arrange(.data$OrderName)
  return(o)
}



#' @rdname read_gdl
#' @export
read_gdl_data <- function(file_data) {
  d <- readr::read_csv(
    file_data,
    col_types = readr::cols(
      DataID = readr::col_integer(),
      OrderName = readr::col_character(),
      GDL_ID = readr::col_character(),
      RingNumber = readr::col_character(),
      Species = readr::col_character(),
      SiteAttached = readr::col_character(),
      UTC_Attached = readr::col_datetime("%m/%d/%y %H:%M:%S"),
      LongitudeAttached = readr::col_double(),
      LatitudeAttached = readr::col_double(),
      DateDeparture = readr::col_datetime("%m/%d/%y %H:%M:%S"),
      DateArrival = readr::col_datetime("%m/%d/%y %H:%M:%S"),
      UTC_Removed = readr::col_datetime("%m/%d/%y %H:%M:%S"),
      LatitudeRemoved = readr::col_double(),
      DateDownload = readr::col_datetime("%m/%d/%y %H:%M:%S"),
      UTC_StartRTC = readr::col_datetime("%m/%d/%y %H:%M:%S"),
      UTC_StopRTC = readr::col_datetime("%m/%d/%y %H:%M:%S"),
      UTC_StopReference = readr::col_datetime("%m/%d/%y %H:%M:%S"),
      UTC_FirstData = readr::col_datetime("%m/%d/%y %H:%M:%S"),
      UTC_LastData = readr::col_datetime("%m/%d/%y %H:%M:%S"),
      DataFileName = readr::col_character(),
      RooftopCalFileName = readr::col_character(),
      UTC_StartRooftopCal = readr::col_datetime("%m/%d/%y %H:%M:%S"),
      UTC_StopRooftopCal = readr::col_datetime("%m/%d/%y %H:%M:%S"),
      LongitudeRooftopCal = readr::col_double(),
      LatitudeRooftopCal = readr::col_double(),
      UTC_StartStatCal1 = readr::col_datetime("%m/%d/%y %H:%M:%S"),
      UTC_StopStatCal1 = readr::col_datetime("%m/%d/%y %H:%M:%S"),
      LongitudeStatCal1 = readr::col_double(),
      LatitudeStatCal1 = readr::col_double(),
      UTC_StartStatCal2 = readr::col_datetime("%m/%d/%y %H:%M:%S"),
      UTC_StopStatCal2 = readr::col_datetime("%m/%d/%y %H:%M:%S"),
      LongitudeStatCal2 = readr::col_double(),
      LatitudeStatCal2 = readr::col_double(),
      NightBaseline = readr::col_double(),
      LevelThreshold = readr::col_double(),
      GDL_Type = readr::col_character(),
      HardwareVersion = readr::col_character(),
      FirmwareVersion = readr::col_character(),
      IntervalBlue = readr::col_integer(),
      LoggingPeriodEnabled = readr::col_logical(),
      UTC_StartLog1 = readr::col_datetime("%m/%d/%y %H:%M:%S"),
      IntervalRGC = readr::col_double(),
      GreenEnabled = readr::col_logical(),
      RedEnabled = readr::col_logical(),
      ClearEnabled = readr::col_logical(),
      LightLevelThreshold = readr::col_double(),
      BaseLoggingInterval = readr::col_integer(),
      IntervalFactorLight = readr::col_integer(),
      IntervalFactorAirTemperature = readr::col_integer(),
      IntervalFactorBodyTemperature = readr::col_integer(),
      IntervalFactorPressure = readr::col_integer(),
      IntervalFactorAcceleration = readr::col_integer(),
      IntervalFactorMagnetic = readr::col_integer(),
      LWL = readr::col_double(),
      LWL_Length = readr::col_double(),
      LWL_Diameter = readr::col_double(),
      DateBatteryConnected = readr::col_character(),
      VoltageReady = readr::col_double(),
      VoltageEnd = readr::col_double(),
      TotalWeight = readr::col_double(),
      MemoryUsed = readr::col_double(),
      Harness = readr::col_character(),
      HarnessMaterial = readr::col_character(),
      HarnessAttachement = readr::col_character(),
      HarnessThickness = readr::col_double(),
      LegHarnessDiameter = readr::col_double(),
      BreastHarnessDiameterHead = readr::col_double(),
      BreastHarnessDiameterTail = readr::col_double(),
      PrintManufacturer = readr::col_character(),
      FinalAssembly = readr::col_character(),
      Remarks = readr::col_character(),
      Frequency = readr::col_double(),
      MagCalib = readr::col_character(),
      SleepCurrent = readr::col_double(),
      PrioritySensor = readr::col_logical(),
      PriorityMemory = readr::col_logical(),
      LongitudeRemoved = readr::col_double()
    )
  )

  return(d)
}

#' @rdname read_gdl
#' @export
read_gdl_access <- function(access_file,
                            file_data = tempfile("GDL_Data", fileext = "csv"),
                            file_order = tempfile("GDL_Orders", fileext = "csv")) {
  # Check Acess file
  if (!file.exists(access_file)) {
    cli_abort("The access file {.file {access_file}} does not exist.")
  }
  if (tolower(tools::file_ext(access_file)) != "accdb") {
    cli_abort("The acess file {.file {access_file}} does not have a {.val .accdb} extension.")
  }

  # Export data
  system(glue::glue("mdb-export {access_file} GDL_Data > {file_data}"))

  if (!file.exists(file_data)) {
    cli_abort("The file {.file {file_data}} does not exist. \\
                     There has been an issue in creating the data file")
  }

  # Export orders
  system(glue::glue("mdb-export {access_file} GDL_Orders > {file_order}"))
  if (!file.exists(file_order)) {
    cli_abort("The file {.file {file_order}} does not exist. \\
                     There has been an issue in creating the order file")
  }

  invisible(c(file_data, file_order))
}
