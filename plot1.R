# Have total emissions from PM2.5 decreased in the United States from 1999
# to 2008? Using the base plotting system, make a plot showing the total
# PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

library(dplyr)

# This function assumes https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip has been downloaded and expanded into a sibling folder called 'data'.
plot1 <- function(){
    # Load data.
    data_location <- paste(getwd(), "/data/exdata-data-NEI_data", sep='')
    data_summary_scc_pm25  <- readRDS(paste(data_location, "/summarySCC_PM25.rds", sep=''), refhook = NULL)

    # Convert data to dplyr data frames.
    summary_scc_pm25  <- tbl_df(data_summary_scc_pm25)

    # Group the data by year.
    pm25_by_year <- group_by(summary_scc_pm25, year)

    # Summarise the data by taking the sum of all emissions for each year group.
    pm25_by_year_totals <- summarise(pm25_by_year, sum(Emissions))

    # Rename the collumns of the summarised data to be more meaningful.
    colnames(pm25_by_year_totals) <- c("Year", "Total.Emissions")

    # Open the PNG stream.
    png(filename = "plot1.png")

    # Plot pm25_by_year_totals to view the trend.
    plot(pm25_by_year_totals$Year,
        pm25_by_year_totals$Total.Emissions,
        main = "Total PM25 Emissions over Time",
        ylab = "Total PM25 Emissions",
        xlab = "Year",
        col  = "blue",
        pch  = 19)

    # Close the PNG stream and write to the file.
    dev.off()
}
