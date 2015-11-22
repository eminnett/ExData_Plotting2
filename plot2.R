# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
# (fips == "24510") from 1999 to 2008? Use the base plotting system to make
# a plot answering this question.

library(dplyr)

# This function assumes https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip has been downloaded and expanded into a sibling folder called 'data'.
plot2 <- function(){
    # Load data.
    data_location <- paste(getwd(), "/data/exdata-data-NEI_data", sep='')
    data_summary_scc_pm25  <- readRDS(paste(data_location, "/summarySCC_PM25.rds", sep=''), refhook = NULL)

    # Convert data to dplyr data frames.
    summary_scc_pm25 <- tbl_df(data_summary_scc_pm25)

    # Filter the data by Baltimore City, Maryland (fips == "24510").
    baltimore_pm25 <- filter(summary_scc_pm25, fips == "24510")

    # Group the data by year.
    baltimore_pm25_by_year <- group_by(baltimore_pm25, year)

    # Summarise the data by taking the sum of all emissions for each year group.
    baltimore_pm25_by_year_totals <- summarise(baltimore_pm25_by_year, sum(Emissions))

    # Rename the collumns of the summarised data to be more meaningful.
    colnames(baltimore_pm25_by_year_totals) <- c("Year", "Total.Emissions")

    # Open the PNG stream.
    png(filename = "plot2.png")

    # Plot baltimore_pm25_by_year_totals to view the trend.
    plot(baltimore_pm25_by_year_totals$Year,
        baltimore_pm25_by_year_totals$Total.Emissions,
        main = "Total PM25 Emissions over Time for Baltimore City, Maryland",
        ylab = "Total PM25 Emissions",
        xlab = "Year",
        col  = "red",
        pch  = 19)

    # Close the PNG stream and write to the file.
    dev.off()
}
