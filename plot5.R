# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

library(dplyr)
library(ggplot2)

# This function assumes https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip has been downloaded and expanded into a sibling folder called 'data'.
plot5 <- function(){
    # Load data.
    data_location <- paste(getwd(), "/data/exdata-data-NEI_data", sep='')
    data_summary_scc_pm25  <- readRDS(paste(data_location, "/summarySCC_PM25.rds", sep=''), refhook = NULL)
    data_source_classifications <- readRDS(paste(data_location, "/Source_Classification_Code.rds", sep=''), refhook = NULL)

    # Convert data to dplyr data frames.
    summary_scc_pm25 <- tbl_df(data_summary_scc_pm25)
    source_classifications <- tbl_df(data_source_classifications)

    # Join pm25 data and the source classifications to allow appropriate filtering.
    pm25_with_source_classifications <-
        left_join(summary_scc_pm25, source_classifications)

    # Filter source_classifications to only those that represent sources from motor vehicles.
    motor_vehicles_pm25 <-
        filter(pm25_with_source_classifications, grepl("Mobile - On-Road", EI.Sector))

    # Filter the data by Baltimore City, Maryland (fips == "24510").
    baltimore_motor_vehicles_pm25 <-
        filter(motor_vehicles_pm25, fips == "24510")

    # Group the data by year and.
    baltimore_motor_vehicles_pm25_by_year <-
        group_by(baltimore_motor_vehicles_pm25, year)

    # Summarise the data by taking the sum of all emissions for each group.
    baltimore_motor_vehicles_pm25_by_year_totals <-
        summarise(baltimore_motor_vehicles_pm25_by_year, sum(Emissions))

    # Rename the collumns of the summarised data to be more meaningful.
    colnames(baltimore_motor_vehicles_pm25_by_year_totals) <- c("Year", "Total.Emissions")

    # Ungroup the dplyr data frame.
    baltimore_motor_vehicles_totals <- ungroup(baltimore_motor_vehicles_pm25_by_year_totals)

    # Open the PNG stream.
    png(filename = "plot5.png")

    # Plot baltimore_motor_vehicles_totals to view the trends.
    plot <- ggplot(data=baltimore_motor_vehicles_totals, aes(x=Year, y=Total.Emissions)) +
        geom_point() +
        ggtitle("Total PM25 Emissions from Motor Vehicle Sources over Time\nfor Baltimore City, Maryland") +
        labs(x="Year", y="Total PM25 Emissions")
    print(plot)

    # Close the PNG stream and write to the file.
    dev.off()
}
