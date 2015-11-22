# Compare emissions from motor vehicle sources in Baltimore City with emissions
# from motor vehicle sources in Los Angeles County, California (fips == "06037").
# Which city has seen greater changes over time in motor vehicle emissions?

library(dplyr)
library(ggplot2)

# This function assumes https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip has been downloaded and expanded into a sibling folder called 'data'.
plot6 <- function(){
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

    # Filter the data by Baltimore City, Maryland (fips == "24510") or Los Angeles County, California (fips == "06037").
    baltimore_and_la_motor_vehicles_pm25 <-
        filter(motor_vehicles_pm25, fips == "24510" | fips == "06037")

    # Group the data by year and.
    baltimore_and_la_motor_vehicles_pm25_by_year_and_loc <-
        group_by(baltimore_and_la_motor_vehicles_pm25, year, fips)

    # Summarise the data by taking the sum of all emissions for each group.
    baltimore_and_la_motor_vehicles_pm25_by_year_and_loc_totals <-
        summarise(baltimore_and_la_motor_vehicles_pm25_by_year_and_loc, sum(Emissions))

    # Rename the collumns of the summarised data to be more meaningful.
    colnames(baltimore_and_la_motor_vehicles_pm25_by_year_and_loc_totals) <-
        c("Year", "Fips", "Total.Emissions")

    # Ungroup the dplyr data frame.
    baltimore_and_la_motor_vehicles_pm25_totals <-
        ungroup(baltimore_and_la_motor_vehicles_pm25_by_year_and_loc_totals)

    # Map fips to locations
    fips_location_mapping <- tbl_df(
        data.frame(
            Fips=c("24510", "06037"),
            Location=c("Baltimore City,\nMaryland", "Los Angeles County,\nCalifornia")))

    # Join the location mapping back to pm25 data.
    baltimore_and_la_motor_vehicles_pm25_totals <-
        left_join(baltimore_and_la_motor_vehicles_pm25_totals, fips_location_mapping)

    # Open the PNG stream.
    png(filename = "plot6.png")

    # Plot baltimore_and_la_motor_vehicles_pm25_totals to view the trends.
    plot <- ggplot(
            data=baltimore_and_la_motor_vehicles_pm25_totals,
            aes(x=Year, y=Total.Emissions, color=Location)) +
        geom_point() +
        ggtitle("Total PM25 Emissions from Motor Vehicle\nSources over Time: Baltimore City, Maryland vs.\nLos Angeles County, California") +
        labs(x="Year", y="Total PM25 Emissions")
    print(plot)

    # Close the PNG stream and write to the file.
    dev.off()
}
