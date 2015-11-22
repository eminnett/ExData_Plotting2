# Across the United States, how have emissions from coal combustion-related
# sources changed from 1999–2008?

library(dplyr)
library(ggplot2)

# This function assumes https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip has been downloaded and expanded into a sibling folder called 'data'.
plot4 <- function(){
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

    # Filter source_classifications to only those that represent sources from coal.
    coal_pm25 <-
        filter(pm25_with_source_classifications, grepl("Coal", EI.Sector))

    # Group the data by year and.
    coal_pm25_by_year <- group_by(coal_pm25, year)

    # Summarise the data by taking the sum of all emissions for each group.
    coal_pm25_by_year_totals <- summarise(coal_pm25_by_year, sum(Emissions))

    # Rename the collumns of the summarised data to be more meaningful.
    colnames(coal_pm25_by_year_totals) <- c("Year", "Total.Emissions")

    # Ungroup the dplyr data frame.
    coal_pm25_totals <- ungroup(coal_pm25_by_year_totals)

    # Open the PNG stream.
    png(filename = "plot4.png")

    # Plot coal_pm25_totals to view the trends.
    plot <- ggplot(data=coal_pm25_totals, aes(x=Year, y=Total.Emissions)) +
        geom_point() +
        ggtitle("Total PM25 Emissions from Coal Sources over Time") +
        labs(x="Year", y="Total PM25 Emissions")
    print(plot)

    # Close the PNG stream and write to the file.
    dev.off()
}
