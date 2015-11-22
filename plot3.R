# Of the four types of sources indicated by the type (point, nonpoint, onroad,
# nonroad) variable, which of these four sources have seen decreases in emissions
# from 1999–2008 for Baltimore City? Which have seen increases in emissions
# from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

library(dplyr)
library(ggplot2)

# This function assumes https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip has been downloaded and expanded into a sibling folder called 'data'.
plot3 <- function(){
    # Load data.
    data_location <- paste(getwd(), "/data/exdata-data-NEI_data", sep='')
    data_summary_scc_pm25  <- readRDS(paste(data_location, "/summarySCC_PM25.rds", sep=''), refhook = NULL)

    # Convert data to dplyr data frames.
    summary_scc_pm25 <- tbl_df(data_summary_scc_pm25)

    # Filter the data by Baltimore City, Maryland (fips == "24510").
    baltimore_pm25 <- filter(summary_scc_pm25, fips == "24510")

    # Group the data by year and type.
    baltimore_pm25_by_year_and_type <-
        group_by(baltimore_pm25, year, type)

    # Summarise the data by taking the sum of all emissions for each group.
    baltimore_pm25_by_year_and_type_totals <-
        summarise(baltimore_pm25_by_year_and_type, sum(Emissions))

    # Rename the collumns of the summarised data to be more meaningful.
    colnames(baltimore_pm25_by_year_and_type_totals) <- c("Year", "Type", "Total.Emissions")

    # Ungroup the dplyr data frame.
    baltimore_pm25_totals <- ungroup(baltimore_pm25_by_year_and_type_totals)

    # Open the PNG stream.
    png(filename = "plot3.png")

    # Plot baltimore_pm25_totals to view the trends.
    plot <- ggplot(data=baltimore_pm25_totals, aes(x=Year, y=Total.Emissions, color=Type)) +
        geom_point() +
        ggtitle("Total PM25 Emissions over Time for Baltimore City, Maryland") +
        labs(x="Year", y="Total PM25 Emissions")
    print(plot)

    # Close the PNG stream and write to the file.
    dev.off()
}
