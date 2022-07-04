# NASCAR Driver Stats Comparison App

This Shiny app is designed to allow for the comparison of lap speed, green flag performance, and pit stop data between any two selected drivers from the 2021
season of the NASCAR Cup Series.

## Use

Making a selection in the Race, Driver 1, and Driver 2 dropdowns will populate a line chart comparing the two drivers' lap times throughout the race,
a density distribution plot to show each drivers' performance on green flag laps, and a data table highlighting details for each of their pit stops in the selected race.

## Data

- Data was scraped from the NASCAR.com API.
- This project currently only features data from the 2021 season.
- Earlier versions of the data scrapers are available in my [NASCAR data repo.](https://github.com/jbrooksdata/nascar-data)

## To-Do

- A small number of race selections fail to join with pit stop data or return duplicate stop information.
- Users should have the option to save a copy of a full comparison in a format like PNG or PDF.
- A reactive "Season" dropdown will allow users to compare drivers within the 2020, 2021, or 2022 seasons.
- Similar data is also available for the Xfinity Series and Truck Series. This could be accessible to users via tabsets or another dropdown.

## References

- [NASCAR.com data](https://www.nascar.com/results/racecenter/2021/nascar-cup-series/daytona-500/stn/recap/#)
