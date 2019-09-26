## _ggplotr_ is an R Shiny app, written for [Effektiv Altruisme Norge](https://effektivaltruisme.no/)
![](ggplotrpreview.png)

## What does it do?
For now, the app allows you to visualise data through a number of different 1-2 variable plots. Titles are customisable, as well as axis ranges, themes, and colours. Regression lines can be added and histogram bin widths can be altered. You can explore data from the default R datasets, upload a CSV file (i.e. Excel stuff), or connect to a SQL database.

See the "Issues" tab for features and fixes that I hope to add soon. 

## Running the app

First, install [__R__](https://cran.r-project.org/bin/windows/base/), the 64-bit version.
Next, install [__RStudio__](https://www.rstudio.com/products/rstudio/).
Then install the following packages in __RStudio__. Simply paste the following code into the console.

```R
install.packages(c("shiny", "shinyBS", "shinydashboard", "tidyverse", "DT", "RODBC" ))
```


Next, just open the "App.R" file in __RStudio__ and press _Run App_ in the top right of the source pane.

Enjoy!

Love,

Nik

## Acknowledgements

This app is inspired by and borrows code from aagarw30's [gist](https://gist.github.com/aagarw30/c593799bc7d8557dc863411bb552e4f4), dashee87's [dbplotR](https://github.com/dashee87/dbplotR), and dgrapov's [gist](https://gist.github.com/dgrapov/dfcf6ab2339b1cf19b090cfb8dadc855).