---
title: 'Weekly Exercises #6'
author: "*Charlie Tran*"
output: 
  html_document:
    keep_md: TRUE
    toc: TRUE
    toc_float: TRUE
    df_print: paged
    code_download: true
---





```r
library(tidyverse)     # for data cleaning and plotting
library(googlesheets4) # for reading googlesheet data
library(lubridate)     # for date manipulation
library(openintro)     # for the abbr2state() function
library(palmerpenguins)# for Palmer penguin data
library(maps)          # for map data
library(ggmap)         # for mapping points on maps
library(gplots)        # for col2hex() function
library(RColorBrewer)  # for color palettes
library(sf)            # for working with spatial data
library(leaflet)       # for highly customizable mapping
library(ggthemes)      # for more themes (including theme_map())
library(plotly)        # for the ggplotly() - basic interactivity
library(gganimate)     # for adding animation layers to ggplots
library(gifski)        # for creating the gif (don't need to load this library every time,but need it installed)
library(transformr)    # for "tweening" (gganimate)
library(shiny)         # for creating interactive apps
library(patchwork)     # for nicely combining ggplot2 graphs  
library(gt)            # for creating nice tables
library(rvest)         # for scraping data
library(robotstxt)     # for checking if you can scrape data
gs4_deauth()           # To not have to authorize each time you knit.
theme_set(theme_minimal())
```


```r
# Lisa's garden data
garden_harvest <- read_sheet("https://docs.google.com/spreadsheets/d/1DekSazCzKqPS2jnGhKue7tLxRU3GVL1oxi-4bEM5IWw/edit?usp=sharing") %>% 
  mutate(date = ymd(date))

#COVID-19 data from the New York Times
covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
```

## Put your homework on GitHub!

Go [here](https://github.com/llendway/github_for_collaboration/blob/master/github_for_collaboration.md) or to previous homework to remind yourself how to get set up. 

Once your repository is created, you should always open your **project** rather than just opening an .Rmd file. You can do that by either clicking on the .Rproj file in your repository folder on your computer. Or, by going to the upper right hand corner in R Studio and clicking the arrow next to where it says Project: (None). You should see your project come up in that list if you've used it recently. You could also go to File --> Open Project and navigate to your .Rproj file. 

# Github link

https://github.com/charlietran01/06_weeklyassignment

## Instructions

* Put your name at the top of the document. 

* **For ALL graphs, you should include appropriate labels.** 

* Feel free to change the default theme, which I currently have set to `theme_minimal()`. 

* Use good coding practice. Read the short sections on good code with [pipes](https://style.tidyverse.org/pipes.html) and [ggplot2](https://style.tidyverse.org/ggplot2.html). **This is part of your grade!**

* **NEW!!** With animated graphs, add `eval=FALSE` to the code chunk that creates the animation and saves it using `anim_save()`. Add another code chunk to reread the gif back into the file. See the [tutorial](https://animation-and-interactivity-in-r.netlify.app/) for help. 

* When you are finished with ALL the exercises, uncomment the options at the top so your document looks nicer. Don't do it before then, or else you might miss some important warnings and messages.

## Your first `shiny` app 

  1. This app will also use the COVID data. Make sure you load that data and all the libraries you need in the `app.R` file you create. Below, you will post a link to the app that you publish on shinyapps.io. You will create an app to compare states' cumulative number of COVID cases over time. The x-axis will be number of days since 20+ cases and the y-axis will be cumulative cases on the log scale (`scale_y_log10()`). We use number of days since 20+ cases on the x-axis so we can make better comparisons of the curve trajectories. You will have an input box where the user can choose which states to compare (`selectInput()`) and have a submit button to click once the user has chosen all states they're interested in comparing. The graph should display a different line for each state, with labels either on the graph or in a legend. Color can be used if needed. 
  


https://charlietran01.shinyapps.io/covidapp/

---------------------
library(tidyverse)
library(shiny)
library(lubridate)

rsconnect::setAccountInfo(name='charlietran01', token='D199D8F26F0D1523B858F4DD3624E6F9', secret='V8g1nP6fX8kIcBORkDuvDnBeTEggs7FACXHOgcob')

covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

ui <- fluidPage(
  selectInput("state",
              "Choose States:",
              choices = covid19$state,
              multiple = TRUE),
  submitButton(text = "Create my plot!"),
  plotOutput(outputId = "timeplot")
  
)
server <- function(input, output) {
  output$timeplot <- renderPlot({
    covid19 %>% 
      mutate(cumsum = cumsum(cases)) %>% 
      mutate(days = difftime(ymd(date), 
                             ymd(20200128), 
                             units = "days")) %>% 
      filter(state == input$state,
             cumsum > 20) %>% 
      ggplot()+
      geom_line(aes(x = days,
                    y = cumsum,
                    color = state))+
      scale_y_log10()+
      labs(title = "US COVID19 Cases",
           y = "Cummulative Cases",
           x = "Days since 20+ Cases")+
      theme_minimal()
      
  })
}
shinyApp(ui = ui, server = server)
---------------------

## Warm-up exercises from tutorial

  2. Read in the fake garden harvest data. Find the data [here](https://github.com/llendway/scraping_etc/blob/main/2020_harvest.csv) and click on the `Raw` button to get a direct link to the data. 
  

```r
X2020_harvest <- read_csv("https://raw.githubusercontent.com/llendway/scraping_etc/main/2020_harvest.csv", 
    col_types = cols(weight = col_number()), 
    na = "MISSING", skip = 2) %>% 
  select(-X1)
```
  3. Read in this [data](https://www.kaggle.com/heeraldedhia/groceries-dataset) from the kaggle website. You will need to download the data first. Save it to your project/repo folder. Do some quick checks of the data to assure it has been read in appropriately.


```r
kaggle <- read.csv("Groceries_dataset.csv")
```
  4. CHALLENGE(not graded): Write code to replicate the table shown below (open the .html file to see it) created from the `garden_harvest` data as best as you can. When you get to coloring the cells, I used the following line of code for the `colors` argument:
  

```r
colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::YlGn"
      ) %>% as.character()
```


```r
#![](garden_table.html){width=600, height=1000}  #error "File garden_table.html not found in resource path" 
```

  5. Create a table using `gt` with data from your project or from the `garden_harvest` data if your project data aren't ready.
  

```r
garden_table <- garden_harvest %>% 
  group_by(vegetable, variety) %>% 
  summarise(total_weight = sum(weight)) %>% 
  ungroup()

gt(garden_table)
```

<!--html_preserve--><style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ejwlmawohf .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ejwlmawohf .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ejwlmawohf .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ejwlmawohf .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ejwlmawohf .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ejwlmawohf .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ejwlmawohf .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ejwlmawohf .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ejwlmawohf .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ejwlmawohf .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ejwlmawohf .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ejwlmawohf .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#ejwlmawohf .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ejwlmawohf .gt_from_md > :first-child {
  margin-top: 0;
}

#ejwlmawohf .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ejwlmawohf .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ejwlmawohf .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#ejwlmawohf .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ejwlmawohf .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#ejwlmawohf .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ejwlmawohf .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ejwlmawohf .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ejwlmawohf .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ejwlmawohf .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ejwlmawohf .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#ejwlmawohf .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ejwlmawohf .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#ejwlmawohf .gt_left {
  text-align: left;
}

#ejwlmawohf .gt_center {
  text-align: center;
}

#ejwlmawohf .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ejwlmawohf .gt_font_normal {
  font-weight: normal;
}

#ejwlmawohf .gt_font_bold {
  font-weight: bold;
}

#ejwlmawohf .gt_font_italic {
  font-style: italic;
}

#ejwlmawohf .gt_super {
  font-size: 65%;
}

#ejwlmawohf .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>
<div id="ejwlmawohf" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;"><table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">vegetable</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">variety</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">total_weight</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr>
      <td class="gt_row gt_left">apple</td>
      <td class="gt_row gt_left">unknown</td>
      <td class="gt_row gt_right">156</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">asparagus</td>
      <td class="gt_row gt_left">asparagus</td>
      <td class="gt_row gt_right">20</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">basil</td>
      <td class="gt_row gt_left">Isle of Naxos</td>
      <td class="gt_row gt_right">490</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Bush Bush Slender</td>
      <td class="gt_row gt_right">10038</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Chinese Red Noodle</td>
      <td class="gt_row gt_right">356</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beans</td>
      <td class="gt_row gt_left">Classic Slenderette</td>
      <td class="gt_row gt_right">1635</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beets</td>
      <td class="gt_row gt_left">Gourmet Golden</td>
      <td class="gt_row gt_right">3185</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beets</td>
      <td class="gt_row gt_left">leaves</td>
      <td class="gt_row gt_right">101</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">beets</td>
      <td class="gt_row gt_left">Sweet Merlin</td>
      <td class="gt_row gt_right">2897</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">broccoli</td>
      <td class="gt_row gt_left">Main Crop Bravado</td>
      <td class="gt_row gt_right">967</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">broccoli</td>
      <td class="gt_row gt_left">Yod Fah</td>
      <td class="gt_row gt_right">372</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">Bolero</td>
      <td class="gt_row gt_right">3761</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">Dragon</td>
      <td class="gt_row gt_right">1862</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">greens</td>
      <td class="gt_row gt_right">169</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">carrots</td>
      <td class="gt_row gt_left">King Midas</td>
      <td class="gt_row gt_right">1858</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">chives</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_right">8</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cilantro</td>
      <td class="gt_row gt_left">cilantro</td>
      <td class="gt_row gt_right">52</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">corn</td>
      <td class="gt_row gt_left">Dorinny Sweet</td>
      <td class="gt_row gt_right">5174</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">corn</td>
      <td class="gt_row gt_left">Golden Bantam</td>
      <td class="gt_row gt_right">727</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">cucumbers</td>
      <td class="gt_row gt_left">pickling</td>
      <td class="gt_row gt_right">19781</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">edamame</td>
      <td class="gt_row gt_left">edamame</td>
      <td class="gt_row gt_right">2763</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">hot peppers</td>
      <td class="gt_row gt_left">thai</td>
      <td class="gt_row gt_right">67</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">hot peppers</td>
      <td class="gt_row gt_left">variety</td>
      <td class="gt_row gt_right">599</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">jalapeño</td>
      <td class="gt_row gt_left">giant</td>
      <td class="gt_row gt_right">4478</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">kale</td>
      <td class="gt_row gt_left">Heirloom Lacinto</td>
      <td class="gt_row gt_right">2697</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">kohlrabi</td>
      <td class="gt_row gt_left">Crispy Colors Duo</td>
      <td class="gt_row gt_right">191</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Farmer's Market Blend</td>
      <td class="gt_row gt_right">1725</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Lettuce Mixture</td>
      <td class="gt_row gt_right">2154</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">mustard greens</td>
      <td class="gt_row gt_right">23</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">reseed</td>
      <td class="gt_row gt_right">45</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">lettuce</td>
      <td class="gt_row gt_left">Tatsoi</td>
      <td class="gt_row gt_right">1313</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">onions</td>
      <td class="gt_row gt_left">Delicious Duo</td>
      <td class="gt_row gt_right">342</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">onions</td>
      <td class="gt_row gt_left">Long Keeping Rainbow</td>
      <td class="gt_row gt_right">1502</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Magnolia Blossom</td>
      <td class="gt_row gt_right">3383</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peas</td>
      <td class="gt_row gt_left">Super Sugar Snap</td>
      <td class="gt_row gt_right">4340</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peppers</td>
      <td class="gt_row gt_left">green</td>
      <td class="gt_row gt_right">2582</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">peppers</td>
      <td class="gt_row gt_left">variety</td>
      <td class="gt_row gt_right">1656</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">potatoes</td>
      <td class="gt_row gt_left">purple</td>
      <td class="gt_row gt_right">1365</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">potatoes</td>
      <td class="gt_row gt_left">red</td>
      <td class="gt_row gt_right">2011</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">potatoes</td>
      <td class="gt_row gt_left">Russet</td>
      <td class="gt_row gt_right">4124</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">potatoes</td>
      <td class="gt_row gt_left">yellow</td>
      <td class="gt_row gt_right">3357</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">Cinderella's Carraige</td>
      <td class="gt_row gt_right">14911</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">New England Sugar</td>
      <td class="gt_row gt_right">20348</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">pumpkins</td>
      <td class="gt_row gt_left">saved</td>
      <td class="gt_row gt_right">34896</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">radish</td>
      <td class="gt_row gt_left">Garden Party Mix</td>
      <td class="gt_row gt_right">429</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">raspberries</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_right">843</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">rutabaga</td>
      <td class="gt_row gt_left">Improved Helenor</td>
      <td class="gt_row gt_right">13490</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">spinach</td>
      <td class="gt_row gt_left">Catalina</td>
      <td class="gt_row gt_right">923</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Blue (saved)</td>
      <td class="gt_row gt_right">18835</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">delicata</td>
      <td class="gt_row gt_right">4762</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Red Kuri</td>
      <td class="gt_row gt_right">10311</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">squash</td>
      <td class="gt_row gt_left">Waltham Butternut</td>
      <td class="gt_row gt_right">11009</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">strawberries</td>
      <td class="gt_row gt_left">perrenial</td>
      <td class="gt_row gt_right">592</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">Swiss chard</td>
      <td class="gt_row gt_left">Neon Glow</td>
      <td class="gt_row gt_right">3122</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Amish Paste</td>
      <td class="gt_row gt_right">29789</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Better Boy</td>
      <td class="gt_row gt_right">15426</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Big Beef</td>
      <td class="gt_row gt_right">11337</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Black Krim</td>
      <td class="gt_row gt_right">7170</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Bonny Best</td>
      <td class="gt_row gt_right">11305</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Brandywine</td>
      <td class="gt_row gt_right">7097</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Cherokee Purple</td>
      <td class="gt_row gt_right">7127</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">grape</td>
      <td class="gt_row gt_right">14694</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Jet Star</td>
      <td class="gt_row gt_right">6815</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Mortgage Lifter</td>
      <td class="gt_row gt_right">11941</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">Old German</td>
      <td class="gt_row gt_right">12119</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">tomatoes</td>
      <td class="gt_row gt_left">volunteers</td>
      <td class="gt_row gt_right">23411</td>
    </tr>
    <tr>
      <td class="gt_row gt_left">zucchini</td>
      <td class="gt_row gt_left">Romanesco</td>
      <td class="gt_row gt_right">45227</td>
    </tr>
  </tbody>
  
  
</table></div><!--/html_preserve-->
  
  6. Use `patchwork` operators and functions to combine at least two graphs using your project data or `garden_harvest` data if your project data aren't read.
  

```r
graph1 <- garden_harvest %>% 
  filter(vegetable == "beets") %>% 
  group_by(variety) %>% 
  summarise(total_weight = sum(weight)) %>% 
  ungroup() %>% 
  ggplot(aes(x = total_weight,
             y = variety,
             fill = variety))+
  geom_col()+
   theme(legend.position = "none")

graph2 <- garden_harvest %>% 
  filter(vegetable == "tomatoes") %>% 
  group_by(variety) %>% 
  summarise(total_weight = sum(weight)) %>% 
  ungroup() %>% 
  ggplot(aes(x = total_weight,
             y = variety,
             fill = variety))+
  geom_col()+
   theme(legend.position = "none")

graph1 + graph2
```

![](06_exercises_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

**DID YOU REMEMBER TO UNCOMMENT THE OPTIONS AT THE TOP?**
