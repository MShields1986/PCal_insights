# M Shields 1/11/2020

# Install any missing libraries and load them----
if (!require(data.table)) { install.packages("data.table", repos = "https://cran.rstudio.com/"); library(data.table) }
if (!require(ggplot2)) { install.packages("ggplot2", repos = "https://cran.rstudio.com/"); library(ggplot2) }
if (!require(plotly)) { install.packages("plotly", repos = "https://cran.rstudio.com/"); library(plotly) }
if (!require(knitr)) { install.packages("knitr", repos = "https://cran.rstudio.com/"); library(knitr) }

if (!require(tidyverse)) { install.packages("tidyverse", repos = "https://cran.rstudio.com/"); library(tidyverse) }
if (!require(rvest)) { install.packages("rvest", repos = "https://cran.rstudio.com/"); library(rvest) }
if (!require(emo)) { install.packages("emo", repos = "https://cran.rstudio.com/"); library(emo) }
if (!require(ggtext)) { install.packages("ggtext", repos = "https://cran.rstudio.com/"); library(ggtext) }

emoji_to_link <- function(x) {
  paste0("https://emojipedia.org/emoji/",x) %>%
    read_html() %>%
    html_nodes("tr td a") %>%
    .[1] %>%
    html_attr("href") %>%
    paste0("https://emojipedia.org/", .) %>%
    read_html() %>%
    html_node('div[class="vendor-image"] img') %>%
    html_attr("src")
}

link_to_img <- function(x, size = 25) {
  paste0("<img src='", x, "' width='", size, "'/>")
}

# Read in CSV and preprocess----
path_to_file = "/home/matthew/Desktop/PCal/Poop.csv"
input = fread(path_to_file, showProgress = TRUE, data.table = FALSE)

# Convert to date time object
input$`Date Time` = strptime(paste(input$Date, input$Time, sep=' '), format='%d %b %Y %H:%M:%S')
input$`Date Time` = as.POSIXct(input$`Date Time`)

# Order ascending
input = input[order(input$`Date Time`), ]

# Calculate interval between visits
input$Interval = NA
input$Interval[2:length(input$`Date Time`)] = -difftime(input$`Date Time`[1:length(input$`Date Time`)-1], input$`Date Time`[2:length(input$`Date Time`)], units = "hours")

# Import poop emoji
poop = link_to_img(emoji_to_link("%F0%9F%92%A9"))
input$emoji = poop

# Calculate visit windowing rolling average
visit_count = 5

input$`Rolling Average Interval` = NA
for (i in 0:(length(input$Interval) - visit_count)) {
  input$`Rolling Average Interval`[visit_count+i] = mean(input$Interval[1:visit_count+i], na.rm = TRUE)
}
input$`Rolling Average Interval` = as.numeric(input$`Rolling Average Interval`)

# Time windowing rolling average----
#start_time = strptime('20 Oct 2020', format='%d %b %Y')
#end_time = strptime('24 Oct 2020', format='%d %b %Y')

#input_mask = input$`Date Time` >= start_time & input$`Date Time` <= end_time

#mean(input$Interval[input_mask])

# Interactive plot----
g = ggplot(input, aes(x = `Date Time`,
                      y = `Rolling Average Interval`,
                      size = Interval,
                      colour = Interval,
                      text = paste("x = ", `Date Time`,
                                   " : y = ", round(`Rolling Average Interval`, 3), 
                                   " : Interval = ", round(Interval, 3), sep = "")
                      )) +
      geom_point(na.rm = TRUE, alpha = 0.6) +
      geom_smooth(method='lm', formula= y~x) +
      scale_x_datetime(date_breaks = "3 days", date_labels = "%d %b") + 
      scale_y_continuous(breaks = seq(0, 55, 2.5), limits = c(0, round(max(input$`Rolling Average Interval`, na.rm = TRUE)+2.5))) + 
#  geom_hline(yintercept = 30, size = 1, colour = titcol) +
      theme_minimal() +
      labs(title = "Poo Log", x = "Date", y = "Five Visit Rolling Average Interval Time (hours)")

ggplotly(g, tooltip="text")

# Static plot with poo emoji points----
ggplot(input, aes(x = `Date Time`,
                  y = `Rolling Average Interval`,
                  size = Interval)) +
  #geom_point(na.rm = TRUE, alpha = 0.6) +
  geom_richtext(aes(x = `Date Time`,
                    y = `Rolling Average Interval`,
                    size = Interval), 
                label = poop, 
                fill = NA, 
                label.color = NA, 
                label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_smooth(method='lm', formula= y~x) +
  scale_x_datetime(date_breaks = "3 days", date_labels = "%d %b") + 
  scale_y_continuous(breaks = seq(0, 55, 2.5), limits = c(0, round(max(input$`Rolling Average Interval`, na.rm = TRUE)+2.5))) + 
  #  geom_hline(yintercept = 30, size = 1, colour = titcol) +
  theme_minimal() +
  theme(legend.position = "none") +
  #theme(legend.text = element_markdown()) +
  labs(title = "Poo Log", x = "Date", y = "Five Visit Rolling Average Interval Time (hours)")

#ggplot(input, aes(x = `Date Time`, y = Interval, size = Interval, colour = Interval)) +
#  geom_point() + 
#  theme_minimal()
