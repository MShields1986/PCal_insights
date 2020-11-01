
#Loading libs----
if (!require(data.table)) { install.packages("data.table", repos = "https://cran.rstudio.com/"); library(data.table) }
if (!require(ggplot2)) { install.packages("ggplot2", repos = "https://cran.rstudio.com/"); library(ggplot2) }
if (!require(knitr)) { install.packages("knitr", repos = "https://cran.rstudio.com/"); library(knitr) }


#Read in CSV----
csv = "/home/matthew/Desktop/PCal/Poop.csv"
input = fread(csv, showProgress = TRUE, data.table = FALSE)

input$`Date Time` = strptime(paste(input$Date, input$Time, sep=' '), format='%d %b %Y %H:%M:%S')

input$Interval = NA

input$Interval[2:length(input$`Date Time`)] = difftime(input$`Date Time`[1:length(input$`Date Time`)-1], input$`Date Time`[2:length(input$`Date Time`)], units = "hours")

# Visit windowing rolling average----
visit_count = 5

input$`Rolling Average Interval` = NA

for (i in 0:(length(input$Interval) - visit_count)) {
  input$`Rolling Average Interval`[visit_count+i] = mean(input$Interval[1:visit_count+i], na.rm = TRUE)
}

# Time windowing rolling average----
#start_time = strptime('20 Oct 2020', format='%d %b %Y')
#end_time = strptime('24 Oct 2020', format='%d %b %Y')

#input_mask = input$`Date Time` >= start_time & input$`Date Time` <= end_time

#mean(input$Interval[input_mask])

# Plotting----
input$`Date Time` = as.POSIXct(input$`Date Time`)

ggplot(input, aes(x = `Date Time`, y = as.numeric(`Rolling Average Interval`), color=Interval, size=Interval)) +
  geom_point(na.rm = TRUE, alpha = 0.6) +
  #stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm', formula= y~x) +
#  scale_x_continuous(breaks = seq(0, max(GNSSmeasurements$Time), (max(GNSSmeasurements$Time) - min(GNSSmeasurements$Time)) / 10)) + 
  scale_y_continuous(breaks = seq(0, 55, 2.5), limits = c(0, round(max(input$`Rolling Average Interval`, na.rm = TRUE)+2.5))) + 
#  geom_hline(yintercept = 30, size = 1, colour = titcol) +
  theme_minimal() +
  #scale_color_brewer(palette = "Set1") +
  labs(title = "Poop", x = "Date Time", y = "Five Visit Rolling Average Interval Time (hours)")


#ggplot(input, aes(x = `Date Time`, y = Interval, size = Interval, colour = Interval)) +
#  geom_point() + 
#  theme_minimal()
