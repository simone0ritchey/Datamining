##### Pirates Worksheet :)

# Working Directory

## Getting the error: cannot change working directory
##
setwd("~MacOS/Users/simone/Documents/Documents/UT/UrbanEco/Datamining")
getwd()

# Install/load packages
# use install.packages("package name") if not yet installed
library(yarrr)
library(dplyr)
library(ggplot2)
library(plyr)

# Different ways of looking at the dataframe
## Shows first parts of dataframe
head(pirates)

## Shows the column/variable names in the dataframe
names(pirates)

## Shows the kind of object (chr, int, dataframe)
str(pirates)

## Shows entire dataframe
### Gives error: no such file or directory
### Fixed by restarting R
View(pirates)

# Descriptive Statistics
## Mean weight
mean(pirates$weight)

## Piping to find mean height
pirates %>% summarise(avg_height = mean(height))

## Piping to find height of the tallest pirate
pirates %>% summarise(max_height = max(height))

## Piping to find how many pirates of each gender there are
### So basically, this says take the pirates data frame group by the sex and
### then summarize each group in a tibble
pirates %>% group_by(sex) %>% summarise(n=n())

## Piping to find the average age of the pirates of each sex
pirates %>% group_by(sex) %>% summarise(age = mean(age))

# Summary of data frame
## Age range is 11 to 46
summary(pirates)

# Scatter plot of pirate weight (y-axis) versus pirate height (x-axis)
ggplot(pirates, aes(x=height, y=weight)) + geom_point()

# Make points slightly transparent so we can visualize density
ggplot(pirates, aes(x=height, y=weight)) + geom_point(alpha=0.5)

# Add title and axis labels
ggplot(pirates, aes(x=height, y=weight)) + geom_point() + labs(title = "Pirate Height Versus Weight", x = "Weight (kg)", y = "Height (cm)")

# Centering title
ggplot(pirates, aes(x=height, y=weight)) + geom_point(alpha=0.5) + labs(title = "Pirate Height Versus Weight", x = "Weight (kg)", y = "Height (cm)") + theme(plot.title = element_text(hjust = 0.5))

# Or, to be more efficient you could change the default behavior of ggplot
theme_update(plot.title = element_text(hjust = 0.5))

# Add line of best fit
## At first I copied and pasted the code from the benching protocol and it 
## didn't work. I had to retype the "".
### se=FALSE hides the confidence interval markers
ggplot(pirates, aes(x=height, y=weight)) + geom_point(alpha=0.5) + labs(title = "Pirate Height Versus Weight", x = "Weight (kg)", y = "Height (cm)") + geom_smooth(method="lm", se=FALSE)

# Scatter plot of pirate age versus how many parrots they have owned.
## geom_point( pch=16) makes a solid round marker. Alternatively, 
## col=gray(level=0.5, alpha=0.6) makes them grey and transparent. theme_classic()
## controls a number of settings relating to plot format, including the background
## color.
### If you use help to look up points you can see the different values of pch that
### give certain styles
ggplot(pirates, aes(x=age, y=parrots)) + geom_point(col=gray(level=0.5, alpha=0.6)) + labs(title = "Pirate Age Versus How Many Parrots They Have Owned", x = "Weight (kg)", y = "Height (cm)") + geom_smooth(method="lm", se=FALSE) + theme_classic()

# Bar graph: use geom_bar when y axis is the frequency. You only specify the x-axis in aes
myplot<- ggplot(data=pirates, aes(sex))+geom_bar()
myplot

# Add title and color
## #denotes the hex code of the color that is added
myplot<- ggplot(data=pirates, aes(sex)) + geom_bar(fill="#bf5700") + labs(title="Frequency of Pirate Sexes")
myplot

# Add additional category to bar graph, stacked
myplot<- ggplot(data=pirates, aes(sex))+geom_bar(aes(fill=headband)) + labs(title = "Frequency of Pirate Sex with Headband Use")
myplot

# Alternative method, adds category side by side
myplot<- ggplot(data=pirates, aes(sex))+geom_bar(aes(fill=headband),position="dodge") + labs(title = "Frequency of Pirate Sex with Headband Use")
myplot

# I'm guessing the shows the percentages of each sex that does/doesn't have a 
# headband.
myplot<- ggplot(data=pirates, aes(sex)) + geom_bar(aes(fill = headband), position="fill") + labs(title = "Frequency of Pirate Sex with Headband Use")
myplot

# Making a column plot of the average beard length for each gender
## First, I make a table with the average beard length and standard deviation 
## for each gender
### the package plyr needs to be off for this to work
Q26table <- pirates %>% group_by(sex) %>% summarise(avg_beard.length = mean(beard.length), sdv_beard.length = sd(beard.length))
Q26table
## Then I make a plot of the table.
## geom_bar() gives the frequencies, geom_col() gives the data values.
Q26plot <- ggplot(data = Q26table, aes(sex, avg_beard.length)) + geom_col(aes(sex, avg_beard.length), fill="#bf5700") + labs(title = "Average Beard Length of Each Pirate Sex", x = "Sex", y = "Average Beard Length") + theme_classic()
Q26plot
## Trying to do them all in one line of code. It works!
Q26plot2 <- pirates %>% group_by(sex) %>% summarise(avg_beard.length = mean(beard.length), sdv_beard.length = sd(beard.length)) %>% ggplot( aes(sex, avg_beard.length)) + geom_col(aes(sex, avg_beard.length), fill="#bf5700") + labs(title = "Average Beard Length of Each Pirate Sex", x = "Sex", y = "Average Beard Length") + theme_classic()
Q26plot2
## Now adding error bars to the graph.
Q26plot <- pirates %>% group_by(sex) %>% summarise(avg_beard.length = mean(beard.length), sdv_beard.length = sd(beard.length)) %>% ggplot( aes(sex, avg_beard.length)) + geom_col(aes(sex, avg_beard.length), fill="#bf5700") + labs(title = "Average Beard Length of Each Pirate Sex", x = "Sex", y = "Average Beard Length") + theme_classic() + geom_errorbar( aes(x=sex, ymin=avg_beard.length-sdv_beard.length, ymax=avg_beard.length+sdv_beard.length), width=0.4, colour="orange", alpha=0.9, size=1.5)
Q26plot

# Box Plot
## Displays distribution of data, including the median, IQ rang, predicted 
## maximum, predicted min, and any outliers.

# Box plot showing the age distribution of pirates of each sex
Q28plot <- pirates %>% ggplot( aes(sex, age)) + labs(title = "Distribution of Ages in Each Pirate Sex", x = "Sex", y = "Age") + theme_classic() + geom_boxplot(fill="#bf5700")
Q28plot
# Add fill = headband to aes. What happens?
## This adds a box plot showing age distributions of different sexes of pirates,
## separated by if they are wear headbands or not.
Q28plot2 <- pirates %>% ggplot( aes(sex, age)) + labs(title = "Distribution of Ages in Each Pirate Sex", x = "Sex", y = "Age") + theme_classic() + geom_boxplot(aes(fill = headband))
Q28plot2
# Add layer "+facet_wrap(~headband)"
Q28plot3 <- pirates %>% ggplot( aes(sex, age)) + labs(title = "Distribution of Ages in Each Pirate Sex", x = "Sex", y = "Age") + theme_classic() + geom_boxplot(aes(fill = headband)) + facet_wrap(~headband)
Q28plot3

#Violin plots
## Horizontal line shows median, the length of the violin shape indicates the 
## range of data values and the shape shows the probability density (the more 
## data points at a given value, the wider the shape)

# Plot the range of pirate age based on their favorite sword
## Note the 2 columns of the pirates data frame separated by "~". This indicates 
##what to plot on the x and y axis.
pirateplot(formula=age~sword.type, data=pirates, main="Pirateplot of ages by favorite sword")

# Plot height range of the different pirate sexes, using plotting theme 3, and 
# the "pony" color palette (pal = "pony")
pirateplot(formula=height~sex, data=pirates, pal = "pony", theme(3), main="Pirateplot of height by sex")

# Why is the pallette called pony? Use piratepal function to show full pallete.
piratepal(palette = "pony", plot.result=TRUE)
# Change pallettee = "all" to see all the the pirate pallettes
piratepal(palette = "all", plot.result=TRUE)

# Hypothesis Testing: compare the ages of pirates who do not wear headbands to
# the ages of pirates who do wear headbands.
## Null hypothesis: there is no difference between the ages of pirates who do
## wear headbands and the ages of pirates who don't wear headbands

# Determine the average age of pirates who do and do not wear headbands
# (see step 15).
## There is a small difference. To determine if this difference is significant
## we need to use statistics.
pirates %>% group_by(headband) %>% summarise(age = mean(age))

# Use filter() to create dataframe called no_headband, containing only the 
# pirates who do not wear headbands
no_headband <- pirates %>% filter(headband == "no")

# Check data frame, using n = 10  to see more of the data frame.
## Make sure that there are "yes"'s in the headband column.
### Looks good to me!
head(no_headband, n = 10)

# Add select() to filter() to make new data frame called no_headband_shorter
# from the pirates data frame. 
no_headband_shorter <- pirates %>% filter(headband == "no") %>% select(c(3,6))
no_headband_shorter

# Now doing the same for pirates who do wear headbands
yes_headband_shorter <- pirates %>% filter(headband == "yes") %>% select(c(3,6))
yes_headband_shorter

# Plot the two data frames as histograms to compare
no_headband_shorter_histogram <- no_headband_shorter %>% ggplot(aes(age)) + geom_histogram(color="black", fill = "#bf5700", binwidth=2) + theme_classic() + labs(title = "Age Distribution of Pirates Who Don't Wear Headbands")
no_headband_shorter_histogram
yes_headband_shorter_histogram <- yes_headband_shorter %>% ggplot(aes(age)) + geom_histogram(color="black", fill = "#bf5700", binwidth=2) + theme_classic() + labs(title = "Age Distribution of Pirates Who Do Wear Headbands")
yes_headband_shorter_histogram

# Add dashed blue vertical line, showing mean age of the pirates who wear headbands
## Using this website: 
## http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization#calculate-the-mean-of-each-group
nomu <- ddply(no_headband_shorter, "headband", summarise, grp.mean = mean(age))
head(nomu)
no_headband_shorter_histogram <- no_headband_shorter %>% ggplot(aes(age)) + geom_histogram(color="black", fill = "#bf5700", binwidth=2) + theme_classic() + labs(title = "Age Distribution of Pirates Who Don't Wear Headbands") + geom_vline(data = nomu, aes(xintercept=grp.mean, color=headband), linetype="dashed")
no_headband_shorter_histogram

## Doing it for yes headbands
yesmu <- ddply(yes_headband_shorter, "headband", summarise, grp.mean = mean(age))
yes_headband_shorter_histogram <- yes_headband_shorter %>% ggplot(aes(age)) + geom_histogram(color="black", fill = "#bf5700", binwidth=2) + theme_classic() + labs(title = "Age Distribution of Pirates Who Do Wear Headbands") + geom_vline(data = yesmu, aes(xintercept=grp.mean, color=headband), linetype="dashed")
yes_headband_shorter_histogram

## Now plotting both at the same time
mu <- ddply(pirates, "headband", summarise, grp.mean = mean(age))
yesandno_plot <- pirates %>% ggplot(aes(age, color=headband, fill = headband)) + geom_histogram(binwidth=2) + theme_classic() + labs(title = "Age Distribution of Pirates Who Do and Don't Wear Headbands") + geom_vline(data = mu, aes(xintercept=grp.mean, color=headband), linetype="dashed")
yesandno_plot

# Now showing it as two histograms superimposed on each other, rather than 
# stacked on top of each other. This is better for making comparisons. Although,
# I don't like that the color for no doesn't match in the legend and on the graph
yesandno_plot2 <- pirates %>% ggplot(aes(age, color=headband, fill = headband)) + geom_histogram(binwidth=2, alpha=0.5, position="identity") + theme_classic() + labs(title = "Age Distribution of Pirates Who Do and Don't Wear Headbands") + geom_vline(data = mu, aes(xintercept=grp.mean, color=headband), linetype="dashed")
yesandno_plot2

# Now making a facetted graph
## This makes it put one on top of the other
yesandno_plot3 <- pirates %>% ggplot(aes(age)) + geom_histogram(binwidth=2, color="black", fill = "white") + theme_classic() + labs(title = "Age Distribution of Pirates Who Do and Don't Wear Headbands") + geom_vline(data = mu, aes(xintercept=grp.mean, color=headband), linetype="dashed") + facet_grid(headband ~ .)
yesandno_plot3
## This makes it put them side by side
yesandno_plot4 <- pirates %>% ggplot(aes(age)) + geom_histogram(binwidth=2, color="black", fill = "white") + theme_classic() + labs(title = "Age Distribution of Pirates Who Do and Don't Wear Headbands") + geom_vline(data = mu, aes(xintercept=grp.mean, color=headband), linetype="dashed") + facet_grid(. ~ headband)
yesandno_plot4

# Make a box plot comparing these
yesandno_boxplot <- pirates %>% ggplot( aes(headband, age)) + labs(title = "Distribution of Ages by Headband Use", x = "Headband", y = "Age") + theme_classic() + geom_boxplot()
yesandno_boxplot

# Running a t-test on this
## p = 0.7258754 > .05, so we cannot say that the two distributions are statistically 
## significant
age_headband.htest <- t.test(no_headband_shorter$age, yes_headband_shorter$age, paired = FALSE, alternative = 'two.sided')
age_headband.htest$p.value

# Now doing this using the original pirates data frame
## Get the same results
age_headband.htest<-t.test(formula = age ~ headband, data = pirates)
age_headband.htest$p.value
### Cannot use piping because t.test is from base R and doesn't recognize piping
### from dplyr

# Correlation test
cor.test(formula = ~height + weight, data = pirates)

# ANOVA test, comparing number of tattoos between groups of pirates with the same
# favorite sword
## ANOVA is used when you are comparing more than 2 distributions

# First make a box plot shoing the number of tattoos for pirates based on sword type
boxplot4anova <- pirates %>% ggplot( aes(sword.type, tattoos)) + labs(title = "Number of Tattoos Depending on Favorite Sword", x = "sword.type", y = "tattoos") + theme_classic() + geom_boxplot()
boxplot4anova 

# Now ANOVA
res.aov <-aov(tattoos ~ sword.type, pirates)
summary(res.aov)

# Indexing
no_headband[1,1] # Gives the intercept of the 1st row and the 1st column
no_headband[1:3,4:8] # Gives rows 1-3 in columns 4-8
no_headband[3:1, c(12,11,16)] # Gives  rows 3-1 in columns 12, 11, and 16

# Select favorite sword variable using indexing
sword_time <- pirates[,14]
str(sword_time)
# Mean of vector
mean(sword_time)
# Length of vector
length(sword_time)
# Sum of vector
sum(sword_time)

# Putting these values into a single vector
swordresults <- c(mean(sword_time), length(sword_time), sum(sword_time))
sample(swordresults, 1)

# Tossing a coin
coin <- c("heads", "tails")
if (sample(coin, 1) == "tails") {
  print("TAILS Woohoo!")
}
# Automating it
counter <- 0
for (i in (1:5)) {
  if (sample(coin, 1) == "tails") {
    counter <- counter + 1
    print("TAILS Woohoo!")
  }
  if (i == 5) {
    print(counter)
  }
}

# Making a function that will do this any number of times
coinflipper <- function(number_of_flips) {
  counter <- 0
  for (i in (1:number_of_flips)) {
    if (sample(coin, 1) == "tails") {
      counter <- counter + 1
      print("TAILS Woohoo!")
    }
    if (i == number_of_flips) {
      print(counter)
    }
  }
}

coinflipper(10)

# Using the function, another loop statement, and a container object, create
# a 5,000 trial experiment, where the coin is flipped 40 times. Use a vector to
# record answers.

# Editing the coinflipper function to just count the the number of tails
coinflipper <- function(number_of_flips) {
  counter <- 0
  for (i in (1:number_of_flips)) {
    if (sample(coin, 1) == "tails") {
      counter <- counter + 1
    } 
    if (i == number_of_flips) {
      return(counter)
    }
  }
}
coinflipper(40)
counter

# Now setting up the experiment

## Empty vector to add values to 
results <-rep(NA,5000)

for (i in 1:5000) {
  
  # Get count of tails
  count <- coinflipper(40)
  
  # Assign count to ith value of result
  results[i] <- count
  
  # Print results
  if (i == 5000) {
    print(results)
  }
}
results_histogram <- as.data.frame(results) %>% ggplot(aes(results)) + geom_histogram(color="black", fill = "#bf5700", binwidth = 1) + theme_classic() + labs(title = "Distribution of Tails Flipped Out of 40 Trails, n = 5000.")
results_histogram
