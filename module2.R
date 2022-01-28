#1. print "Plotting Basics:Lastname"
print("Plotting Basics: Sawant")

r=getOption("repos")
r["CRAN"]="http://cran.us.r-project.org"
options(repos=r)

#2.Installing and importing required packages
install.packages('FSA')
install.packages('FSAdata')
install.packages('magrittr')
install.packages('dplyr')
install.packages('plotrix')
install.packages('ggplot2')
install.packages('moments')

library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(plotrix)
library(ggplot2)
library(moments)

#3.Loading the data
BullTroutRML2
head(BullTroutRML2)

#4. Print the first and last 3 records from the BullTroutRML2 dataset
head(BullTroutRML2,n=3)
tail(BullTroutRML2,n=3)

#5.Remove all records except those from Harisson lake
df <- filter(BullTroutRML2, lake=="Harrison")
df

#6. Display the first and last 5 records from the filtered BullTroutRML2 Dataset
head(df,n=3)
tail(df,n=3)

#7. Display the structure of the filtered BullTroutRML2 Dataset
str(df)

#8. Display the summary of the filtered BullTroutRML2 Dataset
summary(df)

#9. create a scatterplot for "age" (y variable) and "fl"(x variable) with the following specifications
# limit of x axix is (0,500)
# limit of y axix is (0,15)
# Title of graph is "plot 1: Harrison Lake Trout
# Y axis label is "Age(yrs)"
# X axis label is "Fork length (mm)
# use a small filled circle for the plotted data points

scatterplot <- ggplot(df, aes(x=fl,y=age))+geom_point()+xlim(0,500)+ylim(0,15)+
labs(title="Plot1 : Harrison Lake Trout", x="Fork length (mm)", y="Age(yrs)")+
theme(plot.title = element_text(hjust=0.5))
scatterplot

#10. Plot an "Age histogram with the following specifications
# y axis label is "frequency"
# x axis label is "age (yrs)"
# title of the histogram is "plot 2: Harrison Fish Age Distribution"
# X and Y limits is 0,15
# The color of the frequency plots is "cadetblue"
# The color of the title is "cadetblue"

hist(df$age,xlab = "age(yrs)", ylab="frequency", 
main = "plot 2:Harrison Fish Age Distribution", xlim=c(0,15), ylim=c(0,15), col="cadetblue", 
col.main="cadetblue")

#11. create an overdense plot using the same specifications as the previous scatterplot. But,
# Title the plot "plot 3:Harrison Density shaded by Era"
# y axis label is "Age(yrs)
# y axis limits are 0 to 15
# x axis label is "Fork length (mm)"
# x axis limits are 0 to 500
# include two levels of shading for the "green" data points
# plot solid circles as data points

fl <- df$fl
age <- df$age
overdense_plot <- plot(age~fl, main = "plot 3:Harrison Density shaded by Era",
xlab="Fork length(mm)", ylab="Age(yrs)", xlim=c(0,500), ylim=c(0,15), col=rgb(0,(1:2)/2,0))

#12. create a new object called "temp"that includes the first 3 and last 3 records of the BulltroutRML2 data set

temp <- headtail(BullTroutRML2,n=3)
temp

#13. Display the "era" column (variable) in the new "temp" object
temp$era

#14. create a pchs vector with the argument values for + and x
pchs <- c(3,4)

#15. create a cols vector with the two elements "red" and "gray60"
cols <- c("red","grey60")
cols

#16. convert the temp era values to numeric values
converted_temp <- as.numeric(temp$era)
converted_temp

#17. Initialize the cols vector with the temp era values 
cols[temp$era]

#18.  Create a plot of "Age (yrs)" (y variable) versus "Fork Length (mm)" (x variable) with the following specifications: 
# Title of graph is "Plot 4: Symbol & Color by Era" 
# Limit of x axis is (0,500) 
# Limit of y axis is (0,15) 
# X axis label is "Age (yrs)" 
# Y axis label is "Fork Length (mm)" 
# Set pch equal to pchs era values 
# Set col equal to cols era values 

plot(age~fl,data=df, main="Plot 4: Symbol & Color by Era",xlim = c(0,500),
ylim=c(0,15),xlab="Fork Length(mm)", ylab="Age(yrs)", 
pch=pchs[df$era], col=cols[df$era])

#19. Plot a regression line overlay on Plot 4 and title the new graph "Plot 5: Regression Overlay". 

plot(age~fl,data=df, main="Plot 5: Regression Overlay",xlim = c(0,500), 
ylim=c(0,15), xlab="Fork Length (mm)", ylab="Age(yrs)",
pch=pchs[df$era], col=cols[df$era])
abline(lm(age~fl,data=df))

#20.Place a legend of on Plot 5 and call the new graph "Plot 6: :Legend Overlay" 

df$era
plot(age~fl,data=df, main="Plot 6: Legend Overlay",xlim = c(0,500),
ylim=c(0,15), xlab="Fork Length(mm)", ylab="Age(yrs)",
pch=pchs[df$era], col=cols[df$era])
abline(lm(age~fl,data=df))
legend("topleft", legend=c("1977-80","1997-01"),pch = pchs,col = cols)

install.packages('tinytex')
tinytex::install_tinytex()
