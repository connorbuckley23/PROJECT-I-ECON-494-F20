install.packages("readxl") #installs package that allows us to import Excel files into RStudio
library("readxl") #loads library that allows us to import Excel files into RStudio

getwd() #file is in the working directory
my_data<-read_excel("Data_for_RStudio_ECON_494.xlsx") #Imports Excel file data into RStudio
View(my_data) #view data in spreadsheet format
dim(my_data) #check the dimensions of the data frame

###############################################################################
###############################################################################

##combine complete data set into subset solely on stock "Adj Close" price##
a<-cbind(my_data[,2])
b<-cbind(a,my_data[,4])
c<-cbind(b,my_data[,6])
d<-cbind(c,my_data[,8])
e<-cbind(d,my_data[,10])
f<-cbind(e,my_data[,12])
g<-cbind(f,my_data[,14])
h<-cbind(g,my_data[,16])
i<-cbind(h,my_data[,18])
j<-cbind(i,my_data[,20])
k<-cbind(j,my_data[,22])

my_Stocks<-cbind(k,my_data[,24]) #new data set solely on stock Adj Close prices
dim(my_Stocks) #check the dimensions of the data frame
head(my_Stocks) #check the first 6 observations to ensure data is structured correctly 
tail(my_Stocks) #check the last 7 observations to ensure data is structured correctly

###############################################################################
###############################################################################

##combine complete data set into subset solely percentage change in stocks month to month##
a1<-cbind(my_data[,3])
b1<-cbind(a1,my_data[,5])
c1<-cbind(b1,my_data[,7])
d1<-cbind(c1,my_data[,9])
e1<-cbind(d1,my_data[,11])
f1<-cbind(e1,my_data[,13])
g1<-cbind(f1,my_data[,15])
h1<-cbind(g1,my_data[,17])
i1<-cbind(h1,my_data[,19])
j1<-cbind(i1,my_data[,21])
k1<-cbind(j1,my_data[,23])

my_StockChange<-cbind(k1,my_data[,25]) #new data set solely on percentage change in stock price month to month
dim(my_StockChange) #check the dimensions of the data frame
head(my_StockChange) #check the first 6 observations to ensure data is structured correctly
tail(my_StockChange) #check the last 7 observations to ensure data is structured correctly

###############################################################################
###############################################################################

##combine categorical variables into one sub data set##
weekend<-cbind(my_data[,26])
holiday<-cbind(my_data[,27])

categoricals<-cbind(weekend,my_data[,27])
dim(categoricals) #check the dimensions of the data frame
head(categoricals) #check the first 6 observations to ensure data is structured correctly
tail(categoricals) #check the last 7 observations to ensure data is structured correctly

###############################################################################
###############################################################################

##TIDY Data ready for Exploratory Analysis##

###############################################################################
###############################################################################

##generate pairs of scatter plots from the variables to show potential relationships##
pairs(my_Stocks)
pairs(my_StockChange)

###############################################################################
###############################################################################

cor(my_Stocks) #utilize correlation matrix to determine any significant correlation between the stock Adj close
cor(my_StockChange) #utilize correlation matrix to determine any significant correlation between the stock percentage change

###############################################################################
###############################################################################

#variance-covariance matrix between variables  
cov(my_StockChange)

###############################################################################
###############################################################################

##utilize plot function for strongest and weakest correlation for each stock compared to others##

##S&P500 (index)##
plot(my_Stocks$`SP500 Index`~my_Stocks$`JPM Financials`) #plot with JPM (Financials) due to strongest correlation
plot(my_Stocks$`SP500 Index`~my_Stocks$`XOM Energy`) #plot with XOM (Energy) due to weakest correlation

##XOM (Energy)##
plot(my_Stocks$`XOM Energy`~my_Stocks$`KO Consumer Staples`) #plot with KO (Consumer Staples) due to strongest correlation
plot(my_Stocks$`XOM Energy`~my_Stocks$`AMZN Consumer Discretionary`) #plot with AMZN (Consumer Discretionary) due to weakest correlation

##DD (Materials)##
plot(my_Stocks$`DD Materials`~my_Stocks$`SP500 Index`) #plot with SP500 (Index) due to strongest correlation
plot(my_Stocks$`DD Materials`~my_Stocks$`AAPL Information Technology`) #plot with APPL (Information Technology) due to weakest correlation

##UNP (Industrials)##
plot(my_Stocks$`UNP Industrials`~my_Stocks$`AMT Real Estate`) #plot with AMT (Real Estate) due to strongest correlation
plot(my_Stocks$`UNP Industrials`~my_Stocks$`XOM Energy`) #plot with XOM (Energy) due to weakest correlation

##DUK (Utilities)##
plot(my_Stocks$`DUK Utilities`~my_Stocks$`KO Consumer Staples`) #plot with KO (Consumer Staples) due to strongest correlation
plot(my_Stocks$`DUK Utilities`~my_Stocks$`XOM Energy`) #plot with XOM (Energy) due to weakest correlation

##UNH (Healthcare)##
plot(my_Stocks$`UNH Healthcare`~my_Stocks$`JPM Financials`) #plot with JPM (Financials) due to strongest correlation
plot(my_Stocks$`UNH Healthcare`~my_Stocks$`XOM Energy`) #plot with XOM (Energy) due to weakest correlation

##JPM (Financials)##
plot(my_Stocks$`JPM Financials`~my_Stocks$`SP500 Index`) #plot with SP500 (Index) due to strongest correlation
plot(my_Stocks$`UNH Healthcare`~my_Stocks$`XOM Energy`) #plot with XOM (Energy) due to weakest correlation

##AMZN (Consumer Discretionary)##
plot(my_Stocks$`AMZN Consumer Discretionary`~my_Stocks$`AAPL Information Technology`) #plot with AAPL (Information Technology) due to strongest correlation
plot(my_Stocks$`AMZN Consumer Discretionary`~my_Stocks$`XOM Energy`) #plot with XOM (Energy) due to weakest correlation

##KO (Consumer Staples)##
plot(my_Stocks$`KO Consumer Staples`~my_Stocks$`DUK Utilities`) #plot with DUK (Utilities) due to strongest correlation
plot(my_Stocks$`KO Consumer Staples`~my_Stocks$`XOM Energy`) #plot with XOM (Energy) due to weakest correlation

##AAPL (Information Technology)
plot(my_Stocks$`AAPL Information Technology`~my_Stocks$`AMZN Consumer Discretionary`) #plot with AMZN (Consumer Discretionary) due to strongest correlation
plot(my_Stocks$`AAPL Information Technology`~my_Stocks$`XOM Energy`) #plot with XOM (Energy) due to weakest correlation

##VZ (Communication Services)##
plot(my_Stocks$`VZ Communication Services`~my_Stocks$`DUK Utilities`) #plot with DUK (Utilities) due to strongest correlation
plot(my_Stocks$`VZ Communication Services`~my_Stocks$`XOM Energy`) #plot with XOM (Energy) due to weakest correlation

##AMT (Real Estate)##
plot(my_Stocks$`AMT Real Estate`~my_Stocks$`UNP Industrials`) #plot with UNP (Industrials) due to strongest correlation
plot(my_Stocks$`AMT Real Estate`~my_Stocks$`XOM Energy`) #plot with XOM (Energy) due to weakest correlation

################################################################################
################################################################################

##utilize plot function for strongest relationship and weakest correlation for each stock compared to others##

##S&P500 (index)##
plot(my_StockChange$`SP500 Month Change`~my_StockChange$`JPM Month Change`) #plot with JPM (Financials) due to strongest correlation
plot(my_StockChange$`SP500 Month Change`~my_StockChange$`DUK Month Change`) #plot with DUK (Utilities) due to weakest correlation

##XOM (Energy)##
plot(my_StockChange$`XOM Month Change`~my_StockChange$`SP500 Month Change`) #plot with SP500 (Index) due to strongest correlation
plot(my_StockChange$`XOM Month Change`~my_StockChange$`AMZN Month Change`) #plot with AMZN (Consumer Discretionary) due to weakest correlation

##DD (Materials)##
plot(my_StockChange$`DD Month Change`~my_StockChange$`SP500 Month Change`) #plot with SP500 (Index) due to strongest correlation
plot(my_StockChange$`DD Month Change`~my_StockChange$`AMT Month Change`) #plot with AMT (Real Estate) due to weakest correlation

##UNP (Industrials)##
plot(my_StockChange$`UNP Month Change`~my_StockChange$`SP500 Month Change`) #plot with SP500 (Index) due to strongest correlation
plot(my_StockChange$`UNP Month Change`~my_StockChange$`AMT Month Change`) #plot with AMT (Real Estate) due to weakest correlation

##DUK (Utilities)##
plot(my_StockChange$`DUK Month Change`~my_StockChange$`KO Month Change`) #plot with KO (Consumer Staples) due to strongest correlation
plot(my_StockChange$`DUK Month Change`~my_StockChange$`AAPL Month Change`) #plot with AAPL (Information Technology) due to weakest correlation

##UNH (Healthcare)##
plot(my_StockChange$`UNH Month Change`~my_StockChange$`SP500 Month Change`) #plot with SP500 (Index) due to strongest correlation
plot(my_StockChange$`UNH Month Change`~my_StockChange$`AMT Month Change`) #plot with AMT (Real Estate) due to weakest correlation

##JPM (Financials)##
plot(my_StockChange$`JPM Month Change`~my_StockChange$`SP500 Month Change`) #plot with SP500 (Index) due to strongest correlation
plot(my_StockChange$`JPM Month Change`~my_StockChange$`DUK Month Change`) #plot with DUK (Utilities) due to weakest correlation

##AMZN (Consumer Discretionary)##
plot(my_StockChange$`AMZN Month Change`~my_StockChange$`SP500 Month Change`) #plot with SP500 (Index) due to strongest correlation
plot(my_StockChange$`AMZN Month Change`~my_StockChange$`DUK Month Change`) #plot with DUK (Utilities) due to weakest correlation

##KO (Consumer Staples)##
plot(my_StockChange$`KO Month Change`~my_StockChange$`SP500 Month Change`) #plot with SP500 (Index) due to strongest correlation
plot(my_StockChange$`KO Month Change`~my_StockChange$`AAPL Month Change`) #plot with AAPL (Information Technology) due to weakest correlation

##AAPL (Information Technology)
plot(my_StockChange$`AAPL Month Change`~my_StockChange$`SP500 Month Change`) #plot with SP500 (Index) due to strongest correlation
plot(my_StockChange$`AAPL Month Change`~my_StockChange$`DUK Month Change`) #plot with DUK (Utilities) due to weakest correlation

##VZ (Communication Services)##
plot(my_StockChange$`VZ Month Change`~my_StockChange$`SP500 Month Change`) #plot with SP500 (Index) due to strongest correlation
plot(my_StockChange$`VZ Month Change`~my_StockChange$`AAPL Month Change`) #plot with AAPL (Information Technology) due to weakest correlation

##AMT (Real Estate)##
plot(my_StockChange$`AMT Month Change`~my_StockChange$`SP500 Month Change`) #plot with SP500 (Index) due to strongest correlation
plot(my_StockChange$`AMT Month Change`~my_StockChange$`UNH Month Change`) #plot with UNH (Healthcare) due to weakest correlation

################################################################################
################################################################################


##UTILIZE GGPLOT2##


################################################################################
################################################################################

install.packages('ggplot2') #installs GGPLOT2 package
library(ggplot2) #Loads the GGPLOT2 library 


##create a data visualization for the stocks against the SP500 Index##
ggplot(my_data, aes(x = `SP500 Month Change`,y = `XOM Month Change`)) #set up structure and create scatter plot
ggplot(my_data, aes(x = `SP500 Month Change`,y = `XOM Month Change`)) +
  geom_point(color = "green") +geom_smooth(method = "loess") #render the data, add color to rendered data, and add a smoother to the data (loess)
Plot_A<-ggplot(my_data, aes(x = `SP500 Month Change`,y = `XOM Month Change`)) +
  geom_point(color = "green") +geom_smooth(method = "loess")+  #build the plot environment 
Plot_A #store plot as an object variable
Plot_A1<-Plot_A + facet_wrap(col=factor(my_data$`Fri-Mon Closing Day`))   


ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `DD Month Change`)) #set up structure and create scatter plot
ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `DD Month Change`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #render the data, add color to rendered data, and add a smoother to the data (loess)
Plot_B<-ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `DD Month Change`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #build the plot environment
Plot_B #store plot as an object variable


ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `UNP Month Change`)) #set up structure and create scatter plot
ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `UNP Month Change`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #render the data, add color to rendered data, and add a smoother to the data (loess)
Plot_C<-ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `UNP Month Change`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #build the plot environment
Plot_C #store plot as an object variable

ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `DUK Month Change`)) #set up structure and create scatter plot
ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `DUK Month Change`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #render the data, add color to rendered data, and add a smoother to the data (loess)
Plot_D<-ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `DUK Month Change`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #build the plot environment
Plot_D #store plot as an object variable

ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `UNH Month Change`)) #set up structure and create scatter plot
ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `UNH Month Change`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #render the data, add color to rendered data, and add a smoother to the data (loess)
Plot_E<-ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `UNH Month Change`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #build the plot environment
Plot_E #store plot as an object variable


ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `JPM Month Change`)) #set up structure and create scatter plot
ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `JPM Month Change`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #render the data, add color to rendered data, and add a smoother to the data (loess)
Plot_F<-ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `JPM Month Change`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #build the plot environment
Plot_F #store plot as an object variable


ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `AMZN Month Change`)) #set up structure and create scatter plot
ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `AMZN Month Change`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #render the data, add color to rendered data, and add a smoother to the data (loess)
Plot_G<-ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `AMZN Month Change`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #build the plot environment
Plot_G #store plot as an object variable


ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `KO Month Change`)) #set up structure and create scatter plot
ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `KO Month Change`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #render the data, add color to rendered data, and add a smoother to the data (loess)
Plot_H<-ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `KO Month Change`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #build the plot environment
Plot_H #store plot as an object variable

ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `AAPL Month Change`)) #set up structure and create scatter plot
ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `AAPL Month Change`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #render the data, add color to rendered data, and add a smoother to the data (loess)
Plot_I<-ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `AAPL Month Change`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #build the plot environment
Plot_I #store plot as an object variable


ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `VZ Month Change`)) #set up structure and create scatter plot
ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `VZ Month Change`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #render the data, add color to rendered data, and add a smoother to the data (loess)
Plot_J<-ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `VZ Month Change`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #build the plot environment
Plot_J #store plot as an object variable


ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `AMT Month Change`)) #set up structure and create scatter plot
ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `AMT Month Change`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #render the data, add color to rendered data, and add a smoother to the data (loess)
Plot_K<-ggplot(my_StockChange, aes(x = `SP500 Month Change`,y = `AMT Month Change`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #build the plot environment
Plot_K #store plot as an object variable


################################################################################
################################################################################

##Look at visual for heavy correlation between KO (Consumer Staples) and DUK (Utilities)##

##plot percent change month-to-month between the two stocks##
ggplot(my_StockChange, aes(x = `KO Month Change`,y = `DUK Month Change`)) #set up structure and create scatter plot
ggplot(my_StockChange, aes(x = `KO Month Change`,y = `DUK Month Change`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #render the data, add color to rendered data, and add a smoother to the data (loess)
Plot_L<-ggplot(my_StockChange, aes(x = `KO Month Change`,y = `DUK Month Change`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #build the plot environment
Plot_L #store plot as an object variable

##plot stock adjusted close price between the two stocks##
ggplot(my_Stocks, aes(x = `KO Consumer Staples`,y = `DUK Utilities`)) #set up structure and create scatter plot
ggplot(my_Stocks, aes(x = `KO Consumer Staples`,y = `DUK Utilities`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #render the data, add color to rendered data, and add a smoother to the data (loess)
Plot_M<-ggplot(my_Stocks, aes(x = `KO Consumer Staples`,y = `DUK Utilities`)) +
  geom_point(color = "green")+geom_smooth(method = "loess") #build the plot environment
Plot_M #store plot as an object variable

################################################################################
################################################################################


##Conduct test for Normal Distribution based on percent change Month to Month
attach(my_StockChange) #allows data to be searched and selected by R when evaluating variables


install.packages("dplyr") #allows for data manipulation and needed for normality test
install.packages("ggpubr") #data visualization package in R and needed for normality test 
library(dplyr) #loads the library needed to perform test for normality
library(ggpubr) #loads the library needed to perform test for normality


ggdensity(`SP500 Month Change`, main= "SP500 Index",xlab = "SP500 Month Change") #display density curve to show if data is normally distributed or not
ggqqplot(`SP500 Month Change`, main="SP500 Index",xlab = "SP500 Month Change")
shapiro.test(`SP500 Month Change`) #utilize this test for p-value and determine if Normally Distributed 


ggdensity(`XOM Month Change`, main= "XOM Energy",xlab = "XOM Month Change") #display density curve to show if data is normally distributed or not
ggqqplot(`XOM Month Change`, main="XOM Energy",xlab = "XOM Month Change")
shapiro.test(`XOM Month Change`) #utilize this test for p-value and determine if Normally Distributed 


ggdensity(`DD Month Change`, main= "DD Materials",xlab = "DD Month Change") #display density curve to show if data is normally distributed or not 
ggqqplot(`DD Month Change`, main="DD Materials",xlab = "DD Month Change") #show whether data is or is not normally distributed
shapiro.test(`DD Month Change`) #utilize this test for p-value and determine if Normally Distributed


ggdensity(`UNP Month Change`, main= "UNP Industrials",xlab = "UNP Month Change") #display density curve to show if data is normally distributed or not 
ggqqplot(`UNP Month Change`, main="UNP Industrials",xlab = "UNP Month Change") #show whether data is or is not normally distributed
shapiro.test(`UNP Month Change`) #utilize this test for p-value and determine if Normally Distributed


ggdensity(`DUK Month Change`, main= "DUK Utilities",xlab = "DUK Month Change") #display density curve to show if data is normally distributed or not 
ggqqplot(`DUK Month Change`, main="DUK Utilities",xlab = "DUK Month Change") #show whether data is or is not normally distributed
shapiro.test(`DUK Month Change`) #utilize this test for p-value and determine if Normally Distributed


ggdensity(`UNH Month Change`, main= "UNH Healthcare",xlab = "UNH Month Change") #display density curve to show if data is normally distributed or not 
ggqqplot(`UNH Month Change`, main="UNH Healthcare",xlab = "UNH Month Change") #show whether data is or is not normally distributed
shapiro.test(`UNH Month Change`) #utilize this test for p-value and determine if Normally Distributed


ggdensity(`JPM Month Change`, main= "JPM Financials",xlab = "JPM Month Change") #display density curve to show if data is normally distributed or not 
ggqqplot(`JPM Month Change`, main="JPM Financials",xlab = "JPM Month Change") #show whether data is or is not normally distributed
shapiro.test(`JPM Month Change`) #utilize this test for p-value and determine if Normally Distributed


ggdensity(`AMZN Month Change`, main= "AMZN Consumer Discretionary",xlab = "AMZN Month Change") #display density curve to show if data is normally distributed or not 
ggqqplot(`AMZN Month Change`, main="AMZN Consumer Discretionary",xlab = "AMZN Month Change") #show whether data is or is not normally distributed
shapiro.test(`AMZN Month Change`) #utilize this test for p-value and determine if Normally Distributed


ggdensity(`KO Month Change`, main= "KO Consumer Staples",xlab = "KO Month Change") #display density curve to show if data is normally distributed or not 
ggqqplot(`KO Month Change`, main="KO Consumer Staples",xlab = "KO Month Change") #show whether data is or is not normally distributed
shapiro.test(`KO Month Change`) #utilize this test for p-value and determine if Normally Distributed


ggdensity(`AAPL Month Change`, main= "AAPL Information Technology",xlab = "AAPL Month Change") #display density curve to show if data is normally distributed or not 
ggqqplot(`AAPL Month Change`, main="AAPL Information Technology",xlab = "AAPL Month Change") #show whether data is or is not normally distributed
shapiro.test(`AAPL Month Change`) #utilize this test for p-value and determine if Normally Distributed


ggdensity(`VZ Month Change`, main="VZ Communication Services",xlab = "VZ Month Change") #show whether data is or is not normally distributed
ggqqplot(`VZ Month Change`, main="VZ Communication Services",xlab = "VZ Month Change") #show whether data is or is not normally distribute
shapiro.test(`VZ Month Change`) #utilize this test for p-value and determine if Normally Distributed


ggdensity(`AMT Month Change`, main="AMT Real Estate",xlab = "AMT Month Change") #show whether data is or is not normally distributed
ggqqplot(`AMT Month Change`, main="AMT Real Estate",xlab = "AMT Month Change") #show whether data is or is not normally distribute
shapiro.test(`AMT Month Change`) #utilize this test for p-value and determine if Normally Distributed

################################################################################
################################################################################

##run further test using Jarque-Bera test to test for normal distribution
install.packages('tseries') #installs the tseries package needed to test for normal distribution
library(tseries) #loads "tseries" library - need to first install "tseries" package


#utilize this hypothesis test with the null hypothesis: data is distributed normally
jarque.bera.test(my_StockChange$`SP500 Month Change`)

jarque.bera.test(my_StockChange$`XOM Month Change`)

jarque.bera.test(my_StockChange$`DD Month Change`)

jarque.bera.test(my_StockChange$`UNP Month Change`)

jarque.bera.test(my_StockChange$`DUK Month Change`)

jarque.bera.test(my_StockChange$`UNH Month Change`)

jarque.bera.test(my_StockChange$`JPM Month Change`)

jarque.bera.test(my_StockChange$`AMZN Month Change`)

jarque.bera.test(my_StockChange$`KO Month Change`)

jarque.bera.test(my_StockChange$`AAPL Month Change`)

jarque.bera.test(my_StockChange$`VZ Month Change`)

jarque.bera.test(my_StockChange$`AMT Month Change`)

################################################################################
################################################################################

##run exploratory analysis on individual stocks to shed light on potential stock characteristics## 

##compute standard deviation of each stock to identify potential volatility##
sd(my_Stocks$`SP500 Index`)
sd(my_Stocks$`XOM Energy`)
sd(my_Stocks$`DD Materials`)
sd(my_Stocks$`UNP Industrials`)
sd(my_Stocks$`DUK Utilities`)
sd(my_Stocks$`UNH Healthcare`)
sd(my_Stocks$`JPM Financials`)
sd(my_Stocks$`AMZN Consumer Discretionary`)
sd(my_Stocks$`KO Consumer Staples`)
sd(my_Stocks$`AAPL Information Technology`)
sd(my_Stocks$`VZ Communication Services`)
sd(my_Stocks$`AMT Real Estate`)

################################################################################
################################################################################

##compute variance of each stock##
var(my_Stocks$`SP500 Index`)
var(my_Stocks$`XOM Energy`)
var(my_Stocks$`DD Materials`)
var(my_Stocks$`UNP Industrials`)
var(my_Stocks$`DUK Utilities`)
var(my_Stocks$`UNH Healthcare`)
var(my_Stocks$`JPM Financials`)
var(my_Stocks$`AMZN Consumer Discretionary`)
var(my_Stocks$`KO Consumer Staples`)
var(my_Stocks$`AAPL Information Technology`)
var(my_Stocks$`VZ Communication Services`)
var(my_Stocks$`AMT Real Estate`)

###############################################################################
###############################################################################

##compute standard deviation of each stock percentage change to identify potential volatility##
sd(my_StockChange$`SP500 Month Change`)
sd(my_StockChange$`XOM Month Change`)
sd(my_StockChange$`DD Month Change`)
sd(my_StockChange$`UNP Month Change`)
sd(my_StockChange$`DUK Month Change`)
sd(my_StockChange$`UNH Month Change`)
sd(my_StockChange$`JPM Month Change`)
sd(my_StockChange$`AMZN Month Change`)
sd(my_StockChange$`KO Month Change`)
sd(my_StockChange$`AAPL Month Change`)
sd(my_StockChange$`VZ Month Change`)
sd(my_StockChange$`AMT Month Change`)

###############################################################################
###############################################################################

##compute variance of each stock percentage change##
var(my_StockChange$`SP500 Month Change`)
var(my_StockChange$`XOM Month Change`)
var(my_StockChange$`DD Month Change`)
var(my_StockChange$`UNP Month Change`)
var(my_StockChange$`DUK Month Change`)
var(my_StockChange$`UNH Month Change`)
var(my_StockChange$`JPM Month Change`)
var(my_StockChange$`AMZN Month Change`)
var(my_StockChange$`KO Month Change`)
var(my_StockChange$`AAPL Month Change`)
var(my_StockChange$`VZ Month Change`)
var(my_StockChange$`AMT Month Change`)

###############################################################################
###############################################################################

##compute max/min values for individual stock percentage change to determine max/min value within the months##
max(my_StockChange$`SP500 Month Change`)
min(my_StockChange$`SP500 Month Change`)

max(my_StockChange$`XOM Month Change`)
min(my_StockChange$`XOM Month Change`)

max(my_StockChange$`DD Month Change`)
min(my_StockChange$`DD Month Change`)

max(my_StockChange$`UNP Month Change`)
min(my_StockChange$`UNP Month Change`)

max(my_StockChange$`DUK Month Change`)
min(my_StockChange$`DUK Month Change`)

max(my_StockChange$`UNH Month Change`)
min(my_StockChange$`UNH Month Change`)

max(my_StockChange$`JPM Month Change`)
min(my_StockChange$`JPM Month Change`)

max(my_StockChange$`AMZN Month Change`)
min(my_StockChange$`AMZN Month Change`)

max(my_StockChange$`KO Month Change`)
min(my_StockChange$`KO Month Change`)

max(my_StockChange$`AAPL Month Change`)
min(my_StockChange$`AAPL Month Change`)

max(my_StockChange$`VZ Month Change`)
min(my_StockChange$`VZ Month Change`)

max(my_StockChange$`AMT Month Change`)
min(my_StockChange$`AMT Month Change`)


################################################################################
################################################################################








