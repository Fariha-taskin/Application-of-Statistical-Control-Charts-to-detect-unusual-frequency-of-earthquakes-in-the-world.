## Overview
<div align= "justify">This project was done while I was doing my bachelorβs degree at ISRT of the University of Dhaka. This paper has been published in the Journal of Data Science in 2020 and here is the link https://doi.org/10.6339/JDS.202001_18(1).0002 
Apart from R, Minitab was used to build the plots of the CUSUM and the EWMA control charts. 

---------------------

## Introduction
<div align= "justify">Earthquake in recent years has increased tremendously. This paper outlines an evaluation of the Cumulative Sum (πΆππππ) and Exponentially Weighted Moving Average (πΈπππ΄) charting technique to determine if the frequency of earthquakes in the world is unusual. The frequency of earthquakes in the world is considered from the period 1973 to 2016. As our data is autocorrelated we cannot use the regular control chart like the Shewhart control chart to detect unusual earthquake frequency. An approach that has proved useful in dealing with autocorrelated data is to directly model time series models such as Autoregressive Integrated Moving Average (π΄ππΌππ΄) and apply control charts to the residuals. The πΈπππ΄ control chart and the πΆππππ control chart have detected unusual frequencies of earthquakes in the year 2012 and 2013 which are state of statistically out of control.</div>  

--------------------------

## Methods Used
* Cumulative Sum (πΆππππ)
* Exponentially Weighted Moving Average (πΈπππ΄)

## Cumulative Sum:
<div align= "justify">The Shewhart chart for average is very effective if the magnitude of the shift is 1.5Ο to 2Ο or larger. However, for a smaller shift, it is not effective. The cumulative sum (CUSUM) control chart is a good alternative when small shifts are important. Cumulative sum control charts were introduced by Page (1954). The CUSUM chart is more effective than the Shewhart chart in many ways. They detect small process shifts and are effective with a sample size of n=1. CUSUM procedures can be devised for the process mean, Poisson, and Binomial variables for modeling nonconformities and fraction nonconforming.</div>

## Exponentially Weighted Moving Average:
<div align= "justify">The exponential weighted moving average (EWMA) control chart is also a good alternative to the Shewhart control chart when we are interested in detecting small shifts. It was introduced by Roberts (1959) EWMA control chart is approximately the same as the CUSUM chart and in some cases, it is easier to set up and operate. This chart considers all previous points using a weighing factor that makes the outcome more influenced by recent points.</div> 


-----------------
## Data Description
<div align= "justify">United States Geological Survey (USGS) is one of the providers of earthquake data from all over the world. The data provided by USGS for year 1900 to 2016 contains too many variables. Our variables of interest are year of occurrence and magnitude.</div>

-------------------
## Findings: 
* <div align= "justify">In this project, it has shown that there are two shifts in the mean of the number of earthquakes. For the year 1900-1949, the mean was very small. Besides, for the years 1950-1972 the mean had increased slightly.</div>
* <div align= "justify">We witnessed a tremendous shift in mean in 1973.</div> 
* <div align= "justify">There can be several reasons for this significant difference. It is possible that at the beginning of the 1900s, the measurement and detecting equipment of earthquakes were very ancient that those could not detect the earthquake correctly. Yet, maybe in the 1900s earthquake did not appear as frequently as they are now.</div> 

