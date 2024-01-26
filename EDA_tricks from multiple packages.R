#EDA_tricks from multiple packages.R

#From SAS to R - what are "must" packages for reporting
https://stats.stackexchange.com/questions/123274/from-sas-to-r-what-are-must-packages-for-reporting
#My heart goes to R because of its openness and vibrant community. 
#But I must admit that SAS's PROCS are quite powerful out-of-the-box. 
#To partially address those issues I wrote an R package titled summarytools 
# which provides ways to generate decent looking and translatable 
# (thanks to pander, Pandoc implemented in R) simple reports 
#(frequencies, univariate stats, codebook, for the essential part) to 
#various formats like RTF, pdf, and markdown.

#However, even with the use of by() to stratify the stats (be it frequencies or univariate numerical stats), 
#I feel I'm still miles away from generating as flexible and complete tables such as with PROC TABULATE 
#  or PROC MEANS. So my question is: 
#   what R packages do you find are "musts" for needs of extracting essential stats from dataframes, 
# splitting on this variable and filtering on that other one. 
#I hope this is not judged as too broad a question; 
#I have made my homework and tried finding the answer to this question before posting here. 
#I'm sure there are some really really well-made packages that address those issues, 
#    and I simply haven't seen them around ... yet.

#In 2020 the reporter package was released, which operates much like proc report. 
#You get the data and statistics you want using other R packages, 
#   and then send the resulting data frame into reporter. Like this:


#But I believe that we should replace almost all tables with graphics. 
#The new R greport ("graphical report") and hreport ("html report") packages takes the philosophy 
# that graphics should be used for the main presentation, 
#  and graphs should be hyperlinked to supporting tables 
#  that appear in an appendix to the pdf report. 
#See http://hbiostat.org/r. 
#These packages use new functions in the Hmisc package for graphing categorical data 
#(i.e., translating tables to plots) and for showing whole distributions of continuous variables.



#EDA_tricks from multiple packages.R

#https://www.listendata.com/2015/08/how-to-update-r-software.html
#  UPdating R - DANGER
#install.packages("installr")  #DANGER updates can break your environment
#library(installr)             #DANGER updates can break your environment   
#updateR()                     #DANGER updates can break your environment 

install.package``
library(tidyverse)
library(tidymodels)
require(tidymodels)
#What is the difference between require() and library()
#require is used inside functions, as it outputs a warning and continues 
# if the package is not found, whereas library will throw an error.

#################################### fancy tables #########################
#ttps://bookdown.org/wadetroberts/r-you-ready-for-r/cross-tabulation.html

#R package reviews {gtsummary} Publication-Ready Tables of Data, Statistical Tests and Models!
#https://www.youtube.com/watch?v=hyP3Hx_1kTM

#Box Plots in R with t-test, Mann-Whitney, ANOVA, Kruskal-Wallis, Shapiro-Wilk, Levene and post-hocs
#https://www.youtube.com/watch?v=cNJIMwtLWv0

######################### cross tables like freq (hopefully)#################
#############################################################################
# Frequencies and Crosstabs https://www.statmethods.net/stats/frequencies.html
# 2-Way Cross Tabulation 
#Crosstable https://www.statmethods.net/stats/frequencies.html
#The CrossTable( ) function in the gmodels package produces crosstabulations modeled after PROC FREQ in SAS 
# or CROSSTABS in SPSS. It has a wealth of options.
install.packages("gmodels")
library(gmodels)

#Crosstable VERY GOOD  Shows the 
The CrossTable( ) function in the gmodels package produces crosstabulations 
# modeled after PROC FREQ in SAS or CROSSTABS in SPSS. It has a wealth of options.
# 2-Way Cross Tabulation
library(gmodels)
CrossTable(mpg$class, mpg$cyl)

#table NOT VERY GOOD
# You can generate frequency tables using the table( ) function, tables of proportions using prop.table( ) , 
# and marginal frequencies using margin.table( ).
(part_way <- table(mpg$class,mpg$cyl) ) # A will be rows, B will be columns

margin.table(part_way, 1) # A frequencies (summed over B)
margin.table(part_way, 2) # B frequencies (summed over A)

prop.table(part_way) # cell percentages
prop.table(part_way, 1) # row percentages
prop.table(part_way, 2) # column percentages

ftable( table(mpg$class,mpg$cyl,mpg$drv))

#xtabs Not too good allows you to create crosstabulations using formula style input.
# 3-Way Frequency Table
(mytable <- xtabs(~class+drv+cyl, data=mpg))
ftable(mytable) # print table
summary(mytable) # chi-square test of indepedence



#SQL in R
#https://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/sql.html
#https://seandavi.github.io/ITR/dplyr_intro_msleep.html
#https://cran.r-project.org/web/packages/sqldf/sqldf.pdf
install.packages("sqldf")
library(sqldf)

#install a data package
data()  #lists training data sets you can access
install.packages("nycflights13")
library(nycflights13)
data(package="nycflights13")  ## list data sets in the nycflights package
data(package="tidyverse")
data(package="tidymodels")

library(MASS)
data(package="MASS")


( Inst_pkg_DF <- installed.packages() )

#installed. packages() just the non-base packages
#Open RStudio
#Navigate to Help --> R Help (from the menu above)
#You will see the help panel opened.
#Then follow, Reference --> Packages

#returns a vector with names of installed packages
(Instl_Pkg_Vec <- .packages(all.available = TRUE))

#find paths to locatoins of packages
(gg2 <- try(find.package("ggplot2"), silent = TRUE))
(gg3 <- try(find.package("ggplot3"), silent = TRUE))


sessionInfo()
#sessionInfo() answers a subtly different question. 
#It displays the attached and loaded packages, 
# which is a subset of the installed packages on a machine. 
#And sessionInfo() certainly won't help an admin enumerate 
#  the packages installed for other users 
#  (because their session isn't even active)

# pacman package is an R package management tool (not studied yet)
#http://trinker.github.io/pacman/vignettes/Introduction_to_pacman.html

#Functions and datasets to support Venables and Ripley, 
# "Modern Applied Statistics with S" (4th edition, 2002).
require(MASS) #The following object is masked from ‘package:dplyr’:select
data(package="MASS")
help(package=MASS)

#digression on masking
  # a base R command
  conflicts(detail=TRUE) 
  #And to find the list of environments that contain a version of 
  getAnywhere(x = "select")

  x = "select"
  names(which(sapply(search(), 
      FUN = function(env) exists(x, env, 
        inherits = FALSE, mode = "function"))))
  
  
flights
colnames(flights)
dim(flights)
glimpse(flights)
str(flights)
summary(flights)
summarise_all()
class(flights)
typeof(flights)

unique(flights$dest)
flights %>% count(dest)  
flights %>% count(origin,dest)  
count(flights$dest,"MIA" ) #<-- errors
(Flights_to_MIA <- length(which(flights$dest =="MIA")) )
perc <- function(x, n){ 100*length((which(DFVar == vval))) / length(x) }

sum(flights$dest =="MIA")


#digression into atomic data types
#R's basic data types are character, numeric, integer, complex, and logical.
  #integer vector R: objects are (mostly) immutable.
  IVec = c(1L,4L,7L,2L,-1L) #adding L make an integer
  IVec
  IVec[2:4] #"indexing" using square brackets has been conserved
  IVec[5]
  class(IVec)
  print( paste(typeof(IVec),"<-- integer VECTOR would have been more helpful")) 
  
  #character vector: R objects are (mostly) immutable.
  CVec = c( "1L" ,"44" , "77" ,"aa", "dd") #adding L make an integer
  CVec
  CVec[2:4] #"indexing" using square brackets has been conserved
  CVec[5]
  class(CVec)
  print( paste(typeof(CVec),"<-- Character VECTOR would have been more helpful")) 
  
  #Logical (Boolean) vector: R objects are (mostly) immutable.
  LVec = c( T,F,TRUE,FALSE,T ) # you can use T or TRUE and F or False
  LVec
  LVec[2:4] #"indexing" using square brackets has been conserved
  LVec[5]
  class(LVec)
  print( paste(typeof(LVec),"<-- Logical VECTOR would have been more helpful")) 
  
  #Complex vector: R objects are (mostly) immutable.
  # forget this unless youare an electircal engineer 
  
  # IMPORTANT digression on immutable and mutable objects
  #http://adv-r.had.co.nz/memory.html#modification
  #What happens to x in the following code?
  #In some languages, you have to explicitly delete unused objects 
  #  for their memory to be returned. 
  #  R uses an alternative approach: garbage collection (or GC for short). 
  install.packages("pryr")
  library(pryr)
  mem_used()
  x <- 1:10
  x
  x[5] <- 77
  x
  #There are two possibilities:
  #R modifies x in place.
  #R makes a copy of x to a new location, modifies the copy, 
  #  and then uses the name x to point to the new location.
  #It turns out that R can do either depending on the circumstances. 
  #  In the example above, it will modify in place
  
  x <- 1:10
  y <- x
  c(address(x), address(y))
  #> [1] "0x55eace23d790" "0x55eace23d790"
  
  x[5] <- 6L
  c(address(x), address(y))
  #> [1] "0x55eacecdfca8" "0x55eace23d790"

#back to Exploratory Data Analysis (EDA)
#https://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/sql.html
sqldf("SELECT * FROM iris")
sqldf("SELECT * FROM msleep ORDER BY sleep_total desc 
      LIMIT 5")

sqldf("SELECT avg(awake) as Avg_awake, count(*) as NmbrRows 
      FROM msleep group by genus,, vore")

sqldf("SELECT genus ,vore,avg(awake) as Avg_awake, count(*) as NmbrRows FROM msleep
       group by genus ,vore")
  
data(msleep)

#Is there an R equivalent of SAS PROC FREQ?
#https://stats.stackexchange.com/questions/13855/is-there-an-r-equivalent-of-sas-proc-freq
install.packages("summarytools")
library(summarytools)
dfSummary(CO2, style = "grid", plain.ascii = TRUE)

#n newer versions of summarytools, freq() (which produces straightforward frequency tables, 
install.packages("summarytools")
library(summarytools)
freq(CO2$Plant)
freq(iris$Species)
freq(mpg$cyl)
freq(mpg$drv)


#The freqlist function
#https://cran.r-project.org/web/packages/arsenal/vignettes/freqlist.html
#freqlist() is a function meant to produce output similar to SAS’s PROC FREQ procedure 
# when using the /list option of the TABLE statement.
install.packages("arsenal")
require(arsenal)
data(package="arsenal")  ## list data sets in the nycflights package
data(mockstudy)
(tab.ex <- table(mockstudy[, c("arm", "sex", "mdquality.s")], useNA = "ifany"))
summary(freqlist(~arm + sex + mdquality.s, data = mockstudy))
#Including combinations with frequencies of zero
summary(freqlist(~race + sex + arm, data = mockstudy, sparse = TRUE, digits.pct = 1))
#Options for NA handling
summary(freqlist(tab.ex, na.options = "include"))


#Introduction to Crosstable
#https://cran.r-project.org/web/packages/crosstable/vignettes/crosstable.html
install.packages("crosstable")
library(crosstable)
library(dplyr)
mtcars_labels = read.table(header=TRUE, text="
  name  label
  model 'Model'
  mpg   'Miles/(US) gallon'
  cyl   'Number of cylinders'
  disp  'Displacement (cu.in.)'
  hp    'Gross horsepower'
  drat  'Rear axle ratio'
  wt    'Weight (1000 lbs)'
  qsec  '1/4 mile time'
  vs    'Engine'
  am    'Transmission'
  gear  'Number of forward gears'
  carb  'Number of carburetors'
")
mtcars2 = mtcars %>% 
  mutate(model=rownames(mtcars), 
         vs=ifelse(vs==0, "vshaped", "straight"),
         am=ifelse(am==0, "auto", "manual"), 
         across(c("cyl", "gear"), factor),
         .before=1) %>% 
  import_labels(mtcars_labels, name_from="name", label_from="label") %>% 
  as_tibble()
#I also could have used `labelled::set_variable_labels()` to add labels

################################### yuzaR Data Science ##################################
#################################  Excellent Videos #####################################
#Top 10 Must-Know {dplyr} Commands for Data Wrangling in R!
#https://www.youtube.com/watch?v=XcK4chr2jws

#Combine Tables with {dplyr} DPLYR: the SQL Killer #1
#https://www.youtube.com/watch?v=aL7PeNOvAfw

#Join Tables with {dplyr} DPLYR: the SQL Killer #2
#https://www.youtube.com/watch?v=_w6yLNbxabU&t=20s

#Box Plots in R with t-test, Mann-Whitney, ANOVA, Kruskal-Wallis, Shapiro-Wilk, Levene and post-hocs
#https://www.youtube.com/watch?v=cNJIMwtLWv0

#Master Box-Violin Plots in {ggplot2} and Discover 10 Reasons Why They Are Useful
#https://www.youtube.com/watch?v=rvm94zcoKT0&t=191s

#7 Reasons to Master Scatter Plots in {ggplot2} with World Happiness Data
#https://www.youtube.com/watch?v=E-7_TcR83ew&t=17s

#{emmeans} Game-Changing R-package Squeezes Hidden Knowledge out of Models!
#https://www.youtube.com/watch?v=_okuMw4JFfU

#Mastering {dplyr}: 50+ Data Wrangling Techniques!
#https://www.youtube.com/watch?v=n4kmHjKZh0E

#R package reviews | sjPlot | Easily Visualize Data And Model Results
#https://www.youtube.com/watch?v=r3uKkmU4VQE

#Join Tables with {dplyr}
#https://www.youtube.com/watch?v=_w6yLNbxabU

#R package reviews {gtsummary} Publication-Ready Tables of Data, Statistical Tests and Models!
#https://www.youtube.com/watch?v=hyP3Hx_1kTM

#R demo | Repeated Measures ANOVA (One-Way) | How to Conduct, Visualise and Interpret
#https://www.youtube.com/watch?v=CL7WlwKz5aw

#R demo | Correlation Matrix | How to conduct, visualise and interpret
#https://www.youtube.com/watch?v=ffhQil7KhSo

#R demo | Deep Exploratory Data Analysis (EDA) | explore your data and start to test hypotheses
#https://www.youtube.com/watch?v=Swcp0_l65lw

#R demo | How to visualize models Part 1 | multiple linear models, all assumptions & post-hocs
#https://www.youtube.com/watch?v=BNTn_f43U04&t=27s

#R demo | How to visualize models Part 2 | non linear, logistic, multinomial, mixed effects, survival
#https://www.youtube.com/watch?v=wev5a3rwsvo

#R demo | Many (Grouped / Nested) Models Simultaneously are Very Effective
#https://www.youtube.com/watch?v=tQ8dC0oLTnA&t=64s

#R package reviews | performance | Check ALL model assumptions at once! Check model quality!
#https://www.youtube.com/watch?v=EPIxQ5i5oxs

#Survival analysis 1: a gentle introduction into Kaplan-Meier Curves
#https://www.youtube.com/watch?v=TWyQVtchcpc

#Survival analysis 2: Parametric Exponential Models
#https://www.youtube.com/watch?v=rqg4efjQ9dM
