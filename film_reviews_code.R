install.packages("robotstxt")
install.packages("rvest")

library(robotstxt)
library(rvest)

lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")

# This code read and save the data from the webpage "metacritic" in the internet and especially these in the page that is referred to the newspaper "The Washington Post". These data are the critics that has done to various movies along the years that is being a participant to this specific site.
# So after some computations and assignments this code creates a data frame with all the movies, total scores of all the critics, scores of this newspaper and the date that the specific critic is posted, for 27 pages in a row.
# It has to be clarified that each movie is shown to each page based on the date that this newspaper has done its critic. That's why at the end the final data are sorted in this way.

# Check whether scraping is allowed from this webpage (returns TRUE)
# ATTENTION: PUT THE WHOLE URL IN ONE LINE WHEN RUNNING THE CODE
paths_allowed("https://www.metacritic.com/publication/washington-post?filter=movies&num_items=100&sort_options=date&page=0")

# Define character element "main.page", to be used recursively for defining
# multiple pages from metacritic.com
# ATTENTION: PUT THE WHOLE URL IN ONE LINE WHEN RUNNING THE CODE
main.page <- "https://www.metacritic.com/publication/washington-post?filter=movies&num_items=100&sort_options=date&page="

for (i in 0:27){ # This is a "for" loop.
  # This means that all the lines until the closure of }
  # will be repeated for different values of object i
  # thus, on the first run i=0, second run i=1,... last run i=27
  
  # for each step, define...
  step.page <- paste(main.page,i,sep="") #create a vector which combines the vector main.page and the iterator "i" without any separator between them. 
                                        #This means that in every iteration the webpage will change and new data will be shown. 
                                        #This main page shows the Whashington Post's Score ordered by date of post.
 
  webdata <-read_html(step.page) # OK
  
  # Vector ... is created which includes .....
  #creates a character vector named "title" which includes the names of the movies that each page shows 
  title <-c(webdata %>% html_nodes("div.review_product") %>% html_nodes("a") %>% html_text())
  
  #creates a character vector named "metascore" which includes the total score of each movie of the page we are (has the same movies as vector "titles")
  metascore <- c(webdata %>% html_nodes("li.review_product_score.brief_metascore") %>% html_nodes("span.metascore_w") %>% html_text())
  
  #creates a character vector named "critic" that contains for each movie (same as those in "titles" and "metascore") the score that the newspaper "The Washington Post" has put
  critic <- c(webdata %>% html_nodes("li.review_product_score.brief_critscore") %>% html_nodes("span.metascore_w") %>% html_text())
  
  #creates a character vector named "date" that contains the exact date that the newspaper "The Washington Post" has posted its critic and score to each movie in the page that we are in every iteration
  date <- c(webdata %>% html_nodes("li.review_action.post_date") %>% html_text())
  
  if (length(date)<100 ){for (j in length(date):100){ date[j] <- date[length(date)]}} #OK
  
  #creating slices from the vector "date"
  a <- substr(date,12,13) #extract from the vector "date" the day and assign it to a character vector "a"
  b <- substr(date,8,10) #extract from the vector "date" the month and assign it to a character vector "b"
  d <- substr(date,16,19) #extract from the vector "date" the year and assign it to a character vector "d"
  
  date2 <- apply(cbind(a,b,d),1,paste,collapse="/") 
  #use the apply function in order to combine via the function paste the values of each vector ("a","b","d") and at the same time these values are been separated with the "/" symbol creating a character vector "date2".
  #Actually, cbind(a,b,d) means that the system places each value next to each other, the "1" indicates rows, "paste" is a function that puts together each value of "a", "b" and "d" and the "collapse" is the separator between the values that is combined together.  
  date3 <- as.Date(date2,"%d/%b/%Y") #convert the character vector "date2" as Date object with day the first two numbers of each attribute of "date2", month the the 2nd attribute between the two slices ("/") and year the last number of each attribute of "date2"
  
  #assign to "df" a data frame which consists of the vectors "title", in the 1st column, "metascore" in the 2nd, "critic" in the 3rd and "date3" at the last column
  df = data.frame(title,metascore,critic,date3)
  
  #assign as column names of the "df" data frame the characters of the parenthesis (1st character to the 1st column, 2nd to the 2nd etc).
  #If we don't do this step, then the name of the 4th column would be "date3"
  colnames(df) <- c("title", "metascore", "critic","date") 
 
  #convert the column "metascore" of data frame "df" first as character (not needed) and last as numeric
  df$metascore <- as.numeric(as.character(df$metascore))

  #the same as we did with "metascore", we do and here but with the column named "critic"
  df$critic <- as.numeric(as.character(df$critic))
  
  #assign to the data frame "df" the rows of the data frame "df" that are complete with values and they don't have any missing value (NA)
  df <- df[complete.cases(df), ] 
  
  if (i==0){ #OK
    df.tot <- df} #OK
  if (i>0){ #for every positive iterator "i" (actually, for all the times except the first one) the "if" case is true
    df.tot <- rbind(df.tot,df) } #we save in a new data frame called "df.tot" (tot from total) the data of each page (titles of movies, total score etc) that are not missing values and the one under the other.

}

#convert the first column of "df.tot" as character
df.tot$title <- as.character(df.tot$title)

#2
#The data frame "df.tot" shows all the data of the first 27 pages of the "metacritics" website, where "The Washington Post" has made its critics for different kind of movies. 
#This data frame consists of the title of the movie, the total score from all the critics of the site for each movie, the score that this newspaper has put to every movie and the date which the latter score has been published. All the above data are sorted based upon the date of the newspaper's critics.
#So, by using the function str() we can see the structure of this data frame and especially we can see the total observations and variables (as a number), the first rows of each column this data frame has and the type of the data for each column.
str(df.tot)

#3
df.tot$ratio <- df.tot$metascore/df.tot$critic
df.tot$perc.meta <- rank(df.tot$metascore)/nrow(df.tot)
df.tot$perc.critic <- rank(df.tot$critic)/nrow(df.tot)
df.tot$year <- substring(df.tot$date,1,4)

#4
df.tot$title[which.max(df.tot$metascore)]

#5
df.tot
boxplot(split(df.tot[,6], df.tot$year))
abline(h=0.5)
# We can see in our graph, that the vertical line (y=0.5), which depicts the median, cross in the middle the first and sixth boxplot (year = 2010 & 2015).
# From this observation we can conclude that at these two years, half of the values (metascore values) where above the median and half of them where under it.
# This shows us that half of the movies in both two years got above 50 and the other half got under 50.

#6
#In column named "ratio" we have some infinitive values, because the newspaper "The Washington Post" has put "0" as a critic score. 
df.tot2 <- df.tot[!is.infinite(df.tot$ratio),]

#7
matr <- matrix(c(df.tot2$metascore, df.tot2$critic), ncol=2)
v <- c(apply(matr, 1, mean))

#8
plot(df.tot2$date, df.tot2$perc.meta, main="Metascore percentiles", xlab="date", ylab="perc.meta", col = c(ifelse(df.tot2$metascore > 50, "purple", "orange")), las = 1, pch = 16)
abline(h = (df.tot2$perc.meta[which(df.tot2$metascore == 50)]), lty = 2,)

#9
# The above graph includes all the values of metascore's percentile per year, and especially those which are the metascore greater than 50 are coloured with "purple" and the others with "orange".
# Taking into account that the range of metascore's values can be between 0 and 100, we can assume that the newspaper, "The Washington Post", has put as score, at the majority of its critics, higher than 50.
# Because of the fact that the percentile depicts the worth of a value compared to the others, this graph shows that most of the critics of this newspaper are above the vertical line, the median or the 50th percentile.