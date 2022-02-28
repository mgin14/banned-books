# load libraries
library(tidyverse)
library(rvest)

# initial year for project
url <- "https://www.ila.org/initiatives/banned-books-week/books-challenged-or-banned-in-2010-2011"

# read data in
books_2010_2011 <- read_html(url)

# Gather the authors section in the html inspect page
authors <- books_2010_2011 %>% 
  html_nodes("h4") %>% 
  html_text()

# Get the count of authors list to make sure the rows match
length(authors)

# Gather the book title section in the html
book_titles <- books_2010_2011 %>% 
  html_nodes("h3") %>% 
  html_text()

# Get the count of titles list
length(book_titles)

# Rows did not match, more titles than authors. Searched and found empty author
# sections. Will exclude the titles
# Create date frame to join data
bb2010_2011 <- data.frame(Authors = authors,
                          Title = book_titles[c(1:23, 25:46)])

# Save as a csv to manually input year banned/challenged, state that challenged it,
# reason, and whether it was Banned, Challenged, or Other
 write_csv(bb2010_2011, "bb_main_data.csv")
 
 

#################### MORE SCRAPING FROM OTHER PAGES #####################
############## 2011-2012 ###############

# DO SAME THING AS ABOVE
url2 <- "https://www.ila.org/initiatives/banned-books-week/books-challenged-or-banned-in-2011-2012"

books_2011_2012 <- read_html(url2)

authors_2011 <- books_2011_2012 %>% 
  html_nodes("h4") %>% 
  html_text()

length(authors_2011)

book_titles_2011 <- books_2011_2012 %>% 
  html_nodes("h3") %>% 
  html_text()

length(book_titles_2011)

bb2011_2012 <- data.frame(Authors = authors_2011,
                          Title = book_titles_2011[c(1:3, 5:42)])

write_csv(bb2011_2012, "join_books.csv")

# I didn't know how to join the two data frames together once I added info from
# the data frame from earlier. I tried left join but I think there was a problem
# with the column count... or I did it wrong so I had to manually copy and paste
# the new year's titles and authors in the excel sheet.


########################### OTHER YEARS #########################
# PRIOR YEARS CAN BE FOUND USING THIS URL, REPLACE THE YEARS WITH THE NEW FISCAL YEARS
# 2012-2013, 2013-2014, 2014-2015
# https://www.ila.org/initiatives/banned-books-week/books-challenged-or-banned-in-2011-2012

# 2016-2019 had to be manually inputted from looking at pdfs
# http://www.ala.org/advocacy/
# 2019: https://www.utoledo.edu/commissions/banned-books/docs/ALAFieldReport2019.pdf

# AS A REMINDER: I reused the same code since they were all formatted the same
# Which helped since I didn't have to recreate or create new variables about 3x

# load libraries (kept forgetting to do so up above when I would reopen this)
library(tidyverse)
library(rvest)

# 2016 was the last html page ALA had of the book list
url2 <- "https://www.ila.org/initiatives/banned-books-week/books-challenged-or-banned-in-2016-by-ro"

# read in data
books <- read_html(url2)

# Gather authors list
authors <- books %>% 
  html_nodes("h4") %>% 
  html_text()

# Get count of authors to match the titles
length(authors)

# Gather book titles list
book_titles <- books %>% 
  html_nodes("h3") %>% 
  html_text()

# Get count of titles to match the authors list
length(book_titles)

# Create data frame with matching row. Modify 
bb2016 <- data.frame(Authors = authors,
                          Title = book_titles[c(1:26, 28:36, 38:45)])


# ONLY GET CHILDRENS-YA BOOKS, EXCLUDE TITLES BELOW
filter_titles <- c("Fight Club", "Fifty Shades of Grey", "Shakespeare", "Beloved",
                   "Nickel and Dimed: On (Not) Getting By in America", "The Bluest Eye", 
                   "The Handmaid's Tale", "The Kite Runner", "Brave New World",
                   "Song of Solomon", "The Glass Castle", "Persepolis: The Story of a Childhood",
                   "The Working Poor: Invisible in America", "Of Mice and Men",
                   "The Things They Carried", "Fun Home: A Family Tragicomic",
                   "City of Thieves", "The Lovely Bones", "To Kill a Mockingbird",
                   "I Know Why the Caged Bird Sings")

# ONLY GET CHILDRENS-YA BOOKS, EXCLUDE AUTHORS BELOW
filter_authors <- c("Morrison, Toni", "Atwood, Margaret", "Shakespeare", 
                    "Hosseini, Khaled", "Huxley, Aldous", "Walls, Jeannette", 
                    "Shipler, David K.", "Steinbeck, John", "O'Brien, Tim", 
                    "Bechdel, Alison", "Benioff, David", "Sebold, Alice", "Lee, Harper",
                    "Angelou, Maya")

bb2016 <- bb2016 %>% 
  filter(!(Authors %in% filter_authors)) %>% 
  filter(!(Title %in% filter_titles))

# Overwrite csv file and save new data frame to manually join to main_data_csv
write_csv(bb2016, "join_books.csv")



############ TEST OUT DATA AND CODES ###############

library(tidyverse)
library(janitor)

getwd()
dir()

banned_data <- read_csv("bb_main_data.csv")


# Learn to separate values to separate rows
banned_data %>% 
  separate_rows(5,sep = ", ") %>% 
  group_by(`State(s)`) %>% 
  count() %>% 
  View()

banned_data %>% 
  separate_rows(6,sep = ", ") %>%
  group_by(Reason) %>% 
  count(Reason)

#Find out if the number of books fluctuates throughout the decade
# Question: How does the number of books, per challenge, change over time?
banned_data %>% 
  separate_rows(6, sep = ", ") %>%
  mutate(Reason = ifelse(Reason == "Occult",
                         "Unsuitable for age group", Reason)) %>% 
  group_by(Year, Reason) %>% 
  count(Reason) %>% 
  ggplot(aes(x = Year,
             y = n,
             color = Reason)) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +
  geom_point() +
  geom_line() +
  geom_label(aes(label = n)) +
  facet_wrap(~Reason) +
  labs(x = "Years",
       y = "Total Amount of Books")



# Are more books being banned?
banned_data %>% 
  separate_rows(7, sep = ", ") %>% 
  group_by(Year, `Challenged vs Banned`) %>% 
  count(`Challenged vs Banned`) %>% 
  ggplot(aes(x = Year,
             y = n,
             fill = `Challenged vs Banned`)) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +
  geom_col(position = "dodge")



# How many books were challenged between age groups
banned_data %>%
  group_by(Audience) %>% 
  mutate(Audience = ifelse(Audience == "young-adult",
                           "YA", Audience)) %>% 
  count(Audience) %>% 
  ggplot(aes(x = Audience,
             y = n,
             fill = Audience)) +
  geom_col() +
  geom_label(aes(label = n)) +
  ylab("Total Amount of Books")

# Is this true throughout the years?
banned_data %>%
  group_by(Year, Audience) %>% 
  count(Audience) %>% 
  ggplot(aes(x = Year,
             y = n,
             fill = Audience)) +
  scale_x_continuous(breaks = seq(2010, 2020, by = 1)) +
  geom_col(position = "dodge") +
  ylab("Total Amount of Books")


# What reason is most reported for each audience
banned_data %>% 
  separate_rows(6, sep = ", ") %>% 
  mutate(Reason = ifelse(Reason == "Occult",
                         "Unsuitable for age group", Reason)) %>% 
  group_by(Audience, Reason) %>%
  count(Reason) %>%
  ggplot(aes(y = Reason,
             x = n,
             fill = Reason)) +
  geom_col() +
  geom_label(aes(label = n)) +
  facet_wrap(~Audience) +
  xlab("Total Amount of Books")

