library(dplyr)
library(countrycode)
library(ggplot2)
library(broom)
library(purrr)
library(tidyr)

#CLEAN AND MANIPULATE DATA
votes <- readRDS("votes.rds")
head(votes)

# The vote column in the dataset has a number that represents that country's vote:
#1 = Yes, 2 = Abstain, 3 = No, 8 = Not present, 9 = Not a member
# clean the data to remove observations that are "Not present" and "Not a member".
# Translate Correlates of War codes to recognizable country names.
votes_processed <- votes %>%
	filter(vote <= 3) %>%
	mutate(year = session + 1945, #since UN started in 1946
				 country = countrycode(ccode, "cown", "country.name"))

head(votes_processed) 
# Find total and fraction of "yes" votes by country
by_country <- votes_processed %>%
	group_by(country) %>%
	summarize(total = n(),
						percent_yes = mean(vote == 1))

# ANALYSIS:
by_country %>% arrange(percent_yes)
#country that votes yes least often is Zanzibar, 0%

by_country %>% arrange(desc(percent_yes))
#country that votes yes most often is Sao Tome and Principe, 97.6%

#Zazibar has only 2 votes in total, we certainly can't make any substantial conclusions based on that data 
#filter out countries with fewer than 100 votes
by_country %>%
	arrange(percent_yes) %>%
	filter(total >= 100)

by_year <- votes_processed %>%
	group_by(year) %>%
	summarize(total = n(),
						percent_yes = mean(vote == 1))

# Create line plot
ggplot(by_year, aes(year, percent_yes)) +
	geom_line()

# We then explore the trends of voting within specific countries
# Group by year and country: by_year_country
by_year_country <- votes_processed %>%
	group_by(year, country) %>%
	summarize(total = n(),
						percent_yes = mean(vote == 1))

# Vector of six countries to examine
countries <- c("United States", "United Kingdom",
							 "France", "Japan", "Brazil", "India")

# Filtered by_year_country: 
filtered_6_countries <- by_year_country %>%
	filter(country %in% countries)

# Line plot of % yes over time by country
ggplot(filtered_6_countries, aes(year, percent_yes)) +
	geom_line()+
	facet_wrap(~country, scales="free_y")

# Nest all columns besides country, perform a linear regression on each country's data
country_coefficients <- by_year_country %>%
	nest(-country) %>%
	mutate(model = map(data, ~ lm(percent_yes ~ year, data = .)),
				 tidied = map(model, tidy))%>%
	unnest(tidied)

head(country_coefficients)

# Filter by adjusted p-values
filtered_countries <- country_coefficients %>%
	filter(term == "year") %>%
	mutate(p.adjusted = p.adjust(p.value)) %>%
	filter(p.adjusted < .05)

# Sort for the countries increasing most quickly (South Africa)
filtered_countries %>%
	arrange(desc(estimate))

# Sort for the countries decreasing most quickly (Korea)
filtered_countries %>%
	arrange(estimate)

descriptions <- readRDS("descriptions.rds")
# Join votes and descriptions together 
votes_joined <- votes_processed %>% 
	inner_join(descriptions, by=c("rcid", "session"))

#Visualizing colonialism votes
US_co_by_year <- votes_joined %>%
	filter(country == "United States", co == 1) %>%
	group_by(year) %>%
	summarize(percent_yes = mean(vote == 1)) %>%
	ggplot(aes(year, percent_yes)) +
	geom_line()

#tidy the dataset based on topic
votes_tidied <- votes_joined %>% 
	gather(topic, has_topic, 9:14) %>% #transform the data so that each row has one combination of country-vote-topic
	filter(has_topic == 1)%>% #filter our cases where has_topic is 0.
	mutate(topic = recode(topic,
												me = "Palestinian conflict",
												nu = "Nuclear weapons and nuclear material",
												di = "Arms control and disarmament",
												hr = "Human rights",
												co = "Colonialism",
												ec = "Economic development"))

by_country_year_topic <- votes_tidied %>%
	group_by(country, year, topic) %>%
	summarize(total = n(),
						percent_yes = mean(vote==1)) %>%
	ungroup()

# Filter by_country_year_topic for just the US
US_by_country_year_topic <- by_country_year_topic %>%
	filter(country == "United States")

ggplot(US_by_country_year_topic, aes(year, percent_yes))+
	geom_line()+
	facet_wrap(~topic)

# Fit model on the by_country_year_topic dataset
country_topic_coefficients <- by_country_year_topic %>%
	nest(-country, -topic) %>%
	mutate(model = map(data, ~ lm(percent_yes ~ year, data = .)),
				 tidied = map(model, tidy)) %>%
	unnest(tidied)

country_topic_filtered <- country_topic_coefficients %>%
	filter(term == "year") %>%
	mutate(p.adjusted = p.adjust(p.value)) %>%
	filter(p.adjusted <0.05)

arrange(country_topic_filtered, estimate)
# combination of Vanuatu on the Palestinian conflict has the steepest downward trend
# Over its history, Vanuatu (an island nation in the Pacific Ocean) sharply changed its pattern of voting on the topic of Palestinian conflict.
# Let's examine this country's voting patterns more closely.
# Create vanuatu_by_country_year_topic
vanuatu_by_country_year_topic <- by_country_year_topic %>%
	filter(country == "Vanuatu")

ggplot(vanuatu_by_country_year_topic, aes(year, percent_yes)) +
	geom_line()+
	facet_wrap(~topic)


