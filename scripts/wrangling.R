# Script summary ----------------------------------------------------------
# Script for workshop on data wrangling with R
# Updated - March 6, 2023
#
# RStudio Projects --------------------------------------------------------
# Read data from absolute path
mpg <- read.csv('C:/Users/jsm4/Desktop/r_wrangling/data/mpg.csv')

# Read data from relative path
mpg <- read.csv('data/mpg.csv')

# Load packages -----------------------------------------------------------
# Install all Tidyverse packages
install.packages("tidyverse")     # Bulk tidyverse
library(tidyverse)                # Load 8 core packages 

# List tidyverse packages
tidyverse_packages(include_self=FALSE)

# Load penguins data ------------------------------------------------------
df <- read_csv('data/penguins.csv')

# Explore -----------------------------------------------------------------
View(df)                          # View the data
str (df)                          # Data structure
head(df,5); slice_head(df,n=5)    # Print first five rows
tail(df,5); slice_tail(df,n=5)    # Print last five rows
nrow(df)                          # Number of rows (observations)
ncol(df)                          # Number of columns (variables)
dim (df)                          # Dimensions (rows x columns)
rownames(df)                      # Print row names
colnames(df); names(df)           # Print column names
complete.cases(df)                # Check if observations are complete
duplicated(df)                    # Check if observations are duplicated
# Tidy --------------------------------------------------------------------
# Missing data
df_sub  <- drop_na(df); df_sub <- na.omit(df)           # Remove incomplete observations
df_sub2 <- drop_na(df, bill_length_mm)                  # Remove specific NAs
c(nrow(df),nrow(df_sub),nrow(df_sub2))                  # Compare observation numbers
df      <- drop_na(df)                                  # Apply to original dataset

# Data entry mistakes
unique(df$penguin); levels(factor(df$penguin))          # Check unique groups
df <- mutate(df, penguin=str_squish(penguin))           # Fix extra white space
df <- mutate(df,                                        # Fix spelling mistakes
             penguin=case_when(
               penguin == 'Pygoscelis papa' ~ 'Pygoscelis papua',
               penguin == 'Pygoscelis antarctica' ~ 'Pygoscelis antarcticus',
               TRUE ~ penguin
             ))
df <- distinct(df)                                      # Remove duplicates

# Simplify and rearrange
df <- mutate(df, penguin=str_split_i(df$penguin,' ',2)) # Split values
df <- rename(df, species=penguin)                       # Rename variable
df <- relocate(df, year, .before='island')              # Rearrange variables

# Change data form
df_long <- read_csv('data/penguins_long.csv')
df_wide <- pivot_wider(df_long,                         # Long to wide form
                       names_from='measurement',
                       values_from='size')

# Subset data
df_subset <- filter(df, island=='Biscoe' & year==2008)

# Summarize ---------------------------------------------------------------
# Descriptive statistics - overall
summary(df)                              # Quick overall summary of input values

mean(df$bill_length_mm, na.rm=TRUE)      # Average of input value
max(df$bill_length_mm, na.rm=TRUE)       # Maximum of input value
min(df$bill_length_mm, na.rm=TRUE)       # Minimum of input value
sd(df$bill_length_mm, na.rm=TRUE)        # Standard deviation of input value
length(df$bill_length_mm)                # Set length of input value

# Descriptive statistics - by groups
df_group <- group_by(df, species)        # Create a grouped data frame

summarize(df_group,                      # Summarize input value
          avg_bill=mean(bill_length_mm),
          max_bill=max(bill_length_mm),
          min_bill=min(bill_length_mm),
          sd_bill=sd(bill_length_mm),
          n_bill=length(bill_length_mm))

summarize(df_group,
          across(4:7, ~mean(.x,na.rm=TRUE)))
# Analyze -----------------------------------------------------------------
# Linear model fit
with(df, plot(body_mass_g~flipper_length_mm))
lm_mod <- lm(body_mass_g~flipper_length_mm, data=df)
summary(lm_mod)

# Analysis of variance (ANOVA)
with(df, plot(x=factor(species), y=body_mass_g))
lm_mod  <- lm(body_mass_g~species, data=df)
aov_mod <- aov(lm_mod)
summary(aov_mod)

# Exercise 1 --------------------------------------------------------------
crime <- USArrests
crime <- rownames_to_column(crime, 'State')

rbind(
  mean(crime$Assault),
  mean(crime[,3]),
  sum(crime$Assault)/length(crime$Assault)
)
summarize(crime, mean(Assault))

max(crime$Assault)
x <- sort(crime$Assault, decreasing=TRUE)
x[1]
summarize(crime, max(Assault))

rbind(
  crime[crime$State=='Pennsylvania',],
  crime[38,]
)
filter(crime, State=='Pennsylvania')

with(crime, plot(x=Assault, y=Murder))
lm_mod <- lm(Murder~Assault, data=crime)
summary(lm_mod)

# Piping methods ----------------------------------------------------------
# Descriptive statistics - by groups
# Summarize the bill length variable
df_group <- group_by(df, species)        # Create a grouped data frame

summarize(df_group,                      # Summarize input value
          avg_bill=mean(bill_length_mm),
          max_bill=max(bill_length_mm),
          min_bill=min(bill_length_mm),
          sd_bill=sd(bill_length_mm),
          n_bill=length(bill_length_mm))

# Summarize the bill length variable
# Pipe df_group object into the summarize function
df_group %>%  summarize(avg_bill=mean(bill_length_mm),
                        max_bill=max(bill_length_mm),
                        min_bill=min(bill_length_mm),
                        sd_bill=sd(bill_length_mm),
                        n_bill=length(bill_length_mm))

# Multiple object approach
a <- filter(df, year==2008)
b <- group_by(a, species)
c <- summarize(b,
               Avg=mean(body_mass_g))
d <- arrange(c, desc(Avg))
print(d)

# Nested function approach
arrange(
  summarize(
    group_by(
      filter(df,year==2008),
      species),
    Avg = mean(body_mass_g)),
  desc(Avg)
)

# Piping approach
df %>%
  filter(year==2008) %>%
  group_by(species) %>%
  summarize(Avg=mean(body_mass_g)) %>%
  arrange(desc(Avg))

# native pipe
df |> 
  filter(year==2008) |> 
  group_by(species) |> 
  summarize(Avg=mean(body_mass_g)) |> 
  arrange(desc(Avg))

# Exercise 2 --------------------------------------------------------------
# Load LTER data
crabs <- read_csv('data/crabs.csv')

# Check quick summary for variables with NAs
crabs |> summary()

# Check for duplicate observations
crabs |> duplicated()

# Check how many observations at each site
crabs |> group_by(site) |> count()

# Clean up the data
crabs <- crabs |> 
  drop_na(size) |> 
  distinct() |> 
  mutate(site=case_when(
    site == 'PI' ~ 'PIE',
    TRUE ~ site
  ))

# Summarize crab size by site
crabs |> 
  group_by(site) |> 
  summarize(
    avg_size=mean(size),
    min_size=min(size),
    max_size=max(size),
    sd_size=sd(size),
    n=n()
  )

# Does the data support Bergmann’s rule
crabs |> 
  group_by(site, latitude) |> 
  summarize(avg_size=mean(size)) |> 
  ggplot(aes(x=latitude,y=avg_size)) + 
  geom_point() +
  geom_smooth(method='lm')
