#Includes
library(tidyverse)
library(summarytools)
library(kableExtra)

# Import data (Pls make sure to have the data copied into the data folder)
res_raw <- read_csv("data/results-survey421871.csv")

# Colnames need to be made nicer.
# First we make a reference list for the questions/columns.
lut_cols <-names(res_raw)
lut_cols <- str_split_fixed(lut_cols, fixed(". "), n = 2)
lut_cols <- as.data.frame(lut_cols)

# Then we delete the questions and leave the codes.
names(res_raw) <- lut_cols$V1

# Throw away the useless cols.
res <- res_raw |> 
  select(-c(submitdate, startlanguage, seed))

# Replace "N/A" value with the real R NA.
res <- res |>
  mutate(across(everything(), ~ na_if(.x, "N/A")))


# Exclude all empty responses (lastpage == 0).
res <- res |> 
  filter(lastpage > 0)

# List all fully empty cols. This is done with select(), where() and to choose the logical value from all(is.na()), finally we only keep the names.
na_cols <- res |> 
  select(where(function(x) all(is.na(x)))) |> 
  names() 

# Exclude the fully empty cols.
res_2 <- res |> select(-all_of(na_cols))


# Recode rating scales
res_2 <- res_2 %>% 
  mutate(across(c(starts_with("G01Q01"),
                starts_with("G01Q07"),
                starts_with("G01Q08"),
                starts_with("G01Q09"),
                starts_with("G01Q11"),
                starts_with("G01Q12"),
                starts_with("G01Q13")), 
                ~str_replace(.x, "7 - Very much", "7")))

res_2 <- res_2 %>% 
  mutate(across(c(starts_with("G01Q01"),
                  starts_with("G01Q07"),
                  starts_with("G01Q08"),
                  starts_with("G01Q09"),
                  starts_with("G01Q11"),
                  starts_with("G01Q12"),
                  starts_with("G01Q13")), 
                ~str_replace(.x, "1 - Not at all", "1")))

# Rating scales are not yet numeric, thus detect numbers. "No answer" responses will be autorecoded to "N/A"
res_2 <- res_2 %>% 
  mutate(across(c(starts_with("G01Q01"),
                  starts_with("G01Q07"),
                  starts_with("G01Q08"),
                  starts_with("G01Q09"),
                  starts_with("G01Q11"),
                  starts_with("G01Q12"),
                  starts_with("G01Q13")), 
                ~parse_number(.x)))


##### Create Tables for presentation in rmd ############

###### Networks ######

# Select responses to the question
net <- res_2 |>  
  select(6:7)

# The responses are split up in two questions, thus combine the responses in one vector
# Extract first col (Note: magrittr pipe, b/c native pipe does not support dot notation)
net_1 <- net %>% 
  select(1) %>%
  drop_na() %>% 
  rename(V1 = names(.))

# Extract second col (Note: magrittr pipe, b/c native pipe does not support dot notation)
net_2 <- net %>% 
  select(2) %>%
  drop_na() %>% 
  rename(V1 = names(.))
  
# Bind the two vectors
net <- bind_rows(net_1, net_2)

# Create kable table (sort responses ascending)
tab_net <- net |> 
  arrange(V1) |> 
  kable("html", col.names = NULL) |>
  kable_styling()

###### Other OH topics working on: ######

# Create kable table (sort responses ascending)
tab_otherOH <- res_2 |> 
  select(`G01Q04[other]`) |> 
  filter(!is.na(`G01Q04[other]`)) |> 
  arrange(`G01Q04[other]`) |> 
  kable("html", col.names = NULL) |>
  kable_styling()


###### OH activities and projects ###### 

## The OH activities are split up in multiple df columns. 
## Thus we combine them in one vector with a function.
splitmerge <- function(df) {
  
  OHact <- list()
  
  for (i in 1:ncol(df)) {
    OHact[i] <- df %>% 
      select(!!i) %>%
      drop_na() %>% 
      rename(V1 = names(.))
  }
  
  OHact <- unlist(OHact)
  
  return(OHact)
  
}

## Run the previously created function on the respective sub-df.
OHact <- splitmerge(select(res_2, 18:23))

# Create kable table (sort responses ascending)
tab_OHact <- OHact |>
  sort() |> 
  kable("html", col.names = NULL) |>
  kable_styling()


###### Hindered #######

# Create kable table (sort responses ascending)
tab_hindered <- res_2 |>
  select(`G01Q05x1[other]`) |>
  filter(!is.na(`G01Q05x1[other]`)) |>
  arrange(`G01Q05x1[other]`) |> 
  kable("html", col.names = NULL) |>
  kable_styling()


###### Collab #######

# Create kable table (sort responses ascending)
tab_collab <- res_2 |>
  select(`G01Q06[other]`) |>
  filter(!is.na(`G01Q06[other]`)) |>
  arrange(`G01Q06[other]`) |> 
  kable("html", col.names = NULL) |>
  kable_styling()

###### Benefits #######

# Create kable table (sort responses ascending)
tab_benefits <- res_2 |>
  select(`G01Q10[other]`) |>
  filter(!is.na(`G01Q10[other]`)) |>
  arrange(`G01Q10[other]`) |>
  kable("html", col.names = NULL) |>
  kable_styling()


###### Comments #######

# Create kable table (sort responses ascending)
tab_comm <- res_2 |>
  select(G08Q18) |>
  filter(!is.na(G08Q18)) |>
  kable("html", col.names = NULL) |>
  kable_styling()
  
  
###### Contact #######

# Create kable table (sort responses ascending)
# tab_contact <- res_2 |>
#   select(G02Q14) |>
#   filter(!is.na(G02Q14)) |>
#   kable("html", col.names = NULL) |>
#   kable_styling()

##Export contact list
res_2 |>
  select(G02Q14) |>
  filter(!is.na(G02Q14)) |>
  write.csv2(file = "contacts.csv")
