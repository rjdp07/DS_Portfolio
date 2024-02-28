library(tidyverse)
library(tidytext)
library(stringr)
library(data.table)


nasa_eda = fread("projects\\NASA_EDA\\EVA_NASA.csv") %>% as_tibble()

nrow(nasa_eda) == length(unique(nasa_eda$`EVA #`))

nasa_eda %>% 
  filter(is.na(`EVA #`))

nasa_eda = nasa_eda %>% 
  filter(!is.na(`EVA #`))
nrow(nasa_eda) == length(unique(nasa_eda$`EVA #`))


#Convert data to have 1 word per row
tidy_nasa_eda = nasa_eda %>% 
  unnest_tokens(word,Purpose) %>% 
  anti_join(stop_words)


#Count words to see common words

tidy_nasa_eda %>% 
  count(word, sort = TRUE)

tidy_nasa_eda %>% 
  filter(word == "eva")

nasa_eda %>% 
  filter(str_detect(Purpose, "eva")) %>% 
  select(Purpose) %>% 
  pull(Purpose)


#Remove EVA since all of the purpose rows are pertaining to a specific eva
tidy_nasa_eda = tidy_nasa_eda %>% 
  filter(word != "eva")


tidy_nasa_eda %>% 
  count(word, sort = TRUE)

#Check how many integers that are numbers were included on tokenization
tidy_nasa_eda %>% 
  mutate(
    numbers = as.numeric(word)
  ) %>% 
  filter(!is.na(numbers))

#Proceed to remove numbers from the tokens
tidy_nasa_eda = tidy_nasa_eda %>% 
  mutate(
    is_number = as.numeric(word)
  ) %>% 
  filter(is.na(is_number)) %>% 
  select(-is_number)

tidy_nasa_eda %>% 
  count(word, sort = TRUE)


#Explore tf-idf

tidy_nasa_eda %>% 
  count(`EVA #`,word, sort = TRUE) %>% 
  bind_tf_idf(word, `EVA #`,n) %>% 
  group_by(`EVA #`) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(
    word = reorder(word, tf_idf)
  ) %>% 
  filter(
    `EVA #` %in% 1:12
  ) %>% 
  ggplot(aes(x = word, y = tf_idf, fill = as.factor(`EVA #`) )) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~`EVA #`, scales = "free") +
  coord_flip()








