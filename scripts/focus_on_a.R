library(tidyverse)

a <- read_lines("outputs/01_after_ocr/01-a.ocr.txt")


a <- tibble(raw = a, letter = "a")

# Clean up some brackets
a <- 
  a %>% 
  mutate(raw = str_replace_all(raw, "〔", " ["),
         raw = str_replace_all(raw, "〔", " [")) %>% 
  mutate(raw = str_replace_all(raw, "〕", "] "),
         raw = str_replace_all(raw, "】", "] ")) %>% 
  mutate(raw = str_replace_all(raw, "〔", " ["),
         raw = str_replace_all(raw, "【", " ["),
         raw = str_replace_all(raw, "〖", " [")
  )


# Change the following to "ア|"
all <- 
  all %>% 
  mutate(raw = str_replace_all(raw, "ア1", "ア—"),
         raw = str_replace_all(raw, "ア1", "ア—"),
  )

# Sometimes they are small
all <- 
  all %>% 
  mutate(raw = str_replace_all(raw, "ァ", "ア"),
         raw = str_replace_all(raw, "ぃ", "い"),
         raw = str_replace_all(raw, "ぅ", "う"),
  )


write_csv(a, "test.csv")


start <- sample(x = c(1:nrow(all)), size = 1)
all[start:(start+15),]

rm(start)

# Talk to John:
# For a: line 3070, there is some weird: "あ9" thing - 9 should be "り"
# For i: line 3044, there is some weird: "い9" thing



# a is あ or ア
# i is い or イ
# u is う or ヴ or ウ
# e is え or エ
# o is お or 才


# Add flag for if the line starts with  "あ" or "ア"
a <- 
  all %>% 
  filter(letter == "a") %>% 
  mutate(start_of_a_definition = if_else(str_starts(raw, "あ"), 1, 0),
         start_of_a_definition = if_else(str_starts(raw, "ア"), 1, start_of_a_definition),
         ) %>% 
  mutate(word_counts = cumsum(start_of_a_definition)) %>% 
  group_by(word_counts) %>%
  summarise(words=paste(raw,collapse='')) %>% 
  select(-word_counts) %>% 
  mutate(letter = "a")

i <- 
  all %>% 
  filter(letter == "i") %>% 
  mutate(start_of_a_definition = if_else(str_starts(raw, "い"), 1, 0),
         start_of_a_definition = if_else(str_starts(raw, "イ"), 1, start_of_a_definition),
  ) %>% 
  mutate(word_counts = cumsum(start_of_a_definition)) %>% 
  group_by(word_counts) %>%
  summarise(words=paste(raw,collapse='')) %>% 
  select(-word_counts) %>% 
  mutate(letter = "i")

u <- 
  all %>% 
  filter(letter == "u") %>% 
  mutate(start_of_a_definition = if_else(str_starts(raw, "う"), 1, 0),
         start_of_a_definition = if_else(str_starts(raw, "ウ"), 1, start_of_a_definition),
         start_of_a_definition = if_else(str_starts(raw, "ヴ"), 1, start_of_a_definition),
  ) %>% 
  mutate(word_counts = cumsum(start_of_a_definition)) %>% 
  group_by(word_counts) %>%
  summarise(words=paste(raw,collapse='')) %>% 
  select(-word_counts) %>% 
  mutate(letter = "u")

e <- 
  all %>% 
  filter(letter == "e") %>% 
  mutate(start_of_a_definition = if_else(str_starts(raw, "え"), 1, 0),
         start_of_a_definition = if_else(str_starts(raw, "エ"), 1, start_of_a_definition),
  ) %>% 
  mutate(word_counts = cumsum(start_of_a_definition)) %>% 
  group_by(word_counts) %>%
  summarise(words=paste(raw,collapse='')) %>% 
  select(-word_counts) %>% 
  mutate(letter = "e")

o <- 
  all %>% 
  filter(letter == "o") %>% 
  mutate(start_of_a_definition = if_else(str_starts(raw, "お"), 1, 0),
         start_of_a_definition = if_else(str_starts(raw, "才"), 1, start_of_a_definition),
  ) %>% 
  mutate(word_counts = cumsum(start_of_a_definition)) %>% 
  group_by(word_counts) %>%
  summarise(words=paste(raw,collapse='')) %>% 
  select(-word_counts) %>% 
  mutate(letter = "o")

all <- rbind(a, e, i, o, u)


write_csv(all, "outputs/02_after_cleaning/all.csv")

write_csv(a, "outputs/02_after_cleaning/a.csv")
write_csv(e, "outputs/02_after_cleaning/e.csv")
write_csv(i, "outputs/02_after_cleaning/i.csv")
write_csv(o, "outputs/02_after_cleaning/o.csv")
write_csv(u, "outputs/02_after_cleaning/u.csv")

rm(a, e, i, o, u)



start <- sample(x = c(1:nrow(all)), size = 1)
all[start:(start+15),]