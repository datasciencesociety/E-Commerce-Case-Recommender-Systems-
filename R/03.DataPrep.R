
setwd("/Users/anapopova/SummerSchool2019/01.Challenges/03.Recommender/E-Commerce-Case-Recommender-Systems-")

source("R/01.MAIN.R")
source("R/02.HelperFunctions.R")

# ------------------------------------------------------------------------------
# Load the data
# ------------------------------------------------------------------------------

dirName <- "Data/"

# 1) Category tree
CatTree <- read_csv(file = paste0(dirName, "category_tree.csv"))

# 2) Events
Events <- read_csv(file = paste0(dirName, "events.csv"))

# 3) Portion 1 of item properties
Items1 <- read_csv(file = paste0(dirName, "item_properties_part1.csv"))

# 4) Portion 2 of item properties
Items2 <- read_csv(file = paste0(dirName, "item_properties_part2.csv"))

InitItems <- bind_rows(Items1, Items2)
rm(Items1, Items2)

# ------------------------------------------------------------------------------
# Explore separate datasets
# ------------------------------------------------------------------------------

glimpse(CatTree)
glimpse(Events)
glimpse(InitItems)  # only categoryid

# ------------------------------------------------------------------------------
# Merge datasets
# ------------------------------------------------------------------------------

Items <- InitItems %>% 
  filter(property %in% c("categoryid", "available")) %>% 
  dcast(., timestamp + itemid ~ property, value.var = "value") %>% 
  filter(!is.na(categoryid)) %>% 
  mutate_at(vars(categoryid), list(~as.numeric(.)))

Linked <- Items %>% 
  left_join(., CatTree, by = "categoryid") %>% 
  mutate(timeEdited = timestamp) %>% 
  mutate_at(vars(timeEdited), list(~ (./1000))) %>% 
  mutate_at(vars(timeEdited), list(~anytime(.))) %>% 
  mutate(week = week(timeEdited))

write_csv(Linked, "Output/ItemTree.csv")

Events <- Events %>% 
  mutate(timeEdited = timestamp) %>% 
  mutate_at(vars(timeEdited), list(~ (./1000))) %>% 
  mutate_at(vars(timeEdited), list(~anytime(.))) %>% 
  # mutate(year = year(timeEdited)) %>% # all is in 2015
  mutate(week = week(timeEdited))

ComboSet <- Events %>% 
  left_join(., Linked, by = c("itemid", "week")) %>% 
  rename(timeEvents = timeEdited.x) %>% 
  rename(timeItemCats = timeEdited.y)

write_csv(ComboSet, "Output/ComboSet.csv")
  

# TestProp <- Items %>% 
#   filter(property %in% c("categoryid", "available")) %>% 
#   group_by(timestamp, itemid) %>% 
#   filter(property == "categoryid") %>% 
#   summarise(count = n())
# 
# TestAv <- Items %>% 
#   filter(property %in% c("categoryid", "available")) %>% 
#   group_by(timestamp, itemid) %>% 
#   filter(property == "available") %>% 
#   summarise(count = n())

