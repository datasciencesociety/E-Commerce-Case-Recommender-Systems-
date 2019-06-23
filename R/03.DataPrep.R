
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

# tmp <- (Linked %>% select(itemid, categoryid,parentid)) #%>% 
  # group_by(itemid) %>% 
  # summarise(count = n())

ComboSet <- Events %>% 
  left_join(., 
            (Linked %>% select(-categoryid, -parentid)), 
            by = c("itemid", "week")) %>% 
  rename(timeEvents = timeEdited.x) %>% 
  rename(timeItemCats = timeEdited.y) %>% 
  rename(timestampEvents = timestamp.x) %>% 
  rename(timestampItemCats = timestamp.y) %>% 
  left_join(., 
            (Linked %>% select(itemid, categoryid,parentid)),
            by = "itemid")



# to dummy from events
tmp <- ComboSet %>% 
  select(event) %>% 
  to_dummy(., suffix = "label")

ComboSet <- ComboSet %>% 
  bind_cols(., tmp)

info <- infoNA(ComboSet)
write_csv(ComboSet, "Output/ComboSet.csv")

ComboSet %>% skim()

ComboSet %>% 
  filter(event_view <= 0) %>% 
  group_by(visitorid, week) %>% 
  summarise(count =n()) %>%
  arrange(desc(count)) %>% 
  ungroup()

ComboSet %>% 
  filter(event_addtocart <= 0) %>% 
  group_by(visitorid, week) %>% 
  summarise(count =n()) %>%
  arrange(desc(count)) %>% 
  ungroup()

ComboSet %>% 
  group_by(visitorid) %>% 
  summarize(count = n()) %>% 
  top_n(10, wt = count) %>%
  arrange(desc(count)) %>% 
  ggplot(aes(x = reorder(visitorid, count), y = count))+
  geom_bar(stat = "identity", fill = "royalblue", colour = "blue") +
  labs(x = "", y = "Top 10 Best Sellers", title = "Most Ordered Products") +
  coord_flip() +
  theme_grey(base_size = 12)

ComboSet <- ComboSet %>% 
  mutate(tod = hour(timeItemCats))



# ------------------------------------------------------------------------------

# Establish event history

# Create a user-item matrix
Test <- ComboSet %>% 
  group_by(visitorid, week)







# ------------------------------------------------------------------------------
# pick pov - view, add to cart, purchase

# - visitor
# - items


# Check how many unique item-cat-parent combos there are
# test <- ComboSet %>% 
#   group_by(itemid, categoryid, parentid) %>% 
#   summarise(count = n())
# test2 <- test %>% 
#   filter(is.na(categoryid) & is.na(parentid))  # 49815 items with no cat and parent
# 
# # Look into visitors
# length(unique(ComboSet$visitorid))  # 1407580


#event, category, time, availability






# # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# # Detailed look at category tree
# 
# test <- CatTree %>% 
#   left_join(., ., by = c("parentid", "categoryid")) %>% 
#   left_join(., ., by = "parentid") 
#   
# 
# 
# 
# parent <- CatTree %>% 
#   pull(parentid)
# 
# child <- CatTree %>% 
#   pull(categoryid)
# 
# z <- setdiff(aa, a)
# intersecting <- intersect(parent, child) %>% 
#   enframe(name = NULL, value = "Intersect") %>% 
#   left_join(., CatTree$parentid, by = "")
# 
# 
# 
# 
# net <- graph_from_data_frame(d = CatTree,
#                              directed = T)
# plot(net)
# 
# test <- CatTree %>% 
#   select(parentid, categoryid) %>% 
#   as.data.frame() %>% 
#   spread(parentid, categoryid)
# 
# library(data.tree)
# tree <- FromDataFrameNetwork(test)
# tree
# lol <- ToListExplicit(tree, unname = TRUE)
# diagonalNetwork(lol)
# 
# test <- CatTree
# 
# 
# test <- CatTree #%>% 
#   # group_by(categoryid, parentid) %>% 
#   # summarise(Count = n())
# 
# 
# parent <- unique(CatTree$parentid) %>% 
#   enframe(., name = NULL, value = "parentid") %>% 
#   left_join(., CatTree, by = "parentid")
# 
# 
# 
# 
# # ------------------------------------------------------------------------------  
# # TestProp <- Items %>% 
# #   filter(property %in% c("categoryid", "available")) %>% 
# #   group_by(timestamp, itemid) %>% 
# #   filter(property == "categoryid") %>% 
# #   summarise(count = n())
# # 
# # TestAv <- Items %>% 
# #   filter(property %in% c("categoryid", "available")) %>% 
# #   group_by(timestamp, itemid) %>% 
# #   filter(property == "available") %>% 
# #   summarise(count = n())
# 
