source("classification.R")

form_2 <- pbp %>%
  left_join(ROUTE_data) %>%
  left_join(PFS_data) %>%
  filter(FORM == "2x2") %>%
  filter(!is.na(Left_Exact), !is.na(Right_Exact))

combos2 <- rbind(form_2 %>% mutate(routes = Left_Exact) %>% select(EventID, routes), 
      form_2 %>% mutate(routes = Right_Exact) %>% select(EventID, routes)) %>% 
  mutate(routes = str_remove(routes, "::NA::NA")) %>% 
  filter(!str_detect(routes, "NA")) %>% 
  group_by(routes) %>%
  summarize(
    ct = n()
  ) %>%
  slice_max(order_by = ct, n = 10) %>%
  mutate(combo_id = paste0("2", str_pad(row_number(), 2,pad = "0")))
  
  
names(combos2)

form_2 %>%
  left_join(combos2) %>%
  filter(!is.na(combo_id)) %>%
  group_by(CoverageScheme, routes) %>%
  summarize(
    mepa = mean(as.numeric(EPA, na.rm = T))
  ) %>%
  filter(CoverageScheme %in% c("Man Cover 2")) %>%
  ggplot(aes(x = reorder(routes, mepa), y = mepa)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  labs(
    title = "Combination Success vs Coverage",
    subtitle = "Popular 3-Man Routes vs Tampa 2",
    x = "Combination",
    y = "Average EPA"
  )
