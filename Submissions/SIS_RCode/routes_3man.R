#This file will look at 3x1 and 1x3 formations
source("classification.R")

#get only the plays that have the 3x1 or 1x3

pbp %>%
  left_join(DETAIL_data) %>%
  left_join(PFS_data) %>%
  filter(!(STRENGTH == "C")) %>%
  #first analyze 3x1 formations
  ggplot(aes(y = as.numeric(EPA), group = STRENGTH)) + 
  geom_density(aes(fill = STRENGTH, color = STRENGTH), lwd= 1.5) + 
  scale_fill_manual(values = c("Black", "Purple")) + 
  scale_color_manual(values = c("Black", "Purple")) + 
  coord_flip() + 
  theme_minimal() + 
  labs(
    title = "Strength Orientation vs EPA",
    subtitle = "Unbalanced Formations | NFL: 2020",
    x = "Density",
    y = "EPA",
    caption = "Figure: OU SDAA | Data: Sports Info. Solutions"
  ) +
  theme(
    plot.title = element_text(family = "Russo One", color = "Purple", size = 24),
    plot.subtitle = element_text(family = "Quantico", size = 20),
    axis.title = element_text(family = "Quantico", size = 14),
    axis.text = element_text(family = "Quantico", size = 12),
    plot.caption = element_text(family = "Quantico"),
    legend.position = "bottom"
  )

form_3 <- pbp %>%
  left_join(ROUTE_data) %>%
  left_join(PFS_data) %>%
  filter(FORM %in% c("3x1", "1x3")) %>%
  mutate(routes = ifelse(FORM == "3x1", Left_Exact, Right_Exact)) %>%
  filter(!is.na(routes)) %>%
  mutate(routes = str_remove(routes, "::NA")) %>% 
  filter(!str_detect(routes, "NA"))

#create route identifiers
combos3 <- rbind(
  form_3 %>% filter(FORM == "1x3") %>% mutate(routes = Right_Exact) %>% select(EventID, routes),
  form_3 %>% filter(FORM == "3x1") %>% mutate(routes = Left_Exact) %>% select(EventID, routes)
) %>%
  mutate(routes = str_remove(routes, "::NA")) %>% 
  filter(!str_detect(routes, "NA")) %>%
  unique() %>%
  group_by(routes) %>%
  summarize(
    ct = n()
  ) %>%
  slice_max(order_by = ct, n = 10) %>%
  mutate(combo_id = paste0("3", str_pad(row_number(), 2,pad = "0"))) %>%
  select(combo_id, routes, ct)


form_3 %>%
  left_join(combos3) %>%
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
  

