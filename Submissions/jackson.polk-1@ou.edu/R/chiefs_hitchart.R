#scenario file

#most popular routes against the chiefs
most_popular_routes_chiefs <- pbp %>%
  left_join(fib) %>%
  left_join(rts) %>%
  left_join(form) %>% 
  filter(DefensiveTeam == "Chiefs") %>%
  filter(FIB == 0) %>%
  filter(StartYard > 20) %>% 
  filter(EventType == "pass") %>%
  filter(no_receivers > 1, no_receivers < 4) %>%
  group_by(no_receivers, rt_combo) %>%
  summarize(
    ct = n()
  ) %>%
  slice_max(order_by = ct, n = 5)

chiefs_hit_data <- pbp %>%
  left_join(fib) %>%
  left_join(rts) %>%
  filter(DefensiveTeam == "Chiefs") %>%
  filter(FIB == 0) %>%
  filter(StartYard > 20) %>% 
  filter(EventType == "pass") %>%
  filter(rt_combo %in% most_popular_routes_chiefs$rt_combo) %>%
  group_by(rt_combo, CoverageScheme) %>%
  filter(!is.na(EPA)) %>% 
  summarize(
    ct = n(),
    mepa = mean(bayesboot::bayesboot(EPA, weighted.mean,R = 100,R2 = 100, use.weights = T)$V1),
    cmppct = mean(bayesboot::bayesboot(as.numeric(Completion), weighted.mean,R = 100,R2 = 100, use.weights = T)$V1)
  ) %>%
  filter(ct > 1) %>%
  select(rt_combo, CoverageScheme, mepa, cmppct) %>%
  mutate(mepa_st = (mepa-mean(mepa))/sd(mepa),
         cmp_st = (cmppct-mean(cmppct))/sd(cmppct),
         rat = mepa_st + cmp_st) %>%
  select(rt_combo, CoverageScheme, mepa_st, cmp_st, rat) %>%
  arrange(CoverageScheme, rat) %>%
  group_by(CoverageScheme) %>% 
  mutate(cov_rat = mean(rat))

view(chiefs_hit_data)

ggplot(data = chiefs_hit_data,
       aes(x = reorder(CoverageScheme, cov_rat), y = rt_combo)) + 
  geom_point(aes(color = rat), size = 15, shape = 15) + 
  geom_text(label = ifelse(chiefs_hit_data$rat < 0, "D", "O"), fontface = "bold") + 
  scale_color_gradient2(low = "red", high = "green",midpoint = 0) + 
  guides(
    color = "none"
  ) + 
  labs(
    title = "Pattern vs. Coverage\nHit Chart",
    subtitle = "Chiefs Defense: 2020",
    caption = "Figure: OU SDAA | Data: Sports Info. Solutions"
  ) + 
  theme_void() + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, family = "Quantico"),
    axis.text.y = element_text(hjust = 1, family = "Quantico"),
    plot.title = element_text(face = "bold", family = "Russo One"),
    plot.subtitle = element_text(face = "bold", family = "Quantico"),
    plot.caption = element_text(family = "Quantico"),
    plot.margin = unit(c(10,10,10,10), "pt")
  )


