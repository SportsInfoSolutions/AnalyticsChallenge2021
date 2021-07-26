total_pbp$EventType

#order outside to inside (L, R) and throw depth on a play

spp %>% left_join(total_pbp) %>%
  filter(EventType == "pass") %>% 
  filter(form == "3x1") %>% 
  mutate(Completion = as.numeric(Completion)) %>% 
  group_by(CoverageScheme, SideOfCenter, Order_OutsideToInside) %>%
  summarize(
    tgt_rate = mean(Target, na.rm = T),
    cmp_rate = mean(Completion, na.rm = T)
  ) %>%
  group_by(SideOfCenter, Order_OutsideToInside) %>%
  mutate(COE = cmp_rate - mean(cmp_rate, na.rm = T),
         TOE = tgt_rate - mean(tgt_rate, na.rm = T)) %>%
  filter(CoverageScheme == "Cover 6") %>%
  filter(!(SideOfCenter == "NULL")) %>% 
  filter(Order_OutsideToInside < 4) %>% 
  select(SideOfCenter, Order_OutsideToInside, COE, TOE) %>%
  ggplot(aes(x = Order_OutsideToInside, y = SideOfCenter)) + 
  geom_point(aes(color = TOE), size = 15, shape = 15) + 
  scale_color_gradient2(low = "red", high = "#8500ff", midpoint = -0.01) +
  labs(
    title = "Target Over Expectation vs. Cover 6",
    subtitle = "NFL: 2020 | 3x1 Formations",
    x = "Receiver Order",
    y = "Side of Center",
    caption = "Figure: OU SDAA | Data: Sports Info. Solutions"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(family = "Russo One", color = "Purple", size = 24),
    plot.subtitle = element_text(family = "Quantico", size = 20),
    axis.title = element_text(family = "Quantico", size = 14),
    axis.text = element_text(family = "Quantico", size = 12),
    plot.caption = element_text(family = "Quantico"),
    legend.position = "bottom",
    legend.text = element_text(family = "Quantico", size = 12, angle = 90,
                               hjust = 1, vjust = 1),
    legend.title = element_text(family = "Quantico", size = 12, vjust = 0.5)
  )
  
