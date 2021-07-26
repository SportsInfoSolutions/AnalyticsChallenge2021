
#link to the setup R script
source("/Users/jacksonepolk/Desktop/SIS/R/setup.R")


#####MostPopularCoverages#####

#Want to do this in general, so non FIB and all that
pbp %>%
  left_join(fib) %>%
  left_join(form) %>%
  filter(FIB == 0) %>%
  filter(StartYard > 20) %>% 
  filter(EventType == "pass") %>%
  group_by(CoverageScheme) %>%
  summarize(
    ct = n()
  ) %>% 
  filter(CoverageScheme %in% c("Cover 0", "Cover 1", "Cover 2", "Cover 3", "Cover 4",
                               "Cover 6", "Man Cover 2", "Tampa 2")) %>%
  ggplot(aes(x = reorder(CoverageScheme, -ct), y = ct)) + 
  geom_bar(stat = "identity", color = "#25f5e9", fill = "#25f5e9") + 
  geom_label(aes(label = ct), family = "Quantico") +
  coord_flip() + 
  labs(
    title = "Coverage Scheme Usage",
    subtitle = "NFL: 2020 | Major Coverages",
    x = "Scheme",
    y = "Usage",
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
    legend.text = element_text(family = "Quantico", size = 12),
    legend.title = element_text(family = "Quantico", size = 12)
  )

#####StrengthOrientation#####
pbp %>% 
  left_join(fib) %>%
  filter(StartYard > 20) %>% 
  filter(EventType == "pass") %>%
  ggplot(aes(y = EPA, fill = strength)) +
  geom_density(alpha = 0.4) + 
  coord_flip() + 
  labs(
    title = "Strength Orientation",
    subtitle = "NFL: 2020 | Unbalanced Formations",
    x = "Density",
    y = "EPA",
    caption = "Figure: OU SDAA | Data: Sports Info. Solutions",
    fill = "Strength"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(family = "Russo One", color = "Purple", size = 24),
    plot.subtitle = element_text(family = "Quantico", size = 20),
    axis.title = element_text(family = "Quantico", size = 14),
    axis.text = element_text(family = "Quantico", size = 12),
    plot.caption = element_text(family = "Quantico"),
    legend.position = "bottom",
    legend.text = element_text(family = "Quantico", size = 12),
    legend.title = element_text(family = "Quantico", size = 12)
  )


#####FIB#####
pbp %>% 
  left_join(fib) %>%
  filter(StartYard > 20) %>% 
  filter(EventType == "pass") %>%
  group_by(FIB) %>%
  summarize(ct = n()) %>% 
  mutate(pct = 100*ct/sum(ct)) %>% 
  ggplot(aes(x = FIB, y = round(pct,2))) + 
  geom_bar(stat = "identity", color = "#25f5e9", fill = "#25f5e9") + 
  geom_label(aes(label = paste0(round(pct,1), "%")), family = "Quantico") +
  scale_x_discrete(breaks = c(0,1), labels = c("NON FIB", "NON FIB"),) + 
  labs(
    title = "FIB Usage",
    subtitle = "NFL: 2020 | Major Coverages",
    x = "NON-FIB / FIB",
    y = "Rate",
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
    legend.text = element_text(family = "Quantico", size = 12),
    legend.title = element_text(family = "Quantico", size = 12)
  )


#####MostPopularRoutes#####
#What are the top route combinations used in the nfl in 2020?
#Do we care about FIB/open area of the field/normal downs at this stage?
#Assume no
most_popular_routes_general <- rts %>%
  #break it down by number of routes ran
  find_top_routes()

#What if we do care about situation?
most_popular_routes_situation <- rts %>%
  #join to pbp to determine situation
  left_join(pbp) %>% 
  #join to fib to determine fib
  left_join(fib) %>%
  #choose plays between the 20s (open field), non FIB
  filter(StartYard > 20) %>%
  filter(FIB < 1) %>% 
  #choose passes only
  filter(EventType == "pass") %>% 
  find_top_routes()



#There are four levels I want to look at
#WR;WR;WR
#WR;WR;TE
#WR;TE;WR
#TE;WR;WR
rts %>%
  #only check major route combos
  filter(rt_combo %in% most_popular_routes_situation$rt_combo) %>%
  left_join(form) %>%
  filter(!is.na(Position_OutsideToInside)) %>% 
  #There are four levels I want to look at
  #WR;WR;WR
  #WR;WR;TE
  #WR;TE;WR
  #TE;WR;WR
  filter(Position_OutsideToInside %in% 
           c("WR;WR;WR",
             "WR;WR;TE",
             "WR;TE;WR",
             "TE;WR;WR")) %>%
  #join with pbp frame to get access to the EPA
  left_join(pbp) %>%
  #by route combo does te position matter
  group_by(rt_combo, Position_OutsideToInside) %>%
  summarize(
    mepa = mean(EPA, na.rm = T)
  ) %>%
  ggplot(aes(y = mepa, fill = Position_OutsideToInside)) + 
  geom_density(alpha = 0.4)+
  coord_flip() +
  labs(
    title = "EPA Density by TE Position",
    subtitle = "3 Man Routes | NFL: 2020",
    caption = "Figure: OU SDAA | Data: Sports Info. Solutions",
    y = "EPA",
    x = "Density",
    fill = "WR/TE Alignment"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(family = "Russo One", color = "Purple", size = 24),
    plot.subtitle = element_text(family = "Quantico", size = 20),
    axis.title = element_text(family = "Quantico", size = 14),
    axis.text = element_text(family = "Quantico", size = 12),
    plot.caption = element_text(family = "Quantico"),
    legend.position = "bottom",
    legend.text = element_text(family = "Quantico", size = 12),
    legend.title = element_text(family = "Quantico", size = 12)
  )


####HITCHART#####
#top route combos
fcn_data <- pbp %>%
  left_join(fib) %>% 
  left_join(rts) %>% 
  filter(!(Completion == "NULL")) %>%
  mutate(Completion = as.numeric(Completion)) %>% 
  filter(str_detect(rt_combo, ";")) %>% 
  filter(!str_detect(rt_combo, "Blocking"),
         !str_detect(rt_combo, "Screen")) %>%
  mutate(combo_size = str_count(rt_combo, ";") + 1)

#vector to check if combo is in there
top_combos <- fcn_data %>%
  group_by(combo_size, rt_combo) %>%
  summarize(
    ct = n()
  ) %>%
  slice_max(order_by = ct, n = 5) %>%
  filter(combo_size < 4) %>%
  mutate(RID = paste0(combo_size, row_number())) %>%
  select(rt_combo, RID)


coverages <- fcn_data %>%
  left_join(top_combos) %>%
  filter(EventType == "pass") %>% 
  filter(!is.na(RID)) %>%
  filter(!(CoverageScheme == "Combination"),
         !(CoverageScheme == "Screen"),
         !(CoverageScheme == "Prevent"),
         !(CoverageScheme == "Other")) %>% 
  filter(FIB == 0) %>% 
  filter(!is.na(EPA), !is.na(Completion)) %>% 
  filter(StartYard > 20) %>%
  group_by(RID, CoverageScheme) %>%
  summarize(
    mepa = mean(bayesboot::bayesboot(data = EPA, mean, R = 100)$V1),
    cmp = mean(bayesboot::bayesboot(data = Completion, mean, R = 100)$V1)
  ) %>%
  left_join(top_combos) %>%
  select(rt_combo, CoverageScheme, mepa, cmp) %>%
  mutate(mepa_st = (mepa-mean(mepa))/sd(mepa),
         cmp_st = (cmp-mean(cmp))/sd(cmp),
         rat = mepa_st + cmp_st) %>%
  select(rt_combo, CoverageScheme, mepa_st, cmp_st, rat) %>%
  arrange(CoverageScheme, rat) %>%
  group_by(CoverageScheme) %>% 
  mutate(cov_rat = mean(rat))


#chart showing how variable everything is
coverages %>%
  group_by(CoverageScheme) %>%
  summarize(
    mrat = mean(rat),
    sdrat = sd(rat)
  ) %>%
  ggplot(aes(x = reorder(CoverageScheme, mrat))) + 
  geom_hline(yintercept = 0, color = "red", lty = 2) + 
  geom_segment(aes(xend = CoverageScheme, yend = mrat + sdrat, y = mrat), lwd = 2) + 
  geom_segment(aes(xend = CoverageScheme, yend = mrat - sdrat, y = mrat), lwd = 2) +
  geom_point(aes(y = mrat), size = 4, color = "Purple") + 
  labs(
    title = "Coverage Rating vs. Popular Routes",
    subtitle = "NFL: 2020 | Major Coverages | Non-FIB",
    caption = "Figure: OU SDAA | Data: Sports Info. Solutions",
    x = "Coverage",
    y = "Rating"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(family = "Russo One", color = "Purple", size = 24),
    plot.subtitle = element_text(family = "Quantico", size = 20),
    axis.title = element_text(family = "Quantico", size = 14),
    axis.text.x = element_text(family = "Quantico", size = 12, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(family = "Quantico", size = 12),
    plot.caption = element_text(family = "Quantico"),
    legend.position = "bottom",
    legend.text = element_text(family = "Quantico", size = 12),
    legend.title = element_text(family = "Quantico", size = 12)
  )



hitchart <- ggplot(data = coverages,
                   aes(x = reorder(CoverageScheme, cov_rat), y = rt_combo)) + 
  geom_point(aes(color = rat), size = 15, shape = 15) + 
  geom_text(label = ifelse(coverages$rat < 0, "D", "O"), fontface = "bold") + 
  scale_color_gradient2(low = "red", high = "green",midpoint = 0) + 
  guides(
    color = "none"
  ) + 
  labs(
    title = "Pattern vs. Coverage Hit Chart",
    subtitle = "NFL: 2020 | Major Coverages | Non-FIB",
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
hitchart


  



  
  
