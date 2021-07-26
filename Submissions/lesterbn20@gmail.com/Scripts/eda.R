library(tidyverse)
library(scales)
library(cowplot)
source("Scripts/wrangle.R")


### ideas
# targeted receiver by coverage scheme
# throw depth for route

# cb_pallete <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# assign_pallete_names <- function(pallete, df, var) {
#   names(pallete) <- df %>% count(!!as.name(var), sort = TRUE) %>% pull(var)
#   return(pallete)
# }


form_and_cov <- plays_data %>% 
  inner_join(plays_wr_per_side, by = c("GameID", "EventID")) %>% 
  inner_join(select(players_agg, GameID, EventID, Screen), by = c("GameID", "EventID")) %>% 
  inner_join(select(plays, GameID, EventID, CoverageScheme), by = c("GameID", "EventID")) %>% 
  filter(!Screen & CoverageScheme != "Screen") %>% 
  mutate(wide_short_count = paste(wide_n, short_n, sep="-")) %>% 
  group_by(wide_short_count, CoverageScheme) %>% 
  filter(n() >= 100) %>% 
  summarise(
    success_pct = mean(off_success),
    success_epa_pct = mean(off_success_epa)
  ) %>% 
  ungroup()

plot_form_cov_success <- function(metric) {
  
  df <- form_and_cov
  col_rename <- if(metric == "success_pct") "Success Rate" else "Success (EPA) Rate"
  names(df)[names(df) == metric] <- col_rename
  
  df %>% 
    ggplot(aes(x = CoverageScheme, y = wide_short_count, fill = !!as.name(col_rename))) + 
    geom_tile(color = "black") +
    scale_fill_gradient(
      low = "white",
      high = "blue",
    ) +
    labs(
      title = "Coverage Success vs Offensive Alignment",
      x = "",
      y = "Wide Side - Short Side Receivers",
      subtitle = "Excludes Screen plays and Coverage 'Screen'. Min 100 Samples."
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 13),
      axis.text.y = element_text(size = 13),
      axis.title.y = element_text(size = 13)
    )
}

plot_form_cov_success("success_pct")
p_alignment_v_coverage <- plot_form_cov_success("success_epa_pct")



alignment_titles <- tribble(
  ~alignment, ~title,
  "S1", "Short Side Wide Out",
  "S2", "Short Side Outside Slot",
  "S3", "Short Side Inside Slot",
  "S4", "Short Side Inside Slot B",
  "W1", "Wide Side Wide Out",
  "W2", "Wide Side Outside Slot",
  "W3", "Wide Side Inside Slot",
  "W4", "Wide Side Inside Slot B",
  "B", "Backfield"
)

# plot_routes_by_alignment <- function(alignment) {
#   stopifnot(length(alignment==1) & alignment %in% alignment_titles$alignment)
#   pos <- alignment
#   plot_title <- alignment_titles %>% filter(alignment == pos) %>% pull(title)
#   
#   routes_by_alignment %>% 
#     filter(alignment == pos) %>% 
#     ggplot(aes(y = reorder(route, n), x = n)) + 
#     geom_bar(stat = "identity", fill = "steelblue") +
#     geom_text(aes(label = format(n, big.mark = ",")), hjust = 1.1, color = "white", size = 4.5, fontface = "bold") +
#     geom_text(aes(label = route, x = 10), hjust = "inward", color = "white", size = 4.5, fontface = "bold") +
#     ggtitle(plot_title) +
#     theme_void() +
#     theme(plot.title = element_text(hjust=0.05, face='bold'))
# }

plot_routes_by_alignment_simpler <- function(alignment) {
  stopifnot(length(alignment==1) & alignment %in% alignment_titles$alignment)
  pos <- alignment
  plot_title <- alignment_titles %>% filter(alignment == pos) %>% pull(title)
  
  routes_by_alignment %>% 
    filter(alignment == pos) %>%
    filter(route != "None") %>% 
    ggplot(aes(y = reorder(route, n), x = n)) + 
    geom_bar(stat = "identity", fill = "steelblue") +
    scale_x_continuous(limits = c(0, 3500), labels = scales::comma) +
    ggtitle(plot_title) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust=0.05, face='bold'),
      axis.title.x = element_blank(), 
      axis.title.y = element_blank(),
      axis.text.x = element_text(angle = 45),
      panel.grid.major.y = element_blank()
    )
}

plots_rba <- alignment_titles$alignment %>% map(plot_routes_by_alignment_simpler)
names(plots_rba) <- alignment_titles$alignment

no_route_plot <- routes_by_alignment %>% 
  group_by(alignment) %>% 
  summarise(no_route = n[route == "None"]) %>% 
  left_join(alignment_titles, by = "alignment") %>% 
  ggplot(aes(y = reorder(title, no_route), x = no_route)) + 
  geom_bar(stat = "identity", fill = "darkorange") +
  scale_x_continuous(labels = scales::comma) +
  ggtitle("No Route") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust=0.05, face='bold'),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 45),
    panel.grid.major.y = element_blank()
  )

pgrid_routes <- cowplot::plot_grid(
  align = "v",
  nrow = 2,
  #top row
  plots_rba[["S1"]],
  plots_rba[["S2"]],
  plots_rba[["S3"]],
  #plots_rba[["S4"]],
  plots_rba[["B"]],
  #bottom row
  plots_rba[["W1"]],
  plots_rba[["W2"]],
  plots_rba[["W3"]],
  #plots_rba[["W4"]],
  no_route_plot
)

