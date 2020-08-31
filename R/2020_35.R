library(tidyverse)
library(friends)
library(cowplot)
extrafont::loadfonts(device = 'win', quiet = T)

theme_set(hrbrthemes::theme_ipsum_rc(base_family = "Gabriel Weiss' Friends Font"))
theme_update(panel.background = element_rect(fill = NA, color = NA),
             panel.border = element_rect(fill = NA, color = NA),
             panel.grid.major.y = element_blank(),
             panel.grid.major.x = element_blank(),
             panel.grid.minor = element_blank(),
             axis.text.x = element_blank(),
             axis.text.y = element_text(size = 13),
             axis.ticks = element_blank(),
             axis.title.y = element_text(size = 15, 
                                         margin = margin(r = 10),
                                         hjust = 0.5,
                                         face = 'bold'),
             plot.margin = margin(10, 25, 10, 25))


plot_friends <- friends_info %>% 
  arrange(season, episode) %>%
  mutate(season = factor(season),
         rn = row_number()) %>% 
  group_by(season) %>% 
  mutate(avg_rating = mean(imdb_rating),
         start = min(rn),
         end = max(rn),
         median = median(rn)) %>% 
  ungroup()

df_lines <- plot_friends %>% 
  group_by(season) %>% 
  summarise(start = mean(start),
            end = mean(end),
            s_avg = unique(avg_rating)) %>% 
  mutate(lag_rn = lead(start, default = max(end)),
         lag_rating = lead(s_avg, default = max(s_avg)))

p <- plot_friends %>% 
  ggplot(aes(x = rn, 
             y = imdb_rating)) +
  geom_hline(data = tibble(y = 7:10),
             aes(yintercept = y),
             color = '#D3D3D3') +
  geom_segment(aes(y = avg_rating, 
                   yend = avg_rating, 
                   x = start, 
                   xend = end,
                   color = season,
                   color = after_scale(colorspace::darken(color, .1))), 
               lwd = 2.5) +
  geom_segment(aes(y = imdb_rating, 
                   yend = avg_rating,
                   x = rn, 
                   xend = rn,
                   color = season,
                   color = after_scale(colorspace::darken(color, .3)))) +
  geom_segment(data = df_lines,
               aes(x = end, 
                   xend = lag_rn,
                   y = s_avg,
                   yend = lag_rating,
                   color = season,
                   color = after_scale(colorspace::darken(color, .3))),
               lwd = .7) +
  geom_point(aes(color = season,
                 color = after_scale(colorspace::darken(color, .3))),
             size = 3) +
  geom_label(aes(label = glue::glue('Season {season}'),
                 x = median,
                 y = 10.1,
                 color = season,
                 color = after_scale(colorspace::darken(color, .3))), 
             family = "Gabriel Weiss' Friends Font",
             fontface = 'bold',
             label.padding = unit(.3, "lines"),
             label.r = unit(.25, "lines"),
             label.size = .8,
             size = 5) +
  scale_x_continuous(expand = c(.005, .005)) +
  scale_y_continuous(breaks = seq(7, 10, by = .5), 
                     limits = c(7, 10.2),
                     sec.axis = dup_axis(name = NULL)) +
  scale_color_brewer(palette = 'Set3') +
  labs(title = 'Friends - All Episodes Ratings by IMDB',
       subtitle = 'The line represents the distance to the seasons average rating',
       caption = 'Visualization by Pedro Toledo • Data from {friends} by Emil Hvitfeldt',
       y = 'IMDB Rating',
       x = NULL) +
  guides(color = FALSE) +
  theme(plot.title = element_text(size = rel(2), hjust = 0.5),
        plot.subtitle = element_text(family = "Gabriel Weiss' Friends Font",size = rel(1.2), hjust = 0.5),
        plot.caption = element_text(size = rel(1)))

ggdraw(p) +
  draw_image('projetos/friends/logo/friends.svg', 
             x = -.30, y = -.34,
             scale = .15)

ggsave(filename = 'friends.pdf', path = 'projetos/friends/',
       device = cairo_pdf, width = 18, height = 11)

ggsave(filename = 'friends.png', path = 'projetos/friends/',
       device = 'png', width = 18, height = 11)