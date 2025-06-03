## Escapement Otolith TM - Tables & Figures
library(tidyverse)
library(scales)
library(openxlsx)

load("data/escapement_otolith_TM_results.RData")

theme_set(theme_bw() + 
            theme(text = element_text(family = "serif", size = 11, color = "black"),
                  # plot.title = element_text(size = 11, hjust = 0.5),
                  # legend.title = element_text(size = 11),
                  # legend.text = element_text(size = 11),
                  strip.text = element_text(family = "serif", size = 11, color = "black"),
                  axis.title = element_text(family = "serif", size = 11, color = "black"),
                  axis.text = element_text(family = "serif", size = 11, color = "black"),
                  # plot.caption = element_text(size = 11, lineheight = 1, hjust = -1),
                  # panel.grid.major.y = element_line(color = "gray90", linetype = 1, linewidth = 0.5),
                  # panel.grid.minor.y = element_blank(),
                  # panel.grid.major.x = element_blank(),
                  # panel.grid.minor.x = element_blank(),
                  strip.background = element_blank(),
                  legend.position = "inside",
                  legend.position.inside = c(0.8,0.15)))

# Figure 1 ----------------------------------------------------------------

fig1 <- ggplot(hatch.escape.proportion.df, aes(x = as.Date(CarcassDay, origin = as.Date("0000-01-01")), y = PercentHatchery*100)) +
  geom_point(color = "black", size = 2) +
  facet_grid(rows = vars(Stock), cols = vars(Year), labeller = labeller(Stock = label_wrap_gen(width = 8))) +
  ylab("Hatchery Percent Composition of Escapement 100") + 
  xlab("Date") +
  scale_x_date(breaks = "2 weeks", date_labels = "%b %d", limits = c(as.Date(210, origin = as.Date("0000-01-01")),as.Date(280, origin = as.Date("0000-01-01"))), expand = c(0.02,0.02)) +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "black",fill = NA),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# ggsave("output/figures/fig1.png", plot = fig1, dpi = "retina", width = 6.5, height = 8, units = "in")


# Table 1 -----------------------------------------------------------------

table1 <- summary.hatch.percent.esc.df %>% 
  mutate('Escapement Estimate' = number(round(Escapement,0), big.mark = ","),
         'Hatchery Escapement Estimate' = number(round(Hatchery.Escapement.est,0), big.mark = ","),
         'Hatchery Percent of Escapement Estimate' = number(round(Hatchery.Escapement.percent*100,1), suffix = "%")) %>% 
  select(-c(Escapement,Hatchery.Escapement.est,Hatchery.Escapement.percent))

# Table 2 -----------------------------------------------------------------

table2 <- hatch.region.df %>% 
  rename('Sample Date' = SampleDate,
         'Otoliths Read' = Read,
         'Marked Otoliths (Hatchery-origin fish)' = Marked,
         'Unmarked Otoliths (Wild-origin fish)' = NotMarked) %>% 
  mutate(PercentLCI = paste0(round(PercentLCI*100,1),'%'),
         PercentPWS = paste0(round(PercentPWS*100,1),'%'),
         PercentKOD = paste0(round(PercentKOD*100,1),'%'),
         PercentOther = paste0(round(PercentOther*100,1),'%')) %>% 
  rename('# LCI' = N.LCI,
         '% LCI' = PercentLCI,
         '# PWS' = N.PWS,
         '% PWS' = PercentPWS,
         '# KOD' = N.KOD,
         '% KOD' = PercentKOD,
         '# Other' = N.Other,
         '% Other' = PercentOther) %>% 
 select(-c(CarcassDay))


# Appendices -----------------------------------------------------------------

total.sample.df <- sample.df %>%
  group_by(Species,Stock,SampleDate,Year) %>%
  summarise(Marked = sum(Marked),
            NotMarked = sum(NotMarked))

# Appendix A --------------------------------------------------------------

appendixA <- sample.df %>%
  select(Species,Stock,SampleDate,Year,TUTKA16PINK,TUTKA16PINKA,TUTKA17PINK,TUTKA17PINKA,TUTKA18PINK,TUTKA18PINKA,TUTKA18PINKB,TUTKA18PINKC,
         TUTKA19PINK,TUTKA19PINKA,TUTKA19PINKB,TUTKA20PINKB,PORTGRAHAM16,PORTGRAHAM17,PORTGRAHAM17A,PORTGRAHAM18,PORTGRAHAM19,PORTGRAHAM20,
         AFK16B,AFK17B,AFK18B,AFK19B,AFK20A,AFK20B,CCH16,CCH17,CCH18,CCH19,CCH20,CCH20A,SGH16,SGH17,SGH18,SGH19,SGH20,WNH16PINKB,WNH17PINKB,
         WNH18PINKB,WNH19PINKB,WNH20PINKB,KITOI17PINK,KITOI18PINK,KITOI19PINK) %>%
  pivot_longer(cols = !c(Species,Stock,SampleDate,Year), names_to = "MarkID", values_to = "N") %>%
  drop_na() %>%
  group_by(Species,Stock,SampleDate,Year,MarkID) %>%
  summarise(N = sum(N)) %>%
  left_join(total.sample.df,
            by = join_by(Species,Stock,SampleDate,Year)) %>%
  pivot_wider(names_from = MarkID, names_sort = TRUE, values_from = N)

# Appendix B --------------------------------------------------------------


appendixB <- sample.df %>%
  select(Species,Stock,SampleDate,Year,TUTKA16PINK,TUTKA16PINKA,TUTKA17PINK,TUTKA17PINKA,TUTKA18PINK,TUTKA18PINKA,TUTKA18PINKB,TUTKA18PINKC,
         TUTKA19PINK,TUTKA19PINKA,TUTKA19PINKB,TUTKA20PINKB,PORTGRAHAM16,PORTGRAHAM17,PORTGRAHAM17A,PORTGRAHAM18,PORTGRAHAM19,PORTGRAHAM20,
         AFK16B,AFK17B,AFK18B,AFK19B,AFK20A,AFK20B,CCH16,CCH17,CCH18,CCH19,CCH20,CCH20A,SGH16,SGH17,SGH18,SGH19,SGH20,WNH16PINKB,WNH17PINKB,
         WNH18PINKB,WNH19PINKB,WNH20PINKB,KITOI17PINK,KITOI18PINK,KITOI19PINK) %>%
  pivot_longer(cols = !c(Species,Stock,SampleDate,Year), names_to = "MarkID", values_to = "N") %>%
  drop_na() %>%
  group_by(Species,Stock,SampleDate,Year,MarkID) %>%
  summarise(N = sum(N)) %>%
  left_join(total.sample.df,
            by = join_by(Species,Stock,SampleDate,Year)) %>%
  mutate(Percent = number(round(N*100/(Marked + NotMarked),1), suffix = "%")) %>%
  select(-N) %>%
  pivot_wider(names_from = MarkID, names_sort = TRUE, values_from = Percent)

# Save tables as Excel workbook -------------------------------------------

tables <- list(
  "table1" = table1,
  "table2" = table2)

write.xlsx(tables, "output/tables/report_tables_RAW.xlsx")

appendices <- list(
  "appendixA" = appendixA,
  "appendixB" = appendixB)

write.xlsx(appendices, "output/appendices/appendices_RAW.xlsx")

