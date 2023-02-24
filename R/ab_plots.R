d <- all_ap_sf %>% dplyr::select(-geometry) %>% data.frame() %>%
  filter(!Species == "Unknown Porpoise") %>%
  mutate(Species = case_when(
    Species == "Humpback Whale" ~ "HW",
    Species == "Harbour Porpoise" ~ "HP",
    Species == "Dalls Porpoise" ~ "DP",
    Species %like% "Killer Whale" ~ "KW",
    Species == "Fin Whale" ~ "FW"))


class(d)
ab_hw %<>% dplyr::select(Season, N, L95.N, U95.N) %>% mutate(Species = "HW")
ab_hp %<>% dplyr::select(Season, N, L95.N, U95.N) %>% mutate(Species = "HP")
ab_dp %<>% dplyr::select(Season, N, L95.N, U95.N) %>% mutate(Species = "DP")


x <- rbind(ab_hp, ab_dp, ab_hw)

ggplot(x, aes(Season, N, colour = Species)) +
  # geom_point(position=position_dodge(.9)) +
  # geom_linerange(aes(ymin = L95.N, ymax = U95.N), width = 0.2)
  # geom_errorbar(aes(ymin = L95.N, ymax = U95.N), width = 0.2, position=position_dodge(.9))
geom_crossbar(aes(ymin = L95.N, ymax = U95.N), width = 0.2, position=position_dodge(.9))

ggplot(x, aes(Season, N, colour = Species)) +
  geom_point(position=position_dodge(.9)) +
  geom_errorbar(aes(ymin = L95.N, ymax = U95.N), width = 0.2, position=position_dodge(.9))

ggplot(x, aes(Season, N, colour = Species)) +
  geom_bar()
  # geom_linerange(aes(ymin = L95.N, ymax = U95.N), width = 0.2)
  geom_errorbar(aes(ymin = L95.N, ymax = U95.N), width = 0.2, position=position_dodge(.9))



ggplot(data=x) +
  geom_point(mapping=aes(x=Season, y=N, colour = Species)) +
  # geom_errorbar(aes(ymin=L95.N, ymax=U95.N, colour = Species), width = 0.2)
  ## theme(axis.text.x = element_text(angle = 90))

p <- ggplot(df, aes(trt, resp, colour = Species))
p + geom_linerange(aes(ymin = lower, ymax = upper))
p + geom_pointrange(aes(ymin = lower, ymax = upper))
p + geom_crossbar(aes(ymin = lower, ymax = upper), width = 0.2)
p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)

ggplot(x, aes(x=Season, y=N, fill=Species)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=L95.N, ymax=U95.N), width=.2,
                position=position_dodge(.9))

p + scale_fill_brewer(palette="Greys") + theme_minimal()


ggplot() +

  geom_sf(data = survey_area,
          fill=NA,alpha=0.3) +
  # geom_sf(data=all_ap_sf %>% filter(Species %in% c("Harbour Porpoise","Dalls Porpoise", "Humpback Whale"))) +
  geom_sf(data=all_effort_lines) +
  facet_wrap(.~month_abb)

x <- all_ap_sf %>%
  filter(Species %in% c( "Humpback Whale")) %>%
  group_by(Species, month_abb, SurveyID) %>%
  summarise(Count=sum(Group_Size))


ggplot(x, aes(month_abb, Count, colour = Species)) +
  geom_linerange(ymin = L95.N, ymax = U95.N) +
 theme(axis.text.x = element_text(angle = 90))
