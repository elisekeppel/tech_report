all_ap_sf %>% filter(Species == "killer whale - southern resident") %>% nrow() %>% english::as.english() %>% as.character()

all_ap_sf %>% filter(Species == "killer whale - Bigg's") %>% nrow()%>% english::as.english() %>% as.character()

all_ap_sf %>% filter(Species == "killer whale - unknown ecotype") %>% nrow()%>%  english::as.english() %>% as.character()
