sp <-  "humpback"
df.hw <- all_ap_sf %>% filter(Species %like% sp) %>% data.frame()

  hw1.5.hn.vis      <-ds(df.hw, 1.5, key="hn", formula=~Visibility)
  hw1.5.hn.visc     <-ds(df.hw, 1.5, key="hn", formula=~Clumped_Visibility)
  hw1.5.hn.vis.g90c <-ds(df.hw, key = "hn", truncation = 1.5, formula=~Visibility+Glare90)
  hw1.5.hn.v.g90c   <-ds(df.hw, key = "hn", truncation = 1.5, formula=~Clumped_Visibility+Glare90)


hw1.5.hn.vis
hw1.5.hn.visc
hw1.5.hn.vis.g90c
hw1.5.hn.v.g90c


#---------------------------------------
x <- hw1.5.hn.vis
er <- x$dht$individuals$summary[c(1:5),c(1,5,7,9,10)] %>%
  transmute(Season=Region,
            n     =round(n,     digits=0),
            ER    =round(ER   , digits=2),
            cv.ER =round(cv.ER, digits=2),
            GS=round(mean.size, digits=2)
  )
# abundance
Nhat_t <- x$dht$individuals$N[c(1:5),c(1,2,4,5,6)] %>%
  transmute(Season=Label,
            N=    round(Estimate,digits=0),
            cv.N= round(cv,      digits=2),
            L95.N=round(lcl,     digits=0),
            U95.N=round(ucl,     digits=0))
# density
d <- x$dht$individuals$D[c(1:5),c(1,2,5,6)]  %>%
  transmute(Season=Label,
            D =    round(Estimate*25,digits=2),
            # cv.D = round(cv,         digits=2),
            L95.D= round(lcl*25,     digits=2),
            U95.D= round(ucl*25,     digits=2))

ab_hw <- full_join(er, Nhat_t) %>% full_join(d) %>% tibble::as_tibble() %>% dplyr::select(1:6,8:12,7)

names(ab_hw) <- c("Season","n","ER","cv.ER","GS","N","L95.N","U95.N","D","L95.D","U95.D","cv")
ab_hw %>% tibble::as_tibble() %>%
  csas_table(

    font_size=9,
    # align=rep('l', 13),
    # escape = FALSE,
    longtable = F,
    caption = "(ref:cap-ab-hw)")


#---------------------------------------
x <- hw1.5.hn.visc
er <- x$dht$individuals$summary[c(1:5),c(1,5,7,9,10)] %>%
  transmute(Season=Region,
            n     =round(n,     digits=0),
            ER    =round(ER   , digits=2),
            cv.ER =round(cv.ER, digits=2),
            GS=round(mean.size, digits=2)
  )
# abundance
Nhat_t <- x$dht$individuals$N[c(1:5),c(1,2,4,5,6)] %>%
  transmute(Season=Label,
            N=    round(Estimate,digits=0),
            cv.N= round(cv,      digits=2),
            L95.N=round(lcl,     digits=0),
            U95.N=round(ucl,     digits=0))
# density
d <- x$dht$individuals$D[c(1:5),c(1,2,5,6)]  %>%
  transmute(Season=Label,
            D =    round(Estimate*25,digits=2),
            # cv.D = round(cv,         digits=2),
            L95.D= round(lcl*25,     digits=2),
            U95.D= round(ucl*25,     digits=2))

ab_hw <- full_join(er, Nhat_t) %>% full_join(d) %>% tibble::as_tibble() %>% dplyr::select(1:6,8:12,7)

names(ab_hw) <- c("Season","n","ER","cv.ER","GS","N","L95.N","U95.N","D","L95.D","U95.D","cv")
ab_hw %>% tibble::as_tibble() %>%
  csas_table(

    font_size=9,
    # align=rep('l', 13),
    # escape = FALSE,
    longtable = F,
    caption = "(ref:cap-ab-hw)")


#---------------------------------------
x <- hw1.5.hn.vis.g90c
er <- x$dht$individuals$summary[c(1:5),c(1,5,7,9,10)] %>%
  transmute(Season=Region,
            n     =round(n,     digits=0),
            ER    =round(ER   , digits=2),
            cv.ER =round(cv.ER, digits=2),
            GS=round(mean.size, digits=2)
  )
# abundance
Nhat_t <- x$dht$individuals$N[c(1:5),c(1,2,4,5,6)] %>%
  transmute(Season=Label,
            N=    round(Estimate,digits=0),
            cv.N= round(cv,      digits=2),
            L95.N=round(lcl,     digits=0),
            U95.N=round(ucl,     digits=0))
# density
d <- x$dht$individuals$D[c(1:5),c(1,2,5,6)]  %>%
  transmute(Season=Label,
            D =    round(Estimate*25,digits=2),
            # cv.D = round(cv,         digits=2),
            L95.D= round(lcl*25,     digits=2),
            U95.D= round(ucl*25,     digits=2))

ab_hw <- full_join(er, Nhat_t) %>% full_join(d) %>% tibble::as_tibble() %>% dplyr::select(1:6,8:12,7)

names(ab_hw) <- c("Season","n","ER","cv.ER","GS","N","L95.N","U95.N","D","L95.D","U95.D","cv")
ab_hw %>% tibble::as_tibble() %>%
  csas_table(

    font_size=9,
    # align=rep('l', 13),
    # escape = FALSE,
    longtable = F,
    caption = "(ref:cap-ab-hw)")


#---------------------------------------
x <- hw1.5.hn.v.g90c
er <- x$dht$individuals$summary[c(1:5),c(1,5,7,9,10)] %>%
  transmute(Season=Region,
            n     =round(n,     digits=0),
            ER    =round(ER   , digits=2),
            cv.ER =round(cv.ER, digits=2),
            GS=round(mean.size, digits=2)
  )
# abundance
Nhat_t <- x$dht$individuals$N[c(1:5),c(1,2,4,5,6)] %>%
  transmute(Season=Label,
            N=    round(Estimate,digits=0),
            cv.N= round(cv,      digits=2),
            L95.N=round(lcl,     digits=0),
            U95.N=round(ucl,     digits=0))
# density
d <- x$dht$individuals$D[c(1:5),c(1,2,5,6)]  %>%
  transmute(Season=Label,
            D =    round(Estimate*25,digits=2),
            # cv.D = round(cv,         digits=2),
            L95.D= round(lcl*25,     digits=2),
            U95.D= round(ucl*25,     digits=2))

ab_hw <- full_join(er, Nhat_t) %>% full_join(d) %>% tibble::as_tibble() %>% dplyr::select(1:6,8:12,7)

names(ab_hw) <- c("Season","n","ER","cv.ER","GS","N","L95.N","U95.N","D","L95.D","U95.D","cv")
ab_hw %>% tibble::as_tibble() %>%
  csas_table(

    font_size=9,
    # align=rep('l', 13),
    # escape = FALSE,
    longtable = F,
    caption = "(ref:cap-ab-hw)")