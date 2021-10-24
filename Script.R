librarian::shelf(tidyverse, kushtrimvisoka, ggiraph, svglite, ggTimeSeries, htmlwidgets, 
                 readxl, sysfonts, jsonlite, data.table, sf, stringi, zoo, scales, ggstream,
                 lubridate, reactable, htmltools, reactablefmtr)

# LOAD DATA
data <- read_excel("/Users/kushtrimvisoka/Library/Mobile Documents/com~apple~CloudDocs/01 - R/20 - Covi/masterdata.xlsx") %>% 
  drop_na(Name)
data <- data[-1, ]

# THEME ELEMENTS
font_add_google("Literata", "Literata")
myfont <- "Literata"
fontcolor <- "#222222"
kvcolor <- "#f2f2f2"

theme_kushtrim <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "#22211d", family = myfont),
      axis.line = element_blank(),
      strip.text = element_text(color = fontcolor, family = myfont, size = 15),
      axis.text.x = element_text(color = fontcolor, family = myfont, size = 11),
      axis.text.y = element_text(color = fontcolor, family = myfont, size = 11),
      axis.title.y = element_text(color = fontcolor, family = myfont, size = 11,
                                  margin = margin(0, 0.5, 0, 0, 'cm')),
      axis.title.x = element_blank(),
      legend.text = element_text(color = fontcolor, family = myfont, size = 7),
      # panel.grid.major = element_line(color = "#E6E6E6", size = 0.1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      panel.background = element_blank(),
      legend.background = element_blank(),
      panel.border = element_blank(),
      ...
    )
}

theme_calendar <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "#22211d", family = myfont),
      axis.line = element_blank(),
      axis.text.x = element_text(color = fontcolor, family = myfont, size = 11),
      axis.text.y = element_text(color = fontcolor, family = myfont, size = 11),
      axis.title.y = element_text(color = fontcolor, family = myfont, size = 11,
                                  margin = margin(0, 0.5, 0, 0, 'cm')),
      axis.title.x = element_blank(),
      legend.text = element_text(color = fontcolor, family = myfont, size = 7),
      panel.grid.major = element_line(color = "#E6E6E6", size = 0.1),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      panel.background = element_blank(),
      legend.background = element_blank(),
      panel.border = element_blank(),
      ...
    )
}

theme_vaccines <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "#22211d", family = myfont),
      axis.line = element_blank(),
      strip.text = element_text(color = fontcolor, family = myfont, size = 10),
      axis.text.x = element_text(color = fontcolor, family = myfont, size = 8),
      axis.text.y = element_text(color = fontcolor, family = myfont, size = 8),
      axis.title.y = element_text(color = fontcolor, family = myfont, size = 8,
                                  margin = margin(0, 0.5, 0, 0, 'cm')),
      axis.title.x = element_blank(),
      legend.text = element_text(color = fontcolor, family = myfont, size = 7),
      # panel.grid.major = element_line(color = "#E6E6E6", size = 0.1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      panel.background = element_blank(),
      legend.background = element_blank(),
      panel.border = element_blank(),
      axis.text.y.right = element_text(color = "#0B3706"),
      axis.title.y.right = element_text(color = "#0B3706", family = myfont, size = 6),
      ...
    )
}

theme_empty <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "#22211d", family = myfont),
      axis.line = element_blank(),
      strip.text = element_text(color = fontcolor, family = myfont, size = 15),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      legend.text = element_blank(),
      # panel.grid.major = element_line(color = "#E6E6E6", size = 0.1),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      panel.background = element_blank(),
      legend.background = element_blank(),
      panel.border = element_blank(),
      ...
    )
}

theme_rrjedha <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(color = "#22211d", family = myfont),
      axis.line = element_blank(),
      axis.text.x = element_text(color = fontcolor, family = myfont, size = 11),
      axis.text.y = element_blank(),
      axis.title.y = element_text(color = fontcolor, family = myfont, size = 11,
                                  margin = margin(0, 0.5, 0, 0, 'cm')),
      axis.title.x = element_blank(),
      legend.text = element_text(color = fontcolor, family = myfont, size = 7),
      panel.grid.major = element_line(color = "#E6E6E6", size = 0.1),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      panel.background = element_blank(),
      legend.background = element_blank(),
      panel.border = element_blank(),
      ...
    )
}

options(scipen = 999)

# ------- Create the vaccination progress bar ------- 
vaccines <- read_excel("/Users/kushtrimvisoka/Library/Mobile Documents/com~apple~CloudDocs/01 - R/20 - Covi/masterdata.xlsx", sheet = "vaccines") %>% 
  filter(date == max(date)) %>% 
  mutate(A = 1798188, B = 1517595, `onedose_%`= at_least_one_dose/1517595*100,
         `twodoses_%` = two_doses/1517595*100,
         C = at_least_one_dose, D = two_doses) %>% 
  mutate(A = A - B,
         B = B - C,
         C = C - D) %>% 
  gather(key = "fill", value = "value", c( A, B, C, D)) %>% 
  mutate(label = case_when(str_detect(fill, "A") ~ "Nën 12 vjet",
                           str_detect(fill, "B") ~ "Ende të pa vaksinuar",
                           str_detect(fill, "C") ~ "Një dozë",
                           str_detect(fill, "D") ~ "Dy doza")) 

a <- ggplot(vaccines, aes(x = date, y = value, fill = fill, label = label)) +
  geom_bar(stat = "identity") +
  geom_text(size = 2.5, position = position_stack(vjust = 0.5))+
  coord_flip()+
  scale_fill_manual(values = c(D = '#A8BA90', B = '#DDE0E2', C = '#CCD6BE', A = '#EDF0F2'))+
  theme_void()+
  theme(legend.position = "none")

ggsave(file="/Users/kushtrimvisoka/Library/Mobile Documents/com~apple~CloudDocs/Web/kosovo.today/images/bar.svg", plot=a, width=10, height=0.3)

# ------- Create the vaccination progress chart ------- 

vaccines <- read_excel("/Users/kushtrimvisoka/Library/Mobile Documents/com~apple~CloudDocs/01 - R/20 - Covi/masterdata.xlsx", sheet = "vaccines") %>% 
  mutate(total_tooltip = paste("Se paku me nje doze: ", at_least_one_dose, " (", round(`onedose_%`, 1), "%)", "\n", date, sep="")) %>% 
  mutate(fd_tooltip = paste("Të vaksinuar me dy doza: ", two_doses, " (", round(`twodoses_%`, 1), "%)", "\n", date, sep=""))  %>% 
  drop_na(one_dose) 
# slice_tail(n = 30)

b <- ggplot()+
  # geom_area(data = kum1,  aes(x=date, y=value2), alpha = 0.4, show.legend = F, fill = "red")+
  geom_bar_interactive(data = vaccines, aes(x=date, y=at_least_one_dose, tooltip = total_tooltip, data_id = date), stat = "identity", position = "dodge", alpha = 0.3, show.legend = F, fill = "#547A1D")+
  geom_bar_interactive(data = vaccines, aes(x=date, y=two_doses, tooltip = fd_tooltip, data_id = date), stat = "identity", position = "dodge", alpha = 0.3, show.legend = F, fill = "#547A1D")+
  geom_line(data = vaccines, aes(x=date, y=`onedose_%`*(max(vaccines$at_least_one_dose)/100)), color = "#0B3706")+
  scale_y_continuous(breaks = round(seq(0, max(vaccines$at_least_one_dose), max(vaccines$at_least_one_dose)/5), 0), sec.axis = sec_axis(~./(max(vaccines$at_least_one_dose)/100), breaks = c(0, 20, 40, 60, 80, 100), name="Të vaksinuar me së paku një dozë për moshat 12+ (në %)"), labels = scales::comma) +
  xlab('') +
  ylab('Individë të vaksinuar')+
  labs(
    caption = "Burimi: IKSHPK  |  © http://kosovo.today  |  Grafika: https://twitter.com/kushtrimvisoka"
  )+
  theme_vaccines()+
  theme(plot.title = element_text(
    size = 14, 
    hjust = 0,
    family = myfont,
    color = fontcolor), 
    legend.title = element_text(
      size = 8,
      color = fontcolor,
      family = myfont,
      margin = margin(0, 0, 0.2, 0, 'cm')),
    plot.subtitle = element_text(
      size = 6, 
      hjust = 0, 
      color = fontcolor,
      family = myfont,
      margin = margin(
        b = 0.5, 
        t = 0.2, 
        l = 2, 
        unit = "cm"), 
      debug = F),
    text = element_text(size = 12),
    axis.ticks = element_blank(), 
    legend.direction = "horizontal", 
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.key.height = unit(0.3, "cm"), 
    legend.key.width = unit(0.3, "cm"),
    plot.caption = element_text(size = 5, 
                                hjust = 0,
                                margin = margin(t = 0.5, 
                                                b = 0, 
                                                unit = "cm"), family = myfont, 
                                color = "#222222")
  )

tooltip_css <- "background-color:#ffffff;
font-family:sans-serif,arial;
font-size:100%;
color:#000000;
box-shadow: 0px 0px 21px -6px rgba(0, 0, 0, 0.75);
padding:10px 10px 15px;
border: 1px solid #222;
border-radius:0px 0px 0px 0px;"

hover_css <- "
fill: #547A1D;
stroke: #547A1D;
stroke-width: 2;"

b <- girafe(ggobj = b, height_svg = 3,
       options = list(opts_hover(css = hover_css), opts_tooltip(css = tooltip_css), opts_zoom(max = 10)))

htmlwidgets::saveWidget(b, "/Users/kushtrimvisoka/Library/Mobile Documents/com~apple~CloudDocs/Web/kosovo.today/vaccbar.html",
                        selfcontained = F)

# ggsave(plot = b, file="/Users/kushtrimvisoka/Library/Mobile Documents/com~apple~CloudDocs/Web/kosovo.today/images/vaccbar.svg", width=10, height=5)


# ------- Create a JSON file with the data ------- 
# Row 1
row1 <- data %>% 
  filter(date == max(date))

nje <- scales::comma(sum(row1$confirmed))
dy <- scales::comma(sum(row1$healed))
tre <- scales::comma(sum(row1$dead))

# Row 2
row2 <- data %>%
  drop_na(Name) %>%
  select(confirmed, healed, dead) %>%
  summarise(confirmed = sum(confirmed), healed = sum(healed), dead = sum(dead)) %>%
  mutate("Rastet aktive" = confirmed - healed - dead) %>%
  mutate("CFR*" = dead/confirmed) %>% 
  rename("Total të konfirmuar" = confirmed, "Të shëruar" = healed, "Të vdekur" = dead) %>%
  select("Total të konfirmuar", "Rastet aktive", "Të shëruar", "Të vdekur", "CFR*")

kater <- scales::comma(row2$`Total të konfirmuar`)
pese <- scales::comma(row2$`Rastet aktive`)

tbl <- data %>% 
  drop_na(Name) %>%
  group_by(`Data e infektimit`) %>% 
  summarise(confirmed = sum(confirmed), healed = sum(healed), dead = sum(dead)) %>% 
  mutate(confirmed = cumsum(confirmed), healed = cumsum(healed), dead = cumsum(dead), active = confirmed - healed - dead) 

tbl <- tbl %>% 
  mutate(date = seq(1, nrow(tbl), 1), type = "Kosovo")

tbl$name[nrow(tbl)] <- paste(paste(tbl$type[nrow(tbl)], ":", sep = ""), tbl$confirmed[nrow(tbl)])

tbl <- tbl %>% 
  group_by(type) %>% 
  summarise(totalDite = n(), aktual = active[nrow(tbl)]) %>% 
  merge(tbl)

tbl <- tbl %>% 
  filter(date == (totalDite-7) | date == totalDite) %>% 
  group_by(type) %>% 
  summarise(totalRritje = (active[2] - active[1])) %>% 
  merge(tbl)

tbl$rritjePerqind <- tbl$totalRritje/(tbl$aktual - tbl$totalRritje)

tbl <- tbl%>% 
  distinct(type, .keep_all = T) %>% 
  filter(type == "Kosovo")

gjashte <- paste(round(tbl$rritjePerqind * 100, 2), "%")
shtate <- scales::comma(row2$`Të shëruar`)
tete <- scales::comma(row2$`Të vdekur`)
nente <- paste(round(row2$`CFR*`* 100, 2), "%")
dhjete <- format(Sys.time(), "Përditësuar në %R, %B %d, %Y")
njembidhjete <- dhjete

json <- data.frame(nje, dy, tre, kater, pese, gjashte, shtate, tete, nente, dhjete, njembidhjete)
json <- toJSON(json)
json <- str_remove(json, "\\[")
json <- str_remove(json, "\\]")
write(json, "/Users/kushtrimvisoka/Library/Mobile Documents/com~apple~CloudDocs/Web/kosovo.today/data.json")

# ------- Create the Kosovo map with Covid-19 data ------- 

data1 <- data %>% 
  filter(data$Name != "Nuk dihet") %>% 
  drop_na(Name)

data1 <- data1 %>% 
  group_by(id) %>% 
  summarise(confirmed = sum(confirmed), healed = sum(healed), dead = sum(dead))

covidbreaks <- c(0,1,10,50,100,200,300,500,1000,5000,100000)
agelabels <- c("0","1-9","10-49","50-99","100-199","200-299","300-499","500-999", "1000-5000", "5000+")

setDT(data1)[ , covidbreaks := cut(confirmed, 
                                   breaks = covidbreaks, 
                                   right = FALSE, 
                                   labels = agelabels)]


# MAPs
villages <- rksmaps(level = "village", simplify = T)
map <- rksmaps(level = "municipality", simplify = T)
st_crs(villages) <- 3857
st_crs(map) <- 3857

villages <- merge(villages, data1)


# TOOLTIP
max_bar <- max(villages$confirmed, villages$healed, villages$dead)

villages$tooltip <- sprintf('
<div style="text-align:left;">
  <he4 style="padding-top:0; border-bottom:2px solid black; font-size:20px; font-weight:700; padding-bottom:5px;">%s</he4>
  <br><br>
  <span style="font-size:14px; font-weight:400; clear:right; padding-bottom:10px;">Statistikat:</span>
  <table>
    <tr style="vertical-align:middle;">
      <td style="font: 11px sans-serif;text-align:right;padding-right:3px;">Të konfirmuar: </td>
      <td class="tipbardiv"><div style="background-color:steelblue;font:9px sans-serif;text-align:left;padding:3px;margin:1px;line-height:10px;color:black;width:%dpx;">%3.0f</div></td>
    </tr>
    <tr style="vertical-align:middle;">
      <td style="font: 11px sans-serif;text-align:right;padding-right:3px;">Të shëruar: </td>
      <td class="tipbardiv"><div class="tipbar" style="background-color:steelblue;font:9px sans-serif;text-align:left;padding:3px;margin:1px;line-height:10px;color:black;width:%dpx;">%3.0f</div></td>
    </tr>
    <tr style="vertical-align:middle;">
      <td style="font: 11px sans-serif;text-align:right;padding-right:3px;">Të vdekur: </td>
      <td class="tipbardiv"><div class="tipbar" style="background-color:steelblue;font:9px sans-serif;text-align:left;padding:3px;margin:1px;line-height:10px;color:black;width:%dpx;">%3.0f</div></td>
    </tr>
   </table>
</div>',
                            stri_trans_totitle(villages$vendbanimi),
                            round((villages$confirmed/(max_bar/1.25)*100)), villages$confirmed,
                            round((villages$healed/(max_bar/1.25)*100)), villages$healed,
                            round((villages$dead/(max_bar/1.25)*100)), villages$dead)

villages$tooltip <- gsub("\\\n", "", villages$tooltip)

#------GGPLOT------
p <- ggplot(villages) +
  geom_sf_interactive(aes(fill = covidbreaks,
                          tooltip = tooltip, data_id = id),
                      color = '#FFFDFB',
                      size = 0.10) +
  geom_sf(data = map, color = "#737373", size = 0.10, fill = NA)+
  scale_fill_manual("Confirmed:", values = c("white", "#FFEFD9", "#FBD49B", "#F6A820", "#EE6E00", "#D94216", "#A32122", "#9C2021", "#751819", "#300A0A"))+
  # ggspatial::annotation_scale(location = "bl",line_width = 0.1, height = unit(0.10, "cm")) +
  labs(
    caption = "Source: IKSHPK | © http://kosovo.today, Author Kushtrim visoka"
  ) +
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 9, hjust = 0, color = "#4e4d47"), 
        plot.subtitle = element_text(size = 6, hjust = 0, color = "#4e4d47", 
                                     margin = margin(b = -0.1, 
                                                     t = -0.1, 
                                                     l = 2, 
                                                     unit = "cm"), 
                                     debug = F),
        legend.title = element_text(
          size = 12,
          color = fontcolor,
          family = myfont,
          margin = margin(0, 0, 0.2, 0, 'cm')),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.background = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(), 
        legend.direction = "vertical", 
        legend.position = "right",
        legend.box = "vertical",
        # plot.margin = margin(1, 1, 1, 1, 'cm'),
        legend.key.height = unit(0.5, "cm"), legend.key.width = unit(0.2, "cm"),
        plot.caption = element_text(size = 4, 
                                    hjust = 0, 
                                    margin = margin(t = 0.5, 
                                                    b = 0, 
                                                    unit = "cm"), family = myfont,
                                    color = "#222222")
  )

tooltip_css <- "background-color:#ffffff;
font-family:sans-serif,arial;
font-size:100%;
color:#000000;
box-shadow: 0px 0px 21px -6px rgba(0, 0, 0, 0.75);
padding:10px 10px 15px;
border: 1px solid #222;
border-radius:0px 0px 0px 0px;"

hover_css <- "
fill: rgb(32,32,32, 0.8);"

p <- girafe(ggobj = p, 
       options = list(opts_hover(css = hover_css), opts_tooltip(css = tooltip_css), opts_zoom(max = 10)))

htmlwidgets::saveWidget(p, "/Users/kushtrimvisoka/Library/Mobile Documents/com~apple~CloudDocs/Web/kosovo.today/map.html",
                        selfcontained = F)

# ggsave(plot = p, file="/Users/kushtrimvisoka/Library/Mobile Documents/com~apple~CloudDocs/Web/kosovo.today/images/map.svg", width=10, height=5)

# ------- 1 / 4 ------- 

kum <- data %>% 
  drop_na(Name) %>% 
  group_by(date) %>% 
  summarise(confirmed = sum(confirmed), healed = sum(healed), dead = sum(dead)) %>% 
  mutate(active = confirmed - healed - dead) %>% 
  mutate("Mesatarja e të konfirmuarëve" = cummean(confirmed), "Mesatarja e të shëruarëve" = cummean(healed), "Mesatarja e të vdekurve" = cummean(dead), "Mesatarja e rasteve aktive" = cummean(active)) %>% 
  mutate("Mesatarja 7 dit. e të konfirmuarëve" = rollmean(confirmed, 7, na.pad = T, align = "right"), "Mesatarja 7 dit. e të shëruarëve" = rollmean(healed, 7, na.pad = T, align = "right"), "Mesatarja 7 dit. e të vdekurve" = rollmean(dead, 7, na.pad = T, align = "right"), "Mesatarja 7 dit. e rasteve aktive" = rollmean(active, 7, na.pad = T, align = "right")) %>%
  rename("Të konfirmuar" = confirmed, "Të shëruar" = healed, "Të vdekur" = dead, "Rastet Aktive" = active) %>% 
  slice_tail(n = 30)

max_bar <- max(kum$`Të konfirmuar`, kum$`Të shëruar`, kum$`Të vdekur`)

kum$tooltip <- sprintf('
<div style="text-align:left;">
  <he4 style="padding-top:0; border-bottom:2px solid black; font-size:20px; font-weight:700; padding-bottom:5px;">%s</he4>
  <br><br>
  <span style="font-size:14px; font-weight:400; clear:right; padding-bottom:10px;">Statistikat:</span>
  <table>
    <tr style="vertical-align:middle;">
      <td style="font: 11px sans-serif;text-align:right;padding-right:3px;">Të konfirmuar: </td>
      <td class="tipbardiv"><div style="background-color:steelblue;font:9px sans-serif;text-align:left;padding:3px;margin:1px;line-height:10px;color:black;width:%dpx;">%3.0f</div></td>
    </tr>
    <tr style="vertical-align:middle;">
      <td style="font: 11px sans-serif;text-align:right;padding-right:3px;">Të shëruar: </td>
      <td class="tipbardiv"><div class="tipbar" style="background-color:steelblue;font:9px sans-serif;text-align:left;padding:3px;margin:1px;line-height:10px;color:black;width:%dpx;">%3.0f</div></td>
    </tr>
    <tr style="vertical-align:middle;">
      <td style="font: 11px sans-serif;text-align:right;padding-right:3px;">Të vdekur: </td>
      <td class="tipbardiv"><div class="tipbar" style="background-color:steelblue;font:9px sans-serif;text-align:left;padding:3px;margin:1px;line-height:10px;color:black;width:%dpx;">%3.0f</div></td>
    </tr>
   </table>
</div>',
                       stri_trans_totitle(kum$date),
                       round((kum$`Të konfirmuar`/(max_bar/1.25)*100)), kum$`Të konfirmuar`,
                       round((kum$`Të shëruar`/(max_bar/1.25)*100)), kum$`Të shëruar`,
                       round((kum$`Të vdekur`/(max_bar/1.25)*100)), kum$`Të vdekur`)

kum$tooltip <- gsub("\\\n", "", kum$tooltip)

kum1 <- melt(kum, 
             id.vars= c("date", "tooltip"), 
             measure.vars= c("Të konfirmuar", "Të shëruar", "Të vdekur", "Rastet Aktive"),
             variable.name= "type1",
             value.name=    "value1"
)

kum2 <- melt(kum, 
             id.vars= c("date"), 
             measure.vars= c("Mesatarja 7 dit. e të konfirmuarëve", "Mesatarja 7 dit. e të shëruarëve", "Mesatarja 7 dit. e të vdekurve", "Mesatarja 7 dit. e rasteve aktive"),
             variable.name= "type2",
             value.name=    "value2"
)

kum <- bind_cols(kum1, kum2) %>% 
  rename(date = date...1) 

kum$date <- as.Date(kum$date, '%m/%d/%Y')
kum$value1 <- as.numeric(kum$value1)
kum$value2 <- as.numeric(kum$value2)

kum1 <- kum %>% 
  filter(type1 == "Të konfirmuar")

a <- ggplot()+
  geom_area(data = kum1,  aes(x=date, y=value2), alpha = 0.4, show.legend = F, fill = "red")+
  geom_bar_interactive(data = kum1, aes(x=date, y=value1, tooltip = tooltip, data_id = date), stat = "identity", position = "dodge", alpha = 0.3, show.legend = F, fill = "red")+
  geom_line(data = kum1, aes(x=date, y=value2), color = "red")+
  scale_y_continuous(breaks = seq(-3000, 3000, 50)) +
  xlab('') +
  ylab('Rastet')+
  theme_kushtrim()+
  theme(plot.title = element_text(
    size = 14, 
    hjust = 0,
    family = myfont,
    color = fontcolor), 
    legend.title = element_text(
      size = 8,
      color = fontcolor,
      family = myfont,
      margin = margin(0, 0, 0.2, 0, 'cm')),
    plot.subtitle = element_text(
      size = 6, 
      hjust = 0, 
      color = fontcolor,
      family = myfont,
      margin = margin(
        b = 0.5, 
        t = 0.2, 
        l = 2, 
        unit = "cm"), 
      debug = F),
    text = element_text(size = 12),
    axis.ticks = element_blank(), 
    legend.direction = "horizontal", 
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.key.height = unit(0.3, "cm"), 
    legend.key.width = unit(0.3, "cm"),
    plot.caption = element_text(size = 5, 
                                hjust = 0,
                                margin = margin(t = 0.5, 
                                                b = 0, 
                                                unit = "cm"), family = myfont, 
                                color = "#222222")
  )

tooltip_css <- "background-color:#ffffff;
font-family:sans-serif,arial;
font-size:100%;
color:#000000;
box-shadow: 0px 0px 21px -6px rgba(0, 0, 0, 0.75);
padding:10px 10px 15px;
border: 1px solid #222;
border-radius:0px 0px 0px 0px;"

hover_css <- "
fill: black;
stroke: black;
stroke-width: 2;"

p <- girafe(ggobj = a,
            options = list(opts_hover(css = hover_css), opts_tooltip(css = tooltip_css), opts_zoom(max = 10)))

htmlwidgets::saveWidget(p, "/Users/kushtrimvisoka/Library/Mobile Documents/com~apple~CloudDocs/Web/kosovo.today/njeslashkater.html",
                        selfcontained = F)


# ------- 2 / 4 ------- 

kum1 <- kum %>% 
  filter(type1 == "Rastet Aktive")

a <- ggplot()+
  geom_area(data = kum1,  aes(x=date, y=value2), alpha = 0.4, show.legend = F, fill = "red")+
  geom_bar_interactive(data = kum1, aes(x=date, y=value1, tooltip = tooltip, data_id = date), stat = "identity", position = "dodge", alpha = 0.3, show.legend = F, fill = "red")+
  geom_line(data = kum1, aes(x=date, y=value2), color = "red")+
  scale_y_continuous(breaks = seq(-3000, 3000, 50)) +
  xlab('') +
  ylab('Rastet')+
  theme_kushtrim()+
  theme(plot.title = element_text(
    size = 14, 
    hjust = 0,
    family = myfont,
    color = fontcolor), 
    legend.title = element_text(
      size = 8,
      color = fontcolor,
      family = myfont,
      margin = margin(0, 0, 0.2, 0, 'cm')),
    plot.subtitle = element_text(
      size = 6, 
      hjust = 0, 
      color = fontcolor,
      family = myfont,
      margin = margin(
        b = 0.5, 
        t = 0.2, 
        l = 2, 
        unit = "cm"), 
      debug = F),
    text = element_text(size = 12),
    axis.ticks = element_blank(), 
    legend.direction = "horizontal", 
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.key.height = unit(0.3, "cm"), 
    legend.key.width = unit(0.3, "cm"),
    plot.caption = element_text(size = 5, 
                                hjust = 0,
                                margin = margin(t = 0.5, 
                                                b = 0, 
                                                unit = "cm"), family = myfont, 
                                color = "#222222")
  )

tooltip_css <- "background-color:#ffffff;
font-family:sans-serif,arial;
font-size:100%;
color:#000000;
box-shadow: 0px 0px 21px -6px rgba(0, 0, 0, 0.75);
padding:10px 10px 15px;
border: 1px solid #222;
border-radius:0px 0px 0px 0px;"

hover_css <- "
fill: black;
stroke: black;
stroke-width: 2;"

p <- girafe(ggobj = a,
            options = list(opts_hover(css = hover_css), opts_tooltip(css = tooltip_css), opts_zoom(max = 10)))

htmlwidgets::saveWidget(p, "/Users/kushtrimvisoka/Library/Mobile Documents/com~apple~CloudDocs/Web/kosovo.today/dyslashkater.html",
                        selfcontained = F)


# ------- 3 / 4 ------- 

kum1 <- kum %>% 
  filter(type1 == "Të shëruar")

a <- ggplot()+
  geom_area(data = kum1,  aes(x=date, y=value2), alpha = 0.4, show.legend = F, fill = "red")+
  geom_bar_interactive(data = kum1, aes(x=date, y=value1, tooltip = tooltip, data_id = date), stat = "identity", position = "dodge", alpha = 0.3, show.legend = F, fill = "red")+
  geom_line(data = kum1, aes(x=date, y=value2), color = "red")+
  scale_y_continuous(breaks = seq(-3000, 6000, 250)) +
  xlab('') +
  ylab('Rastet')+
  theme_kushtrim()+
  theme(plot.title = element_text(
    size = 14, 
    hjust = 0,
    family = myfont,
    color = fontcolor), 
    legend.title = element_text(
      size = 8,
      color = fontcolor,
      family = myfont,
      margin = margin(0, 0, 0.2, 0, 'cm')),
    plot.subtitle = element_text(
      size = 6, 
      hjust = 0, 
      color = fontcolor,
      family = myfont,
      margin = margin(
        b = 0.5, 
        t = 0.2, 
        l = 2, 
        unit = "cm"), 
      debug = F),
    text = element_text(size = 12),
    axis.ticks = element_blank(), 
    legend.direction = "horizontal", 
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.key.height = unit(0.3, "cm"), 
    legend.key.width = unit(0.3, "cm"),
    plot.caption = element_text(size = 5, 
                                hjust = 0,
                                margin = margin(t = 0.5, 
                                                b = 0, 
                                                unit = "cm"), family = myfont, 
                                color = "#222222")
  )

tooltip_css <- "background-color:#ffffff;
font-family:sans-serif,arial;
font-size:100%;
color:#000000;
box-shadow: 0px 0px 21px -6px rgba(0, 0, 0, 0.75);
padding:10px 10px 15px;
border: 1px solid #222;
border-radius:0px 0px 0px 0px;"

hover_css <- "
fill: black;
stroke: black;
stroke-width: 2;"

p <- girafe(ggobj = a, 
            options = list(opts_hover(css = hover_css), opts_tooltip(css = tooltip_css), opts_zoom(max = 10)))

htmlwidgets::saveWidget(p, "/Users/kushtrimvisoka/Library/Mobile Documents/com~apple~CloudDocs/Web/kosovo.today/treslashkater.html",
                        selfcontained = F)


# ------- 4 / 4 ------- 

kum1 <- kum %>% 
  filter(type1 == "Të vdekur")

a <- ggplot()+
  geom_area(data = kum1,  aes(x=date, y=value2), alpha = 0.4, show.legend = F, fill = "gray")+
  geom_bar_interactive(data = kum1, aes(x=date, y=value1, tooltip = tooltip, data_id = date), stat = "identity", position = "dodge", alpha = 0.3, show.legend = F, fill = "black")+
  # scale_fill_manual("", values = c("Black", "Red", "Gray", "Black", "Red", "Gray")) +
  geom_line(data = kum1, aes(x=date, y=value2), color = "black")+
  scale_y_continuous(breaks = seq(-3000, 3000, 2)) +
  # facet_wrap(vars(kum$type1, kum$type2), scales = "free")+
  xlab('') +
  ylab('Rastet')+
  theme_kushtrim()+
  theme(plot.title = element_text(
    size = 14, 
    hjust = 0,
    family = myfont,
    color = fontcolor), 
    legend.title = element_text(
      size = 8,
      color = fontcolor,
      family = myfont,
      margin = margin(0, 0, 0.2, 0, 'cm')),
    plot.subtitle = element_text(
      size = 6, 
      hjust = 0, 
      color = fontcolor,
      family = myfont,
      margin = margin(
        b = 0.5, 
        t = 0.2, 
        l = 2, 
        unit = "cm"), 
      debug = F),
    text = element_text(size = 12),
    axis.ticks = element_blank(), 
    legend.direction = "horizontal", 
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.key.height = unit(0.3, "cm"), 
    legend.key.width = unit(0.3, "cm"),
    plot.caption = element_text(size = 5, 
                                hjust = 0,
                                margin = margin(t = 0.5, 
                                                b = 0, 
                                                unit = "cm"), family = myfont, 
                                color = "#222222")
  )

tooltip_css <- "background-color:#ffffff;
font-family:sans-serif,arial;
font-size:100%;
color:#000000;
box-shadow: 0px 0px 21px -6px rgba(0, 0, 0, 0.75);
padding:10px 10px 15px;
border: 1px solid #222;
border-radius:0px 0px 0px 0px;"

hover_css <- "
fill: black;
stroke: black;
stroke-width: 2;"


p <- girafe(ggobj = a,
            options = list(opts_hover(css = hover_css), opts_tooltip(css = tooltip_css), opts_zoom(max = 10)))

htmlwidgets::saveWidget(p, "/Users/kushtrimvisoka/Library/Mobile Documents/com~apple~CloudDocs/Web/kosovo.today/katerslashkater.html",
                        selfcontained = F)


# ------- Main Chart ------- 

kum <- data %>%
  drop_na(Name) %>% 
  group_by(date) %>% 
  summarise(confirmed = sum(confirmed), healed = sum(healed), dead = sum(dead)) %>% 
  mutate(active = confirmed - healed - dead) %>% 
  mutate(Confirmed = cumsum(confirmed), Healed = cumsum(healed), Dead = cumsum(dead), active = cumsum(active)) %>% 
  rename("Të konfirmuar" = Confirmed, "Rastet aktive" = active, "Të shëruar" = Healed, "Të vdekur" = Dead) 


kum <- melt(kum, 
            id.vars= c("date"), 
            measure.vars= c("Të konfirmuar", "Të shëruar", "Të vdekur", "Rastet aktive"),
            variable.name= "type",
            value.name=    "value"
)

kum$date <- as.Date(kum$date, '%m/%d/%Y')
kum$value <- as.numeric(kum$value)

p <- ggplot(kum, aes(x=date, y=value, col=type)) + 
  geom_line(size = 1)+
  scale_y_continuous(breaks = round(seq(0, max(kum$value), max(kum$value)/10), 0), labels = scales::comma) +
  scale_x_date(labels = date_format("%d/%m"), breaks = date_breaks("month")) +
  xlab('') +
  ylab('Rastet')+
  scale_color_manual(name = "Statusi", values = c("#B90B2E", "#047373", "#2D2D2D", "#F4C363")) +
  theme_kushtrim()+
  theme(plot.title = element_text(
    size = 14, 
    hjust = 0,
    family = myfont,
    color = fontcolor), 
    legend.title = element_text(
      size = 8,
      color = fontcolor,
      family = myfont,
      margin = margin(0, 0, 0.2, 0, 'cm')),
    plot.subtitle = element_text(
      size = 6, 
      hjust = 0, 
      color = fontcolor,
      family = myfont,
      margin = margin(
        b = 0.5, 
        t = 0.2, 
        l = 2, 
        unit = "cm"), 
      debug = F),
    text = element_text(size = 12),
    axis.ticks = element_blank(), 
    legend.direction = "horizontal", 
    legend.position = "bottom",
    legend.box = "horizontal",
    axis.text.x = element_text(angle=45),
    # plot.margin = margin(1, 1, 1, 1, 'cm'),
    legend.key.height = unit(0.3, "cm"), 
    legend.key.width = unit(0.3, "cm"),
    plot.caption = element_text(size = 5, 
                                hjust = 0,
                                margin = margin(t = 0.5, 
                                                b = 0, 
                                                unit = "cm"), family = myfont, 
                                color = "#222222")
  )

ggsave(plot = p, file="/Users/kushtrimvisoka/Library/Mobile Documents/com~apple~CloudDocs/Web/kosovo.today/images/main.svg", width=10, height=5)


# ------- Table Confirmed by municipality ------

new <- data %>% 
  drop_na(Name) %>% 
  group_by(Komuna = Municipality) %>% 
  summarise(confirmed = sum(confirmed), healed = sum(healed), dead = sum(dead)) %>%
  mutate(Aktive = confirmed-healed-dead) %>% 
  select(Komuna, "Të konfirmuar" = confirmed, "Të shëruar" = healed, "Të vdekur" = dead, Aktive)

new1 <- read_excel("/Users/kushtrimvisoka/Library/Mobile Documents/com~apple~CloudDocs/01 - R/20 - Covi/masterdata.xlsx", sheet = "Sheet5") %>% 
  select(Komuna, Popullsia) 

new <- merge(new, new1, by = "Komuna")
remove(new1)

new$`Inf. për 1000 banorë` <- round(new$`Të konfirmuar`/new$Popullsia*1000, 2)
new$`Vdekur për 1000 banorë` <- round(new$`Të vdekur`/new$Popullsia*1000, 2)

new$Popullsia <- NULL

new2 <- data %>% 
  group_by(date, Komuna = Municipality) %>% 
  summarise("Të konfirmuar" = sum(confirmed)) %>% 
  ungroup() %>% 
  filter(date >= today() - days(14)) %>%
  group_by(Komuna) %>% 
  summarise(`Inf. në dy javet e fundit` = sum(`Të konfirmuar`))

new <- merge(new, new2, all = T)

# new <- new %>% select(Komuna, `Të konfirmuar`, `Inf. në dy javet e fundit`, `Inf. për 1000 banorë`, `Të vdekur`, `Vdekur për 1000 banorë`, `Të shëruar`) %>% 
#   replace(is.na(.), 0)

new <- new %>% select(Komuna, `Të konfirmuar`, `Inf. për 1000 banorë`) %>% 
  replace(is.na(.), 0)

# Render a bar chart with a label on the left
bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}

table <- reactable(
  style = list(fontFamily = "Literata", fontSize = "14px"),
  new,
  pagination = T,
  defaultPageSize = 20,
  searchable = TRUE,
  defaultSorted = "Të konfirmuar",
  defaultColDef = colDef(headerClass = "header", align = "left"),
  columns = list(
    account = colDef(
      name = "Municipality"
    ),
    `Të konfirmuar` = colDef(
      name = "Të konfirmuar",
      defaultSortOrder = "desc",
      # Render the bar charts using a custom cell render function
      cell = function(value) {
        width <- paste0(value * 100 / max(new$`Të konfirmuar`), "%")
        # Add thousands separators
        value <- format(value, big.mark = ",")
        bar_chart(value, width = width, fill = "#e74a3b")
      },
      # And left-align the columns
      align = "left"
    ),
    `Inf. për 1000 banorë` = colDef(
      name = "Inf. për 1000 banorë",
      defaultSortOrder = "desc",
      # Render the bar charts using a custom cell render function
      cell = function(value) {
        width <- paste0(value * 100 / 1000, "%")
        # Add thousands separators
        value <- format(value, big.mark = ",")
        bar_chart(value, width = width, fill = "#e74a3b")
      },
      # And left-align the columns
      align = "left"
    )
  )
)

save_reactable(table, "/Users/kushtrimvisoka/Library/Mobile Documents/com~apple~CloudDocs/Web/kosovo.today/table.html")


# ------ HISTORIKU NE REGJION -------

ks <- data
git <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
ks <- ks %>% 
  group_by(`Data e infektimit`) %>% 
  summarise(confirmed = sum(confirmed)) %>% 
  mutate(confirmed = cumsum(confirmed)) 

ks <- ks %>% 
  mutate(date = seq(1, nrow(ks), 1), type = "Kosovo")

ks$name[nrow(ks)] <- paste(paste(ks$type[nrow(ks)], ":", sep = ""), ks$confirmed[nrow(ks)])

#-------ALBANIA------
al <- git %>% 
  filter( Country.Region == "Albania") %>% 
  select(-1, -3, -4) 

al <- melt(al, 
           id.vars= c("Country.Region"), 
           measure.vars= c(2:ncol(al)),
           variable.name= "type",
           value.name=    "value"
) %>% 
  filter(value > 0)

al <- al %>% 
  mutate(date = seq(1, nrow(al), 1)) 

al$name[nrow(al)] <- paste(paste(al$Country.Region[nrow(al)], ":", sep = ""), al$value[nrow(al)])

al <- al %>% 
  select(`Data e infektimit` = type, confirmed = value, date, type = Country.Region, name)

#-------Bosnia and Herzegovina------
bh <- git %>% 
  filter( Country.Region == "Bosnia and Herzegovina") %>% 
  select(-1, -3, -4) 

bh <- melt(bh, 
           id.vars= c("Country.Region"), 
           measure.vars= c(2:ncol(bh)),
           variable.name= "type",
           value.name=    "value"
) %>% 
  filter(value > 0)

bh <- bh %>% 
  mutate(date = seq(1, nrow(bh), 1)) 

bh$name[nrow(bh)] <- paste(paste("BH", ":", sep = ""), bh$value[nrow(bh)])

bh <- bh %>% 
  select(`Data e infektimit` = type, confirmed = value, date, type = Country.Region, name)

#-------Serbia------
rs <- git %>% 
  filter( Country.Region == "Serbia") %>% 
  select(-1, -3, -4) 

rs <- melt(rs, 
           id.vars= c("Country.Region"), 
           measure.vars= c(2:ncol(rs)),
           variable.name= "type",
           value.name=    "value"
) %>% 
  filter(value > 0)

rs <- rs %>% 
  mutate(date = seq(1, nrow(rs), 1)) 

rs$name[nrow(rs)] <- paste(paste(rs$Country.Region[nrow(rs)], ":", sep = ""), rs$value[nrow(rs)])

rs <- rs %>% 
  select(`Data e infektimit` = type, confirmed = value, date, type = Country.Region, name)

#-------North Macedonia------
nm <- git %>% 
  filter( Country.Region == "North Macedonia") %>% 
  select(-1, -3, -4) 

nm <- melt(nm, 
           id.vars= c("Country.Region"), 
           measure.vars= c(2:ncol(nm)),
           variable.name= "type",
           value.name=    "value"
) %>% 
  filter(value > 0)

nm <- nm %>% 
  mutate(date = seq(1, nrow(nm), 1)) 

nm$name[nrow(nm)] <- paste(paste("NM", ":", sep = ""), nm$value[nrow(nm)])

nm <- nm %>% 
  select(`Data e infektimit` = type, confirmed = value, date, type = Country.Region, name)

#-------Montenegro------
mn <- git %>% 
  filter( Country.Region == "Montenegro") %>% 
  select(-1, -3, -4) 

mn <- melt(mn, 
           id.vars= c("Country.Region"), 
           measure.vars= c(2:ncol(mn)),
           variable.name= "type",
           value.name=    "value"
) %>% 
  filter(value > 0)

mn <- mn %>% 
  mutate(date = seq(1, nrow(mn), 1)) 

mn$name[nrow(mn)] <- paste(paste(mn$Country.Region[nrow(mn)], ":", sep = ""), mn$value[nrow(mn)])

mn <- mn %>% 
  select(`Data e infektimit` = type, confirmed = value, date, type = Country.Region, name)

#------BIND Data-----

region <- rbind(al, bh, ks, mn, nm, rs)
remove(al, bh, ks, mn, nm, rs, git)

#------ggplot-----

mult_format <- function() {
  function(x) format(x^2, digits = 1)
}

ybreaks <- c(sqrt(1), sqrt(10), sqrt(100), sqrt(1000), sqrt(10000), sqrt(100000), sqrt(200000), sqrt(500000), sqrt(750000), sqrt(1000000), sqrt(1500000))

p <- ggplot(region, aes(x=date, y=sqrt(confirmed), col=type)) + 
  geom_line(size = 0.9, aes(group = type), show.legend =F)+
  # geom_point_interactive(aes(tooltip = value), size = 0.2, show.legend = F) +
  ggrepel::geom_text_repel(size = 3, aes(date, sqrt(confirmed), label = name), colour = "#6C6C6C", fontface = "bold", box.padding = 0.5)+
  # scale_y_continuous(breaks = seq(0, 2000, 400)) +
  scale_y_continuous(trans = 'sqrt', labels = mult_format(), breaks = ybreaks)+
  scale_x_continuous(limits = c(1, max(region$date)), breaks = seq(1, max(region$date), 20)) +
  xlab('Ditët që nga shfaqja e parë e virusit') +
  ylab('Nr. i të konfirmuarëve')+
  scale_color_manual(name = "Country", values = c("#F60000","#D3D3D3", "black", "#D3D3D3",  "#D3D3D3", "#D3D3D3")) +
  # scale_fill_manual(name = "Country", values = brewer.pal(11, "Paired")) +
  # scale_color_manual(name = LegendTitle, values = c("#1A244A", "#C72E26")) +
  theme_kushtrim()+
  theme(plot.title = element_text(
    size = 14, 
    hjust = 0,
    family = myfont,
    color = fontcolor), 
    legend.title = element_text(
      size = 10,
      color = fontcolor,
      family = myfont,
      margin = margin(0, 0, 0.2, 0, 'cm')),
    plot.subtitle = element_text(
      size = 6, 
      hjust = 0, 
      color = fontcolor,
      family = myfont,
      margin = margin(
        b = 0.5, 
        t = 0.2, 
        l = 2, 
        unit = "cm"), 
      debug = F),
    text = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.ticks = element_blank(), 
    legend.direction = "horizontal", 
    legend.position = "bottom",
    legend.box = "horizontal",
    # plot.margin = margin(1, 1, 1, 1, 'cm'),
    legend.key.height = unit(0.2, "cm"), 
    legend.key.width = unit(0.6, "cm"),
    plot.caption = element_text(size = 8, 
                                hjust = 0,
                                margin = margin(t = 0.5, 
                                                b = 0, 
                                                unit = "cm"), family = myfont, 
                                color = "#222222")
  )

ggsave(plot = p, file="/Users/kushtrimvisoka/Library/Mobile Documents/com~apple~CloudDocs/Web/kosovo.today/images/region.svg", width=10, height=6)

# ------ Confirmed by city ------

kum <- data
kum1 = data.frame()

for(i in unique(kum$Municipality)) {
  start_date <- as.Date("2020-03-13")
  date <- seq(start_date, by = "day", length.out = length(unique(kum$date)))
  date <- data.frame(date)
  date$Municipality <- i
  model <- kum %>% 
    filter(Municipality == i) %>% 
    group_by(Municipality, date) %>% 
    summarise(Confirmed = sum(confirmed), Healed = sum(healed), Dead = sum(dead)) %>% 
    mutate(Active = Confirmed - Healed - Dead) 
  df <- merge(date, model, all = T)
  df <- df %>% 
    replace(is.na(.), 0)%>% 
    mutate("ConfirmedSevenDayAverage" = rollmean(Confirmed, 7, na.pad = T, align = "right"), "HealedSevenDayAverage" = rollmean(Healed, 7, na.pad = T, align = "right"), "DeadSevenDayAverage" = rollmean(Dead, 7, na.pad = T, align = "right"), "ActiveSevenDayAverage" = rollmean(Active, 7, na.pad = T, align = "right"))
  kum1 <- rbind(kum1,df)
}

kum1 <- kum1 %>% 
  filter(!str_detect(Municipality, "Nuk dihet"))
remove(kum, model, df, date)
kum1 <- kum1 %>% 
  group_by(Municipality) %>% 
  mutate(max = max(Confirmed)) %>% 
  mutate(maksi = max == Confirmed) 
idx <- which(kum1$maksi == "TRUE")
kum1$max[-idx] <- NA
kum1 <- kum1 %>% 
  group_by(Municipality) %>% 
  mutate(max2 = duplicated(max))
idx <- which(kum1$max2 == "FALSE")
kum1$max[-idx] <- NA
a <- ggplot()+
  geom_area(data = kum1,  aes(x=date, y=ConfirmedSevenDayAverage), alpha = 0.4, show.legend = F, fill = "red")+
  geom_bar_interactive(data = kum1, aes(x=date, y=Confirmed, data_id = date), stat = "identity", position = "dodge", alpha = 0.3, show.legend = F, fill = "red")+
  # scale_fill_manual("", values = c("Black", "Red", "Gray", "Black", "Red", "Gray")) +
  geom_line(data = kum1, aes(x=date, y=ConfirmedSevenDayAverage), color = "red")+
  geom_text(data = kum1, aes(x = date, y = Confirmed, label = max), position = position_dodge(0.9), colour = "black", fontface = "bold", size = 2.5)+
  scale_y_continuous(breaks = seq(-3000, 3000, 20)) +
  facet_wrap(~kum1$Municipality, scales = "free_y")+
  xlab('') +
  ylab('Rastet')+
  theme_empty()+
  theme(plot.title = element_text(size = 10, hjust = 0, family = myfont, color = fontcolor), 
    legend.title = element_text(size = 8, color = fontcolor, family = myfont, margin = margin(0, 0, 0.2, 0, 'cm')),
    plot.subtitle = element_text(size = 6, hjust = 0, color = fontcolor, family = myfont, margin = margin(b = 0.5, t = 0.2, l = 2, unit = "cm"), debug = F),
    text = element_text(size = 12),
    axis.ticks = element_blank(), 
    legend.direction = "horizontal", 
    legend.position = "bottom",
    legend.box = "horizontal",
    strip.text = element_text(size = 10),
    # plot.margin = margin(1, 1, 1, 1, 'cm'),
    legend.key.height = unit(0.3, "cm"), 
    legend.key.width = unit(0.3, "cm"),
    plot.caption = element_text(size = 5, hjust = 0, margin = margin(t = 0.5, b = 0, unit = "cm"), family = myfont, color = "#222222")
  )


ggsave(plot = a, file="/Users/kushtrimvisoka/Library/Mobile Documents/com~apple~CloudDocs/Web/kosovo.today/images/bymuni.jpeg", width=10, height=7, dpi = 300)

# ------ Rrjedha ------

new <- data %>%
  group_by(date) %>%
  summarise(confirmed = sum(confirmed), healed = sum(healed), dead = sum(dead)) %>%
  rename("Të konfirmuar" = confirmed, "Të shëruar" = healed, "Të vdekur" = dead)

new <- melt(new,
            id.vars= c("date"),
            measure.vars= c("Të konfirmuar", "Të shëruar", "Të vdekur"),
            variable.name= "type",
            value.name=    "value"
)

new$date <- as.Date(new$date, '%m/%d/%Y')
new$value <- as.numeric(new$value)

p <- ggplot(new, aes(x=date, y=value, group=type, fill = type)) +
  geom_stream(bw = 0)+
  scale_fill_manual(name = "Statusi", values = c("#B90B2E", "#047373", "#2D2D2D")) +
  scale_y_continuous(breaks = seq(-3000, 3000, 1000)) +
theme_rrjedha()+
  xlab('') +
  ylab('')+
  theme(plot.title = element_text(
    size = 14,
    hjust = 0,
    family = myfont,
    color = fontcolor),
    legend.title = element_text(
      size = 8,
      color = fontcolor,
      family = myfont,
      margin = margin(0, 0, 0.2, 0, 'cm')),
    plot.subtitle = element_text(
      size = 6,
      hjust = 0,
      color = fontcolor,
      family = myfont,
      margin = margin(
        b = 0.5,
        t = 0.2,
        l = 2,
        unit = "cm"),
      debug = F),
    text = element_text(size = 12),
    axis.ticks = element_blank(),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.box = "horizontal",
    # plot.margin = margin(1, 1, 1, 1, 'cm'),
    legend.key.height = unit(0.3, "cm"),
    legend.key.width = unit(0.3, "cm"),
    plot.caption = element_text(size = 5,
                                hjust = 0,
                                margin = margin(t = 0.5,
                                                b = 0,
                                                unit = "cm"), family = myfont,
                                color = "#222222")
  )

ggsave(plot = p, file="/Users/kushtrimvisoka/Library/Mobile Documents/com~apple~CloudDocs/Web/kosovo.today/images/rrjedha.svg", width=10, height=5)
