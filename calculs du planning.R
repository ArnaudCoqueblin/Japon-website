library(tidyverse)
library(toastui)

df_cal = tribble(
  ~calendarId , ~title, ~start,  ~category,
  1 , "Laventinois", dmy("26062024"),  "allday",
  2 , "Belges", dmy("28062024"),  "allday",
  3 , "Rouennais", dmy("26062024"),  "allday"
) %>%
  mutate(end = start + days(15)) 

trajets = tibble(ville = c("TOKYO","NAGOYA","OSAKA","HIROSHIMA","TOKYO"),
                 durée = c(3,2,6,2,2))

for (i in 1:nrow(trajets)) {
  df_cal =  bind_rows(
    df_cal,
    tibble(calendarId = max(df_cal$calendarId) + 1, title = trajets$ville[i],
           start = case_when(i == 1 ~ min(df_cal$start) + days(1), 
                             TRUE ~ last(df_cal$end) + days(1) ) , 
           end = case_when(i == 1 ~ min(df_cal$start) + days(trajets$durée[i]) , 
                           TRUE ~ last(df_cal$end) + days(trajets$durée[i]) ) ,
           category = "allday", backgroundColor = "#5E81AC", color = "white")
  )
}

# ajoute la date de retour
df_cal = bind_rows(df_cal,
                   df_cal %>% filter(! title %in% trajets$ville) %>% group_by(title) %>%
                     mutate( title = paste("RETOUR -",title), start = min(start) + days(16), end = start)   )

# 
# df_cal =  bind_rows(
#   df_cal,
#   tibble(calendarId = max(df_cal$calendarId) + 1, title = "TOKYO",
#          start = min(df_cal$start) + days(1), end = max(df_cal$start) + days(1),
#          category = "allday", backgroundColor = "#5E81AC", color = "white")
# )

calendar(df_cal, view = "month", useDetailPopup = TRUE, 
         navigation = TRUE, defaultDate = min(df_cal$start)) %>%
  cal_month_options(
    startDayOfWeek  = 1, 
    daynames = c("Dim", "Lun", "Mar", "Mer", "Jeu", "Ven", "Sam"),
    narrowWeekend = TRUE
  ) %>% 
  cal_props(cal_demo_props())

# ==============================================================================================
