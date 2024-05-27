#DADOS SIA 
library(microdatasus)
library(tidyverse)
#Produção ambulatorial

df = fetch_datasus(information_system = "SIA-AM", 
                   year_start = 2019, 
                   year_end = 2019, 
                   month_start = 1, 
                   month_end = 6)
gc()
df1 = df %>% filter(grepl("K50",AP_CIDPRI, fixed = TRUE))
df2 = df %>% filter(grepl("K51",AP_CIDPRI, fixed = TRUE))
gc()
df = rbind(df1,df2) 
rm(df1,df2)

gc()
#AP_TPAPAC é bem importante, fala se é apac é de início ou continudade

df1 = fetch_datasus(information_system = "SIA-AM", 
                   year_start = 2019, 
                   year_end = 2019, 
                   month_start = 7, 
                   month_end = 12)
gc()
df2 = df1 %>% filter(grepl("K50",AP_CIDPRI, fixed = TRUE))
df3 = df1 %>% filter(grepl("K51",AP_CIDPRI, fixed = TRUE))
gc()
df1 = rbind(df2,df3) 
rm(df2,df3)
gc()
df = rbind(df,df1)
rm(df1)
gc()
################################################### 2020
gc()
df1 = fetch_datasus(information_system = "SIA-AM", 
                   year_start = 2020, 
                   year_end = 2020, 
                   month_start = 1, 
                   month_end = 6)

df2 = df1 %>% filter(grepl("K50",AP_CIDPRI, fixed = TRUE))
df3 = df1 %>% filter(grepl("K51",AP_CIDPRI, fixed = TRUE))

df1 = rbind(df2,df3) 
rm(df2,df3)

df = rbind(df,df1)
rm(df1)
gc()
################################################### 2020
gc()
df1 = fetch_datasus(information_system = "SIA-AM", 
                    year_start = 2020, 
                    year_end = 2020, 
                    month_start = 7, 
                    month_end = 12)
gc()
gc()
gc()
df2 = df1 %>% filter(grepl("K50",AP_CIDPRI, fixed = TRUE))
df3 = df1 %>% filter(grepl("K51",AP_CIDPRI, fixed = TRUE))
gc()
df1 = rbind(df2,df3) 
rm(df2,df3)
gc()
gc()
df = rbind(df,df1)
rm(df1)
gc()
################################################### 2021
gc()
df1 = fetch_datasus(information_system = "SIA-AM", 
                    year_start = 2021, 
                    year_end = 2021, 
                    month_start = 1, 
                    month_end = 6)
gc()
gc()
df2 = df1 %>% filter(grepl("K50",AP_CIDPRI, fixed = TRUE))
df3 = df1 %>% filter(grepl("K51",AP_CIDPRI, fixed = TRUE))
gc()
gc()
gc()
df1 = rbind(df2,df3) 
rm(df2,df3)

df = rbind(df,df1)
rm(df1)
gc()
################################################### 2021
gc()
df1 = fetch_datasus(information_system = "SIA-AM", 
                    year_start = 2021, 
                    year_end = 2021, 
                    month_start = 7, 
                    month_end = 12)
gc()
gc()
gc()
df2 = df1 %>% filter(grepl("K50",AP_CIDPRI, fixed = TRUE))
df3 = df1 %>% filter(grepl("K51",AP_CIDPRI, fixed = TRUE))
gc()
df1 = rbind(df2,df3) 
rm(df2,df3)
gc()
df = rbind(df,df1)
rm(df1)
gc()
################################################### 2022
gc()
gc()
gc()
df1 = fetch_datasus(information_system = "SIA-AM", 
                    year_start = 2022, 
                    year_end = 2022, 
                    month_start = 1, 
                    month_end = 6)
gc()
df2 = df1 %>% filter(grepl("K50",AP_CIDPRI, fixed = TRUE))
df3 = df1 %>% filter(grepl("K51",AP_CIDPRI, fixed = TRUE))
gc()
df1 = rbind(df2,df3) 
rm(df2,df3)
gc()
df = rbind(df,df1)
rm(df1)
gc()
################################################### 2022
gc()
gc()
gc()
gc()
gc()
gc()
gc()
df1 = fetch_datasus(information_system = "SIA-AM", 
                    year_start = 2022, 
                    year_end = 2022, 
                    month_start = 7, 
                    month_end = 12)

df2 = df1 %>% filter(grepl("K50",AP_CIDPRI, fixed = TRUE))
df3 = df1 %>% filter(grepl("K51",AP_CIDPRI, fixed = TRUE))
gc()
gc()
gc()
df1 = rbind(df2,df3) 
rm(df2,df3)
gc()
gc()
df = rbind(df,df1)
rm(df1)
gc()
gc()
gc()
gc()
save(df, file = "crohn.RData")
