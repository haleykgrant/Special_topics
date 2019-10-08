library(ggplot2)
library(tidyverse)
library(magrittr)

load("PAXINTEN_C.rda")
load("PAXINTEN_D.rda")
load("Covariate_C.rda")
load("Covariate_D.rda")
load("FLAGS_C.rda")
load("FLAGS_D.rda")

PAXINTEN <- bind_rows(PAXINTEN_C, PAXINTEN_D)
Flags    <- bind_rows(Flags_C, Flags_D)

PAXINTEN_log <- bind_cols(PAXINTEN[, 1:5],
                          log(PAXINTEN[, -c(1:5)] + 1))

dat <- bind_rows(
    (PAXINTEN %>% mutate(KEY = "count")),
    (PAXINTEN_log %>% mutate(KEY = "log")),
    (Flags %>% mutate(KEY = "flag"))
)

rm(PAXINTEN, Flags, PAXINTEN_log)

mean_column <- function(col_name) {
    tmp <- dat %>% filter(KEY == col_name)
    sapply(1:7, function(i) {
        tmp <- tmp %>%
            filter(WEEKDAY == i) %>%
            select(-c(SEQN, PAXCAL, PAXSTAT, WEEKDAY, SDDSRVYR, KEY))
        colMeans(tmp, na.rm = TRUE)
    }) %>%
        set_colnames(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")) %>%
        as_tibble() %>%
        mutate(minute = 1:n()) %>%
        gather(key = "day", value = "value", -minute)
}

aves <- bind_rows(
    mean_column("count") %>% mutate(key = "count"),
    mean_column("log") %>% mutate(key = "log"),
    mean_column("flag") %>% mutate(key = "flag")
)


aves %>%
    mutate(key = factor(key, 
                        levels = c("count", "log", "flag"),
                        labels = c("average activity count",
                                   "average log activity count",
                                   "fraction wearing device"))) %>%
    ggplot(aes(x = minute, y = value, colour = day)) +
    geom_line() +
    facet_grid(key ~ ., scales = "free_y") +
    theme_bw()
