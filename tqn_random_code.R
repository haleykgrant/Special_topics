library(ggplot2)
library(tidyverse)
library(magrittr)
library(rnhanesdata)

data(PAXINTEN_C)
data(PAXINTEN_D)
data(Flags_C)
data(Flags_D)
data(Covariate_C)
data(Covariate_D)

sum(PAXINTEN_C$PAXCAL != Flags_C$PAXCAL)
sum(PAXINTEN_C$PAXSTAT != Flags_C$PAXSTAT)

wear <- bind_rows(Flags_C, Flags_D) %>%
    filter(PAXCAL==1 & PAXSTAT==1) %>%
    select(-c(PAXCAL, PAXSTAT)) %>%
    mutate(WEEKDAY = factor(WEEKDAY,
                            levels = c(2:7, 1),
                            labels = c("Mon", "Tue", "Wed", "Thu", "Fri",
                                       "Sat", "Sun")))
count <- bind_rows(PAXINTEN_C, PAXINTEN_D) %>%
    filter(PAXCAL==1 & PAXSTAT==1) %>%
    select(-c(PAXCAL, PAXSTAT)) %>%
    mutate(WEEKDAY = factor(WEEKDAY,
                            levels = c(2:7, 1),
                            labels = c("Mon", "Tue", "Wed", "Thu", "Fri",
                                       "Sat", "Sun")))

px_wear <- function(w = wear, id) {
    w %>% 
        filter(SEQN==id) %>%
        gather(key = minute, value = wear, -c(SEQN, WEEKDAY, SDDSRVYR)) %>%
        mutate(minute = substr(minute, 4, nchar(minute)),
               minute = as.integer(minute))
}

px_count <- function(c = count, id) {
    c %>% 
        filter(SEQN==id) %>%
        gather(key = minute, value = count, -c(SEQN, WEEKDAY, SDDSRVYR)) %>%
        mutate(minute = substr(minute, 4, nchar(minute)),
               minute = as.integer(minute)) %>%
        group_by(WEEKDAY) %>%
        mutate(maxdaycount = max(count)) %>%
        ungroup()
}

plot_day <- function(w = wear,
                     c = count,
                     id = NULL,
                     seed = NULL) {
    
    if (is.null(id)) {
        ids <- unique(w$SEQN)
        if (is.null(seed)) { seed <- sample(.Machine$integer.max, 1) }
        set.seed(seed)
        id <- sample(ids, 1)
    }
    
    wear <- px_wear(w = w, id = id)
    count <- px_count(c = c, id = id)
    
    wear %>%
        ggplot(aes(x = minute*24/1440, y = wear)) +
        geom_area(fill = "red", alpha = .7) +
        geom_line() +
        facet_grid(WEEKDAY ~ .) +
        scale_x_continuous(breaks = seq(0, 24, 3)) +
        labs(x = "time (hour)", 
             y = "wear (black/red), normed count (blue)",
             title = paste0("SEQN = ", id, ", seed =", seed)) +
        theme_bw() +
        geom_line(data = count, 
                  aes(x = minute*24/1440, y = count/maxdaycount),
                  color = "blue")
}

plot_hour <- function(w = wear, 
                      c = count, 
                      id, 
                      h) {
    
    wear <- px_wear(w = w, id = id)
    count <- px_count(c = c, id = id) %>% 
        select(-maxdaycount) %>%
        left_join(wear %>% select(WEEKDAY, SDDSRVYR, minute, wear),
                  by = c("WEEKDAY", "SDDSRVYR", "minute")) %>%
        mutate(count = ifelse(wear==1, count, NA)) %>%
        select(-wear) %>%
        filter(floor(minute*24/1440) == h) %>%
        mutate(minute = 1 + minute - min(minute))
    
    p1 <- count %>%
        ggplot(aes(x = minute, y = count)) +
        geom_point(color = "blue") +
        geom_line() +
        facet_grid(WEEKDAY ~ .) +
        labs(x = "time (minute)",
             y = "count",
             title = paste0("SEQN = ", id, ", hour = ", h, ", scale = raw")) +
        theme_bw()
    
    p2 <- count %>%
        ggplot(aes(x = minute, y = log(count + 1))) +
        geom_point(color = "blue") +
        geom_line() +
        facet_grid(WEEKDAY ~ .) +
        labs(x = "time (minute)",
             y = "log count",
             title = paste0("SEQN = ", id, ", hour = ", h, ", scale = log")) +
        theme_bw()
    
    gridExtra::grid.arrange(p1, p2, ncol = 2)
    
}




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
