library(tidyverse)
library(fs)
library(here)
library(tictoc)
library(patchwork)

fls <- dir_ls("data/processed_MHW/")

## function to summarize each file
mhw_summary <- function(file){
    dat <- read_csv(file)
    x <- dat |> 
        group_by(lon, lat) |> 
        summarize(
            mean_intensity = mean(intensity_mean),
            n_events = n(),
            tot_days = sum(duration),
            mp_moderate = mean(p_moderate, na.rm = TRUE),
            mp_strong = mean(p_strong, na.rm = TRUE),
            mp_severe = mean (p_severe, na.rm = TRUE),
            mp_extreme = mean(p_extreme, na.rm = TRUE))
    return(x)
    rm(dat, x)
}
# create a function that slice only one year to check if results align with other papers
mhw_year <- function(file, date1, date2){
    dat <- read_csv(file) 
    x <- dat |> 
        filter(date_start >= date1, date_end <= date2) |> 
        group_by(lon, lat, category) |>
        add_column(n = 1) |> 
        summarize(tot = sum(n)) |> 
        pivot_wider(names_from = category, values_from = tot)
    return(x)
    rm(dat, x)
}

mhw_categories <- function(file){
    dat <- read_csv(file)
    x <- dat |> 
        group_by(lon, lat, category) |>
        add_column(n = 1) |> 
        summarize(tot = sum(n)) |> 
        pivot_wider(names_from = category, values_from = tot)
    return(x)
    rm(dat, x)
}
# create a function that extracts the proportion of the events above / below the 
# categories thresholds.
dat <- read_csv(fls[1])


tic()
out <- map(fls, mhw_summary) #, date1 = "2016-01-01", date2 = "2016-12-31"
toc() # 178s

out <- out |>
    bind_rows()
# 
out |>
    pivot_longer(cols = starts_with("I"), names_to = "category", values_to = "events" ) |>
    group_by(lon,lat, category) |>
    summarize(total_events = sum(events, na.rm = TRUE)) |> 
    #filter(category != "I Moderate") |>
    ggplot(aes(lon, lat)) +
    geom_tile(aes(fill = total_events)) +
    scale_fill_viridis_c(option = "D") +
    facet_wrap(~category) +
    theme_void(base_size = 6) +
    theme(legend.position = "bottom")

out |> ggplot(aes(mean_intensity)) + geom_density()
out$mean_intensity |> summary()

out |> 
    ungroup() |> 
    ggplot(aes(lon, lat)) + 
    geom_tile(aes(fill = mean_intensity, alpha = (mean_intensity > 1.45))) +
    #geom_tile(aes(fill = mean_intensity, alpha = n_events)) +
    scale_fill_viridis_c( option = "D") + 
    theme_void(base_size = 6) +
    theme(legend.position = "bottom")

out |> 
    group_by(lon,lat) |> 
    select(starts_with("mp")) |> 
    pivot_longer(cols = starts_with("mp"), names_to = "category", values_to = "probs") |> 
    ggplot(aes(lon,lat)) +
    geom_tile(aes(fill = probs)) +
    facet_wrap(~category) +
    scale_fill_continuous(type = "viridis", trans = "log10") +
    theme_void(base_size = 6) +
    theme(legend.position = "bottom")

a <- out |> #ggplot(aes(mp_extreme)) + #geom_density() + scale_x_log10()
    ggplot(aes(lon,lat)) + 
    geom_tile(aes(fill = mp_extreme)) +
    scale_fill_continuous("Mean number of days in extreme heatwaves\n [log scale]",
        type = "viridis", trans = "log10", guide = guide_colorbar(
            barwidth = unit(4,"cm"), barheight = unit(2, "mm"), title.position = "top", title.hjust = 0.5 )
        ) + 
    coord_sf(default_crs = sf::st_crs(4326)) +
    labs(tag = "A") +
    theme_void(base_size = 6) +
    theme(legend.position = "bottom")

b <- out |> #ggplot(aes(mp_extreme)) + #geom_density() + scale_x_log10()
    ggplot(aes(lon,lat)) + 
    geom_tile(aes(fill = mp_severe)) +
    scale_fill_continuous(
        "Mean number of days in severe heatwaves\n [log scale]",
        type = "viridis", trans = "log10", guide = guide_colorbar(
            barwidth = unit(4,"cm"), barheight = unit(2, "mm"), title.position = "top", 
            title.hjust = 0.5)) + 
    coord_sf(default_crs = sf::st_crs(4326)) +
    labs(tag = "B") +
    theme_void(base_size = 6) +
    theme(legend.position = "bottom")

## Rocha's map
#load("~/Documents/Projects_old/ESDL_earlyadopter/ESDL/Results/210212_deltas_chlorA_log.RData") #19Mb
load("~/Documents/Projects_old/ESDL_earlyadopter/ESDL/Results/210301_delta_detected_ChlorA_log.RData")


c <- df_delta_detected |> 
    ## transform to the same coords system manually
    ungroup() |> 
    mutate(lon = case_when(
        lon >= 0 ~ lon, lon < 0 ~ lon + 360)) |> # pull(lon) |> range()
    mutate(n_ews = case_when(n_ews == 0 ~ NA, .default = n_ews)) |> 
    ggplot(aes(lon,lat)) +
    geom_tile(aes(fill = n_ews)) +
    scale_fill_continuous(
        "Number of early warning signals",
        type = "viridis", option = "D",
        guide = guide_colorbar( 
            barwidth = unit(4,"cm"), barheight = unit(2, "mm"), title.position = "top")) +
    #scale_alpha_discrete("Number of early warnings >= 3", range = c(0.5, 1)) + 
    coord_sf(default_crs = sf::st_crs(4326)) +
    labs(tag = "C") + lims(y = c(-78.375,  89.875)) +
    theme_void(base_size = 6) +
    theme(legend.position = "bottom")

c

ggsave(
    plot = a + b + c,
    filename = "maps_marine_tipping_240920.png", device = "png", width = 7, height = 2,
    path = "figures/", dpi = 500, bg = "white"
)


# ggsave(
#     plot = last_plot(),
#     filename = "mhw_probabilities_log.png", device = "png", width = 6, height = 5,
#     path = "figures/", dpi = 500, bg = "white"
# )


pxls <- out |> 
    ungroup() |>  
    filter(mp_severe > 0, mp_extreme > 0)

# save(pxls, file = "data/mhw_exposure.Rda")
load("data/mhw_exposure.Rda")
