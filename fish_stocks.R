library(tidyverse)
library(ramlegacy)
library(tictoc)
library(patchwork)
# Download the most recent database, only need to do it once:
#download_ramlegacy()
ram_dir()

dat <- load_ramlegacy(tables = c("metadata", "tbbest.data", "area", "stock"))
dat$metadata |> as_tibble()
dat$stock |> as_tibble()
dat$area |> as_tibble() |>  pull(areacode) |> unique()
dat$tbbest.data |> as.data.frame() |> rownames_to_column("year") |> as_tibble()

## only use data / stocks for which there are long time series: at least 25yrs data
stock_25 <- dat$tbbest.data |> 
    map_df(.f = function(x) sum(!is.na(x))) |> 
    pivot_longer(cols = 1:last_col(), names_to = "stock", values_to = "obs") |> #summarize(med = median(obs))
    #ggplot(aes(obs)) + geom_density() + geom_vline(aes(xintercept = median(obs)))
    filter(obs >= 25) |> 
    pull(stock)

# reduce dataset to stocks with at least 25 years of obs
dat$tbbest.data <- dat$tbbest.data |> 
    select(all_of(stock_25)) 

#### Break points ####
# One function that calculates the slope, kendall_tau, and break points
years <- dat$tbbest.data |> 
    as.data.frame() |> 
    rownames_to_column("year") |> 
    as_tibble() |> pull(year) |> 
    as.numeric()


break_point <- function(x){
    dfr <- tibble::tibble(year = years, biomass = x) |> 
        mutate(bio_lag1 = lag(biomass)) |> 
        dplyr::filter(!is.na(biomass)) |> 
        rownames_to_column("tid") |> mutate(tid = as.numeric(tid)) 
    sss <- trend::sens.slope(dfr$biomass)
    ptt <- trend::pettitt.test(dfr$biomass)
    # structural change: break poin in residuals
    qlr <- strucchange::Fstats(biomass ~ bio_lag1, data = dfr)
    bps <- strucchange::breakpoints(qlr)
    sct <- strucchange::sctest(qlr, type = "supF")
    ## calculate difference in means
    m1 <- dfr |> dplyr::filter(tid < ptt$estimate[1]) |> 
        dplyr::summarize(mean = mean(biomass)) |> dplyr::pull(mean)
    m2 <- dfr |> dplyr::filter(tid > ptt$estimate[1]) |> 
        dplyr::summarize(mean = mean(biomass)) |> dplyr::pull(mean)
    return(c(pettitt_point = dfr |> filter(tid == as.integer(unname(ptt$estimate)[1])) |> pull(year), 
             pettitt_pval = ptt$p.value, 
             struct_point = dfr |> filter(tid == as.integer(bps$breakpoints)) |> pull(year) ,
             Fstat = unname(sct$statistic), 
             Fstat_pval = sct$p.value, 
             sen_slope = unname(sss$estimates),
             sen_pval = sss$p.value,
             m1 = m1, m2 =  m2))
}

break_point(dat$tbbest.data[,133])

out <- list()
tic()
out <- map(
    .x = dat$tbbest.data,
    .f = safely(break_point),
    .progress = TRUE
)
toc() # 2.2s

out <- transpose(out)
out$error |> map_lgl(is.null) |> all() # all good!
out <- out$result |> bind_rows() |> 
    mutate(stock = stock_25)

out <- out |> 
    mutate(p_values = case_when(
        pettitt_pval <= 0.05 & Fstat_pval <= 0.05  ~ "both",
        pettitt_pval <= 0.05 & Fstat_pval > 0.05 ~ "Pettitt",
        pettitt_pval > 0.05 & Fstat_pval <= 0.05 ~ "Fstat",
        pettitt_pval > 0.05 & Fstat_pval > 0.05 ~ "none",
        .default = "none"
    ) |> as_factor())  |> 
    mutate(prop_change = m2/m1) 

a <- out |> 
    ggplot(aes(pettitt_point, struct_point)) +
    geom_point(aes(color = p_values), alpha = 0.6, stroke = 0) +
    geom_abline(slope = 1, intercept = 0, color = "grey50", linewidth = 0.1) +
    scale_color_manual("P values < 0.05",values = c("orange", "purple", "blue", "grey30")) +
    labs(x = "Break point with Pettitt's test", y = "Break point with structural change test", tag = "A") +
    theme_light(base_size = 7) + 
    theme(legend.position = c(0.2,0.75), legend.position.inside = TRUE, legend.background = element_blank()) 

b <- out |> 
    ggplot(aes(pettitt_point, prop_change)) +
    geom_point(aes(colour = p_values), alpha = 0.6, stroke = 0) +
    geom_hline(yintercept = c(1, 0.5), linetype = 2, color = c("black", "red"), linewidth = 0.1) +
    scale_color_manual("P values < 0.05",values = c("orange", "purple", "blue", "grey30")) +
    #geom_text(aes(label = ifelse(prop_change>4, stock, NA)) )
    labs(x = "Break point with Pettitt's test", y = "Proportion of change in stock biomass", tag = "B") +
    theme_light(base_size = 7) + 
    theme(legend.position = c(0.2,0.75), legend.position.inside = TRUE, legend.background = element_blank()) 

c <- out |> 
    ggplot(aes(sen_slope, prop_change)) +
    geom_point(aes(color = sen_pval < 0.05), alpha = 0.6, stroke = 0) +
    geom_hline(yintercept = c(1, 0.5), linetype = 2, color = c("black", "red"), linewidth = 0.1) +
    geom_vline(xintercept = 1, linetype = 2, color = "black", linewidth = 0.1) +
    scale_color_manual("Sen slope",values = c("grey30", "orange"), labels = c("p > 0.05", "p < 0.05")) +
    labs(x = "Sen slope", y = "Proportion of change in stock biomass", tag = "C") +
    theme_light(base_size = 7) +
    theme(legend.position = c(0.2,0.75), legend.position.inside = TRUE, legend.background = element_blank()) 

cor(out$pettitt_point, out$struct_point)

ggsave(
    plot = (a+b+c), path = "img/", file = "fish_stocks.png", device = "png",
    width = 7, height = 3, dpi = 400, bg = "white"
)

out
a <- dat$tbbest.data |> 
    as.data.frame() |> 
    rownames_to_column("year") |> mutate(year = as.numeric(year)) |> 
    pivot_longer(cols = 2:last_col(), names_to = "stock", values_to = "biomass") |> 
    filter(!is.na(biomass), stock %in% stock_25) |> 
    left_join(out |> select(stock, pettitt_point, p_values, prop_change)) |> 
    mutate(reltime = year - pettitt_point) |> 
    ggplot(aes(reltime, biomass)) +
    geom_line(aes(group = stock, color = prop_change), alpha = 0.9, linewidth = 0.15) +
    geom_vline(xintercept = 0, linetype = 2, color = "red", linewidth = 0.2) +
    xlim(c(-25,25)) +
    scale_y_sqrt() +
    scale_color_gradient2("Proportion of change",midpoint = 1, transform = "log1p", mid = "grey") +
    facet_wrap(~p_values, nrow=1) +
    labs(x="Time relative to break point", y = "Biomass", tag = "A") +
    theme_light(base_size = 6) +
    theme(legend.position = "bottom", legend.title.position = "top")



## stasts for writing
out |> summarize(
    pettit = sum(pettitt_pval < 0.05),
    Fstat = sum(Fstat_pval < 0.05),
    decline = sum(p_values != "none" & m1 > m2),
    increase = sum(p_values != "none" & m2 > m1)
)
# 339 stocks had a break point with Pettitt test, 270 with structural change test
# 271 stocks significanly increased, 82 decreased.
out |> filter(sen_pval < 0.05) |> 
    summarise(n = sum(sen_slope < 0))


b <- out |> 
    filter(p_values != "none") |> 
    ggplot(aes(prop_change)) + geom_density(linewidth = 0.25, fill = "grey90", color = "grey90") +
    geom_rug(aes(color = prop_change)) +
    geom_vline(xintercept = c(1, 0.5), linetype = 2, color = c("black", "red"), linewidth = 0.1) +
    scale_color_gradient2("Proportion of change",midpoint = 1, transform = "log1p", mid = "grey") +
    labs(x = "Proportion of change in biomass", y = "Density", tag = "B") +
    theme_light(base_size = 6) +
    theme(legend.position = "bottom", legend.title.position = "top")


### Most relevant countries
c <- out |> 
    rename(stockid = stock) |> 
    filter(p_values != "none") |> 
    left_join(dat$stock) |> 
    left_join(dat$area) |> 
    mutate(country = case_when(country == "multinational" ~ "Multinational", .default = country)) |> 
    group_by(country) |> 
    summarize(
        n = n()
    ) |> arrange(n) |> 
    mutate(country = as_factor(country)) |> 
    ggplot(aes(n, country)) +
    geom_col() +
    labs(x = "Number of stocks with\n abrupt changes", y = "Territory", tag = "C") +
    theme_light(base_size = 6)

## Most relevant areas
d <- out |> 
    rename(stockid = stock) |> 
    filter(p_values != "none") |> 
    left_join(dat$stock) |> 
    left_join(dat$area) |> 
    group_by(areaname) |> 
    summarize(
        n = n()
    ) |> arrange(n) |> slice_tail(n = 15) |> 
    mutate(areaname = as_factor(areaname)) |> 
    ggplot(aes(n, areaname)) +
    geom_col() +
    labs(x = "Number of stocks with\n abrupt changes", y = "Area Name", tag = "D") +
    theme_light(base_size = 6)


## Most relevant spp
e <- out |> 
    rename(stockid = stock) |> 
    filter(p_values != "none") |> 
    left_join(dat$stock) |> 
    left_join(dat$area) |> 
    mutate(sp_name = str_c(
        commonname, "\n", scientificname )) |># select(sp_name)
    group_by(sp_name) |> 
    summarize(
        n = n()
    ) |> arrange(n) |> slice_tail(n = 15) |> 
    mutate(sp_name = as_factor(sp_name)) |> 
    ggplot(aes(n, sp_name)) +
    geom_col() +
    #scale_y_discrete(label = scales::label_parse()) +
    labs(x = "Number of stocks with\n abrupt changes", y = "Species name [common name / scientific name]", tag = "E") +
    theme_light(base_size = 6) 

(a+b + plot_layout(widths = c(4,1), guides = "collect")) / (c+d+e) +
    plot_layout(heights = c(1,1))  & 
    theme(legend.position = "bottom", legend.key.height = unit(2, "mm"), 
          legend.key.width = unit(20, "mm"))


ggsave(
    path = "img/", file = "stocks_ts_zero.png", device = "png", bg="white",
    width = 7, height = 6, plot = last_plot(), dpi = 400
)    
