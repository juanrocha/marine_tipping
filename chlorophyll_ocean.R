library(tidyverse)
library(fs)
library(stars)
library(tictoc)
library(furrr)
library(patchwork)
## Only needed for change point analysis
library(trend)
library(strucchange)
library(changepoint)
set.seed(2025)


#### monthly dataset ####
fls <- dir_info("~/Documents/Projects/DATA/BICEP_biological_carbon_pump/marine_primary_production_9km_monthly/") |> 
    filter(str_detect(path, "\\.nc$"))

# dat <- read_stars(.x = fls$path[1], sub = "chl_a", RasterIO = tiles[1,])
# dat#
# plot(dat, col = viridis::viridis_pal(), axes = TRUE)

# reading a small chunk for testing (see: https://r-spatial.github.io/stars/articles/stars2.html)
#rasterio <- list(nXOff = 400, nYOff = 400, nXSize = 100, nYSize = 100)

tic("reading stack")
dat <- read_stars(
    .x = fls$path,  proxy = TRUE, sub = 5,#sub = "chl_a",
    #RasterIO = rasterio,
    quiet = TRUE)
toc() #2s

dat |> st_set_crs(6422)

lon <- st_get_dimension_values(dat, 'x')
lat <- st_get_dimension_values(dat, 'y')
time <- st_get_dimension_values(dat, "time")

# bb <- st_bbox(dat)
tiles <- st_tile(img_rows = nrow(dat), img_cols = ncol(dat), 216, 432) 

# bb <- tiles |> 
#     as_tibble() |> 
#     ## bbox are 0-based
#     mutate(xmin = nXOff - 1, ymin = nYOff - 1, xmax = xmin+nXSize, ymax = ymin+nYSize) |> 
#     select(-starts_with("n"))
# 
# tile <- st_crop(dat, st_bbox(flatten_dbl(bb[1,])) ) |> 
#     st_as_stars()

tic()
tile <- dat["attr", 3889:(3889+431), 1945:(1945+215),] |> st_as_stars()
toc() # 2.3s for one tile

slope_trend <- #carrier::crate(
    function(x){
    df <- tibble::tibble(clo = as.vector(x))
    df <- df |> 
        dplyr::mutate(clo = dplyr::case_when(clo>900 ~ NA, .default = clo)) |> 
        dplyr::mutate(clo = imputeTS::na_interpolation(clo, "spline")) |> 
        dplyr::mutate(id = dplyr::row_number())
    fit <- df |> tsibble::as_tsibble(index = "id") |>  
        fabletools::model(feasts::STL(clo ~ season(12))) |> 
        fabletools::components() |> 
        stats::lm(formula = remainder ~ id)
    #sss <- trend::sea.sens.slope(df$clo)
    
    kendal <- stats::cor.test(df$clo, df$id, method = "kendall")
    intercept = stats::coefficients(fit)[1] |> as.vector()
    slope = stats::coefficients(fit)[2] |> as.vector()
    kendall_pval = kendal$p.value |> as.vector()
    kendall_tau = kendal$estimate |> as.vector()
    
    return(c(intercept = intercept, slope = slope, kendall_pval = kendall_pval,
             kendall_tau = kendall_tau))
} #)

slope <- #carrier::crate(
    function(x){
    x[x > 900] <- NA # remove weird errors, make them NAs 
    nas <- sum(is.na(x))
    if (nas > 184)
        c(intercept = 9999, slope = 9999, kendall_pval = 9999, kendall_tau=9999)
    else
        slope_trend(x)
        #coefficients(lm(t(x)~t1))[2]
} #)



### Parallel 
# library(foreach)
# library(doFuture)
plan(multisession)

## RasterIO didn't work, so using a different approach to read the chunk
## tic("Reading tile")
# tile <- read_stars(fls$path, proxy = FALSE, sub = "chl_a",
#                    RasterIO = tiles[1,], quiet = TRUE)
# toc()  #3.1s | 206MB
## It seems the problem with RasterIO or reading the data by chunks is that the
## indexing in RasterIO is 0-based while are is 1-based. So the size in the x and y
## direction needed to be cut by one.

tic()
future_walk(
    .x = (1:nrow(tiles))[pblms],
    .f = function(i){

        tile <- dat["attr", 
                    (tiles[i,"nXOff"]):(tiles[i,"nXOff"]+431), 
                    (tiles[i,"nYOff"]):(tiles[i,"nYOff"]+215),] |> 
            st_as_stars()
        
        #tic("Calculating...")
        out <- st_apply(adrop(tile), c(1,2), slope) 
        #toc() # 2414.586s | 40min for one tile
        
        out <- out |> 
            aperm(c(2,3,1)) |> 
            split("slope") |> 
            #dplyr::rename(intercept = X1, slope = X2,kendall_pval = X3, kendall_tau = X4) |> 
            merge(name = "chl_a")
        
        #tic("Saving file")
        write_mdim(
            x = out,
            filename = paste0("temp/tile_", i, ".nc")
        )
        #toc() 
    }, .options = furrr_options(
        globals = list(slope = slope, slope_trend = slope_trend, tiles = tiles, fls = fls, dat = dat),
        packages = "stars", seed = TRUE
    ), .progress = TRUE)
toc()

#### Notes for future exercises:
# Assingn missing values as a number, less likelihood of failing. If the tile is all
# NAs then GDAL fails at creating a fail because the type of output is not numeric, it is logical.
# Use named outputs in return (), that way the rename does not fail given that the two functions
# were using initially different named outputs.
# For future exercises, perhaps useful to store everyting as integers, so multiplying the output by a 
# known factor (e.g. 1000) to get rid of decimal point and reduce memory usage.

# recover errors:
pblms <- dir_info("temp/")
nrs <- pblms |> 
    mutate(file_name = str_remove_all(path, "temp\\/|\\.nc")) |> 
    select(file_name) |> 
    separate(file_name, into = c("x", "tile_nr")) |> 
    pull(tile_nr)

pblms <- !(1:nrow(tiles)) %in% as.numeric(nrs)

(1:nrow(tiles))[pblms] # until no problems are identified

tiles |> as_tibble() |> 
    mutate(id = row_number()) |> 
    ggplot() +
    geom_rect(aes(xmin = nXOff, xmax = nXOff + nXSize, ymin = nYOff, ymax = nYOff + nYSize),
              fill = NA, color = "black") +
   geom_text(aes(x = nXOff + 216, y = nYOff + 108, label = id)) + #, color = pblms
    coord_equal() +
    scale_y_reverse()




plan(sequential)

### test that files can be readable again
pblms <- dir_info("temp/")
pblms |> arrange(desc(modification_time)) |> 
    slice(1) |> pull(path) 

tst <- read_stars("temp/tile_91.nc")

### combine results
rm(fls, nrs, out, pblms, tile, tst)
fls <- dir_ls("temp/")
# this doesn't work because it doesn't know how to stich the tiles together
# gdal_utils(util = "buildvrt", fls, destination = "temp.vrt")
# gdal_utils(util = "translate", "temp.vrt", destination = "chlor_A.nc")
# out <- read_stars("chlor_A.nc", proxy = TRUE)

tls <- tiles |> 
    as_tibble() |> 
    mutate(id = row_number(), file = paste0("temp/tile_", id, ".nc")) |> 
    split(~nXOff) 

xxs <- map(
    .x = tls, 
    .f = function(x){
        read_stars(x$file, along = "y") |> 
            st_set_dimensions(which = "y", values = lat) |> 
            split("chl_a") |> 
            dplyr::rename(intercept = X1, slope = X2,kendall_pval = X3, kendall_tau = X4) 
            }
    )

out <- c(xxs[[1]], xxs[[2]], xxs[[3]], xxs[[4]], xxs[[5]], xxs[[6]], xxs[[7]],
  xxs[[8]], xxs[[9]], xxs[[10]], along = "x")

## Fix the NAs
out[out == 9999] <- NA
plot(out[2], col = viridis::viridis_pal(), breaks = "quantile", nbreaks = 10)

plot(outplot(split(tst)[4], breaks = "equal", main = "Slope", downsample = 4,
     col = viridis::viridis_pal(), axes = TRUE))
 
out_df <- out |> as.data.frame() |> as_tibble()

out_df |> 
    ggplot(aes(kendall_pval)) +
    geom_density()

tic()
a <- out_df |> 
    filter(!is.na(kendall_tau)) +
    ggplot(aes(x,y)) +
    geom_tile(aes(fill = kendall_tau, alpha = kendall_pval < 0.05)) + #
    coord_equal() +
    scale_fill_gradient2(
        "Kendall tau",
        guide = guide_colorbar(position = "bottom", theme = theme(
            legend.position = "bottom", legend.title.position = "top",
            legend.key.height = unit(0.3,"cm"),legend.key.width = unit(5,"cm")
        ), order = 1)) +
    scale_alpha_discrete(
        name = "P value", labels = c("p > 0.05", "p < 0.05", "Missing"),
        guide = guide_legend(position = "bottom", theme = theme(
            legend.position = "bottom", legend.title.position = "top",
            legend.key.height = unit(0.3,"cm"),legend.key.width = unit(0.3,"cm")
        ), order = 2)) +
    labs(tag = "A") + 
    theme_void(base_size = 10) 
toc() #20s

tic()
b <- out_df |> 
    filter(slope > -0.00001, slope < 0.00001) |> 
    ggplot(aes(x,y)) +
    geom_tile(aes(fill = slope, alpha = kendall_pval < 0.05)) +
    coord_equal() +
    scale_fill_gradient2(
        "Slope [change in C*mg/m^3*month]", 
        guide = guide_colorbar(position = "bottom", theme = theme(
            legend.position = "bottom", legend.title.position = "top",
            legend.key.height = unit(0.3,"cm"),legend.key.width = unit(5,"cm")
        ), order = 1)) +
    scale_alpha_discrete(
        name = "P value", labels = c("p > 0.05", "p < 0.05", "Missing"),
        guide = guide_legend(position = "bottom", theme = theme(
            legend.position = "bottom", legend.title.position = "top",
            legend.key.height = unit(0.3,"cm"),legend.key.width = unit(0.3,"cm")
        ), order = 2)) + labs(tag = "B") +
    theme_void(base_size = 10) 
toc() #20s

## Make this one into a figure for the paper
a/b
ggsave(
    plot = (a/b), path = "img/", file = "slopes.png", device = "png", 
    bg = "white", dpi = 400, width = 7, height = 7
)

tic()
plot(split(out)[4], breaks = "equal", main = "Slope", downsample = 4,
     col = viridis::viridis_pal(), axes = TRUE)
toc() # 23s, many missing values | 43s for slopes
# for a chunk of 100 x 100 pixels * 276 time steps: 109s


#### Change point analysis ####
x <- st_extract(x = dat , at = matrix(c(2100,1250), nrow = 1))

df <- tibble(
    date = rownames(t(x)),
    clo =  as.vector(x)
)

df_time <- tibble(
    time = time
) |> rownames_to_column()

df <- df |> 
    mutate(clo = case_when(clo>900 ~ NA, .default = clo)) |> 
    mutate(clo = imputeTS::na_interpolation(clo, "spline")) |> 
    mutate(id = row_number()) |> 
    mutate(date = str_remove(date, "\\.chl_a")) |> 
    mutate(date = parse_date_time(date, orders = "Y_m"))

df <- df |> 
    mutate(clo_lag1 = lag(clo))
    
df |> tsibble::as_tsibble(index = "id") |>  
    fabletools::model(feasts::STL(clo ~ season(12))) |> 
    fabletools::components() |> 
    select(-.model) |> 
    pivot_longer(cols = clo:last_col(), names_to = "var", values_to = "value") |> 
    ggplot(aes(id, value)) +
    geom_line() + facet_wrap(~var, ncol = 1)
    #lm(formula = trend ~ id) 

## test with all NAs
df <- tibble(clo = rep(NA, 200))

# Break point in means with pettit test
tic()
pt <- pettitt.test(df$clo)
toc()
# break point with change point, once can also do change in variance
tic()
pt <- cpt.mean(df$clo, method = "AMOC") # AMOC looks for single change point
toc()
# change point with structural change
tic()
qlr <- Fstats(clo ~ clo_lag1, data = df)
bps <- breakpoints(qlr)
sct <- sctest(qlr, type = "supF")
toc()

plot(qlr)


break_point <- function(x){
    x <- as.vector(x)
    x[x > 900] <- NA # remove weird errors, make them NAs 
    nas <- sum(is.na(x))
    if (nas > 50 || var(x, na.rm = TRUE) == 0) 
        return(c(rep(9999,7)))
    else {
        dfm <- tibble::tibble(clo = as.vector(x))
        dfm <- dfm |> 
            dplyr::mutate(clo = dplyr::case_when(clo>900 ~ NA, .default = clo)) |> 
            dplyr::mutate(clo = imputeTS::na_interpolation(clo, "spline")) |> 
            dplyr::mutate(id = dplyr::row_number()) |> 
            dplyr::mutate(clo_lag1 = dplyr::lag(clo))
        # pettitt test: break point in means
        ptt <- trend::pettitt.test(dfm$clo)
        # structural change: break poin in residuals
        qlr <- strucchange::Fstats(clo ~ clo_lag1, data = dfm)
        bps <- strucchange::breakpoints(qlr)
        sct <- strucchange::sctest(qlr, type = "supF")
        ## calculate difference in means
        m1 <- dfm |> dplyr::filter(id < ptt$estimate) |> 
            dplyr::summarize(mean = mean(clo)) |> dplyr::pull(mean)
        m2 <- dfm |> dplyr::filter(id > ptt$estimate) |> 
            dplyr::summarize(mean = mean(clo)) |> dplyr::pull(mean)
        return(c(as.integer(unname(ptt$estimate)[1]), ptt$p.value, as.integer(bps$breakpoints),
          unname(sct$statistic), sct$p.value, m1,  m2))
    }
}

# test
tic()
break_point(x)
toc() # 0.02 s

break_point(rep(NA, 200))

break_point(as.vector(rep(2.4, 100))) # constant is the problem?

#### calculate for all oceans
plan(multisession)

tic()
future_walk(
    .x = (1:nrow(tiles)),
    .f = safely(function(i){
        #tic("Reading tile: ")
        tile <- dat["attr", 
                    (tiles[i,"nXOff"]):(tiles[i,"nXOff"]+431), 
                    (tiles[i,"nYOff"]):(tiles[i,"nYOff"]+215),] |> 
            st_as_stars()
        #toc() #2s 
        
        #tic("Calculating...")
        out <- st_apply(
            X = adrop(tile),MARGIN = c(1,2), FUN = break_point)
        #toc() # 0.8s, sometimes >560s, 77s with FUTURE = TRUE, 
        
        out <- out |> 
            aperm(perm = c(2,3,1)) |> 
            split("break_point")
        
        if (names(out)[1] == "X1")
            out <- out |> 
            dplyr::rename(
                pettitt_point = X1, pettitt_pval = X2,
                Fstat_point = X3, Fstat = X4,
                Fstat_pval = X5, m1 = X6, m2 = X7
            ) # merge(name = "break_point")
        
        save(out, file = paste0("brk_point/tile_", i, ".Rda"))
         
    }), .options = furrr_options(
        globals = list(break_point = break_point, tiles = tiles, fls = fls, dat = dat),
        packages = c("stars", "trend", "strucchange"), seed = TRUE
    ), .progress = TRUE)
toc() #6103.026 sec elapsed

tile |> aggregate(
    by = "8370 days", #tile |> st_get_dimension_values("time") |> range() |> diff()
    FUN = function(x) sum(is.na(x))/276) |> 
    plot()

pblms <- dir_info("brk_point//")
nrs <- pblms |> 
    mutate(file_name = str_remove_all(path, "brk_point\\/|\\.Rda")) |> 
    select(file_name) |> 
    separate(file_name, into = c("x", "tile_nr")) |> 
    pull(tile_nr)

pblms <- !(1:nrow(tiles)) %in% as.numeric(nrs)

## bug fixed!
plan(sequential)

#### Figures ####

fls <- dir_info("brk_point/")
fls <- fls |> 
    mutate(id = str_remove_all(path, "brk_point/tile_|\\.Rda") |> 
               as.numeric()) |> 
    arrange(id)


res <- list()

for(i in seq_along(fls$path)){
    load(fls$path[i])
    res[[i]] <- out |> st_set_crs(6422)
}

res[[3]]

tls <- tiles |> as_tibble() |> 
    mutate(path = fls$path) |> 
    split(~nXOff)

lons <- list()
j <-  seq(0,100, by = 10) 
for (i in 1:10){
    lons[[i]] <-  c(res[[1+j[i]]], res[[2+j[i]]],  res[[3+j[i]]], res[[4+j[i]]], res[[5+j[i]]], 
      res[[6+j[i]]], res[[7+j[i]]], res[[8+j[i]]], res[[9+j[i]]], res[[10+j[i]]], along= "y") |> 
        st_set_dimensions(which = "y", values = NULL, offset = 2160, delta = -1) |> 
        st_set_crs(6422)
}
lons[[1]] 

out <- c(lons[[1]], lons[[2]], lons[[3]], lons[[4]], lons[[5]], lons[[6]], lons[[7]],
  lons[[8]], lons[[9]], lons[[10]], along = "x") |> 
    st_set_dimensions(which = "x", values = NULL, offset = 0, delta = 1) |> 
    st_set_crs(6422)
    
out[out == 9999] <- NA

out[3] 


plot(out[5], col = viridis::viridis_pal())

a <- out |> as.data.frame() |> 
    as_tibble() |> 
    mutate(diff_mean = m1-m2, prop_diff = m2/m1) |> 
    #ggplot(aes(prop_diff)) + geom_density() +xlim(c(0,2))
    filter(prop_diff > 0 , prop_diff < 2) |> 
    ggplot(aes(x,y)) +
    geom_tile(aes(fill = prop_diff)) + ylim(c(450,1750)) +
    coord_equal() + labs(tag = "A") +
    scale_fill_gradient2(
        name = "Proportion of difference", midpoint = 1, mid = "grey",
        guide = guide_colorbar(position = "bottom" ,theme = theme(
            legend.position = "bottom", legend.title.position = "top",
            legend.key.height = unit(0.25,"cm"),legend.key.width = unit(5,"cm")))) +
    theme_void(base_size = 10) 
    

b <- ggplot() +
    geom_stars(aes(fill = Fstat_point, alpha = Fstat_pval < 0.05), 
               data = out, downsample = 6) +
    coord_equal() +
    #using df_time to get the right breaks
    scale_fill_viridis_c("Break point: structural change test", na.value = "white", 
        breaks = c(49,97,145,193,241), labels = c(2002,2006,2010,2014,2018),
        guide = guide_colorbar(position = "bottom", theme = theme(
            legend.position = "bottom", legend.title.position = "top",
            legend.key.height = unit(0.25,"cm"),legend.key.width = unit(5,"cm")
        ))) +
    ylim(c(450,1750)) +
    scale_alpha_discrete(
        name = "F test p value", labels = c("p > 0.05", "p < 0.05", "Missing"),
        guide = guide_legend(position = "bottom", theme = theme(
            legend.position = "bottom", legend.title.position = "top",
            legend.key.height = unit(0.3,"cm"),legend.key.width = unit(0.3,"cm")
        ))) + labs(tag = "B") +
    theme_void(base_size = 10) 

c <- ggplot() +
    geom_stars(aes(fill = pettitt_point, alpha = pettitt_pval < 0.05), 
               data = out, downsample = 6) +
    coord_equal() +
    #using df_time to get the right breaks
    scale_fill_viridis_c(
        "Break point: Pettitt test", na.value = "white", 
        breaks = c(49,97,145,193,241), labels = c(2002,2006,2010,2014,2018),
        guide = guide_colorbar(position = "bottom", theme = theme(
            legend.position = "bottom", legend.title.position = "top",
            legend.key.height = unit(0.3,"cm"),legend.key.width = unit(5,"cm")
        ))) +
    ylim(c(450,1750)) +
    scale_alpha_discrete(
        name = "P value", labels = c("p > 0.05", "p < 0.05", "Missing"),
        guide = guide_legend(position = "bottom", theme = theme(
            legend.position = "bottom", legend.title.position = "top",
            legend.key.height = unit(0.3,"cm"),legend.key.width = unit(0.3,"cm")
        ))) + labs(tag = "C") +
    theme_void(base_size = 10) 


ggsave(
    filename = "break_point.png", path = "img/", device = "png", 
    plot = (a/b/c), width = 7.5, height = 8, dpi = 400, bg = "white"
)

# df |> 
#     mutate(date = str_remove(date, "\\.chl_a")) |> 
#     mutate(date = parse_date_time(date, orders = "Y_m")) |> 
#     mutate(clo = case_when(clo > 900 ~ NA, .default = clo)) |> 
#     ggplot(aes(date, ff)) +
#     geom_line() +
#     geom_miss_point()
# 
# plot(t1, log1p(x), 'l')















#### Leftovers ####

## tests with aggregate using full time series
# t1 <- st_get_dimension_values(dat, "time")
# range(t1) |> diff()
# 
# tic()
# m1 <- aggregate(dat, by = "8370 days", FUN = mean, na.rm = TRUE)
# toc() # 0.4s
# 
# tic()
# plot(m1, col = viridis::viridis_pal(), downsample = 4)
# toc() # 30s, computes yearly means
# 

## calculate slope using code here: https://r-spatial.github.io/stars/articles/stars7.html
# slope <- function(x){
#     if (anyNA(x))
#         NA_real_
#     else
#         lm.fit(cbind(1, t1), x)$coefficients[2]
# }
# tic()
# out <- st_apply(adrop(m1), c(1,2), slope)
# toc()
# 
# tic()
# plot(out, breaks = "equal", main = "Slope Clor A 1998-2022", downsample = 4,
#      col = viridis::viridis_pal())
# toc() # 23s, many missing values

## Notes:
## Compute first number of observations: decide if use as is or interpolate
## c() the number of obs to dat on a new variable, or use it on a ifelse statement
## only calculate slopes for a min n_obs

# thigs that didn't work
library(gdalcubes)
fls <- dir_info(path = "/Users/juanrocha/Documents/Projects/DATA/ESA_ocean_color/netcdf/chlor_a/",
                recurse = TRUE)
fls$path[2]

# make sure that the files are organized in temporal order
fls <- fls |> 
    filter(type == "file") |> 
    mutate(date = str_extract(path, "\\d{8}")) |> 
    mutate(date = parse_date_time(date, "ymd")) |> 
    #select(date) |> 
    arrange(date)


fls

gdalcubes_gdalformats() 
collection_formats()

tic()
x <- create_image_collection(
    files = fls |> filter(type == "file") |>  pull(path),
    format = "ESA_CCI_SM_ACTIVE")
toc() #98s


# l <- 1:nrow(tiles) |> as.list()
# #dir_create("temp")
# tic()
# finished <- foreach(
#     i= 1:nrow(tiles),
#     .options.future = list(seed = TRUE)) %dofuture% {
#         
#     tile <- read_stars(fls$path, sub = "chl_a", proxy = FALSE, 
#                        RasterIO = tiles[i,], quiet = TRUE)
#     
#     tic()
#     out <- st_apply(adrop(tile), c(1,2), slope)
#     toc() # 2414.586s | 40min 
#     
#     tic()
#      out <-   out |>  aperm(c(2,3,1)) |> 
#         split("slope") |> 
#         dplyr::rename(intercept = X1, slope = X2,kendall_pval = X3, kendall_tau = X4) |> 
#         merge(name = "chl_a")
#     toc() # 0.6s
#     saved_file <- paste0("temp/tile_", i, ".nc")
#     tic()
#     write_mdim(
#         x = out,
#         filename = saved_file
#     )
#     toc() #0.01s
#     return(saved_file)
# } 
# toc()