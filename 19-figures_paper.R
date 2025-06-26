library(tidyverse)
library(patchwork)
library(network)
library(ggnetwork)
library(ggnewscale)


load("data/cleaned_data_240321.Rda")
load("data/boats_owners.Rda")
## country lists
countries <- readr::read_csv(
    file = "~/Documents/Projects/DATA/WorldBank/SDG_csv/SDGCountry.csv") |> 
    janitor::clean_names()
## correct Namibia
countries[countries$short_name == "Namibia","x2_alpha_code"] <- "NA"


#### Fig 1 ####

# map of areas prone to tipping points:
## run script map_heatwaves.R to get the map figures a,b,c

#### Fig 2 ####

a <- df_boats |> 
    mutate(flag = case_when(
        flag == "U.S.A." ~ "United States",
        flag == "U.K." ~ "United Kingdom",
        flag == "Taiwan, China" ~ "Taiwan",
        .default = flag)) |> 
    group_by(flag) |> 
    summarize(n = n()) |> 
    arrange(n) |> 
    filter(n> 300) |> 
    mutate(flag = case_when(is.na(flag) ~ "Unknown", TRUE ~ flag)) |> 
    mutate(flag = as_factor(flag)) |> 
    ggplot(aes(n, flag)) +
    geom_col() +
    labs(x = "Number of vessels", y = "Vessel flag", 
         caption = "Only countries with > 300 vessels shown.\n Data source: Lloyds",
         tag = "A") +
    theme_light(base_size = 6)


# shareholder by country
b <- shrs |> 
    filter(!is.na(name)) |> 
    mutate(country = case_when(is.na(country) ~ "NA", TRUE ~ country)) |> 
    group_by(country) |>     
    summarize(n=n()) |> 
    filter(n>20) |> 
    left_join(select(countries, short_name, country = x2_alpha_code)) |> 
    mutate(short_name = case_when(
        country == "TW" ~ "Taiwan",
        country == "n.a." ~ "Unknown",
        .default =  short_name)) |> 
    arrange(n) |> 
    mutate(short_name = as_factor(short_name)) |> 
    ggplot(aes(n, short_name)) +
    geom_col() +
    labs(x = "Number of shareholders", y = "Shareholder country of origin", 
         caption = "Only countries with > 20 shareholders shown.\nData source: Orbis",
         tag = "B") +
    theme_light(base_size = 6)

# shareholder types
c <- shrs |> #pull(type) |> unique()
    filter(!is.na(name)) |> 
    mutate(shr_type = case_when(
        is.na(type) ~ "Missing",
        type == "A" ~ "Insurance company",
        type == "B" ~ "Bank",
        type == "C" ~ "Corporate companies",
        type == "D" ~ "Unnamed private shareholders",
        type == "F" ~ "Financial company",
        type == "G" ~ "Wholesale and retail trade",
        type == "H" ~ "Self ownership",
        type == "I" ~ "Individuals or families",
        type == "J" ~ "Foundation or Research Institute",
        type == "Z" ~ "Public",
        type == "L" ~ "Employees/Managers/Directors",
        type == "M" ~ "Professional, scientific and technical activities",
        type == "V" ~ "Venture capital",
        type == "Y" ~ "Hedge fund",
        type == "P" ~ "Private equity firms",
        type == "Q" ~ "Branch",
        type == "W" ~ "Marine vessel",
        type == "S" ~ "Public authorities, States,\n Governments",
        type == "E" ~ "Mutual & Pension Fund"
    )) |> 
    group_by(shr_type) |> 
    summarize(n=n()) |> 
    arrange(n) |> 
    mutate(shr_type = as_factor(shr_type)) |> 
    ggplot(aes(n, shr_type)) +
    geom_col() +
    labs(x = "Number of shareholders", y="Shareholder type", tag = "C",
         caption = "Data source: Orbis") +
    theme_light(base_size = 6)

d <- shrs |> 
    ggplot(aes(ownership_total)) +
    geom_density() +
    labs(x = "Total ownership (%)", y = "Density", tag = "D") +
    #scale_x_continuous(labels = scales::label_percent()) +
    theme_light(base_size = 6)

e <- shrs |> 
    group_by(company) |> 
    summarize(n = n()) |> 
    ggplot(aes(n)) + 
    geom_bar() +
    scale_y_log10() +
    labs(x = 'Number of shareholders', y = "Companies", tag = "E") +
    theme_light(base_size = 6)

## f is computed below after creating the network

ggsave(
    filename = "fig2_descriptive_stats.png", 
    plot = (a+b+c) /(d+e+f),
    device = "png", path = "paper/figures/", width = 6.5, height = 4,
    dpi = 500, bg = "white"
)




# Boats in top companies

aa <- df_boats |> 
    filter(!is.na(reg_owner)) |> 
    group_by(reg_owner, flag) |> 
    add_tally(name = "boats") |> ungroup() |> 
    select(flag, reg_owner, boats) |> unique() |> 
    slice_max(boats, n = 25) |>
    # make pretty labels
    mutate(reg_owner = str_remove_all(reg_owner, pattern = "Company|Limited|Proprietary"),
           reg_owner = str_remove(reg_owner, "OOO Rybokombinat \\'Ostrovnoy\\' "),
           reg_owner = str_remove(reg_owner, "\\(OOO 'Antey'\\)"),
           reg_owner = str_remove(reg_owner, "PAO Murmanskiy Tralovyi Flot \\("),
           reg_owner = str_remove(reg_owner, "\\(ZAO 'Sakhalinlizingflot'\\)"),
           reg_owner = str_remove_all(reg_owner, "\\(|\\)"),
           reg_owner = str_trim(reg_owner, "both")) |> 
    # print(n=25)
    mutate(reg_owner = fct_rev(as_factor(reg_owner))) |> 
    ggplot(aes(boats, reg_owner)) +
    geom_col(aes(fill = flag), alpha = 0.75) +
    scale_fill_brewer("Company country\nof origin",palette = "Paired",
                      guide = guide_legend(ncol = 2)) +
    labs(x = "Number of vessels", y = "Companies", tag = "A",
         caption = "Only companies with > 25 vessels shown.\n Data source: Lloyds") +
    theme_light(base_size = 6) + 
    theme(legend.position = c(0.65, 0.15), legend.key.size = unit(2, "mm"))

# Using colors from ColorBrewer: brewer.pal(10,"Paired")

bb <- shrs |> 
    mutate(name = case_when(
        name == "STATE-OWNED ASSETS SUPERVISION AND ADMINISTRATION COMMISSION OF THE STATE COUNCIL" ~
            "STATE-OWNED ASSETS SUPERVISION AND ADMINISTRATION\n COMMISSION OF THE STATE COUNCIL",
        name == "SHANGHAI STATE-OWNED ASSETS SUPERVISION AND ADMINISTRATION COMMISSION" ~
            "SHANGHAI STATE-OWNED ASSETS SUPERVISION\n AND ADMINISTRATION COMMISSION",
        .default = name
    )) |> 
    group_by(name, country, type) |> 
    summarize(n = n()) |> 
    arrange(desc(n)) |> 
    filter(n > 8) |> 
    ungroup() |> 
    mutate(name = str_to_title(name)) |> 
    mutate(name = str_remove(name, "\\(Domestic And Global Uo\\)")) |> 
    mutate(name = str_remove(name, "Co\\., Ltd\\.")) |>
    mutate(name = str_remove(name, "\\(Group\\)")) |> 
    mutate(
        name = as_factor(name) |> fct_rev(),
        shr_type = case_when(
            is.na(type) ~ "Missing",
            type == "C" ~ "Corporate\ncompanies",
            type == "S" ~ "Public authorities &\n Governments",
            type == "E" ~ "Mutual & Pension\n Funds "),
        country = case_when(
            country == "CN" ~ "China",
            country == "GB" ~ "United Kingdom",
            country == "TW" ~ "Chinese Tapei"
        )) |>
    ggplot(aes(n, name)) +
    geom_col(aes(fill = country, color = shr_type), alpha = 0.75) +
    scale_color_brewer(
        "Shareholder type", palette = "Set1"
        # values = c("#FF7F00", "#E31A1C", "#33A02C")
        ) +
    scale_fill_manual("Shareholder country\nof origin", values = c("#1F78B4", "#ffff99",  "#6A3D9A")) +
    labs(x = "Number of companies", y = "Top shareholders", tag = "B") +
    theme_light(base_size = 6) + 
    theme(legend.position = c(0.7, 0.45), legend.key.size = unit(2, "mm"),
          legend.text = element_text(size = 5))


aa + bb 

ggsave(
    filename = "fig3_top_actors.png", 
    plot = aa+bb ,
    device = "png", path = "paper/figures/", width = 7, height = 3,
    dpi = 500, bg = "white"
)

guos$guo |> head()


#### Network: fig 3 ####



sum(shrs$reg_owner |> unique() %in% df_boats$reg_owner ) # 452 matches / 855

df_boats <- df_boats |> 
    mutate(short_name = str_remove_all(
        reg_owner, 
        pattern =  "S.A.|Company|Limited|LLC|H/f|AS|Incorporated|Inc|LTD|LTDA|IMP|EXP|LLC|S\\.L\\.|YK$|\\.\\,|\\.\\, \\.|\\,$|\\, \\.$")) |> 
    mutate(short_name = str_trim(short_name, "both"))

sum(shrs$reg_owner |> unique() %in% df_boats$short_name) # 441


net <-  shrs |> 
    filter(!is.na(name), !is.na(country)) |> 
    select(country, name, reg_owner) |> #skimr::skim() # no nas
    left_join(
        df_names_all |> 
            filter(flag != "Unknown") |> #remove one unknown flag
            mutate(flag = str_remove_all(flag, " \\(Int. Register\\)")) |> 
            mutate(flag = case_when(flag == "South Korea" ~ "Dem. People's Rep. Korea", .default = flag)) ,
        relationship = "many-to-many"
    ) |> mutate(flag= case_when(is.na(flag) ~ "Reunion", .default = flag) ) |> # correcting Peche Avenir, company from Reunion.
    group_by(country, flag) |> 
    summarize(n = n()) |> #print(n=300)
    filter(country != "n.a.") |> # remove missing iso2 codes: we don't know where the shareholder are from
    left_join(countries |> 
              select(short_name, x2_alpha_code), 
          by = c("country"= "x2_alpha_code")) |> #print(n = 300)
    mutate(short_name = case_when(
        country == "TW" ~ "Taiwan, China",
        TRUE ~ short_name)) |> 
    mutate(flag = case_when(
        flag == "U.K." ~ "United Kingdom",
        flag == "Republic of Ireland" ~ "Ireland",
        flag == "U.S.A." ~ "United States",
        TRUE ~ flag),
        short_name = case_when(
            short_name == "South Korea" ~ "Dem. People's Rep. Korea",
            short_name == "Curacao"~"Curaçao", 
            TRUE ~ short_name
        )) |> #print(n=300)
    ungroup() |> 
    select(from = short_name, to = flag, n) |># skimr::skim()
    #filter( is.na(to)) |>#$print(n = 300)
    network(directed = TRUE, bipartite = FALSE, matrix.type = "edgelist", 
            ignore.eval=FALSE, loops = TRUE)

network.vertex.names(net)[!network.vertex.names(net) %in% countries$short_name]

plot(
    net,
    edge.lwd = 0.001, edge.col = alpha("gray75", 0.75),
    vertex.col = "orange", vertex.border = 0, vertex.cex = 2)

df_stats <- tibble(
    country = network.vertex.names(net),
    indegree = sna::degree(net, gmode = "digraph", cmode = "indegree"),
    outdegree = sna::degree(net, gmode = "digraph", cmode = "outdegree")
) 

df_stats <- df_stats |> 
    left_join(select(countries, country = short_name, iso2 = x2_alpha_code)) |> 
    mutate(iso2 = case_when(country == "Taiwan, China" ~ "TW", .default = iso2))

df_stats |> filter(is.na(iso2))
# 
# library(gplots)
# heatmap.2(
#     as.sociomatrix(net, "n") |> log1p(),
#     trace = 'none'
#     )

w <- as.sociomatrix(net, "n") 
diag(w) <- 0

net %e% "w" <- w
net %v% "loops" <- diag(as.sociomatrix(net, "n") )
net %v% "label" <-  df_stats |> 
    mutate(iso2 = case_when( # remove missing values, add a code to add in the legend figure.
        country == "Curacao" ~ "1",
        country == "Cook Islands" ~ "2",
        country == "Falkland Islands" ~ "3",
        country == "French Southern Territories" ~ "4",
        country == "Mayotte" ~ "5",
        country == "Reunion" ~ "6",
        country == "St. Pierre & Miquelon" ~ "7",
        country == "St. Helena" ~ "8",
        country == "Azores" ~ "9", .default = iso2
    )) |> 
    pull(iso2)


library(scico)


f4 <- ggplot(ggnetwork::ggnetwork(net, arrow.gap = 0.015) ,
             aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(aes(color = w), linewidth = 0.1,
               arrow = arrow(length = unit(2, "pt"), type = "closed")) +
    scale_color_scico(
        name = "Number of foreign investments [links]", 
        palette = "berlin") +
    new_scale_color() +
    geom_nodes(aes(colour = loops), size = 2.5, alpha = 1) +
    scale_colour_scico(name = "Number of national investments [nodes]", palette = "roma" ) +
    geom_text(aes(label = label), size = 1.5, color = "white") +
    theme_void(base_size = 6) +  labs(tag = "A") +
    theme(legend.position = "bottom", legend.key.height = unit(2, "mm"),
          legend.key.width = unit(8, "mm"), legend.title.position = "top")

f4

df_stats$loops <- net %v% "loops"

f <- df_stats |> 
    mutate(label = case_when(
        indegree > 5 | outdegree > 5 ~ country
    )) |> 
    ## name change by JB request
    mutate(label = case_when(label == "Taiwan, China" ~ "Chinese Tapei", .default = label)) |> 
    ggplot(aes(indegree, outdegree)) +
    geom_point(aes(size = loops), alpha = 0.5) +
    geom_abline(slope = 1, color = "orange", alpha = 0.5) +
    ggrepel::geom_text_repel(aes(label = label), size = 2) +
    scale_size("Domestic",range = c(1,5), breaks = c(0, 100, 200)) +
    labs(x = "In-degree", y  = "Out-degree", tag = "F") +
    theme_light(base_size = 6) + 
    theme(legend.position = c(0.88,0.78), legend.key.size = unit(3,"mm"))
f

# ((a/b/c) | d) + plot_layout(widths = c(1,3))

w <- as.sociomatrix(net, "n") |> 
    as_tibble(rownames = "from") |> 
    pivot_longer(cols = Albania:last_col(), names_to = "to", values_to = "investments")

w |> filter(investments > 25) |>
    arrange(desc(from)) |> 
    mutate(from = as_factor(from)) |> 
    arrange(to) |> 
    mutate(to = as_factor(to)) |> 
    ggplot(aes(to, from)) +
    geom_tile(aes(fill = investments)) +
    scale_fill_viridis_c() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))


# ggsave(
#     filename = "fig3_network.png", plot = ((a/b/c) | d) + plot_layout(widths = c(1,3)),
#     device = "png", path = "paper/figures/", width = 7, height = 4.5,
#     dpi = 500, bg = "white"
# )





#### Fig 4: ergms ####
load("data/ergms_marine.Rda")

fig4 <- fits |> 
    bind_rows() |> 
    mutate(p_value = case_when(
        p.value < 0.05 ~ "p < 0.05",
        p.value > 0.05 & p.value <= 0.1 ~ "p < 0.1",
        p.value > 0.1 ~ "p > 0.1")) |> 
    mutate(term = case_when(
        term == "edges" ~ "Edges",
        term == "diff.t-h.corruption" ~ "Difference in corruption",
        term == "diff.t-h.gini" ~ "Difference in inequality (Gini)",
        term == "diff.t-h.gov_effectiveness" ~ "Difference in governance effectiveness",
        term == "diff.t-h.pol_stability" ~ "Difference in political stability",
        term == "diff.t-h.regulatory_qual" ~ "Difference in regulatory quality",
        term == "diff.t-h.rule_law" ~ "Difference in rule of law",
        term == "diff.t-h.voice_acc" ~ "Difference in voice accountability",
        term == "diff.t-h.GDP" ~ "Difference in GDP (2020)",
        term == "nodeicov.corruption" ~ "Corruption",
        term == "nodeicov.gov_effectiveness" ~ "Government effectiveness",
        term == "nodeicov.pol_stability" ~ "Political stability",
        term == "nodeicov.regulatory_qual" ~"Regulatory quality",
        term == "nodeicov.rule_law" ~ "Rule of law",
        term == "nodeicov.gini" ~ "Inequality (Gini)",
        term == "nodeicov.GDP" ~ "GDP per capita (2020)")) |> 
    mutate(model = as_factor(model)) |> 
    mutate(term = fct_relevel(term, "Edges")) |>
    mutate(term = fct_rev(term)) |> 
    ggplot(aes(estimate, term)) + 
    geom_vline(aes(xintercept = 0), color = "gray50", linetype = 2, size = 0.1) +
    geom_point(aes(color = p_value), size = 0.8) +
    geom_errorbarh(
        aes(xmin = estimate - std.error, xmax = estimate + std.error, color = p_value),
        height = 0.5, size = 0.15) + 
    #geom_text(aes( label = logLik), data = df_stats) +
    scale_color_brewer(
        "p value", palette = "Dark2",
        guide = guide_legend(title.position = "top", keywidth = unit(2,"mm"), 
                             keyheight = unit(2, "mm"))) +
    facet_wrap(~model, ncol = 5) +
    labs(x = "Estimate", y = "Model term", tag = "B") +
    theme_light(base_size = 6) +
    theme(legend.position = 'bottom')

fig4

f4 + fig4

ggsave(
    filename = "fig4_network-ergm.png", plot = f4 + fig4 + plot_layout(widths = c(2,1)),
    device = "png", path = "paper/figures/", width = 7, height = 3,
    dpi = 500, bg = "white"
)

#### Supplementary figures ####

sm1 <- df_boats |> 
    mutate(lli_vessel_type = fct_rev(as_factor(lli_vessel_type))) |> 
    ggplot(aes(lli_vessel_type)) +
    geom_bar() + scale_y_log10() + 
    coord_flip() + 
    labs(x = "Vessel type", caption = "Data source: Lloyds") +
    theme_light(base_size = 6)

ggsave(
    filename = "sm1_GUOs.png", plot = sm1 ,
    device = "png", path = "paper/figures/", width = 3.5, height = 2.5,
    dpi = 500, bg = "white"
) 

   
## networks
## 
bip <- shrs |> 
    left_join(guos |> select(orbis_name, company) |> unique() ) |> 
    select(name, orbis_name, ownership_total) |> 
    filter(!is.na(orbis_name)) |> 
    filter(name != orbis_name) |> 
    mutate(ownership_total = case_when(
        is.na(ownership_total) ~ -10, 
        .default = ownership_total)) |> 
    group_by(name, orbis_name) |> 
    #add_tally() |> arrange(desc(n))
    summarize(mean_ownership = mean(ownership_total, na.rm = TRUE)) |> 
    unique() |> 
    #slice(-338) |>  # problems: the name is the IMO and the shareholder name the same
    network(directed = FALSE, bipartite = FALSE, matrix.type = "edgelist", 
            ignore.eval=FALSE, loops = FALSE)

plot(bip)

mat <- bip |> as.sociomatrix() #|> dim()

one <- mat %*% t(mat)
shr_net <- network(one, directed = FALSE)
shr_net %e% "w" <- one
shr_net %v% "outdegree" <- sna::degree(shr_net, gmode = "digraph", cmode = "outdegree")

plot(shr_net)


c <- ggplot(ggnetwork::ggnetwork(shr_net, arrow.gap = 0.005) ,
            aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(aes(color = w), size = 0.15, alpha = .5
               #arrow = arrow(length = unit(2, "pt"), type = "closed")
    ) +
    geom_nodes( color = "orange", alpha = 0.4) +
    #geom_nodes(aes(size = betw), color = "blue", alpha = 0.4) +
    #geom_nodetext(aes(label = vertex.names, size = degree/4)) +
    # scico::scale_color_scico(
    #     "Channel", palette = "batlow", direction = -1,
    #     guide = guide_colorbar(
    #         barwidth = unit(3, "mm"), barheight = unit(1.5, "cm"))) +
    scale_size(name = "Outdegree", range = c(0.2,4)) +
    labs(title = "Network of shareholders",
         subtitle = "Shareholders are connected if they have shares on the same fishing companies", 
         caption = "Data source: Orbis") + 
    scale_color_continuous(
        "Companies", breaks = c(1,2,3), 
        guide = guide_colorbar(barwidth = unit(2,"cm"), barheight = unit(2, "mm")))+
    theme_blank(base_size = 6) +
    theme(legend.position = "bottom")

#### GUOs ####

guos <- guos |>
    mutate(class = case_when(
        str_detect(guo, "Public") ~ "Public",
        str_detect(guo, "Private") ~ "Private",
        str_detect(guo, "Formerly") ~ "Former public",
        .default = guo
    )) |> # pull(class) |> table()
    mutate(
        ## remove all the cases
        guo = str_remove(guo, "Private The Global Ultimate Owner of this controlled subsidiary is "), 
        guo = str_remove(guo, "Formerly publicly quoted The Global Ultimate Owner of this controlled subsidiary is "),
        guo = str_remove(guo, "Private This entity is a marine vessel of "),
        guo = str_remove(guo, "Publicly quoted The Global Ultimate Owner of this controlled subsidiary is "),
        guo = str_remove(guo, "Private This entity was a branch of "),
        guo = str_remove(guo, "Private This entity is a branch of "), 
        guo = str_remove(guo, "Private This entity is a foreign company of "),
    ) |> #pull(guo) |> unique() #|> 
    mutate(
        guo = case_when(
            guo =="Private This entity is a marine vessel." ~ NA,
            guo == "Private The Global Ultimate Owner of this company could not be identified."  ~ NA,
            guo == "Private This entity is a branch." ~ NA,
            guo == "Publicly quoted This company is Independent (but not the Global Ultimate Owner of a Corporate Group)"  ~ NA,
            guo == "Publicly quoted This company is a Single location" ~ NA, 
            guo == "Private" ~ NA,
            guo == "Private This entity is a foreign company." ~ NA,
            guo == "Private This company is Independent (but not the Global Ultimate Owner of a Corporate Group)" ~NA,
            guo == "Publicly quoted This company is the Global Ultimate Owner of the corporate group"  ~ orbis_name,
            guo == "Private This company is the Global Ultimate Owner of the corporate group"  ~ orbis_name,
            guo == "Formerly publicly quoted This company is the Global Ultimate Owner of the corporate group" ~ orbis_name,
            guo == "Private This company is a Single location" ~ NA,
            .default = guo
        )
    )  |> 
    mutate(person = str_starts(guo, pattern = "MR|MRS|MS")
    )

guos <- guos |> 
    mutate(coma = stringi::stri_locate_first(location, regex = ","), end = str_length(location)) |> 
    mutate(country = str_sub(location, start = coma[,1]+2, end = end)) |> 
    mutate(country = case_when(is.na(country) ~ location, .default = country)) |> 
    select(-coma, -end) 
    


b <- guos |> 
    ggplot(aes(class)) +
    geom_bar() +
    coord_flip() +
    labs(x = "GUO type", y = "", tag = "B" ) +
    theme_light(base_size = 6)

c <- guos |> 
    ggplot(aes(person)) + 
    geom_bar() +
    coord_flip() +
    labs(x = "GUO is a person?", y = "", tag = "C") +
    theme_light(base_size = 6)

a <- guos |> 
    filter(!is.na(guo), person == FALSE) |> 
    group_by(guo) |> 
    summarize(n = n()) |>
    arrange(n) |> 
    slice_tail(n = 25) |>
    mutate(guo = str_to_title(guo)) |> 
    # left_join( # I believe this creates errors
    #     guos |> select(guo, country) |> unique()
    # ) |> 
    mutate(guo = as_factor(guo)) |> 
    ggplot(aes(n, guo)) +
    geom_col() +
    labs(x = "Number of companies", y = "Global Ultimate Owners", tag = "A") +
    theme_light(base_size = 6)

a|(b/c) 

ggsave(
    filename = "sm_guos.png", plot = a|(b/c)  ,
    device = "png", path = "paper/figures/", width = 5, height = 3,
    dpi = 500, bg = "white"
)


##### ergms #####
load("data/ergms_marine.Rda")

df_stats2 |> 
    pivot_longer(cols = logLik:AIC, names_to = "stat",
                 values_to = "values") |> 
    mutate(model = as_factor(model) ) |> 
    ggplot(aes(values, model)) + 
    geom_col() +
    facet_wrap(~stat, scales = "free_x")

df_stats2


#### Revenues ####
df_fig <- df_boats |> 
    filter(!is.na(reg_owner)) |> 
    left_join(revs, by = join_by(reg_owner == company), relationship = "many-to-many") |> 
    select(flag, reg_owner, revenue_last, employees) |> 
    unique() |> 
    #group_by(reg_owner) |> add_count() |> arrange(desc(n)) |> print(n=100)
    mutate(revenue_last = str_remove_all(revenue_last, ","),
           employees = str_remove_all(employees, ","), 
           employees = case_when(employees == "n.a." ~ NA, .default = employees),
           revenue_last = case_when(revenue_last == "n.a." ~ NA, .default = revenue_last)) |> 
    mutate(revenue_last = as.numeric(revenue_last), employees = as.numeric(employees)) 
    #skimr::skim() |> 
df_fig |> pull(revenue_last) |> sum(na.rm = TRUE) # for writing > 6.6 billion
df_fig |> pull(employees) |> sum(na.rm = TRUE) # for writing > 100k jobs
df_fig |> skimr::skim()

a <- df_fig |> 
    group_by(flag) |> 
    summarize(revenues = sum(revenue_last, na.rm = TRUE)) |> 
    arrange(revenues) |> 
    filter(revenues > 10e6) |> 
    mutate(flag = as_factor(flag)) |> 
    ggplot(aes(revenues, flag)) +
    geom_col() +
    labs(x = "Revenues", y = "Vessel flag", tag = "A") +
    theme_light(base_size = 6)

b <- df_fig |> 
    group_by(flag) |> 
    summarize(employees = sum(employees, na.rm = TRUE)) |> 
    arrange(employees) |> 
    filter(employees > 500) |> 
    mutate(flag = as_factor(flag)) |> 
    ggplot(aes(employees, flag)) +
    geom_col() +
    labs(x = "Employees", y = "Vessel flag", tag = "B") +
    theme_light(base_size = 6)

a+b

ggsave(
    filename = "fig5_impacts.png", plot = a+b ,
    device = "png", path = "paper/figures/", width = 6, height = 2.5,
    dpi = 500, bg = "white"
)