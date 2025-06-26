library(tidyverse)
library(ergm)

## Load datasets
load("data/cleaned_data_240321.Rda")
load("data/boats_owners.Rda")
## country lists
countries <- readr::read_csv(
    file = "~/Documents/Projects/DATA/WorldBank/SDG_csv/SDGCountry.csv") |> 
    janitor::clean_names()
## correct Namibia
countries[countries$short_name == "Namibia","x2_alpha_code"] <- "NA"


## Create network
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

w <- as.sociomatrix(net, "n") 
diag(w) <- 0

net %e% "w" <- w
net %v% "loops" <- diag(as.sociomatrix(net, "n") )
net %v% "label" <-  df_stats |> pull(iso2)


df_stats <- tibble(
    country = network.vertex.names(net),
    indegree = sna::degree(net, gmode = "digraph", cmode = "indegree"),
    outdegree = sna::degree(net, gmode = "digraph", cmode = "outdegree")
)

df_stats <- df_stats |> 
    left_join(select(countries, country = short_name, iso2 = x2_alpha_code))



### the network for modelling has 
net2 <- network(w)

## remove nodes without stats
## load the wgi
load("~/Documents/Projects/Financial actos and zoonotic disease/finance_tipping/data/governance_index_cleaned.RData")
df_wgi <- df_wgi |> 
    mutate(country_name = case_when(
        country_name == "Korea, Dem. People's Rep." ~ "Dem. People's Rep. Korea",
        country_name == "Korea, Rep." ~ "Korea",
        country_name == "Russian Federation" ~ "Russia",
        country_name == "Venezuela, RB" ~ "Venezuela",
        TRUE ~ country_name
    )) |> 
    pivot_wider(names_from = indicator_name, values_from = estimate) |> 
    filter(country_name %in% df_stats$country)

## adding GDP to the ergm by reviewer request
df_wdi <- read_csv("~/Documents/Projects/DATA/WorldBank/WDI_csv/WDIData.csv") |> 
    janitor::clean_names() |> 
    select(country_name, country_code, indicator_name, `x2020`) |> 
    filter(indicator_name == "GDP per capita, PPP (constant 2017 international $)") |> 
    rename(`GDP per capita` = x2020) |> 
    select(-country_code, -indicator_name) |> 
    mutate(country_name = case_when(
        country_name == "Korea, Dem. People's Rep." ~ "Dem. People's Rep. Korea",
        country_name == "Korea, Rep." ~ "Korea",
        country_name == "Russian Federation" ~ "Russia",
        country_name == "Venezuela, RB" ~ "Venezuela",
        TRUE ~ country_name
    )) |> 
    filter(country_name %in% df_stats$country)
    

c_name <- (df_wgi |> pull(country_name) |> unique())
c_name <- (df_wdi |> pull(country_name) |> unique())
df_stats$country[df_stats$country %in% c_name == FALSE] # 0 means all names are compatible!
# c_name[c_name %in% nodes == FALSE]

## load inequality data:
inq <- readxl::read_xlsx(
    path = "~/Documents/Projects/DATA/WIID_World_Income_Inequality_DB/WIID_06MAY2020.xlsx")

inq <- inq %>% 
    filter(!is.na(gini_reported), year >= 1990) %>% 
    select(country, isoa2 = c2, country_code = c3, year, gini_reported) %>% 
    group_by(country, year, isoa2, country_code) %>% 
    summarize(gini_mean = mean(gini_reported, na.rm = TRUE)) |> 
    ungroup() |> group_by(country, isoa2, country_code) |> 
    summarize(gini_mean = mean(gini_mean, na.rm = TRUE))

df_stats <- df_stats |> 
    left_join(df_wgi, by = c("country" = "country_name")) |> 
    left_join(df_wdi, by = c("country" = "country_name")) |> 
    left_join(inq, by = c("iso2" = "isoa2"))

df_stats |> select(country.x, country.y, `Control of Corruption`, gini_mean) |>  print(n=200) 

## reduced dataset with full stats
df_red <- df_stats |> 
    filter(!is.na(gini_mean), !is.na(`Control of Corruption`), !is.na(`GDP per capita`)) 

delete_countries <- which(!df_stats$country.x %in% df_red$country.x)

net2 <- delete.vertices(net2, delete_countries)

## add network attributes
net2 %v% "corruption" <- df_red$`Control of Corruption`
net2 %v% "gov_effectiveness" <- df_red$`Government Effectiveness`
net2 %v% "pol_stability" <- df_red$`Political Stability and Absence of Violence/Terrorism`
net2 %v% "regulatory_qual" <- df_red$`Regulatory Quality`
net2 %v% "rule_law" <- df_red$`Rule of Law`
net2 %v% "voice_acc" <- df_red$`Voice and Accountability`
net2 %v% "gini" <- df_red$gini_mean
net2 %v% "GDP" <- df_red$`GDP per capita`

## ERGM
## 
#### ergm: binary ####
# null model
f0 <- ergm(net2 ~ edges)
# governance model
tic()
f1 <- ergm(
    net2 ~ edges + diff("corruption") + diff("gini") +
        diff("gov_effectiveness") +  diff("pol_stability") +
        diff("regulatory_qual") + diff("rule_law") + diff("voice_acc") + diff("GDP"))
toc()
# governance + inequality
tic()
f2 <- ergm(
    net2 ~ edges + diff("corruption") + diff("gini") +
        diff("gov_effectiveness") +  diff("pol_stability") +
        diff("regulatory_qual") + diff("rule_law") + diff("voice_acc") + diff("GDP") +
        nodeicov("corruption") + nodeicov("gov_effectiveness") +
        nodeicov("pol_stability" ) +
        nodeicov("regulatory_qual" ) + nodeicov("rule_law") +
        nodeicov("gini") + nodeicov("GDP"))
toc()

# g+i+structure
# tic()
# f3 <- ergm(
#     net_countries ~ edges + diff("corruption") + diff("gini") +
#         diff("gov_effectiveness") +  diff("pol_stability") +
#         diff("regulatory_qual") + diff("rule_law") + diff("voice_acc") +
#         nodeicov("corruption") + nodeicov("gov_effectiveness") +
#         nodeicov("pol_stability" ) +
#         nodeicov("regulatory_qual" ) + nodeicov("rule_law") + 
#         nodeicov("gini") +  mutual + dgwnsp(type ="ISP") 
# )
# toc() # 91660s = 25.4hrs

fits <- list(f0, f1, f2)
fit_name <- c("null", "difference","full")

df_stats2 <- tibble(
    model = fit_name,
    case = names(fits),
    logLik = map_dbl(fits, logLik),
    AIC = map_dbl(fits, AIC)
)

#### structural models ####
#f5 <- ergm(net_countries ~ edges + triadcensus())

fits <- fits |> map(broom::tidy, exponentiate = FALSE) 
fits <- map2(fits, fit_name, function(x,y) {x$model <- y; return(x)})
#fits <- map2(fits, names(fits), function(x,y) {x$case <- y; return(x)})

save(fits, df_stats2, file = "data/ergms_marine.Rda")


