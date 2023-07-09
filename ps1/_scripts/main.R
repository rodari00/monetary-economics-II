# Federico Rodari 2022
# ECON8862 COMPUSTAT CLEANING

### -- Initialize Session ---------------------------

suppressMessages(library('tidyverse'))
suppressMessages(library('zoo'))
suppressMessages(library(hrbrthemes))
suppressMessages(library(viridis))
suppressMessages(library(kableExtra))




### -- Setup Directories  ------------------------------------------------------------------

root <- getwd()
script_name <- "cleaning"

# Results
results.dir <- paste0(root,"/_results")


# figures (child)
figures.dir <- paste0(results.dir,
                      "/figures")

dir.create(figures.dir,
           showWarnings = FALSE, recursive = TRUE)

# data (child)
data.dir <- paste0(results.dir,
                   "/data")

dir.create(data.dir,
           showWarnings = FALSE, recursive = TRUE)


digits <- 2


# Load functions
message('\nLoading functions...')

funcs <- list.files(path = paste0(getwd(),"/functions"))

for (f in 1:length(funcs)) {
  
  source(paste0(getwd(), "/functions/", funcs[f]))
  
}

rm(funcs)


# Load Data
names_df <- list.files(path = paste0(getwd(),"/_data"))

# NBER CES
df_nber <- read_csv(paste0("_data/",names_df[2]))
cols_nber <- colnames(df_nber)

# Compustat
df_cstat <- read_csv(paste0("_data/",names_df[1]))
cols_cstat <- colnames(df_cstat)



### -- (1.1)  ------------------------------------------------------------------------------

df_nber %>%
  select(sic,year,pay,vadd,vship,piship,piinv) %>%
  arrange(sic,year) %>%
  group_by(sic) %>%
  mutate(
         # Labor Share (10-year rolling window)
         labshare = rollmean(x = pay/vadd, 10, align = "right", fill = NA),
         # Capital Share
         capshare = 1 - labshare,
         # Value Added to Gross Output
         vaddfrac = rollmean(x = vadd/vship , 10, align = "right", fill = NA),
         sic = as.numeric(sic)) %>%
  select(sic,year,labshare,capshare,vaddfrac,vadd,vship,piship,piinv) %>%
  na.omit() %>%
  ungroup() -> nber_ces

summary(nber_ces)

### -- (1.2) ------------------------------------------------------------------------------

df_cstat %>%
  mutate(sic = as.numeric(sic)) %>%
  filter(
         # Select only USA
         fic == "USA" & 
         # Select final version of statements   
         final == "Y" &
         # Remove regulated utilities and financial companies
         ((sic < 4900 | sic >= 5000) &
         (sic < 6000 | sic >= 7000)) &
         # Drop extreme acquisitions
         aqc <= 0.05*at) %>%
  arrange(gvkey,fyear) %>%
  group_by(gvkey) %>%
  # Shift forward capital by one year
  mutate(ppent = lead(ppent)) %>%
  # Filter nonmissing elements and cap duration
  filter(!is.na(at)    &
         !is.na(ppent) &
         !is.na(emp)   &
         !is.na(capxv) &
         !is.na(sale)) %>%
  # Filter number of years >= 2
  mutate(nyears = n_distinct(fyear)) %>%
  filter(nyears >= 2) %>%
  # Rename year to merge with NBER-CES data
  rename(year =fyear) %>%
  inner_join(.,
             nber_ces,
             by = c("sic","year")) -> df

remove(df_cstat,nber_ces,df_nber)

### -- (1.3) ------------------------------------------------------------------------------

df %>%
  arrange(gvkey,datadate) %>%
  group_by(gvkey) %>%
  mutate(across(c(oibdp,intpn, txt, nopi, dp, spioa),
                ~ifelse( is.na(.x) == 1 , 0,.x))) %>%
  mutate(investment = capxv/piinv,
         capital = ppent/piinv,
         irate = investment/capital,
         outputva = (sale*vaddfrac)/piship,
         outputgrowth = ((outputva - lag(outputva))/(outputva + lag(outputva)))*2,
         empgrowth = ((emp - lag(emp))/(emp + lag(emp)))*2,
         tfpr = log(vadd) - capshare*log(capital) - labshare*log(emp),
         tfprgrowth = tfpr - lag(tfpr),
         oibdprate = oibdp/ppent,
         cashflowrate = (oibdp - intpn - txt + nopi + dp + spioa)/at, 
         output = sale/piship,
         output_cap_r = output/capital) %>%
  filter(nyears >= 5 & row_number() > 1) %>%
  # Resolve Inf/-Inf (e.g. irate when divided by capital = 0)
  mutate(across(c(investment,capital,irate,
                  capxv,ppent,piinv,outputva,outputgrowth,
                  sale,vaddfrac,piship,emp,empgrowth,tfpr,
                  tfprgrowth,oibdprate,oibdp,cashflowrate,
                  xint,txt,nopi,at,spioa,sale,piship,output_cap_r),
                ~ifelse( .x == Inf, NA,
                        ifelse(.x == -Inf, NA, .x)))) -> df_tmp
# 
# # Subselection
# df_tmp %>%
# select(gvkey,year,nyears,investment,capital,irate,capxv,ppent,piinv,outputva,outputgrowth,sale,vaddfrac,piship,
#        emp,empgrowth,tfpr,tfprgrowth,oibdprate,oibdp,cashflowrate,xint,txt,nopi,
#        at,spioa,sale,piship,output_cap_r) -> df_tmp_sub

# Winsorize specific variables
df_tmp %>%
  ungroup() %>%
  mutate(across(c(empgrowth,tfprgrowth,outputgrowth,oibdprate,cashflowrate),
                ~(winsorize(.)),.names = "{.col}_w")) %>%
  mutate(across(c(irate),
                ~(winsorize_top(.)),.names = "{.col}_w")) ->  df_winsorized

df_winsorized %>%
  #select(order(colnames(df_winsorized)))%>%
  select(gvkey,year,everything()) -> df_winsorized

# Plot winsorized variables
df_winsorized %>%
  select(gvkey,year, ends_with('_w')) %>%
gather(variable,value,-c(gvkey,year)) %>%
arrange(gvkey,year) %>%
  ggplot(aes(x=value, color=variable, fill=variable)) +
  geom_histogram(alpha=0.6) +
  facet_wrap(~variable,scales = "free_x") +  
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("Variable Name") +
  ylab("Count") 

ggsave("wins_distr1.pdf", path = figures.dir,
       width = 10,
       height = 5)  

# Summary Statistics 
df_winsorized %>%
  ungroup() %>%
summarise(across(c(investment,capital,irate,outputva,outputgrowth,
                          emp,empgrowth,tfpr,tfprgrowth,oibdprate,
                          cashflowrate,output_cap_r,ends_with('_w')), 
                 ~ mean(.x, na.rm = TRUE))) %>%
gather(varname,avg) %>%
arrange(varname) -> summary_mean

df_winsorized %>%
  ungroup() %>%
  summarise(across(c(investment,capital,irate,outputva,outputgrowth,
                     emp,empgrowth,tfpr,tfprgrowth,oibdprate,
                     cashflowrate,output_cap_r,ends_with('_w')), 
                   ~ sd(.x, na.rm = TRUE))) %>%
  gather(varname,sd) %>%
  arrange(varname) -> summary_sd


# Save table
summary_mean %>%
  inner_join(summary_sd,by = 'varname') %>%
  kbl(caption="Summary Statistics of Main Variables in the Data",
      format="latex",
      col.names = c("Variable Name",'Mean','St. Dev.'),
      align="l",
      digits = 3,
      booktabs = TRUE) %>%
  kable_minimal(full_width = F) -> tbl_summary
  
  writeLines(tbl_summary, paste0(data.dir,'/tbl_summary.tex'))
  
  
  # Save dataset
  write.csv(df_winsorized, file=paste0(data.dir,'/data_winsorized.csv'),
            row.names=FALSE,na="")
  

  
  
  


