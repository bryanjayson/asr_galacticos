library(readxl)
library(plm)
library(xtable)
library(ggplot2)
library(dplyr)
library(pgmm)
library(lme4)
library(stargazer)
library(sandwich)
library(car)
library(lmtest)
library(fixest)

#loading dataset
data_raw = read_excel("C:/Users/Bryan/SynologyDrive/Universität/Master/24 FS/Applied Sports Research/python/data/engineered/galacticos_dataset.xlsx")

# defining datatypes
data_raw$season = as.factor(data_raw$season)
data_raw$team = as.factor(data_raw$team)
data_raw$elo_season_rank = as.factor(data_raw$elo_season_rank)
data_raw$league = as.factor(data_raw$league)
data_raw$promoted = as.factor(data_raw$promoted)
data_raw$cup_won = as.factor(data_raw$cup_won)
data_raw$overall = as.numeric(data_raw$overall)
data_raw$high_value_transfers = as.factor(data_raw$high_value_transfers)

# handle outliers
data_raw = data_raw[data_raw$net_spend < 500, ]

data = data_raw[c("team", "season", "league", "PPG", "elo", "LOR", "league_normalized_net_spend", "league_normalized_lagged_team_value", "league_normalized_team_value", 'high_value_transfers', "out", "promoted")]
data = data %>%
  rename(NNS = league_normalized_net_spend,
         LNTV = league_normalized_lagged_team_value,
         NTV = league_normalized_team_value,
         galacticos = high_value_transfers)

data_balanced = na.omit(data)

# checking correlation; --> EDA and corplot in plots.R script
corr = cor(data[c("PPG", "elo", "LOR", "NNS", "LNTV", 'galacticos', "out")], use ="complete.obs")
print(corr)

#LM: PPG
lm_PPG_base = lm(PPG ~ NNS + LNTV + galacticos, data = data)
lm_PPG_comp = lm(PPG ~ NNS + LNTV + galacticos + promoted + out, data = data)
lm_PPG_inter = lm(PPG ~ NNS + LNTV + galacticos + out + promoted + NNS:promoted, data = data)

summary(lm_PPG_base)
summary(lm_PPG_comp)
summary(lm_PPG_inter)

plot(lm_PPG_inter) # diagnostics look good
vif(lm_PPG_inter) # VIF no larger than 2.2 for all variables -> no multicollinearity concerns

#LM: elo
lm_elo_inter = lm(elo ~ NNS + LNTV + galacticos + out + promoted + NNS:promoted, data = data)

summary(lm_elo_inter)
plot(lm_elo_inter) # okay
vif(lm_elo_inter) # okay

#LM: LOR

lm_LOR_inter = lm(LOR ~ NNS + LNTV + galacticos + out + promoted + NNS:promoted, data = data)

summary(lm_LOR_inter)
plot(lm_LOR_inter)
vif(lm_LOR_inter) 

### panel methods ###

panel_data = pdata.frame(data)

# using only teams that have > 3 observations; NOTE: not used in final paper 
panel_filtered = panel_data %>%
  group_by(team) %>%
  filter(n() >= 6) %>%
  ungroup()

### NOTE: between models were not used in paper --> no added benefit to FE model, also not used in similar research

#Between: PPG 
plm_PPG_be = plm(PPG ~ NNS + LNTV + galacticos + out, data = panel_filtered, model = "between")
summary(plm_PPG_be)

qqnorm(plm_PPG_be$residuals)
qqline(plm_PPG_be$residuals) # some influential observations, but model fit is okay

vif(plm_PPG_be) # higher, but still < 10

#Between: elo 
plm_elo_be = plm(elo ~ NNS + LNTV + galacticos + out, data = panel_filtered, model = "between")
summary(plm_elo_be)

qqnorm(plm_elo_be$residuals)
qqline(plm_elo_be$residuals) # very heavy tails

hist(plm_elo_be$residuals) # okay

vif(plm_elo_be) # higher, but still < 10

#Between: LOR
plm_LOR_be = plm(LOR ~ NNS + LNTV + galacticos + out, data = panel_filtered, model = "between")
summary(plm_LOR_be)

qqnorm(plm_LOR_be$residuals)
qqline(plm_LOR_be$residuals) # some influential observations

vif(plm_LOR_be) # higher, but still < 10

### FE methods ###

#FE: PPG
PPG_inter_fe = feols(PPG ~ NNS + LNTV + galacticos + out + promoted + NNS:promoted | team + season, data = data)
summary(PPG_inter_fe)

plot(PPG_inter_fe$residuals) # good

#FE: elo
elo_inter_fe = feols(elo ~ NNS + LNTV + galacticos + out + promoted + NNS:promoted | team + season, data = data)
summary(elo_inter_fe)

plot(elo_inter_fe$residuals)

#FE: LOR
LOR_inter_fe = feols(LOR ~ NNS + LNTV + galacticos + out + promoted + NNS:promoted | team + season, data = data)
summary(LOR_inter_fe)

plot(LOR_inter_fe$residuals)

#### summarizing for latex ####

library(modelsummary)

# LM Models
lm_models = list("Base PPG" = lm(PPG ~ NNS + LNTV + galacticos, data = data),
              "Comp PPG" = lm(PPG ~ NNS + LNTV + galacticos + out + promoted, data = data),
              "Inter PPG" = lm(PPG ~ NNS + LNTV + galacticos + out + promoted + NNS:promoted, data = data))

modelsummary(lm_models,
             output = "latex",
             gof_omit = "AIC|BIC|Log|RMSE|Num.Obs",
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             title = "OLS Summary"
             )

BE_models = list("PPG" = plm_PPG_be,
          "Elo" = plm_elo_be,
          "LOR" = plm_LOR_be)

modelsummary(BE_models,
             output = "latex",
             gof_omit = "AIC|BIC|Log|RMSE|Num.Obs",
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             title = "Between Estimation Summary"
)

FE_models = list("PPG" = PPG_inter_fe,
                             "Elo" = elo_inter_fe,
                             "LOR" = LOR_inter_fe)

modelsummary(FE_models,
             output = "latex",
             gof_omit = "AIC|BIC|Log|RMSE|Num.Obs",
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             title = "FE Estimation Summary"
)

all_models = list(
                  "OLS PPG" = lm(PPG ~ NNS + LNTV + galacticos + out + promoted + NNS:promoted, data = data),
                  "OLS Elo" = lm(elo ~ NNS + LNTV + galacticos + out + promoted + NNS:promoted, data = data),
                  "OLS LOR" = lm(LOR ~ NNS + LNTV + galacticos + out + promoted + NNS:promoted, data = data),
                  "FE PPG" = PPG_inter_fe,
                  "FE Elo" = elo_inter_fe,
                  "FE LOR" = LOR_inter_fe)

modelsummary(all_models,
             output = "latex",
             gof_omit = "AIC|BIC|Log|RMSE|Num.Obs",
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             title = "Summary of OLS and FE regression models with SMs as dependent variables.")
