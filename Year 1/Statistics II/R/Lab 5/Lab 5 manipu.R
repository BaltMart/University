################################
### Lab 5; Data manipulation
### 2024-04-19
################################

# install packages:
install.packages("openxlsx")

# load package
library(openxlsx)

# Load data
setwd("C:/Users/martynas/Desktop/uni/Statistika")

bank_data_full <- read.xlsx("Banking_sector_main_indicators_24Q4.xlsx", #file name
                            sheet = "Q4 2023", # sheet name
                            sep.names = " ",
                            rows = c(3:30)) # rows

################################
# Data manipulation with dplyr package
################################

install.packages("dplyr")

library(dplyr)

# select()

bank_swed_seb <- bank_data_full %>% 
  select(Item.of.balance.sheet, `Swedbank,.AB`, AB.SEB.bankas)

bank_data_full <- bank_data_full %>% select(-"X18")

bank_level_data <- bank_data_full %>% 
  select(- c(Banking.sector, starts_with("Total")))

# filter() ----------------------------
total_assets <- bank_level_data %>%
  filter(Item.of.balance.sheet == "Total assets")

# mutate() -----------------------------

share_swed_seb = bank_data_full %>% 
  mutate(Market.share = (`Swedbank,.AB` + AB.SEB.bankas)/Banking.sector)


# mutate(), filter(), select()
market_share_2banks = bank_data_full %>%
  mutate(Market.share = (`Swedbank,.AB` + AB.SEB.bankas)/Banking.sector) %>%
  filter(Item.of.balance.sheet == "Total assets") %>%
  select(Market.share)

###################################
## Pivoting
###################################
install.packages("tidyr")
library(tidyr)

data_wide0 = bank_level_data

# from wide to long format
data_long = data_wide0 %>%
  pivot_longer(cols = c(- Item.of.balance.sheet),
               names_to = "Bank.names") %>%
        rename(Item = Item.of.balance.sheet)

# from long to wide
data_wide1 = data_long %>%
  pivot_wider(names_from = "Item",
              values_from = "value")

data_long %>%
  group_by(Item) %>%
  summarise(total = sum(value)) %>%
  ungroup()

data_long %>%
  filter(Item == "Total assets",
         value > 10000000) %>%
  arrange(value) %>%
  arrange(desc(value)) %>%
  filter(value == max(value))

#####################################
## ggplot2
#####################################
install.packages("ggplot2")

library(ggplot2)

data.subset = data_long %>%
  filter(Item %in% c("Total assets", "Total equity")) %>%
  pivot_wider(names_from = Item,
              values_from = value)

# Option 1:
ggplot(data.subset,
       aes(x = `Total assets`, y = `Total equity`)) +
  geom_point(color = "blue",
             size = 2,
             shape = 2)
# Option 2:
ggplot(data.subset) +
  geom_point(aes(x = `Total assets`, y = `Total equity`),
            color = "blue",
             size = 2,
             shape = 2)


ggplot(data = data.subset) +
  geom_col(aes(x = reorder(Bank.names, `Total assets`), y = `Total assets`/1000),
           fill = "red") +
  coord_flip() + 
  ylab("Eur. mln") +
  xlab("") +
  labs(title = "Total assets") +
  theme_bw()











