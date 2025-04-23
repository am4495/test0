library(readxl)
data_evaluation <- read_excel('C:/Users/LG GRAM/Documents/evaluation_test/evaluation test.xlsx')

colnames(data_evaluation) #column names in excel

library(dplyr)

data_evaluation <- data_evaluation %>%
  mutate(
    Safety_Stock = 10, #set safety stock to 10 
    Daily_Usage = Inventory_Level/Lead_Time) # calculating dailty usage

ROP_data_evaluation <- data_evaluation %>%
    group_by(Product_ID, Category, Warehouse_Location) %>%
    summarise(
      Avg_Daily_Usage = mean(Daily_Usage, na.rm = TRUE),
      Avg_Lead_Time = mean(Lead_Time, na.rm = TRUE),
      Safety_Stock = mean(Safety_Stock, na.rm = TRUE)
    ) %>%
  mutate(ROP = (Avg_Daily_Usage * Avg_Lead_Time) + Safety_Stock)
print(ROP_data_evaluation)
View(ROP_data_evaluation)

#Top 5 late suppliers

top_5_late_suppliers <- data_evaluation %>%
  group_by(Supplier) %>%
  summarise(
    Total_Orders = n(),
    Late_Orders = sum(Is_Late == "LATE"),
    Late_Delivery_Rate = round((Late_Orders / Total_Orders) * 100)
  ) %>%
  arrange(desc(Late_Delivery_Rate)) %>%
  slice_head(n = 5)

print(top_5_late_suppliers)


