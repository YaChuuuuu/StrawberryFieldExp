#Install the required package
install.packages("dplyr")
install.packages("ggplot2")
install.packages("skimr")
install.packages("tidyr")
install.packages("readxl")

#Load the required library 
library(dplyr)
library(skimr)
library(tidyr)
library(readxl)

FieldExp109 <- read_excel("109草莓田間試驗.xlsx",sheet="「綜合數據」刪除異常值")
skim_without_charts(FieldExp109)
glimpse(FieldExp109)

#分組計算
FieldExp109Grouped <- group_by(FieldExp109,場域,時間,菌株)%>%
  summarize("冠_平均值"=mean(冠平均,na.rm = TRUE),"冠_標準差"=sd(冠平均,na.rm = TRUE),
            "葉綠素_平均值"=mean(葉綠素,na.rm = TRUE),"葉綠素_標準差"=sd(葉綠素,na.rm = TRUE),
            "葉面積_平均值"=mean(葉面積,na.rm = TRUE),"葉面積_標準差"=sd(葉面積,na.rm = TRUE))
FieldExp109Grouped_long <- FieldExp109Grouped %>%
  pivot_longer(
    cols = -c(場域, 時間, 菌株),
    names_to =c("測量值",".value"),
    names_sep = "_")
print(FieldExp109Grouped_long)

FieldExp109Disease <- group_by(FieldExp109,場域,時間,菌株)%>%
  summarize('病害嚴重度'=mean(病害指數,na.rm = TRUE)/5,
            "有病的植株數量"=sum(病害指數 != 0, na.rm = TRUE))
FieldExp109live <- FieldExp109 %>%
  group_by(場域, 時間, 菌株) %>%
  summarize(存活率 = sum(狀態 == "存活") / 30)

FieldExp109Grouped%>%arrange(-冠_平均值)
FieldExp109Grouped%>%arrange(-葉綠素_平均值)
FieldExp109Grouped%>%arrange(-葉面積_平均值)

#繪製bar
library(ggplot2)
library(dplyr)
library(tidyr)

ggplot(data = FieldExp109live,mapping = aes(x=菌株 , y = 存活率*100, fill = 菌株)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) + 
  labs(title="Survival rate of each treatment",x="",y="Survival rate (%)",fill = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_blank())+
  facet_grid(場域~時間)

ggplot(FieldExp109Grouped_long%>%filter(測量值=="葉綠素"), aes(x = 測量值, y = 平均值, fill = 菌株)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = 平均值 - 標準差, ymax = 平均值 + 標準差),
                position = position_dodge(0.8), width = 0.25) +
  labs(title="Chlorophyll content of each treatment",x="",y = "Chlorophyll content (SPAD)", fill = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_blank())+
  facet_grid(場域~時間)

ggplot(FieldExp109Grouped_long%>%filter(測量值=="葉面積"), aes(x = 測量值, y = 平均值, fill = 菌株)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = 平均值 - 標準差, ymax = 平均值 + 標準差),
                position = position_dodge(0.8), width = 0.25) +
  labs(title="Leaf area of each treatment",x="",y = "Leaf area (mm^2)", fill = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_blank())+
  facet_grid(場域~時間)

ggplot(FieldExp109Grouped_long%>%filter(測量值=="冠"), aes(x = 測量值, y = 平均值, fill = 菌株)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = 平均值 - 標準差, ymax = 平均值 + 標準差),
                position = position_dodge(0.8), width = 0.25) +
  labs(title="Crown diameter of each treatment",x="",y = "Crown diamete (mm)", fill = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_blank())+
  facet_grid(場域~時間)

ggplot(FieldExp109Disease, aes(x=菌株 , y = 病害嚴重度, fill = 菌株)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_text(aes(label = 有病的植株數量), position = position_dodge(1), vjust = 0)+ 
  scale_y_continuous(limits =c(0,0.6))+ 
  labs(title="Disease severity of each treatment",x="",y="Disease severity", fill = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_blank())+
  facet_grid(場域~時間)

#繪製boxplot
ggplot(FieldExp109, aes(x = 菌株, y = 葉綠素, fill = 菌株)) +
  geom_boxplot() +
  labs(title="Chlorophyll content of each treatment",x="",y = "Chlorophyll content (SPAD)", fill = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_blank())+
  facet_grid(場域~時間)

ggplot(FieldExp109, aes(x = 菌株, y = 葉面積, fill = 菌株)) +
  geom_boxplot() +
  labs(title="Leaf area of each treatment",x="",y = "Leaf area (mm^2)", fill = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_blank())+
  facet_grid(場域~時間)

ggplot(FieldExp109, aes(x = 菌株, y = 冠平均, fill = 菌株)) +
  geom_boxplot() +
  labs(title="Crown diameter of each treatment",x="",y = "Crown diamete (mm)", fill = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_blank())+
  facet_grid(場域~時間)

#提取離群值
calculate_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- x < lower_bound | x > upper_bound
  return(outliers)
}

FieldExp109 <- FieldExp109 %>% group_by(場域, 時間, 菌株) %>%
  mutate(
    冠平均_outliers = calculate_outliers(冠平均),
    葉面積_outliers = calculate_outliers(葉面積),
    葉綠素_outliers = calculate_outliers(葉綠素),
    
  )
print(FieldExp109)
glimpse(FieldExp109 %>% filter(葉面積_outliers==1))

#重新繪圖
ggplot(FieldExp109 %>% filter(!葉綠素_outliers), aes(x = 菌株, y = 葉綠素, fill = 菌株)) +
  geom_boxplot() +
  labs(title="Chlorophyll content of each treatment(w/o outliers)",x="",y = "Chlorophyll content (SPAD)", fill = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_blank())+
  facet_grid(場域~時間)

ggplot(FieldExp109 %>% filter(!葉面積_outliers), aes(x = 菌株, y = 葉面積, fill = 菌株)) +
  geom_boxplot() +
  labs(title="Leaf area of each treatment(w/o outliers)",x="",y = "Leaf area (mm^2)", fill = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_blank())+
  facet_grid(場域~時間)

ggplot(FieldExp109 %>% filter(!冠平均_outliers), aes(x = 菌株, y = 冠平均, fill = 菌株)) +
  geom_boxplot() +
  labs(title="Crown diameter of each treatment(w/o outliers)",x="",y = "Crown diamete (mm)", fill = "Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_blank())+
  facet_grid(場域~時間)

#去除outliers檢驗常態
install.packages("nortest")
library(nortest)

qqnorm((FieldExp109%>% filter(!冠平均_outliers))$`冠平均`)
qqline((FieldExp109%>% filter(!冠平均_outliers))$`冠平均`, col = "red")
corwn_shapiro_result <- shapiro.test((FieldExp109%>% filter(!冠平均_outliers))$`冠平均`)
crown_ad_result <-ad.test((FieldExp109%>% filter(!冠平均_outliers))$`冠平均`)
print(corwn_shapiro_result)
print(crown_ad_result)

qqnorm((FieldExp109%>% filter(!葉面積_outliers))$`葉面積`)
qqline((FieldExp109%>% filter(!葉面積_outliers))$`葉面積`, col = "red")
leaf_shapiro_result <- shapiro.test((FieldExp109%>% filter(!葉面積_outliers))$`葉面積`)
leaf_ad_result <-ad.test((FieldExp109%>% filter(!葉面積_outliers))$`葉面積`)
print(leaf_shapiro_result)
print(leaf_ad_result)

qqnorm((FieldExp109%>% filter(!葉綠素_outliers))$`葉綠素`)
qqline((FieldExp109%>% filter(!葉綠素_outliers))$`葉綠素`, col = "red")
chl_shapiro_result <- shapiro.test((FieldExp109%>% filter(!葉綠素_outliers))$`葉綠素`)
chl_ad_result <-ad.test((FieldExp109%>% filter(!葉綠素_outliers))$`葉綠素`)
print(chl_shapiro_result)
print(chl_ad_result)


'''
anova_result_leafarea <- aov(`葉面積` ~ 場域 * 時間 * 菌株, data = FieldExp109)
summary(anova_result_leafarea)
anova_result_chl <- aov(`葉綠素` ~ 場域 * 時間 * 菌株, data = FieldExp109)
summary(anova_result_chl)
anova_result_crown <- aov(`冠平均` ~ 場域 * 時間 * 菌株, data = FieldExp109)
summary(anova_result_crown)
'''
