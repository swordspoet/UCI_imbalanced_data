## 数据可视化
library(ggplot2)
library(plotly)
# geom_histogram()直方图
# geom_density()密度图
tr <- function(a){ggplot(data = num_train, aes(x= a, y=..density..)) +
    geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) + geom_density()
  ggplotly()
}

tr(num_train$age)
tr(num_train$capital_losses)
# 添加目标变量income_level
num_train[,income_level := cat_train$income_level]
# 连续型变量可视化
ggplot(data=num_train,aes(x = age,y=wage_per_hour))+geom_point(aes(colour=income_level))+
scale_y_continuous("wage per hour", breaks = seq(0,10000,1000))

ggplot(data=num_train,aes(x = age,y=weeks_worked_in_year))+geom_point(aes(colour=income_level))+
  scale_y_continuous("weeks worked in a year", breaks = seq(0,60,5))

ggplot(data=num_train,aes(x = age,y=dividend_from_Stocks))+geom_point(aes(colour=income_level))+
scale_y_continuous("dividend from stocks", breaks = seq(0,10000,5000))

ggplot(data=num_train,aes(x = age,y=capital_gains))+geom_point(aes(colour=income_level))+
  scale_y_continuous("capital gains", breaks = seq(0,30000,3000))

ggplot(data=num_train,aes(x = age,y=capital_losses))+geom_point(aes(colour=income_level))+
  scale_y_continuous("capital losses", breaks = seq(0,30000,3000))

# 类别型变量可视化
all_bar <- function(i){
  ggplot(cat_train,aes(x=i,fill=income_level))+geom_bar(position = "dodge",  color="black")+
    scale_fill_brewer(palette = "Pastel1")+
    theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=8))
  ggplotly()
}

all_bar(cat_train$class_of_worker)
all_bar(cat_train$education)
all_bar(cat_train$marital_status)
all_bar(cat_train$race)
all_bar(cat_train$hispanic_origin)
all_bar(cat_train$industry_code)
all_bar(cat_train$major_industry_code)
all_bar(cat_train$major_occupation_code)
all_bar(cat_train$sex)
all_bar(cat_train$member_of_labor_union)
all_bar(cat_train$reason_for_unemployment)
all_bar(cat_train$full_parttime_employment_stat)
all_bar(cat_train$tax_filer_status)
all_bar(cat_train$region_of_previous_residence)
all_bar(cat_train$state_of_previous_residence)
all_bar(cat_train$d_household_family_stat)
all_bar(cat_train$d_household_summary)
all_bar(cat_train$migration_msa)
all_bar(cat_train$migration_reg)
all_bar(cat_train$migration_within_reg)
all_bar(cat_train$live_1_year_ago)
all_bar(cat_train$migration_sunbelt)
all_bar(cat_train$family_members_under_18)
all_bar(cat_train$country_father)
all_bar(cat_train$country_mother)
all_bar(cat_train$country_self)
all_bar(cat_train$citizenship)
all_bar(cat_train$business_or_self_employed)
all_bar(cat_train$fill_questionnaire_veteran_admin)
all_bar(cat_train$veterans_benefits)
all_bar(cat_train$year)

#下面是一种可以替代的办法，不过没有图形直观
prop.table(table(cat_train$class_of_worker,cat_train$income_level),1)