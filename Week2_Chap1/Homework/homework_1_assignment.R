# HOMEWORK 1 ----

remove(list=ls())
# Libraries ----
library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)

# Source Scripts ----
source("00_Scripts/assess_attrition.R")

# Data ----
path_train     <- "00_Data/telco_train.xlsx"
train_raw_tbl  <- read_excel(path_train, sheet = 1)

dept_jobrole_tbl <- train_raw_tbl %>%
    select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

kpi_industry_turnover_pct <- 0.088

# Productivity Cost by Role ----

productivity_cost_by_role_tbl <- read_excel("00_Data/productivity_cost_by_role.xlsx")
productivity_cost_by_role_tbl

dept_jobrole_tbl<-dept_jobrole_tbl%>%
  left_join(select(productivity_cost_by_role_tbl,-Department),
            by = "JobRole")


dept_jobrole_tbl<-dept_jobrole_tbl%>%
  count(Department, JobRole, Attrition)%>%
  count_to_pct(Department, JobRole)%>%
  assess_attrition(Attrition, 
                   attrition_value = "Yes", 
                   baseline_pct = 0.088)%>%
  left_join(select(productivity_cost_by_role_tbl,-Department),
            by = "JobRole") %>% 
  mutate(
    cost_of_attrition = calculate_attrition_cost(n = n,
                                                 salary=Salary_Average,
                                                 net_revenue_per_employee=Revenue_Average
                                                 )
  ) 


dept_jobrole_tbl%>% 
  plot_attrition(Department, JobRole, 
                 .value=cost_of_attrition,
                 units="M")+
  labs(
    title="Estimated cost of attrition by job role",
    x="Cost of Attrition", 
    subtitle="Looks like sales exective and lab techs are the biggest drivers of cost"
  )

names(dept_jobrole_tbl)


dept_jobrole_tbl %>% 
  arrange(desc(cost_of_attrition))%>%
  mutate(cum_pct=cumsum(cost_of_attrition)/sum(cost_of_attrition)) %>% 
  View()

dept_jobrole_tbl%>%
  group_by(Department)%>%
  summarise(m=sum(cost_of_attrition))%>%
  mutate(pct=m/sum(m))
  





# Q1: Which Job Role has the highest total cost of attrition? ----
##  --> Sales: Sales Executive


# Q2: What is the total cost of attrition for the Research & Development: Research Scientist job role? ----

##  --> $2.28MM

# Q3: What percentage do the top four Job Roles account for in terms of the total cost of attrition? ----

## 0.7511401

# Q4. Which Department has the highest total cost of attrition? ----

## --> 6577717

# Q5: What percentage does the top Department account for in terms of the total cost of attrition? ----

## --> 0.500 
