library(tidyverse)
library(reshape2)

#Data collection and binding processes
#Load in the dataset
#existing_employee <- existing_employee %>% 
 # mutate(status = "exist")

#who_left_employees <- who_left_employees %>% 
 # mutate(status = "left")

#Bind both columns and write the scv file
#rbind.data.frame(existing_employee, who_left_employees) %>% 
 # write.csv("attrition.csv")

#Read newly saved file
attrition <- read.csv("attrition.csv", stringsAsFactors = T)


#Examine data structures
str(attrition)

#Lets start with some correlation relationshios
library(polycor)
attrition_corr <- hetcor(attrition)
attrition_corr$correlations %>% 
  melt() %>% 
  ggplot(., aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "grey", high = "darkred") +
  geom_text(aes(Var1, Var2, label = round(value,2)), size = 2)+
  labs(title = "Correlation Matrix", x = "Numeric column", y = "Numeric Column",
       fill = "Coefficient Range") +
  theme(axis.text.x = element_text(vjust = 0.5, angle = 45),
        plot.title = element_text(face = "bold", hjust = 0.5))



#Lets examine the class 
attrition %>% 
  count(status) %>% 
  ggplot(., aes(status, n, fill = status)) +
  geom_bar(stat = "identity") +
  ggtitle("Existing staffs vs Exits")

#Department with highest mumber of employess
attrition %>% 
  count(dept) %>% 
  ggplot(., aes(reorder(dept, n), n, fill = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Deparment staff counts",
       x = "Department")


#Lets examine the department who has more people who left
attrition %>% 
  count(status, dept) %>% 
  filter(status == "left") %>% 
  ggplot(., aes(reorder(dept,n), n, fill = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Department with Largest Attrition Number") +
  xlab("Department")

attrition %>% 
  count(status, dept) %>% 
  filter(status == "left") %>% 
  select(n)


#Examine the average working hrs of each dept
attrition %>% 
  group_by(dept) %>% 
  summarise(n = mean(average_montly_hours)) %>% 
  ggplot(., aes(reorder(dept,n), n, fill = n)) +
  geom_bar(stat = "identity") +
  coord_flip()

#relationship between department size and the churn size
cbind.data.frame(size = attrition %>% 
                   count(dept) %>% 
                   select(n),
                 churn = attrition %>% 
                   count(status, dept) %>% 
                   filter(status == "left") %>% 
                   select(n)) %>% 
  cor()


#Working hrs appear to be almost the same across all dept
#Dept with the lowest salary range
attrition %>% 
  group_by(dept) %>% 
  count(salary, sort = T) %>% 
  ggplot(., aes(reorder(dept, n), n, fill = factor(salary, levels = c("high", "medium", "low")))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Salary Range by Department") + 
  scale_fill_discrete(name = "Salary", labels = c("High", "Medium", "Low")) +
  labs(x = "Departments")



#ratio of people in a dept versus those who left
attrition %>% 
  group_by(dept) %>% 
  count(status) %>% 
  mutate(n2 = sum(n)) %>% 
  mutate(n3 = n/n2) %>% 
  ggplot(., aes(x = "", y = n3, fill = status)) +
  geom_bar(stat = "identity", width = 2) +
  coord_polar(theta = "y") +
  facet_wrap(~dept) +
  scale_fill_manual(values=c("#999999", "#E69F00"))

#Satisfaction level by department
attrition %>% 
  group_by(dept) %>% 
  summarise(level = mean(satisfaction_level)) %>% 
  ggplot(., aes(reorder(dept, level), level, fill = dept)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Satisfaction Level by Department")



#Percentage of high, medium and low salaries compared to each department statistics
attrition %>% 
  group_by(dept) %>% 
  count(salary) %>% 
  mutate(n2 = sum(n)) %>% 
  mutate(n3 = n/n2) %>% 
  ggplot(., aes(x = "", y = n3, fill = salary)) +
  geom_bar(stat = "identity", width = 2) +
  coord_polar(theta = "y") +
  facet_wrap(~dept) #+
  scale_fill_manual(values=c("#999999", "#E69F00", ))

  

#Ratio of people who left and are remaining by the number of projects they conducted
attrition %>% 
  group_by(number_project) %>% 
  count(status) %>% 
  ggplot(., aes(factor(number_project), n, fill = status)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Projects by Employment Status",
       x = "Number of Projects", y = "Counts")


#It would make more send to do it in terms of ratio
attrition %>% 
  group_by(number_project) %>% 
  count(status) %>% 
  mutate(n2 = sum(n)) %>% 
  ungroup() %>% 
  mutate(n3 = n/n2) %>% 
  ggplot(., aes(x = "", y = n3, fill = status)) +
  geom_bar(stat = "identity", width = 2) +
  coord_polar(theta = "y") +
  facet_wrap(~number_project) +
  scale_fill_manual(values=c("#999999", "#E69F00"))

#People with 2,6,7 projects have left. The have the highest atrrition rate
#Lets look at the people most common the firm by projects
attrition %>% 
  group_by(number_project) %>% 
  count() %>% 
  ggplot(., aes(factor(number_project), n, fill = n)) +
  geom_bar(stat = "identity")



#Lets examine the last evaluation of people who left and people who stayed
ggplot(attrition, aes(last_evaluation, fill = status)) +
  geom_density(alpha = 0.1)

#The average evaluation for people who are left
attrition %>% 
  filter(status == "left") %>% 
  summarise(n = mean(last_evaluation))

attrition %>% 
  filter(status == "exist") %>% 
  summarise(n = mean(last_evaluation))


#People who left ap
ggplot(attrition, aes(average_montly_hours, fill = status)) +
  geom_density(alpha = 0.1) +
  ggtitle("Average Working hours")


#People who left appear to have a bimodal distribution of working hrs and evaluation
#This indicates that they are largely overworked and some are underworked also.


#Lets examine the time spent in the company
attrition %>% 
  group_by(time_spend_company) %>% 
  count(status) %>% 
  mutate(n2 = sum(n)) %>% 
  ungroup() %>% 
  mutate(n3 = n/n2) %>% 
  ggplot(., aes(x = "", y = n3, fill = status)) +
  geom_bar(stat = "identity", width = 2) +
  coord_polar(theta = "y") +
  facet_wrap(~time_spend_company) +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  ggtitle("Time Spent in Company")

  
attrition %>% 
  group_by(time_spend_company, number_project) %>% 
    count(status) 


install.packages("Boruta")
