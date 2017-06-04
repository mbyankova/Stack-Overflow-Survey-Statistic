raw_data <- read_csv("~/R/Stack-Overflow-Survey-Statistic/raw_data.csv")
data_frame = data.frame(exp_mid, salary_mid, gender)
salary_mid = raw_data[['salary_midpoint']]
gender = raw_data[['gender']]
d<-data_frame[!(data_frame$gender=="Other" | data_frame$gender=="Prefer not to disclose"),]

#Comaprison between exprience and salary with man and women
sal_exp_gender_bp <- ggplot(d, aes(x=factor(exp_mid), y=salary_mid, fill=gender)) +
  geom_boxplot() +
  labs(x="Experience", y= "Salary")


#Proportion of people in different occupations
occupation_g = raw_data$occupation_group
r = revalue(occupation_g, c( "Mathematics Developers (Data Scientists, Machine Learning Devs & Devs with Stats & Math Backgrounds)" = "Mathematics Developers"))
sorted = sort(table(r))
par(mar=c(2, 14 ,4.1 ,2.1))
barplot(sorted/length(r), cex.names = 0.7, horiz = TRUE, las = 1, xlim = c(0, 0.3), beside = TRUE, col = c("#87CEFA"))

#Proprtion of people who love their job compare to their salary
satisfaction = raw_data$job_satisfaction
sal_sat = data.frame(salary_mid, satisfaction)
sal_love = sal_sat[sal_sat$satisfaction == "I love my job",]
sal_love_clear = sal_love[complete.cases(sal_love),]
sal_sat_clear = sal_sat[complete.cases(sal_sat),]
df1  = data.frame(count(sal_love_clear), count(factor(sal_sat_clear$salary_mid)))
proportion_love_salary = count(sal_love_clear)$freq/count(factor(sal_sat_clear$salary_mid))$freq
salary_num = as.numeric(levels(factor(salary_mid)))
plot(salary_num, proportion_love_salary, xlab = "Salary", ylab = "Proprtion of people who love their job")
abline(lm(proportion_love_salary~salary_num))

#Self Identification proprtions
Programmer = rep(FALSE, 56030)
Developer = rep(FALSE, 56030)
Engineer = rep(FALSE, 56030)
Sr.Developer = rep(FALSE, 56030)
FullStackDeveloper = rep(FALSE, 56030)
Other = rep(FALSE, 56030)
Hacker = rep(FALSE, 56030)
Guru = rep(FALSE, 56030)
Expert = rep(FALSE, 56030)
Ninja = rep(FALSE, 56030)
Manager = rep(FALSE, 56030)
Rockstar = rep(FALSE, 56030)
FullStackOverflowDeveloper = rep(FALSE, 56030)
for(i in seq_along(ident)) {
  split = strsplit(ident[i], ";")
  split = unlist(split)
  for(s in split){
    s = trimws(s, which = c("both", "left", "right"))
    if(!is.na(s) && s == "Developer"){
      Developer[i] = TRUE
    }
    if(!is.na(s) && s == "Programmer"){
    Programmer[i] = TRUE
    }
    if(!is.na(s) && s == "Engineer"){
    Engineer[i] = TRUE
    }
    if(!is.na(s) && s == "Sr. Developer"){
    Sr.Developer[i] = TRUE
    }
    if(!is.na(s) && s == "Full-stack Developer"){
      FullStackDeveloper[i] = TRUE
    }
    if(!is.na(s) && s == "Other"){
      Other[i] = TRUE
    }
    if(!is.na(s) && s == "Hacker"){
      Hacker[i] = TRUE
    }
    if(!is.na(s) && s == "Expert"){
      Expert[i] = TRUE
    }
    if(!is.na(s) && s == "Ninja"){
      Ninja[i] = TRUE
    }
    if(!is.na(s) && s == "Manager"){
      Manager[i] = TRUE
    }
    if(!is.na(s) && s == "Rockstar"){
      Rockstar[i] = TRUE
    }
    if(!is.na(s) && s == "Guru"){
      Guru[i] = TRUE
    }
    if(!is.na(s) && s == "Full-stack Developer"){
      FullStackDeveloper[i] = TRUE
    }
    if(!is.na(s) && s == "Full Stack Overflow Developer"){
      FullStackOverflowDeveloper[i] = TRUE
    }
    if(is.na(s)){
      Other[i] = TRUE
    }
  }
}
propOfId = c(count(Developer)$freq[2]/56030, count(Programmer)$freq[2]/56030,
count(Engineer)$freq[2]/56030, count(Sr.Developer)$freq[2]/56030, count(FullStackDeveloper)$freq[2]/56030, count(Other)$freq[2]/56030, count(Hacker)$freq[2]/56030, count(Expert)$freq[2]/56030, count(Ninja)$freq[2]/56030, count(Manager)$freq[2]/56030,count(Rockstar)$freq[2]/56030, count(Guru)$freq[2]/56030, count(FullStackOverflowDeveloper)$freq[2]/56030)
ID_df = data.frame(namesOfId, propOfId)
Id_df_sorted = ID_df[order(ID_df$propOfId, decreasing =TRUE) ,]
par(mar=c(9.1, 4.1, 3.1, 2.1))
barplot(Id_df_sorted$propOfId, names.arg = Id_df_sorted$namesOfId, cex.names = 0.7, las=2, ylim = c(0, 0.8),beside = TRUE, col = c("#3CB371"))

#where money matters
library(plyr)

money_matter=rep(FALSE, 31132)
for(i in seq_along(data_with_exp$new_job_value)) {
  split = strsplit(data_with_exp$new_job_value[i], ";")
  split = unlist(split)
  if(!is.na(split[1]) && split[1] == "Salary"){
    money_matter[i] = TRUE
  }
}

answers_by_countries = count(data_with_exp$country)
cool_countries = answers_by_countries[answers_by_countries$freq >= 200 & !is.na(answers_by_countries$x),]$x
relavant_data = data_with_exp[data_with_exp$country %in% cool_countries,]

data_with_exp["money_matters"] = money_matter
#money_by_contry = aggregate(data_with_exp$money_matters, by=list(country=data_with_exp$country), FUN=length)
money_by_contry = relavant_data[relavant_data$money_matters == TRUE,]

relavant_data_without_nas = relavant_data[!is.na(relavant_data$salary_midpoint),]
mean_sal_country = aggregate(relavant_data_without_nas$salary_midpoint, by=list(country=relavant_data_without_nas$country), FUN=mean)

money_by_contry = relavant_data[relavant_data$money_matters == TRUE,]
count_of_all = count(relavant_data$country)$freq
count_of_money_lovers = count(money_by_contry$country)$freq
rates_money_lovers = count_of_money_lovers/count_of_all

par(mar=c(3.5, 3.5 ,1 ,1))
options(scipen = 999)
countries = revalue(mean_sal_country$country, c("Russian Federation" = "Russia"))
text(mean_sal_country$x, rates_money_lovers, labels=mean_sal_country$country, cex= 0.7, pos = 3)
plot(mean_sal_country$x, rates_money_lovers,main= "Mean salary vs. rates of people who values salary",
     xlab= "Mean salary",
     ylab= "Rates of people who values salary",
     col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2)

abline(lm(rates_money_lovers~mean_sal_country$x))
text(mean_sal_country$x, rates_money_lovers, labels=countries, cex= 0.7, pos = 3)