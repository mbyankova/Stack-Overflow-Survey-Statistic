x=read.csv(file="data.csv")
x=edit(x)
x[['country']]
exp_mid = data[['experience_midpoint']]
salary_mid = data[['salary_midpoint']]
cor.test(salary_mid, exp_mid)
barplot(table(salary_mid,exp_mid),
        + beside=TRUE,
        + legend.text=T)
plot(exp_mid~salary_mid)
> hist(salary_mid)
lm(formula = salary_mid ~ exp_mid)


data_frame[data_frame$gender == 'Female',]

d<-data_frame[!(data_frame$gender=="Other" | data_frame$gender=="Prefer not to disclose"),]

library(ggplot2)
ggplot(data_frame, aes(x=exp_mid, y=salary_mid, fill=gender)) +
  + geom_boxplot()

sal_exp_gender_bp <- ggplot(d, aes(x=factor(exp_mid), y=salary_mid, fill=gender)) +
geom_boxplot() +
labs(x="Experience", y= "Salary")

occupation_g = raw_data$occupation_group
r = revalue(occupation_g, c( "Mathematics Developers (Data Scientists, Machine Learning Devs & Devs with Stats & Math Backgrounds)" = "Mathematics Developers"))
par(mar=c(2, 14 ,4.1 ,2.1))
table(r)
sorted = sort(table(r))
barplot(sorted/length(r), cex.names = 0.7, horiz = TRUE, las = 1, xlim = c(0, 0.3), beside = TRUE, col = c("#87CEFA"), main = "")


plot(d$salary_mid ~ d$exp_mid, pch = c(1,2))


sal_sat[complete.cases(sal_sat),]

cor(df1$salary_mid, prop_loving)

for(i in seq_along(ident)) {
  split = strsplit(ident[i], ";")
  split = unlist(split)
  for(s in split){
    s = trimws(s, which = c("both", "left", "right"))
    if(!is.na(s) && s == "Developer"){
      Developer[i] = TRUE
    }
}}
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
    if(!is.na(s) && s == "Full Stack Overflow Developer"){
      FullStackOverflowDeveloper[i] = TRUE
    }
  }
}



  split = strsplit(ident[2], ";")
  for(s in split){
    s = trimws(s, which = c("both", "left", "right"))
    print(s)
    if(!is.na(s) && s == "Programmer"){
      Programmer[i] = TRUE
    }
  }

  
  for(i in seq_along(ident)) {
    split = strsplit(ident[i], ";")
    split = unlist(split)
    for(s in split){
      s = trimws(s, which = c("both", "left", "right"))
      if(is.na(s)){
        Other[i] = TRUE
      }
      
  if(!is.na(s) && s == "Programmer"){
    Programmer[i] = TRUE
  }
  if(!is.na(s) && s == "Engineer"){
    Engineer[i] = TRUE
  }
  if(!is.na(s) && s == "Full-stack Developer"){
    FullStackDeveloper[i] = TRUE
  }
  if(!is.na(s) && s == "Full Stack Overflow Developer"){
    FullStackOverflowDeveloper[i] = TRUE
  }
  }
}
