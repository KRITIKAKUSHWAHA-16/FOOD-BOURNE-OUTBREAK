data<-read.csv("data/outbreak.csv")
dim(data)
head(data)
table(data$illness)
attack_rate<-mean(data$illness=="Yes")
attack_rate
data.frame(total_participation=nrow(data),
           cases=sum(data$illness=="Yes"),
           attack_rate_percentage=attack_rate)
table(data$Sex,data$illness
      data%>%
        group_by(sex)%>%
        summarise(
          participants=n(),
          cases=sum(illness=="Yes"),
          attack_rate=cases/participants*100
        )
      data.frame(Total_participants=nrow(data),
                 Cases=sum(data$illness=="Yes"),
                 Attack_rate_percent=attack_rate)
table(data$sex,data$illness)
data%>%
  summarise(
    participants=n(),
    cases=sum(illness=="Yes"),
    attack_rate=cases/participants*100
  )
table(data$residence,data$illness)
data%>%
  group_by(residence)%>%
  summarise(
    participants=n(),
    cases=sum(illness=="Yes"),
    attack_rate=cases/participants*100
  )
table(data$residence,data$illness)
data%>%
  group_by(sex)%>%
  summarise(
    participants=n(),
    cases=sum(illness=="Yes"),
    attack_rate=cases/participants*100)
table(data$residence,data$illness)  
data%>%
  group_by(residence)%>%
  summarise(
    participants=n(),
    cases=sum(illness=="Yes"),
    attack_rate=cases/participants*100
  )
data$onset_date<-as.Date(data$onset_date)
cases<-data%>%
  filter(illness=="Yes")
table(cases$onset_date)
ggplot(cases,aes(x=onset_date))+
  geom_histogram()
title("Epidemic Curve Of the University Outbreak",
      x="Date of Symptom Onset",
      y= "Number of cases")
  
table(data$ate_chicken,data$illness)
table(data$ate_salad,data$illness)
table(data$ate_pasta,data$illness)
table(data$drank_juice,data$illness)
table(data$ate_chicken)
chicken_table<-table(data$ate_chicken,data$illness)
chicken_table
risk_exposed<-chicken_table["Yes","Yes"]/sum(chicken_table["Yes",])
risk_unexposed<-chicken_table["No","Yes"]/sum(chicken_table["No"])
RR_chicken<-risk_exposed/risk_unexposed
RR_chicken
chicken_table<-table(data$ate_chicken,data$illness)
chicken_table
prop.table(chicken_table,margin=1)*100
risk_exposed
risk_unexposed

calculate_rr <- function(food) {
  
  tab <- table(data[[food]], data$illness)
  
  risk_exposed <- tab["Yes", "Yes"] / sum(tab["Yes", ])
  
  risk_unexposed <- tab["No", "Yes"] / sum(tab["No", ])
  
  risk_exposed / risk_unexposed
}

food_rr <- data.frame(
  Food = c("Chicken", "Salad", "Pasta", "Dessert", "Juice"),
  Relative_Risk = c(
    calculate_rr("ate_chicken"),
    calculate_rr("ate_salad"),
    calculate_rr("ate_pasta"),
    calculate_rr("ate_dessert"),
    calculate_rr("drank_juice")
  )
)

food_rr
chicken_table <- table(
  data$ate_chicken,
  data$illness
)

chicken_table
epi_table <- matrix(
  c(
    chicken_table["Yes", "Yes"],
    chicken_table["Yes", "No"],
    chicken_table["No", "Yes"],
    chicken_table["No", "No"]
  ),
  nrow = 2,
  byrow = TRUE
)

epi_table
riskratio(epi_table)
chicken_table <- table(data$ate_chicken, data$illness)

chicken_table
epi_table <- matrix(
  c(
    chicken_table["Yes", "Yes"],
    chicken_table["Yes", "No"],
    chicken_table["No", "Yes"],
    chicken_table["No", "No"]
  ),
  nrow = 2,
  byrow = TRUE
)

epi_table
riskratio(epi_table)
chicken_table <- table(data$ate_chicken, data$illness)

chisq.test(chicken_table)
chisq.test(chicken_table)
calculate_rr <- function(food) {
  
  tab <- table(data[[food]], data$illness)
  
  risk_exposed <- tab["Yes", "Yes"] / sum(tab["Yes", ])
  risk_unexposed <- tab["No", "Yes"] / sum(tab["No", ])
  
  risk_exposed / risk_unexposed
}
food_rr <- data.frame(
  Food = c(
    "Chicken",
    "Salad",
    "Pasta",
    "Dessert",
    "Juice"
  ),
  
  Relative_Risk = c(
    calculate_rr("ate_chicken"),
    calculate_rr("ate_salad"),
    calculate_rr("ate_pasta"),
    calculate_rr("ate_dessert"),
    calculate_rr("drank_juice")
  )
)

food_rr
calculate_rr_ci <- function(food) {
  
  tab <- table(data[[food]], data$illness)
  
  a <- tab["Yes", "Yes"]
  b <- tab["Yes", "No"]
  c <- tab["No", "Yes"]
  d <- tab["No", "No"]
  
  risk_exposed <- a / (a + b)
  risk_unexposed <- c / (c + d)
  
  rr <- risk_exposed / risk_unexposed
  
  se_log_rr <- sqrt(
    (1/a) - (1/(a+b)) +
      (1/c) - (1/(c+d))
  )
  
  lower_ci <- exp(log(rr) - 1.96 * se_log_rr)
  upper_ci <- exp(log(rr) + 1.96 * se_log_rr)
  
  data.frame(
    Food = food,
    RR = rr,
    Lower_95_CI = lower_ci,
    Upper_95_CI = upper_ci
  )
}
food_results <- rbind(
  calculate_rr_ci("ate_chicken"),
  calculate_rr_ci("ate_salad"),
  calculate_rr_ci("ate_pasta"),
  calculate_rr_ci("ate_dessert"),
  calculate_rr_ci("drank_juice")
)

food_results
table(data$ate_chicken, data$residence, data$illness)
college_a <- subset(data, residence == "College A")

table(college_a$ate_chicken, college_a$illness)
a_table <- table(college_a$ate_chicken, college_a$illness)

risk_exposed <- a_table["Yes", "Yes"] / sum(a_table["Yes", ])
risk_unexposed <- a_table["No", "Yes"] / sum(a_table["No", ])

rr_college_a <- risk_exposed / risk_unexposed

rr_college_a

overall_attack_rate <- mean(data$illness == "Yes") * 100

overall_attack_rate
cat("Number of participants:", nrow(data), "\n")
cat("Number of cases:", sum(data$illness == "Yes"), "\n")
cat("Overall attack rate:", round(overall_attack_rate, 1), "%\n")
food_results
food_results$RR <- round(food_results$RR, 2)

food_results$Lower_95_CI <- round(
  food_results$Lower_95_CI, 2
)

food_results$Upper_95_CI <- round(
  food_results$Upper_95_CI, 2
)

food_results
food_results$RR_95CI <- paste0(
  food_results$RR,
  " (",
  food_results$Lower_95_CI,
  "–",
  food_results$Upper_95_CI,
  ")"
)

food_results










































