 setwd("~/Documents/Monash University/FIT 3152 Data Analysis/Assignment 1")
library(ggplot2)
library(dplyr)

rm(list = ls())
set.seed(31994695)
cvbase = read.csv("PsyCoronaBaselineExtract.csv")
cvbase <- cvbase[sample(nrow(cvbase),40000),]

str(cvbase)

# remove the row if the country is blank
cvbase_filter_country <- cvbase[!cvbase$coded_country == '',]

# Pre-processing session based on column type
preprocessed_data <- function(data) {
  for (i in 1:ncol(data)) {
    column <- data[[i]]
    if (i %in% 1:10) {
      # the NA value in column [1:10] will be 0
      column[is.na(column)] <- 0
    } else if (i %in% 11:26) {
      # the NA value in column [11:24] will be median of their value
      median_val <- median(column, na.rm = TRUE)
      column[is.na(column)] <- median_val
    } else if (i %in% 33:38) {
      # the NA value in column [33:38] will be median of their value
      median_val <- median(column, na.rm = TRUE)
      column[is.na(column)] <- median_val
    } else if (i %in% 39:43) {
      # the NA value in column [39:43] will be 0
      column[is.na(column)] <- 0
    } else if (i %in% 44:46) {
      # the NA value in column [44:46] will be median of their value
      median_val <- median(column, na.rm = TRUE)
      column[is.na(column)] <- median_val
    } else if (i %in% 48:52) {
      # the NA value in column [48:52] will be median of their value
      median_val <- median(column, na.rm = TRUE)
      column[is.na(column)] <- median_val
    }
    data[[i]] <- column
  }
  return(data)
}



# 변환된 데이터 얻기
cvbase_updated <- preprocessed_data(cvbase_filter_country)

# Change Chr(A~F) to 1~6 int
cvbase_updated[27:32] <- lapply(cvbase_updated[27:32], function(x) ifelse(is.na(x), NA, match(x, LETTERS)))

preprocessed_data2 <- function(data) {
  # make it NA to median which column is [27:32]
  data[, 27:32] <- lapply(data[, 27:32], function(x) {
    median_val <- median(x, na.rm = TRUE)
    replace(x, is.na(x), median_val)
  })
  
  return(data)
}

cvbase_final <- preprocessed_data2(cvbase_updated)

## pre-processing done!

## Question 2

# 2 - a
# How do participants responses for your focus country 
# differ from the other countries in the survey as a group?



# Pre-Tutorial Activity Q1 - 1

#before start, make it copy
Q2_sample <- cvbase_final

Q2_sample_for_italy = Q2_sample %>% filter(Q2_sample$coded_country == 'Italy')

Q2_sample_not_italy = Q2_sample %>% filter(Q2_sample$coded_country != 'Italy')


# Coded_Country 열을 제외한 모든 열을 integer로 변환
Q2_sample_for_italy <- Q2_sample_for_italy %>%
  mutate_if(function(x) !is.character(x), as.integer)


Q2_sample_for_italy <- subset(Q2_sample_for_italy, select = -coded_country)

Q2_sample_not_italy <- subset(Q2_sample_not_italy, select = -coded_country)

mean_data_italy <- colMeans(Q2_sample_for_italy)

mean_data_non_italy <- colMeans(Q2_sample_not_italy)

mean_diff <- mean_data_italy - mean_data_non_italy

diff_table <- data.frame(
  Attribute = names(mean_diff),
  Mean_Difference = mean_diff)

# Attribute 열을 요인(factor) 변수로 변환하고 순서를 지정
diff_table$Attribute <- factor(diff_table$Attribute, levels = unique(diff_table$Attribute))

# 가장 작은 5개의 값과 해당 값의 속성
smallest_values <- diff_table[order(diff_table$Mean_Difference), ][1:5, ]

smallest_values

# 가장 큰 5개의 값과 해당 값의 속성
largest_values <- diff_table[order(diff_table$Mean_Difference, decreasing = TRUE), ][1:5, ]

largest_values

# ggplot2 그래프 생성
comparison_plot <- ggplot(diff_table, aes(x = Attribute, y = Mean_Difference)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(title = "Difference in Mean Values between Italy and Non-Italy",
       x = "Attribute",
       y = "Mean Difference (italy - non italy)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))

plot(comparison_plot)

attach(Q2_sample_for_italy)


Q2_italy<- lm(c19ProSo01 + c19ProSo02 + c19ProSo03 + c19ProSo04 ~ . ,data = Q2_sample_for_italy)

summary(Q2_italy)



Q2_not_italy<- lm(c19ProSo01 + c19ProSo02 + c19ProSo03 + c19ProSo04 ~ . ,data = Q2_sample_not_italy)

summary(Q2_not_italy)

##############
# Question 3 #  
##############



