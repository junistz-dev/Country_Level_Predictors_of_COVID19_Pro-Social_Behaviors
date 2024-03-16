library(ggplot2)

rm(list = ls())
set.seed(31994695)
cvbase = read.csv("PsyCoronaBaselineExtract.csv")
cvbase <- cvbase[sample(nrow(cvbase),40000),]

head(cvbase)
tail(cvbase)

# Question 1

dim(cvbase)
# 40000 row, and 52 columns

str(cvbase)
# data type

# distribution of numerical attributes
# [1:10] 은 자신의 고용 상태를 나타내는데, 그 조건에 해당하면 1, 아니면 NA로 남긴다.

employment_count <- colSums(cvbase[,1:10] == 1, na.rm = TRUE)

# 데이터프레임 생성
employment_count_df <- data.frame(
  empstatus = paste0("employstatus_", 1:10),
  count = employment_count
)

# barplot for [1:10]
barplot(employment_count_df$count, names.arg = employment_count_df$empstatus, 
        main = "Employment Count by Employment Status",
        xlab = "Employment Status", ylab = "Count",
        col = "grey", ylim = c(0, max(employment_count_df$count) * 1.12))

# [11:26] 는 자신의 상황에 따라 숫자 범위에 맞추어서 int 의 값
iso_off_friend <- table(cvbase[11])
iso_off_people <- table(cvbase[12])

iso_off_friend_df <- data.frame(
  iso_off_friend
)

iso_off_friend_df

# barplot for only iso_off_friend >> 내가 하고 있는게 맞나?
barplot(iso_off_friend_df$Freq, 
        names.arg = iso_off_friend_df$isoFriends_inPerson,
        main = "Frequency of isoFriends_inPerson",
        xlab = "isoFriends_inPerson",
        ylab = "Frequency",
        ylim = c(0, max(iso_off_friend_df$Freq) * 1.3),
        col = "skyblue")


### trying ###
na_count <- colSums(is.na(cvbase))

na_count_df <- data.frame(na_count)

na_count_df

# Overall the number of NA in the cvbase.
barplot(na_count_df$na_count,
        names.arg = row.names(na_count_df),
        ylim = c(0, max(na_count_df$na_count) * 1.3),
        main = "The number of NA in the cvbase",
        xlab = "column name",
        ylab = "frequency")

# 어쩔수 없이 NA 가 많이 들어가는 부분에 대해서 의도적으로 제외
cvbase_cut <- c(1:10, 27:32,39:44)

filtered_cvbase <- subset(cvbase, select = -cvbase_cut)

filtered_na_count <- colSums(is.na(filtered_cvbase))

filtered_na_count_df <- data.frame(filtered_na_count)

barplot(filtered_na_count_df$filtered_na_count,
        names.arg = row.names(filtered_na_count_df),
        ylim = c(0, max(filtered_na_count_df$filtered_na_count) * 1.3),
        main = "The number of NA in the cvbase",
        xlab = "column name",
        ylab = "frequency")

