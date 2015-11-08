
names(data)

## looking at correlation between categorical variables

table(data$What_are_you_current_locations, data$Are_you_intending_to_return_to)
chisq.test(data$What_are_you_current_locations, data$Are_you_intending_to_return_to, correct=FALSE)
fisher.test(data$What_are_you_current_locations, data$Are_you_intending_to_return_to)


table(data$Religion, data$Ethnicity)
chisq.test(data$Religion, data$Ethnicity, correct=FALSE)
