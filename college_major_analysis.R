# Loading the college dataframe
install.packages("devtools")
devtools::install_github("jhudsl/collegeIncome")
library(collegeIncome)
data(college)

# To record analysis
install.packages("matahari")
library(matahari)
dance_start(value = FALSE, contents = FALSE)

# Convert the variables interested to check the relationship between them
college$major <- as.factor(college$major)
college$major_code <- as.factor(college$major_code)
college$major_category <- as.factor(college$major_category)

# Visualize the relationship
boxplot(median/1000 ~ major_category, data = college, main = "Income vs. Major",
        ylab="Income (in $1000)", xlab = "Major Category", las = 2)

# Analysis
# First we will reorder the major categories alphabeticaly
college <- college[order(college$major_category), ]

# Applying the linear model with Income as outcome and major as predictor
mdl <- lm(median ~ major_category - 1, data = college)
summary(mdl)$coef

# Apparently, most majors have similar income, except the Business(the highest)
major_category_bus <- relevel(college$major_category, "Business")
mdlBus <- lm(median ~ major_category_bus, data = college)
summary(mdlBus)$coef

business_diff <- summary(mdlBus)$coef[-1,]

# Check for the lowest(significant) p-values
business_diff[order(business_diff[,4])[1:6], ]
# Income for business is significantly different from Computers & Mathematics, 
# Education, Engineering, Humanities & Liberal Arts and Social Science

# We will do the same for the lowest income category i.e. Interdisciplinary
major_category_inter <- relevel(college$major_category, "Interdisciplinary")
mdlInter <- lm(median ~ major_category_inter, data = college)
summary(mdlInter)$coef

inter_diff <- summary(mdlInter)$coef[-1,]

# Check for the lowest(significant) p-values
inter_diff[order(inter_diff[,4])[1:5], ]

# Income for Interdisciplinary is insignificant to other categories

# Gender comparision with business as the major category
mdlGen <- lm(median ~ major_category + perc_men + perc_women, data = college)
summary(mdlGen)$coef
Gen_diff <- summary(mdlGen)$coef[-1,]
# Check for the lowest(significant) p-values
Gen_diff[order(Gen_diff[,4])[1:5], ]

mdlBusGen <- lm(median ~ major_category_bus + perc_men + perc_women, 
                data = college)
summary(mdlBusGen)$coef

businessGen_diff <- summary(mdlBusGen)$coef[-1,]
# Check for the lowest(significant) p-values
businessGen_diff[order(businessGen_diff[,4])[1:5], ]

# Saving the record
dance_save("college_major_analysis.rds")
dance_stop()
