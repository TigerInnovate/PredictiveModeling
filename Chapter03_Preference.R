# Traditional Conjoint Analysis (R)

# R preliminaries to get the user-defined function for spine chart: 
# place the spine chart code file <R_utility_program_1.R>
# in your working directory and execute it by
#     source("R_utility_program_1.R")
# Or if you have the R binary file in your working directory, use
#     load(file="mtpa_spine_chart.Rdata")

# spine chart accommodates up to 45 part-worths on one page
# |part-worth| <= 40 can be plotted directly on the spine chart
# |part-worths| > 40 can be accommodated through standardization

###################################################################################################
# 该 package主要用于创建 用户问卷，与本例子其实无太大关系，仅仅用作采集初始数据的问卷生成的一种方式
library(support.CEs)  # package for survey construction 

# generate a balanced set of product profiles for survey
provider.survey <- Lma.design(attribute.names =           #Lma是L^MA方法
  list(brand = c("AT&T","T-Mobile","US Cellular","Verizon"), 
  startup = c("$100","$200","$300","$400"), 
  monthly = c("$100","$200","$300","$400"),
  service = c("4G NO","4G YES"), 
  retail = c("Retail NO","Retail YES"),
  apple = c("Apple NO","Apple YES"), 
  samsung = c("Samsung NO","Samsung YES"), 
  google = c("Nexus NO","Nexus YES")), nalternatives = 1, nblocks=1, seed=9999)
print(questionnaire(provider.survey))  # print survey design for review

sink("questions_for_survey.txt")  # send survey to external text file
questionnaire(provider.survey)
sink() # send output back to the screen
####################################################################################################


# read in conjoint survey profiles with respondent ranks
conjoint.data.frame <- read.csv("mobile_services_ranking.csv")

# set up sum contrasts for effects coding as needed for conjoint analysis
options(contrasts=c("contr.sum","contr.poly"))

# main effects model specification
main.effects.model <- {ranking ~ brand + startup + monthly + service + 
  retail + apple + samsung + google}

# fit linear regression model using main effects only (no interaction terms)
main.effects.model.fit <- lm(main.effects.model, data=conjoint.data.frame)
print(summary(main.effects.model.fit)) 

# save key list elements of the fitted model as needed for conjoint measures
conjoint.results <- 
  main.effects.model.fit[c("contrasts","xlevels","coefficients")]

conjoint.results$attributes <- names(conjoint.results$contrasts) #获得每一个变量的contrast设置信息的名字，即获取每个分类变量的名字。

# compute and store part-worths in the conjoint.results list structure
part.worths <- conjoint.results$xlevels  # list of same structure as xlevels：xlevels就是每个分类变量的具体的水平信息列表

# 开始计算最后一个dummy variable（最后一个Level）的系数，因为使用contr.sum编码，只要获得前K-1个Level的dummy variables的系数之和再求负即可
end.index.for.coefficient <- 1  # intitialize skipping the intercept
part.worth.vector <- NULL # used for accumulation of part worths
# conjoint.results$coefficients是向量，总共有8个Factor需要求part worths，所以用seq来控制循环次数
for(index.for.attribute in seq(along=conjoint.results$contrasts)) {
  #nlevels，每次循环，获得当前factor的水平数
  nlevels <- length(unlist(conjoint.results$xlevels[index.for.attribute]))
  #当前Factor的第一个dummy variable的下标，+1是因为跳过第一个元素（截距项）
  begin.index.for.coefficient <- end.index.for.coefficient + 1
  #根据该Factor的水平数nlevels，计算出第K-1个dummy variable的下标：-2是因为R不会把最后一个水平的dummy variable放入回归模型
  end.index.for.coefficient <- begin.index.for.coefficient + nlevels -2
  #计算出最后一个水平的系数，即第K个水平的系数
  #通过向量下标操作，取出某一Factor所有的系数，求和、取反
  last.part.worth <- -sum(conjoint.results$coefficients[
    begin.index.for.coefficient:end.index.for.coefficient])
	
  #把原来每个Factor的Level名字的list，替换为其对应的系数值
  part.worths[index.for.attribute] <- 
    list(as.numeric(c(conjoint.results$coefficients[
      begin.index.for.coefficient:end.index.for.coefficient],
      last.part.worth)))
  
  #把所有的Factor的系数打平，拼接在一个向量里
  #Note: 一个有名字的list，打平unlist后，会变成命名向量，命名规则是listname下标号，如brand1, brand2, brand3
  part.worth.vector <- 
    c(part.worth.vector,unlist(part.worths[index.for.attribute]))    
  } 
conjoint.results$part.worths <- part.worths

# compute standardized part-worths
# 标准化：这是很有用的一种方式：每个元素减去均值在除以标准差
# 这样，即时不同度量单位的Factor，都放在一个统一的度量单位里进行比较。
# standardize(x):x 是一个向量，返回一个向量
standardize <- function(x) {(x - mean(x)) / sd(x)}

#part.worths 是一个list
#lapply通过把list的每一个元素应用到函数standardrize，即brand、startup...这些向量传入standardrize。
conjoint.results$standardized.part.worths <- 
  lapply(conjoint.results$part.worths,standardize)
 
# compute and store part-worth ranges for each attribute 
part.worth.ranges <- conjoint.results$contrasts #获取list的结构以及元素名字，下面的操作将替换元素内容
for(index.for.attribute in seq(along=conjoint.results$contrasts)) 
  part.worth.ranges[index.for.attribute] <- 
  dist(range(conjoint.results$part.worths[index.for.attribute]))
conjoint.results$part.worth.ranges <- part.worth.ranges

sum.part.worth.ranges <- sum(as.numeric(conjoint.results$part.worth.ranges))

# compute and store importance values for each attribute 
attribute.importance <- conjoint.results$contrasts
for(index.for.attribute in seq(along=conjoint.results$contrasts)) 
  attribute.importance[index.for.attribute] <- 
  (dist(range(conjoint.results$part.worths[index.for.attribute]))/
  sum.part.worth.ranges) * 100
conjoint.results$attribute.importance <- attribute.importance
 
# data frame for ordering attribute names
# 按照importance降序排列，取出name，因为只有一列，所有返回vector
attribute.name <- names(conjoint.results$contrasts)
attribute.importance <- as.numeric(attribute.importance)
temp.frame <- data.frame(attribute.name,attribute.importance)
conjoint.results$ordered.attributes <- 
  as.character(temp.frame[sort.list(
  temp.frame$attribute.importance,decreasing = TRUE),"attribute.name"])

# respondent internal consistency added to list structure
conjoint.results$internal.consistency <- summary(main.effects.model.fit)$r.squared 
 
print.digits <- 2  # set number of digits on print and spine chart
 
# user-defined function for printing conjoint measures
if (print.digits == 2) 
  pretty.print <- function(x) {sprintf("%1.2f",round(x,digits = 2))} 
if (print.digits == 3) 
  pretty.print <- function(x) {sprintf("%1.3f",round(x,digits = 3))} 
 
# report conjoint measures to console 
# use pretty.print to provide nicely formated output
for(k in seq(along=conjoint.results$ordered.attributes)) {
  cat("\n","\n")
  cat(conjoint.results$ordered.attributes[k],"Levels: ",
  unlist(conjoint.results$xlevels[conjoint.results$ordered.attributes[k]]))
  
  cat("\n"," Part-Worths:  ")
  cat(pretty.print(unlist(conjoint.results$part.worths
    [conjoint.results$ordered.attributes[k]])))
    
  cat("\n"," Standardized Part-Worths:  ")
  cat(pretty.print(unlist(conjoint.results$standardized.part.worths
    [conjoint.results$ordered.attributes[k]])))  
    
  cat("\n"," Attribute Importance:  ")
  cat(pretty.print(unlist(conjoint.results$attribute.importance
    [conjoint.results$ordered.attributes[k]])))
  }

# plotting of spine chart begins here

# user-defined function for plotting descriptive attribute names 
effect.name.map <- function(effect.name) { 
  if(effect.name=="brand") return("Mobile Service Provider")
  if(effect.name=="startup") return("Start-up Cost")
  if(effect.name=="monthly") return("Monthly Cost")
  if(effect.name=="service") return("Offers 4G Service")
  if(effect.name=="retail") return("Has Nearby Retail Store")
  if(effect.name=="apple") return("Sells Apple Products")
  if(effect.name=="samsung") return("Sells Samsung Products")
  if(effect.name=="google") return("Sells Google/Nexus Products")
  } 

# all graphical output is routed to external pdf file
pdf(file = "fig_preference_mobile_services_results.pdf", width=8.5, height=11)
spine.chart(conjoint.results)
dev.off()  # close the graphics output device

# Suggestions for the student:
# Enter your own rankings for the product profiles and generate
# conjoint measures of attribute importance and level part-worths.
# Note that the model fit to the data is a linear main-effects model.
# See if you can build a model with interaction effects for service
# provider attributes.

