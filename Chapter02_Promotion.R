# Predictive Model for Los Angeles Dodgers Promotion and Attendance (R)

library(car)  # special functions for linear regression
library(lattice)  # graphics package

# read in data and create a data frame called dodgers
dodgers <- read.csv("dodgers.csv")
print(str(dodgers))  # check the structure of the data frame

# define an ordered day-of-week variable 
# for plots and data summaries
# 按星期进行排序，如果不转换成factor，那么将按字典序排列
# 本句新创建一列，把星期变换为数值，在下一句，把该列变换为factor
# 这样排序按照数值进行排列，而作图时，会显示factor的label。

# ifelse(b, u, v) 函数是类似于SQL case when语句, b, u, v都是向量，b是布尔向量
dodgers$ordered_day_of_week <- with(data=dodgers,
  ifelse ((day_of_week == "Monday"),1,
  ifelse ((day_of_week == "Tuesday"),2,
  ifelse ((day_of_week == "Wednesday"),3,
  ifelse ((day_of_week == "Thursday"),4,
  ifelse ((day_of_week == "Friday"),5,
  ifelse ((day_of_week == "Saturday"),6,7)))))))
  
dodgers$ordered_day_of_week <- factor(dodgers$ordered_day_of_week, levels=1:7,
labels=c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun"))

# exploratory data analysis with standard graphics: attendance by day of week
# To Do: 研究这里的plot为什么会画boxplot？
with(data=dodgers,plot(ordered_day_of_week, attend/1000, 
xlab = "Day of Week", ylab = "Attendance (thousands)", 
col = "violet", las = 1))

#上面这个plot 等价于
# with(data=dodgers,boxplot(attend/1000 ~ ordered_day_of_week, 
# xlab = "Day of Week", ylab = "Attendance (thousands)", 
# col = "violet", las = 1))

# when do the Dodgers use bobblehead promotions
with(dodgers, table(bobblehead,ordered_day_of_week)) # bobbleheads on Tuesday

# define an ordered month variable 
# for plots and data summaries
dodgers$ordered_month <- with(data=dodgers,
  ifelse ((month == "APR"),4,
  ifelse ((month == "MAY"),5,
  ifelse ((month == "JUN"),6,
  ifelse ((month == "JUL"),7,
  ifelse ((month == "AUG"),8,
  ifelse ((month == "SEP"),9,10)))))))
dodgers$ordered_month <- factor(dodgers$ordered_month, levels=4:10,
labels = c("April", "May", "June", "July", "Aug", "Sept", "Oct"))

# exploratory data analysis with standard R graphics: attendance by month 
with(data=dodgers,plot(ordered_month,attend/1000, xlab = "Month", 
ylab = "Attendance (thousands)", col = "light blue", las = 1))

# exploratory data analysis displaying many variables
# looking at attendance and conditioning on day/night
# the skies and whether or not fireworks are displayed
library(lattice) # used for plotting 
# let us prepare a graphical summary of the dodgers data
group.labels <- c("No Fireworks","Fireworks")
group.symbols <- c(21,24)
group.colors <- c("black","black") 
group.fill <- c("black","red")
xyplot(attend/1000 ~ temp | skies + day_night, 
    data = dodgers, groups = fireworks, pch = group.symbols, 
    aspect = 1, cex = 1.5, col = group.colors, fill = group.fill,
    layout = c(2, 2), type = c("p","g"),
    strip=strip.custom(strip.levels=TRUE,strip.names=FALSE, style=1),
    xlab = "Temperature (Degrees Fahrenheit)", 
    ylab = "Attendance (thousands)",
    key = list(space = "top", 
        text = list(rev(group.labels),col = rev(group.colors)),
        points = list(pch = rev(group.symbols), col = rev(group.colors),
        fill = rev(group.fill))))  
# xyplot(attend/1000 ~ temp | skies + day_night,  # y轴 attend/1000, x轴temp，按(skies, day_night)分组条件（术语：conditioning variables），每一组一个panel画图；| 后边表示分组条件，一般都是factor
#     data = dodgers, 
#     groups = fireworks, # 每一个panel内的分组条件（术语：grouping variable），用于区别不同点，通过str(dodgers)查看，第一个值是No，第二个值是Yes
#     pch = group.symbols,# 指定groups后，指明第一个组的点的形状和第二个组的形状
#     col = group.colors, fill = group.fill, #指明第一第二的线条和填充颜色
#     aspect = 1, cex = 1.5, # aspect: panel的纵横比；cex：每个panel内点的大小乘以1.5
#     layout = c(2, 2), # 因为是画多个panel，所有可以通过layout这个向量进行排版设置(column, row, page), where page is optional, 
#     type = c("p","g"), #p表示point；g表示grid，要画栅格线
#     strip=strip.custom(strip.levels=TRUE,strip.names=FALSE, style=1), # 一个布尔值，决定是否要标示（skies，day_night)在每一组的值，默认显示在panel上方。
#																		# strip.custom用于进行设定strip的风格：strip.levels 是分组条件的值，TRUE表示要显示分组条件的值，FALSE不显示
#																		# strip.names是分组条件的名称（列名，变量名），TRUE显示，FALSE不显示
#																		# style：一般=1
#     xlab = "Temperature (Degrees Fahrenheit)", 
#     ylab = "Attendance (thousands)",
#     key = list(space = "top",                                         # 当指定groups，需要用key来设定legend：space：legend显示在顶部
#         text = list(rev(group.labels),col = rev(group.colors)),       # text：指定legend的文本说明，一个list; 另外，调用rev是返回向量的逆序，这里的目的是想把Yes的显示在No前面
#         points = list(pch = rev(group.symbols), col = rev(group.colors), #points：逻辑变量，是否显示点。用list设置风格：legend的点的形状颜色等风格
#         fill = rev(group.fill))))  
		
# attendance by opponent and day/night game
group.labels <- c("Day","Night")
group.symbols <- c(1,20)
group.symbols.size <- c(2,2.75)
bwplot(opponent ~ attend/1000, data = dodgers, groups = day_night, 
    xlab = "Attendance (thousands)",
    panel = function(x, y, groups, subscripts, ...) 
       {panel.grid(h = (length(levels(dodgers$opponent)) - 1), v = -1)
        panel.stripplot(x, y, groups = groups, subscripts = subscripts, 
        cex = group.symbols.size, pch = group.symbols, col = "darkblue")
       },
    key = list(space = "top", 
    text = list(group.labels,col = "black"),
    points = list(pch = group.symbols, cex = group.symbols.size, 
    col = "darkblue")))
     
# employ training-and-test regimen for model validation
set.seed(1234) # set seed for repeatability of training-and-test split
training_test <- c(rep(1,length=trunc((2/3)*nrow(dodgers))),
rep(2,length=(nrow(dodgers) - trunc((2/3)*nrow(dodgers)))))
dodgers$training_test <- sample(training_test) # random permutation 
dodgers$training_test <- factor(dodgers$training_test, 
  levels=c(1,2), labels=c("TRAIN","TEST"))
dodgers.train <- subset(dodgers, training_test == "TRAIN")
print(str(dodgers.train)) # check training data frame
dodgers.test <- subset(dodgers, training_test == "TEST")
print(str(dodgers.test)) # check test data frame

# specify a simple model with bobblehead entered last
my.model <- {attend ~ ordered_month + ordered_day_of_week + bobblehead}

# fit the model to the training set
train.model.fit <- lm(my.model, data = dodgers.train)
# summary of model fit to the training set
print(summary(train.model.fit))
# training set predictions from the model fit to the training set
dodgers.train$predict_attend <- predict(train.model.fit) 

# test set predictions from the model fit to the training set
dodgers.test$predict_attend <- predict(train.model.fit, 
  newdata = dodgers.test)

# compute the proportion of response variance
# accounted for when predicting out-of-sample
cat("\n","Proportion of Test Set Variance Accounted for: ",
round((with(dodgers.test,cor(attend,predict_attend)^2)),
  digits=3),"\n",sep="")

# merge the training and test sets for plotting
dodgers.plotting.frame <- rbind(dodgers.train,dodgers.test)

# generate predictive modeling visual for management
group.labels <- c("No Bobbleheads","Bobbleheads")
group.symbols <- c(21,24)
group.colors <- c("black","black") 
group.fill <- c("black","red")  
xyplot(predict_attend/1000 ~ attend/1000 | training_test, 
       data = dodgers.plotting.frame, groups = bobblehead, cex = 2,
       pch = group.symbols, col = group.colors, fill = group.fill, 
       layout = c(2, 1), xlim = c(20,65), ylim = c(20,65), 
       aspect=1, type = c("p","g"),
       panel=function(x,y, ...)
            {panel.xyplot(x,y,...)
             panel.segments(25,25,60,60,col="black",cex=2)
            },
       strip=function(...) strip.default(..., style=1),
       xlab = "Actual Attendance (thousands)", 
       ylab = "Predicted Attendance (thousands)",
       key = list(space = "top", 
              text = list(rev(group.labels),col = rev(group.colors)),
              points = list(pch = rev(group.symbols), 
              col = rev(group.colors),
              fill = rev(group.fill))))            
        
# use the full data set to obtain an estimate of the increase in
# attendance due to bobbleheads, controlling for other factors 
my.model.fit <- lm(my.model, data = dodgers)  # use all available data
print(summary(my.model.fit))
# tests statistical significance of the bobblehead promotion
# type I anova computes sums of squares for sequential tests
print(anova(my.model.fit))  

cat("\n","Estimated Effect of Bobblehead Promotion on Attendance: ",
round(my.model.fit$coefficients[length(my.model.fit$coefficients)],
digits = 0),"\n",sep="")

# standard graphics provide diagnostic plots
plot(my.model.fit)

# additional model diagnostics drawn from the car package
library(car)
residualPlots(my.model.fit)
marginalModelPlots(my.model.fit)
print(outlierTest(my.model.fit))

# Suggestions for the student:
# Examine regression diagnostics for the fitted model.
# Examine other linear predictors and other explanatory variables.
# See if you can improve upon the model with variable transformations.



