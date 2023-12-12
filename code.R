library(tidyverse)
library(dplyr)
library(ggcorrplot)
library(gridExtra)
library(car)
library(sets)
library(glue)




d <- read_csv("D:/Projects/Self Project/3/CO2 Emissions_Canada.csv",
              show_col_types = F)
# setwd('D:/Projects/Self Project/3')


# Basic observation
dim(d)
d %>% is.na() %>% sum()
glimpse(d)
summary(d)
View(d)
# colnames(d) <- gsub(' ','_',colnames(d))


d %>% select(where(is.character)) %>% 
  colnames() -> char_var
d$Cylinders <- as.factor(d$Cylinders)


for(i in char_var){
  d %>% pull(i) %>% unique() %>% length() -> l
  glue('{v}: {c}', v = i, c = l) %>% print()
}


# Correlation plot:
d %>% select(where(is.numeric)) %>% 
  cor() %>% ggcorrplot(lab = T, type = 'upper')



# distribution of response: (log version)
d %>% mutate(`CO2 Emissions(g/km) (log)` = 
               log(`CO2 Emissions(g/km)`)) -> d

response_plot <- function(var, arg){
  d %>% select({{var}}) %>% scale() %>% 
    as.data.frame() %>% 
    ggplot(aes({{var}})) + 
    geom_histogram(aes(y = after_stat(density)),
                   fill = 'lightgreen', colour = 'black') +
    stat_function(fun = dnorm, args = list(0,1), 
                  colour = 'blue', lty = 2, lwd = 1) +
    theme_minimal() -> p
  if(arg == 1){
    p + labs(title = 'Without Log')
  }else{
    p + labs(title = 'With Log')
  }
}

response_plot(`CO2 Emissions(g/km)`, 1) -> p1
response_plot(`CO2 Emissions(g/km) (log)`, 2) -> p2
grid.arrange(p1,p2, ncol = 2)


# qqnorm(d$`CO2 Emissions(g/km)`)
# abline(a = 0, b = 1)


# Frequency distribution of categorical variables:
cat_plot <- function(var){
  d %>% count({{var}}) %>% 
    ggplot(aes(x = {{var}}, y = n)) +
    geom_bar(stat = 'identity', width = 0.4, 
             fill = 'magenta', colour = 'black') +
    geom_text(aes(label = n, vjust = -0.3)) +
    labs(y = 'Count') + theme_minimal() +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5),
          axis.title = element_text(face = 'bold',
                                    size = 20))
}

attach(d)
cat_plot(Make)
# cat_plot(Model)
cat_plot(`Vehicle Class`)
cat_plot(Transmission)
cat_plot(`Fuel Type`)
cat_plot(Cylinders)
detach(d)

# Categorical analysis (re-leveling the variables)
d1 <- d # saving a copy of the data

  # Transmission:
d1 %>% mutate(Transmission = case_when(
  Transmission %in% paste0('A',4:10, by = '') ~ 'A',
  Transmission %in% paste0('AM',5:9, by = '') ~ 'AM',
  Transmission %in% paste0('AS',4:10, by = '') ~ 'AS',
  Transmission %in% paste0('AV',c(6,7,8,10,''), by = '') ~ 'AV',
  Transmission %in% paste0('M',5:7, by = '') ~ 'M',
)) -> d1

  # Vehicle class:
d1 %>% mutate(`Vehicle Class` = case_when(
  `Vehicle Class` %in% c('COMPACT', 'MINICOMPACT', 
                         'SUBCOMPACT', 'MID-SIZE', 'FULL-SIZE', 
                         'TWO-SEATER') ~ 'Automobiles and Two-Seater',
  `Vehicle Class` %in% c('SUV - SMALL', 'SUV - STANDARD') ~ 'SUV',
  `Vehicle Class` %in% c('PICKUP TRUCK - SMALL', 
                         'PICKUP TRUCK - STANDARD') ~ 'Pickup Truck',
  `Vehicle Class` %in% c('STATION WAGON - SMALL', 
                         'STATION WAGON - MID-SIZE') ~ 'Station Wagons',
  `Vehicle Class` %in% c('VAN - CARGO', 'VAN - PASSENGER', 
                        'MINIVAN') ~ 'Vans',
  `Vehicle Class` == 'SPECIAL PURPOSE VEHICLE' ~ 'Special Purpose Vehicles' 
)) -> d1

  # Fuel Type:
d1$`Fuel Type`[d$`Fuel Type` %in% c('D','E','N')] <- 'D_E_N'

  # Makes:
d1 %>% mutate(Make = case_when(
  Make %in% c('ASTON MARTIN', 'BENTLEY', 'BUGATTI', 
              'FERRARI', 'LAMBORGHINI', 'MASERATI', 
              'ROLLS-ROYCE') ~ 'Luxury',
  Make %in% c('ALFA ROMEO', 'AUDI', 'BMW', 'CADILLAC', 
              'INFINITI', 'JAGUAR', 'LAND ROVER', 'LEXUS', 
              'MERCEDES-BENZ', 'PORSCHE') ~ 'Premium',
  Make %in% c('ACURA', 'GENESIS', 'LINCOLN', 
              'VOLVO') ~ 'Midrange',
  Make %in% c('CHEVROLET', 'FORD', 'GMC', 'HONDA', 
              'HYUNDAI', 'KIA', 'MAZDA', 'MITSUBISHI', 
              'NISSAN', 'SUBARU', 'TOYOTA', 
              'VOLKSWAGEN') ~ 'Mainstream',
  Make %in% c('FIAT', 'MINI', 'SMART') ~ 'Entry Level',
  Make %in% c('DODGE', 'RAM', 'SRT', 'SCION', 
              'SUBARU', 'VOLKSWAGEN') ~ 'Performance',
  Make %in% c('BUICK','CHRYSLER','JEEP') ~ 'Others'
)) -> d1

d1 %>% count(Make)

  # Model drop:
d1 %>% select(-Model) -> d1

  # Cylinders:
d1 %>% mutate(Cylinders = case_when(
  Cylinders %in% 3:4 ~ '<5',
  Cylinders %in% 5:6 ~ '5-6',
  Cylinders %in% 8:16 ~ '>6'
)) -> d1


glimpse(d1)
View(d1)
#==================================================================
# Response ~ character variables:
char_cont <- function(var){
  d %>% ggplot(aes(x = {{var}}, y = `CO2 Emissions(g/km)`)) +
    geom_boxplot(fill = 'lightblue', outlier.color = 'red',
                 outlier.size = 1) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90,
                                     hjust = 1,
                                     vjust = 0.5),
          axis.title = element_text(face = 'bold',
                                    size = 20),
          axis.text = element_text(face = 'bold'))
}

attach(d)
char_cont(`Fuel Type`)
char_cont(`Vehicle Class`)
char_cont(Transmission)
char_cont(Make)
char_cont(Cylinders)
detach(d)


# Response ~ continuous variables:
cont_pair_plot <- function(res_var){
  cont_plot <- function(var){
    d %>% ggplot(aes(x = {{var}}, 
                     y = {{res_var}})) +
      geom_point(colour = 'darkviolet', size = 1) + theme_minimal() +
      theme(axis.title = element_text(face = 'bold'))
  }
  
  cont_plot(`Engine Size(L)`) -> p1
  cont_plot(`Fuel Consumption City (L/100 km)`) -> p2
  cont_plot(`Fuel Consumption Hwy (L/100 km)`) -> p3
  cont_plot(`Fuel Consumption Comb (L/100 km)`) -> p4
  cont_plot(`Fuel Consumption Comb (mpg)`) -> p5
  
  grid.arrange(p1,p2,p3,p4,p5, ncol = 3)
}

cont_pair_plot(`CO2 Emissions(g/km)`) # without log transformation
cont_pair_plot(`CO2 Emissions(g/km) (log)`) # with log transformation


{
# Multicollinearity:
l <- lm(`CO2 Emissions(g/km)` ~ .-Make - `Vehicle Class` -
          `Engine Size(L)` - Cylinders - Transmission - 
          `Fuel Type`,
        data = d1)
summary(l)

v <- vif(l); v
}


# To make the relationship between the continuous variables linear 
# we need to do the following transformations:

d1 %>% mutate(`Engine Size(L)` = log(`Engine Size(L)`),
              `Fuel Consumption City (L/100 km)` = 
                log(`Fuel Consumption City (L/100 km)`),
              `Fuel Consumption Hwy (L/100 km)` = 
                log(`Fuel Consumption Hwy (L/100 km)`),
              `Fuel Consumption Comb (L/100 km)` = 
                log(`Fuel Consumption Comb (L/100 km)`),
              `Fuel Consumption Comb (mpg)` = 
                log(`Fuel Consumption Comb (mpg)`)) -> d2

plot(d2$`Fuel Consumption Comb (L/100 km)`,
     d2$`CO2 Emissions(g/km) (log)`, pch = 20)



# Function for R^2 printing:-
r2_print <- function(r1, r2, s){
  glue('R^2 ({s}): {r1}', 
       'R^2 Adjusted ({s}): {r2}', .sep = '\n',
       r1 = round(r1, 4), r2 = round(r2, 4))
}


# ANOVA:--------------------------------------------------------
summary(aov(d$`CO2 Emissions(g/km) (log)` ~ d2$Transmission))









## Train - Test Splitting:----------------------------------------
n = nrow(d2); set.seed(42)
rs <- sample(1:n, size = 0.8*n, replace = F)
train_data <- d2[rs,]
test_data <- d2[-rs,]


glue('Dimension of Training data: {d1}',
     'Dimension of Testing data: {d2}',
     .sep = '\n', 
     d1 = paste0(dim(train_data)[1], 'x', dim(train_data)[2]), 
     d2 = paste0(dim(test_data)[1], 'x', dim(test_data)[2]))


# fitting a full model:
l <- lm(`CO2 Emissions(g/km) (log)` ~ .
        -`CO2 Emissions(g/km)`, data = train_data)

car::vif(l)


l <- lm(`CO2 Emissions(g/km) (log)` ~ .
        -`CO2 Emissions(g/km)` - `Fuel Consumption Comb (L/100 km)`, 
        data = train_data)

car::vif(l)


l <- lm(`CO2 Emissions(g/km) (log)` ~ .
        -`CO2 Emissions(g/km)` - `Fuel Consumption Comb (L/100 km)`-
          `Fuel Consumption Comb (mpg)`, 
        data = train_data)

car::vif(l)

summary(l)

train_data['Residuals'] <- l$residuals
train_data['Emission (predicted) (log)'] <- l$fitted.values


train_data %>% ggplot(aes(x = `Emission (predicted) (log)`,
                           y = Residuals)) + 
  geom_point(colour = 'darkgreen', size = 0.7) + 
  geom_hline(yintercept = 0, lty = 2, colour = 'darkblue') +
  theme_minimal()


train_data %>% 
  filter(Residuals > -0.4) %>% 
  select(-c(Residuals, `Emission (predicted) (log)`)) ->
  train_data2










## Model fitting :::
# Forward selection:----------------------------------------------
null_model <- lm(`CO2 Emissions(g/km) (log)` ~ 1, data = train_data)
full_model <- lm(`CO2 Emissions(g/km) (log)` ~ .
                 -`CO2 Emissions(g/km)`, data = train_data)

forward <- step(null_model, direction = 'forward', 
                scope = formula(full_model), trace = 0); forward



r <- forward$residuals
f <- forward$fitted.values

plot(f, r, pch = 20)


# R^2
summary(forward) -> sf

r2_print(sf$r.squared, sf$adj.r.squared, 'forward')



# Best subset selection:---------------------------------------

# n <- 10
d3 <- train_data  # saving a copy of the training data
colnames(d3) <- paste0('x',1:12, by = '')
col_names <- colnames(d3)[1:10]

M <- matrix(ncol = n+2, nrow = 2^n)
colnames(M) <- c(colnames(d3)[1:10], 'Rsq', 'Adj Rsq')
rownames(M) <- paste(1:(2^n))

M[1,] <- rep(FALSE, n+2)

colnames(M)

for(i in 2:(2^n)){
  x <- col_names %>% set_power() %>% unclass() %>% 
    .[[i]] %>% as.character()
  
  reformulate(x %>% 
                paste0(collapse = '+'),
              response = 'x12') %>% 
    lm(data = train_data) %>% summary() -> s
  
  M[i,] <- c((col_names %in% x), s$r.squared, 
             s$adj.r.squared)
}


M <- as.data.frame(M)
r <- M[which.max(M$`Adj Rsq`),]

r2_print(r$Rsq, r$`Adj Rsq`, 'Best subset')

# write.csv(M, file = 'best_selection_matrix.csv')

# comment: Exclude 9 th feature.

plot(1:1024, M$`Adj Rsq`, type = 'l')



# Backward subset selection:------------------------------------
backward <- step(full_model, direction = 'backward', 
                scope = formula(full_model), trace = 0); backward


r <- forward$residuals
f <- forward$fitted.values

plot(f, r, pch = 20)

# R^2
summary(backward) -> sb

r2_print(sb$r.squared, sb$adj.r.squared, 'backward')
#================================================================

### Prediction:---------------------------------train data
train_data['Emission (predicted) (log)'] <- predict(forward)
train_data['Residuals'] <- residuals(forward)



# distribution of the predicted and observed values:
train_data %>% select(`CO2 Emissions(g/km) (log)`, 
                     `Emission (predicted) (log)`) %>% 
  pivot_longer(everything(),
               names_to = "Type", values_to = 'Values')%>%
  ggplot(aes(Values, fill = Type, colour = Type)) + 
  geom_density(alpha = 0.4) + theme_minimal()

# residual plot
train_data %>% ggplot(aes(x = `Emission (predicted) (log)`,
                         y = Residuals)) + 
  geom_point(colour = 'darkgreen', size = 0.7) + 
  geom_hline(yintercept = 0, lty = 2, colour = 'darkblue') +
  theme_minimal()



# variables in forward:
# `Fuel Consumption Comb (mpg)` + `Fuel Type` + `Engine Size(L)` +
# `Vehicle Class` + Make + Transmission + Cylinders + 
# `Fuel Consumption Hwy (L/100 km)` 

# Comment: Many outliers are there, we first remove it from the data.
# View(train_data)
train_data2 <- train_data %>% filter(Residuals > -0.4)


# re-fitting the after removing the outlier:----------------------
forward2 <- lm(`CO2 Emissions(g/km) (log)` ~ 
                 `Fuel Consumption Comb (mpg)` + `Fuel Type` + 
      `Engine Size(L)` + `Vehicle Class` + Make + 
        Transmission + Cylinders + `Fuel Consumption Hwy (L/100 km)`,
      data = train_data2)
summary(forward2)


train_data2['Emission (predicted) (log)'] <- predict(forward2)
train_data2['Residuals2'] <- residuals(forward2)



# distribution of the predicted and observed values:
train_data2 %>% select(`CO2 Emissions(g/km) (log)`, 
                      `Emission (predicted) (log)`) %>% 
  pivot_longer(everything(),
               names_to = "Type", values_to = 'Values')%>%
  ggplot(aes(Values, fill = Type, colour = Type)) + 
  geom_density(alpha = 0.4) + theme_minimal()

# residual plot
train_data2 %>% ggplot(aes(x = `Emission (predicted) (log)`,
                          y = Residuals2)) + 
  geom_point(colour = 'darkgreen', size = 0.7) + 
  geom_hline(yintercept = 0, lty = 2, colour = 'darkblue') +
  theme_minimal()

qqnorm(train_data2$Residuals2, pch = 20)
qqline(train_data2$Residuals2, lwd = 2, lty = 2, col = 'blue')

# Comment: No Heteroscedasticity

train_data2 %>% mutate(Index = 1:nrow(train_data2)) %>% 
  ggplot(aes(x = Index, y = Residuals2)) + 
  geom_point(col = 'red', size = 0.5) + theme_minimal() +
  geom_hline(yintercept = 0, colour = 'blue', linetype = 2) +
  scale_x_continuous(n.breaks = 10)
# Comment: No autocorrelation.




# prediction on test data:-----------------------------------------
test_data['Emission (pred) (log)'] <- predict(l, newdata = test_data,
                                              type = 'response')

colnames(test_data)


test_data['Residuals']

cor(test_data$`Emission (predicted) (log)`,
    test_data$`CO2 Emissions(g/km) (log)`)^2


plot(test_data$`Emission (predicted) (log)`,
     test_data$`CO2 Emissions(g/km) (log)`- test_data$`Emission (predicted) (log)`,
     pch = 20)



## -----
l = lm(abs(train_data2$Residuals2) ~ 
         train_data2$`Fuel Consumption Comb (mpg)`)
summary(l)

l = lm(abs(train_data2$Residuals2) ~ 
         train_data2$`Fuel Consumption Hwy (L/100 km)`)
summary(l)

l = lm(abs(train_data2$Residuals2) ~ 
         train_data2$`Engine Size(L)`)
summary(l)



train_data3 <- train_data2 # creating a copy of the data

train_data3$`Engine Size(L)` <- train_data3$`Engine Size(L)`/
  train_data3$`Fuel Consumption Hwy (L/100 km)`
train_data3$`Fuel Consumption Comb (mpg)` <- 
  train_data3$`Fuel Consumption Comb (mpg)`/
  train_data3$`Fuel Consumption Hwy (L/100 km)`
train_data3$`CO2 Emissions(g/km) (log)` <- 
  train_data3$`CO2 Emissions(g/km) (log)`/
  train_data3$`Fuel Consumption Hwy (L/100 km)`






forward3 <- lm(`CO2 Emissions(g/km) (log)` ~ 
                 `Fuel Consumption Comb (mpg)` + `Fuel Type` + 
                 `Engine Size(L)` + `Vehicle Class` + Make + 
                 Transmission + Cylinders,
               data = train_data3)
summary(forward3)




train_data3['Emission (predicted) (log)'] <- predict(forward3)
train_data3['Residuals3'] <- forward3$residuals


# residual plot:
train_data3 %>% ggplot(aes(x = `Emission (predicted) (log)`,
                           y = Residuals3)) + 
  geom_point(colour = 'darkgreen', size = 0.7) + 
  geom_hline(yintercept = 0, lty = 2, colour = 'darkblue') +
  theme_minimal()



#===================================================================
l = lm(abs(test_data$Residuals) ~ 
         test_data$`Fuel Consumption Comb (mpg)`)
summary(l)

l = lm(abs(test_data$Residuals) ~ 
         test_data$`Fuel Consumption Hwy (L/100 km)`)
summary(l)

l = lm(abs(test_data$Residuals) ~ 
         test_data$`Fuel Type`)
summary(l)

l = lm(abs(test_data$Residuals) ~ 
         test_data$Cylinders)
summary(l)

l = lm(abs(test_data$Residuals) ~ 
         test_data$Transmission)
summary(l)

l = lm(abs(test_data$Residuals) ~ 
         test_data$Make)
summary(l)


# heteroscedasticity is present








