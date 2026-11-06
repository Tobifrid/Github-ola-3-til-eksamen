library(dplyr)
df_cars <- data.frame(mtcars)
head(mtcars,6)
structure(df_cars)
nrow(df_cars)
ncol(df_cars)
dim(df_cars)
mean(df_cars$mpg)
max(df_cars$hp)
min(df_cars$hp)
select(df_cars,,mpg)
select(df_cars,,mpg|hp)
head(df_cars,5)
df_cars[1:5,]
df_cars[df_cars$hp>=150, ]
df_cars[df_cars$cyl>=6,]
#opgave 3.1
df_cars[df_cars$mpg>20, ]
#opgave 3.2
df_cars[df_cars$cyl== 8, ]
#opgave 3.3
df_cars[df_cars$hp>=150, ]
#opgave 3.4
df_cars[df_cars$cyl>5 & df_cars$mpg<20, ]
#opgave 3.5
df_cars[df_cars$cyl==4 | df_cars$cyl==8, ]
df_cars[df_cars$cyl %in% c(4,8)]
#opgave 4.1
select(df_cars,,mpg|hp|wt|cyl)

#opgave 4.2
arrange(df_cars,desc(mpg))
#opgave 4.3
arrange(df_cars,desc(hp))
#opgave 4.4

df_cars[order(df_cars$mpg,decreasing=T), ][1:5, ]

#opgave 4.5
select(df_cars,!disp&!carb)

#opgave 5.1 man kan skrive pipe som cmd + shit + M
df_cars <- df_cars |>  
  mutate(kw_hp = hp * 0.7457)
print(df_cars$kw_hp)
#opgave 5.2
df_cars <- df_cars|>
  mutate(weight_kg=wt*(1000*0.453592))
print(df_cars$weight_kg)
#opgave 5.3 opret så kun mpg der er højere end 20 
df_cars <- df_cars |>  
  mutate(high_mpg = mpg > 20)
print(df_cars$high_mpg)

df_cars <- df_cars |>
  mutate(high_mpg = ifelse(df_cars$mpg > 20, "Over 20", "20 eller under"))  
print(df_cars)

#opgave 5.4
head(df_cars,10)

#opgave 6.1 gennemsnit for for mpg
mean(df_cars$mpg)

#opgave 6.2 median for mpg
median(df_cars$mpg)

#opgave 6.3 standartafvigelse for mpg
sd(df_cars$mpg)

#opgave 6.4 minimum MPG
min(df_cars$mpg)

#opgave 6.5 max mpg
max(df_cars$mpg)

#opgave 6.6 observationer
length(df_cars$mpg)
# eller str, str giver bare mere information

#ekstra opgave få vist første og trede kvartil
summary(df_cars$mpg)
#median vil være kvartil 2 og de sidste 75% vil være kvartil 3

#opgave 7.1
df_cars <- df_cars = order_by(cyl)
