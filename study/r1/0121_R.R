# 연령대 및 종교 유뮤에 따른 이혼율
library(dplyr)
ageg_marriage <- df %>%
  filter(!is.na(marriage2)) %>%
  group_by(ageg,marriage2) %>%
  summarise(cnt=n()) %>%
  mutate(hap=sum(cnt)) %>%
  mutate(pct = round(cnt/hap*100, 1)) %>%
  head(6)

ageg_divorce <- ageg_marriage %>%
  filter(marriage2=='이혼') %>%
  select(ageg, pct)
ageg_divorce
library(ggplot2)
ggplot(data=ageg_divorce,aes(x=ageg, y=pct))+geom_col()

ageg_religion_marriage <- df %>%
  filter(!is.na(marriage2) & !(is.na(ageg))) %>%
  group_by(ageg,religion2,marriage2) %>%
  summarise(cnt=n()) %>%
  mutate(hap=sum(cnt)) %>%
  mutate(pct = round(cnt/hap*100, 1))
ageg_religion_marriage

ageg_religion_divorce <- ageg_religion_marriage %>%
  filter(marriage2=='이혼') %>%
  select(ageg, religion2, pct)
ageg_religion_divorce
ggplot(data=ageg_religion_divorce,aes(x=ageg, y=pct, fill = religion2))+geom_col(position = 'dodge')

# 지역별 연령대 비율
summary(df$code_region)
table(df$code_region)

list_region <- data.frame(code_region=c(1:7),region=c('서울',
                                                      '수도권(인천/경기)',
                                                      '부산/경남/울산',
                                                      '대구/경북',
                                                      '대전/충남',
                                                      '강원/충북',
                                                      '광주/전남/전북/제주도'))
list_region

df<-left_join(df,list_region, id = "code_region")
df
head(df$region)
tail(df$region)
region_ageg <- df %>%
  filter(!is.na(ageg)) %>%
  group_by(region, ageg) %>%
  summarise(cnt=n()) %>%
  mutate(hap=sum(cnt)) %>%
  mutate(pct=round(cnt/hap*100,1))
list_order <- region_ageg %>% filter(ageg=='노년') %>% arrange(desc(pct))
ggplot(data = region_ageg, aes(x=region,y = pct, fill = ageg))+geom_col()+coord_flip()+scale_x_discrete(region_ageg$ageg)
region_ageg$ageg <- factor(region_ageg$ageg, levels = c("노년",'중년','초년'))


df <- read.csv('d1.csv')
df

plot(df$HOUR,df$SCORE)
cor(df$HOUR,df$SCORE)

# 귀무가설
# 대립가설 : 연구자의 주장
# 유의확률 : 귀무가설이 참이라는 가정아래 얻은 통계량, p-value가 적을수록(일반적으로 0.05) 대립가설이 의미를 갖는다.
cor.test(df$HOUR,df$SCORE)
#---- 수치형 변수의 검정
economics
plot(economics$pce,economics$unemploy)
cor.test(economics$pce,economics$unemploy)

#---------
df <- read.csv('heights.csv')
df
plot(df$father, df$son)
cor.test(df$father, df$son)

# 범주형 변수와 수치형 변수의 검정
df <- read.csv('TEAM_SCORE.csv')
df
aggregate(SCORE~TEAM, data=df,mean)
boxplot(SCORE~TEAM,data=df)
# t검정
result <- aov(SCORE~TEAM,data=df)
summary(result)
t.test(SCORE~TEAM,data=df, var.equal = T)

#-----------

bstudy <- c(34,76,76,63,73,75,67,78,81,53,58,81,77,80,43,65,76,63,54,64,85,54,70,71,71,55,40,78,79,100,51,93,64,42,63,61,82,67,98,59,63,84,50,67,80,83,66,86,57,48)

astudy <- c(74,87,89,98,65,82,70,70,70,84,56,76,72,69,73,61,83,82,89,75,48,72,80,66,82,71,49,54,70,65,74,63,65,101,82,75,62,83,90,76,87,90,78,63,59,79,74,65,77,74)
length(bstudy)
length(astudy)
summary(bstudy)
summary(astudy)
boxplot(bstudy,astudy,names = c('수강전', '수강후'))
t.test(bstudy,astudy,var.equal = T)

#------------
df1 <- mpg %>% filter(class %in% c('compact','suv')) %>% select(class, cty)
summary(df1)
t.test(data = df1,cty~class, var.equal = T)

# 324p 혼자해보기
mpg$tot = (mpg$hwy+mpg$cty)/2
mpg$tot

aggregate(data = mpg[mpg$class=="compact" | mpg$class=="suv",],mpg$tot~mpg$class,mean)

# 331p 혼자해보기
# 1번
class(mpg$drv)
mode(mpg$drv)

# 2번
mpg$drv <- as.factor(mpg$drv)
class(mpg$drv)

# 3번 
levels(mpg$drv)
 m
