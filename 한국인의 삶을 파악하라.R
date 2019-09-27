setwd("C:/Python/RBasic-master/RBasic-master/R_Data")
getwd()

# 한국인의 삶을 파악하라
#2006~2015년까지 전국에서 7000여 가구를 선정하여 매년 추적 조사한 자료
#데이터 셋 : Koweps_hpc10_2015_beta1.sav
#2016년도 발간한 복지패널 데이터 6,914가구, 16,664명에 대한 정보


install.packages("foreign")

library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

# 데이터 불러오기
dat_welfare <- read.spss("Koweps_hpc10_2015_beta1.sav", to.data.frame=T)
welfare <- dat_welfare

# 데이터 탐색해 보기
head(welfare)
tail(welfare)
View(welfare)
dim(welfare)
str(welfare)
summary(welfare)

# 컬럼명 재설정
welfare <- rename(welfare, 
                  sex=h10_g3,
                  birth=h10_g4,
                  marriage=h10_g10,
                  religion=h10_g11,
                  income=p1002_8aq1,
                  code_job=h10_eco9,
                  code_region=h10_reg7)
names(welfare)


# 살펴볼 컬럼 추출
welfare_m <- select(welfare, sex, birth, marriage, religion, income, code_job, code_region)
names(welfare_m)


head(welfare_m,10)
View(welfare_m)

# < 1 > 성별에 따른 월급 차이
str(welfare_m)
class(welfare_m$sex)
table(welfare_m$sex) # 1- 남자 , 2 - 여자, 9 - 응답없음

welfare_m$sex <- ifelse(welfare_m$sex == 9, NA, welfare_m$sex)
table(is.na(welfare_m$sex))  # 결측치 확인

welfare_m$sex <- ifelse(welfare_m$sex == 1, "male", "female")
table(welfare_m$sex)

# 성별 표
qplot(welfare_m$sex)

# 월급 확인
names(welfare_m)
hist(welfare_m$income)
summary(welfare_m$income)
### 자세히 보자.
qplot(welfare_m$income)
### 범위 축소
qplot(welfare_m$income) + xlim(0,1000)


# 수입 NA전처리
### 모름/무응답 = 9999
### 범위 1~9998 이므로 0도 결측치 처리
welfare_m$income <- ifelse(welfare_m$income %in% c(0,9999), NA, welfare$income)
table(is.na(welfare_m$income))
sex_income <- welfare_m %>% filter(!is.na(income)) %>% group_by(sex) %>% summarise(mean_income = mean(income))

sex_income
# !! 월급 평균 남자 312만원, 여자 163만원
# 월급 표 작성
ggplot(data = sex_income, aes(x=sex, y=mean_income)) + geom_col()

# --------------------------------------------------

# < 2 >  나이와 월급의 관계
#몇살때 월급을 가장 많이 받을까?
# 대상 변수 : 나이(birth), 월급(income)
# 2006~2015년까지 전국에서 7000여 가구를 선정하여 매년 추적 조사한 자료


# 변수 확인
class(welfare$birth)
class(welfare$income)

dim(welfare)

head(welfare)

summary(welfare$birth)
ggplot(welfare, aes(x=birth)) + geom_histogram()

# 결측치 확인 및 파생변수 생성
# 1900~2014사이의 값을 지니고, 모름/무응답은 9999로 코딩되어 있음.

table(is.na(welfare$birth))
## 만약 결측치가 있다면 다음과 같이 처리 가능
welfare$birth <- ifelse(welfare$birth==9999, NA, welfare$birth)
table(is.na(welfare$birth))


# 나이 변수 만들기
# 2015년도 조사 진행. 2015에서 연도를 뺀 후, 1년을 더하면 된다.
# age <- 2015 - welfare$birth + 1

welfare$age <- 2015 - welfare$birth + 1
welfare


ggplot(welfare, aes(x=age)) + geom_histogram()

# 나이(age)와 월급(income)의 관계 분석

# (가) 나이에 따른 월급 평균표만들기
age_income <- welfare %>% 
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income = mean(income))
head(age_income)

# (나) ggplot를 이용한 그래프 그리기
# x : age, y축 : 월급평균(mean_income)

ggplot(age_income, aes(x=age, y=mean_income)) + geom_line()

# 한국인의 삶을 파악하라(2) - 나이와 월급의 관계
# 어떤 연령의 월급이 가장 많이 받을까?
# 대상 변수 : 나이(birth)=> 연령대(class), 월급(mean_income)

welfare <- welfare %>%
  mutate(class=ifelse(age<25, 'young',
                      ifelse(age >= 59, "middle", "old")))
table(welfare$class)

ggplot(welfare, aes(x=class)) + geom_bar()

### 연령대별 월급 평균표 만들기 
class_income <- welfare %>% filter(!is.na(income)) %>%
  group_by(class) %>%
  summarise(mean_income = mean(income))
class_income
ggplot(class_income, aes(x=class, y=mean_income)) + geom_col()

# x축의 값을 초년, 중년, 노년의 나이 순으로 정렬하도록 설정
ggplot(class_income, aes(x=class, y=mean_income)) + geom_col() + 
  scale_x_discrete(limits = c('young', 'middle', 'old'))

# !! 중년이 280만원 정도로 가장 높다. 노년은(131만) 초년이 받는 것보다 적은 월급(128만원)


# 한국인의 삶을 파악하라(3) - 연령대 및 성별 월급 차이

summary(welfare)
head(welfare$sex)
table(welfare$sex)
sex_income <- welfare %>% filter(!is.na(income)) %>%
  group_by(class, sex) %>%
  summarise(mean_income = mean(income))
sex_income

ggplot(data=sex_income, aes(x=class, y=mean_income, fill=sex)) +   # fill=성별에 따른 색깔 표시
  geom_col() +                                           # 그래프 종류
  scale_x_discrete(limits=c("young", "middle", "old"))   # 축 순서 설정

ggplot(data=sex_income, aes(x=class, y=mean_income, fill=factor(sex))) +   # fill=성별에 따른 색깔 표시
  geom_col(position="dodge") +                          # 그래프 종류
  scale_x_discrete(limits=c("young", "middle", "old"))   # 축 순서 설정

# 연령대 구분없이 나이와 성별을 이용한 평균표


welfare$sex <- ifelse(welfare$sex==1, 'male', 'female')
qplot(welfare$sex)

sex_age <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age, sex) %>%
  summarise(mean_income=mean(income))
head(sex_age)

ggplot(data=sex_age, aes(x=age, y=mean_income, col=sex)) + geom_line()



# 직업별 월급의 관계
# 어떤 직업이 월급을 가장 많이 받을까?

listjob <- read_excel("Koweps_Codebook.xlsx", sheet=2)
head(listjob,20)


dim(listjob)
# 조인으로 데이터 결합 
table(welfare$code_job)

length(table(welfare$code_job))
names(welfare)

names(welfare)
welfare <- left_join(welfare, listjob, id="code_job")
names(welfare)

# 데이터 확인 - NA의 제거는 filter를 이용하면 된다.
welfare %>% select(code_job, job, income)


job_income <- welfare %>%
  filter(!is.na(job) & !is.na(income)) %>%
  group_by(job) %>%
  summarise(mean_income = mean(income))
head(job_income)

welfare %>% filter(job=='스포츠 및 레크레이션 관련 전문가')

job_income %>% filter(job=='스포츠 및 레크레이션 관련 전문가')

top20 <- job_income %>% 
  arrange(desc(mean_income)) %>%
  head(20)
top20


ggplot(top20, aes(x=job, y=mean_income)) + geom_col()

ggplot(top20, aes(x=job, y=mean_income)) + geom_col() + coord_flip()


