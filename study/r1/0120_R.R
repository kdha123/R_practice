url<-'https://openapi.naver.com/v1/search/blog.xml?'
searchStr<-'query=설날'
searchStr<- iconv(searchStr,to='UTF-8') #utf-8 인코딩
searchStr<-URLencode(searchStr) #url 인코딩
searchStr
etcStr <- '&display=100'
reqUrl <- paste(url,searchStr,etcStr, sep = '')
reqUrl

# 지정된 경로에 접근하기 위해서
install.packages('httr')
library(httr)

clientid<-'9eYRtkGNmEzbrUhDYxym'
clientpw<-'QFU3tG14u2'

#웹페이지 접속
result <- GET(reqUrl, add_headers('X-Naver-Client-Id'=clientid,'X-Naver-Client-Secret'=clientpw))
result

# 응답결과확인 - raw형으로 나타남
result$content
result<-rawToChar(result$content) # 아직 utf-8은 아님
result
Encoding(result)<- 'UTF-8'
result

#gsub(패턴, 변환 후 문자열, 변환할 문자열, ignore.case)
result <- gsub('[[:punct:]]','',result)
result <- gsub('[a-zA-Z]','',result)
result <- gsub('[0-9]','', result)
result <- gsub(' +',' ', result)

install.packages('KoNLP')
install.packages('rJava')
install.packages('memoise')
library(KoNLP)
useSejongDic()
n <- extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다")
str(n)
#------------------------------
n <- extractNoun(result)
n
# 제외 단어 지정
outword <- c('설날','한')
n<- n[!n %in% outword]
wordcount <- sort(table(unlist(n)),decreasing = T)[1:50]
wordcount

install.packages('wordcloud2')
library(wordcloud2)
set.seed(1234)
wordcloud2(wordcount, shape = 'pentagon', size = 1)
wordcloud2(wordcount, size = 0.7)






