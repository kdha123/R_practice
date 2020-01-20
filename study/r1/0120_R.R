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
