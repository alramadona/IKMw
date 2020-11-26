
# Ch-1 --------------------------------------------------------------------
## library
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)

#install.packages("tm")
library(tm)
#install.packages("SnowballC")
library(SnowballC)

# https://www.kaggle.com/dionisiusdh/covid19-indonesian-twitter-sentiment
tweets <- read.csv("dat/tweetsCovid.csv", stringsAsFactors=FALSE)

names(tweets)
head(tweets)
str(tweets)

tweets$date <- as.Date(tweets$date, format="%Y-%m-%d")
str(tweets)

tweetsSumz <- tweets %>%
  group_by(date) %>%
  summarise(n = n())

sum(tweetsSumz$n)

qplot(date, n, data=tweetsSumz, geom=c("point", "line"),
      xlab = "t", 
      ylab = "tweets") + 
  theme_light()

tweetsSumz <- tweets %>%
  group_by(username) %>%
  summarise(n = n())

head(tweetsSumz)

tweetsSumz %>%
  filter(n > 200) %>%
  mutate(username = reorder(username, n)) %>%
  ggplot(aes(n, username)) +
  geom_col() +
  labs(y = NULL) + 
  theme_light()

tweetsSumz <- tweets %>%
  filter(username %in% c('humaspolis', 'sabhara_barsel')) %>%
  group_by(date, username) %>%
  summarise(n = n())

ggplot(data = tweetsSumz, mapping = aes(x = date, y = n)) +
  geom_line(mapping = aes(color = username)) + 
  geom_point(mapping = aes(color = username)) +
  theme_light()

tweets %>%
  filter(username %in% c('radioelshinta', 'kompascom', 'tempodotco', 'detikcom')) %>%
  group_by(date, username) %>%
  summarise(n = n()) %>%
  ggplot(aes(date, n)) +
  geom_line(mapping = aes(color = username)) + 
  geom_point(mapping = aes(color = username)) +
  theme_light()

tweets %>%
  filter(username %in% c('radioelshinta', 'kompascom', 'tempodotco', 'detikcom')) %>%
  group_by(date, username) %>%
  summarise(n = n()) %>%
  ggplot(aes(date, n)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~ username, nrow = 2) +
  theme_light()

#
stopwordsID <- c("ada","adanya","adalah","adapun","agak","agaknya","agar","akan","akankah","akhirnya","aku","akulah","amat","amatlah","anda","andalah","antar","diantaranya","antara","antaranya","diantara","apa","apaan","mengapa","apabila","apakah","apalagi","apatah","atau","ataukah","ataupun","bagai","bagaikan","sebagai","sebagainya","bagaimana","bagaimanapun","sebagaimana","bagaimanakah","bagi","bahkan","bahwa","bahwasanya","sebaliknya","banyak","sebanyak","beberapa","seberapa","begini","beginian","beginikah","beginilah","sebegini","begitu","begitukah","begitulah","begitupun","sebegitu","belum","belumlah","sebelum","sebelumnya","sebenarnya","berapa","berapakah","berapalah","berapapun","betulkah","sebetulnya","biasa","biasanya","bila","bilakah","bisa","bisakah","sebisanya","boleh","bolehkah","bolehlah","buat","bukan","bukankah","bukanlah","bukannya","cuma","percuma","dahulu","dalam","dan","dapat","dari","daripada","dekat","demi","demikian","demikianlah","sedemikian","dengan","depan","di","dia","dialah","dini","diri","dirinya","terdiri","dong","dulu","enggak","enggaknya","entah","entahlah","terhadap","terhadapnya","hal","hampir","hanya","hanyalah","harus","haruslah","harusnya","seharusnya","hendak","hendaklah","hendaknya","hingga","sehingga","ia","ialah","ibarat","ingin","inginkah","inginkan","ini","inikah","inilah","itu","itukah","itulah","jangan","jangankan","janganlah","jika","jikalau","juga","justru","kala","kalau","kalaulah","kalaupun","kalian","kami","kamilah","kamu","kamulah","kan","kapan","kapankah","kapanpun","dikarenakan","karena","karenanya","ke","kecil","kemudian","kenapa","kepada","kepadanya","ketika","seketika","khususnya","kini","kinilah","kiranya","sekiranya","kita","kitalah","kok","lagi","lagian","selagi","lah","lain","lainnya","melainkan","selaku","lalu","melalui","terlalu","lama","lamanya","selama","selama","selamanya","lebih","terlebih","bermacam","macam","semacam","maka","makanya","makin","malah","malahan","mampu","mampukah","mana","manakala","manalagi","masih","masihkah","semasih","masing","mau","maupun","semaunya","memang","mereka","merekalah","meski","meskipun","semula","mungkin","mungkinkah","nah","namun","nanti","nantinya","nyaris","oleh","olehnya","seorang","seseorang","pada","padanya","padahal","paling","sepanjang","pantas","sepantasnya","sepantasnyalah","para","pasti","pastilah","per","pernah","pula","pun","merupakan","rupanya","serupa","saat","saatnya","sesaat","saja","sajalah","saling","bersama","sama","sesama","sambil","sampai","sana","sangat","sangatlah","saya","sayalah","se","sebab","sebabnya","sebuah","tersebut","tersebutlah","sedang","sedangkan","sedikit","sedikitnya","segala","segalanya","segera","sesegera","sejak","sejenak","sekali","sekalian","sekalipun","sesekali","sekaligus","sekarang","sekarang","sekitar","sekitarnya","sela","selain","selalu","seluruh","seluruhnya","semakin","sementara","sempat","semua","semuanya","sendiri","sendirinya","seolah","seperti","sepertinya","sering","seringnya","serta","siapa","siapakah","siapapun","disini","disinilah","sini","sinilah","sesuatu","sesuatunya","suatu","sesudah","sesudahnya","sudah","sudahkah","sudahlah","supaya","tadi","tadinya","tak","tanpa","setelah","telah","tentang","tentu","tentulah","tentunya","tertentu","seterusnya","tapi","tetapi","setiap","tiap","setidaknya","tidak","tidakkah","tidaklah","toh","waduh","wah","wahai","sewaktu","walau","walaupun","wong","yaitu","yakni","yang")

# Create corpus
corpus <- Corpus(VectorSource(tweets$tweet))
corpus
corpus[[1]]$content

corpus <- tm_map(corpus, removePunctuation)
corpus[[1]]$content
corpus <- tm_map(corpus, tolower)
corpus[[1]]$content
corpus <- tm_map(corpus, removeWords, c("banget", "gak", "kalo", "yg", "utk", "lg", "aja", "dah", "amp", "https", "udah", "untuk", stopwordsID))
corpus[[1]]$content
corpus <- tm_map(corpus, stripWhitespace)
corpus[[1]]$content

#install.packages("wordcloud")
library(wordcloud)

textdata <- as.character(c(1:52959))

for(i in c(1:52959)){
  textdata[i] <- corpus[[i]]$content
  }

wordcloud(textdata, max.words=150)

# Biblioshiny
# install.packages("bibliometrix") 
library(bibliometrix)
biblioshiny()


# Ch-2 --------------------------------------------------------------------
# https://cran.r-project.org/web/packages/covid19mobility/vignettes/animating_covid19_mobility.html
library(covid19mobility) #to get data on mobility from Apple/Google
library(dplyr) # for joining, piping data frames
library(lubridate) # for dealing with our date data
library(sf) # shapefile manipulation
# install.packages("devtools")
# devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr) #to get county and state data in a shapefile
library(gganimate) # to animate plots

head(refresh_covid19mobility_apple_subregion())

apple_us <- refresh_covid19mobility_apple_subregion() %>%
  filter(country=="United States") %>% # we are only going to map the US
  group_by(location,date) %>% #we are going to group by location (which is the state) and date
  summarize_if(is.numeric, mean) %>% #we are going to take the mean mobility score across counties by date and state
  mutate(weeks=round_date(date, "weeks")) %>% #we are going to take only the first date of each week
  group_by(weeks) %>% # we are going to group by each week
  filter(date==min(weeks)) #again we want to take only the first date of each week

states_sf <- urbnmapr::get_urbn_map("states", sf=TRUE) %>%
  st_as_sf() # coerce this to a shapefile.

total_apple_us  <- left_join(apple_us,states_sf, by=c("location"="state_name")) %>%
  st_as_sf() # this function makes sure the shape files are readable.

total_apple_us <- as.data.frame(total_apple_us) %>%
  st_as_sf()

ggplot() +
  geom_sf(data=states_sf)

ggplot() + #call ggplot
  geom_sf(data=states_sf) + # map the shape files to the map of the USA
  geom_sf(data= total_apple_us, aes(fill=value)) + # fill with our data frame and value, which is mobility score
  labs(fill="% change in mobility", 
       title="Date: {current_frame}", 
       subtitle="Percent Change in Movement, Source: Apple Mobility Data",
       fill = "% change in mobility") + #add titles
  scale_fill_viridis_c() +
  transition_manual(date) #animate by day - this is a gganimate function!
