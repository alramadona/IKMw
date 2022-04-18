datTotal <- read.csv("dat/denTot.csv", na.string="#N/A")
names(datTotal)

library(dplyr)
dat <- select(datTotal, year,month,denguel0,templ0,rainl0)
dat$term <- rep(rep(c(1,2), each=6),13)

library(zoo)
library(ggplot2)

source("multiplot.R")

dat$YM <- paste(dat$month, dat$year, sep="-")
dat$YM <- as.yearmon(dat$YM, format="%m-%Y")
dat$YM <- as.Date(dat$YM)

p1 <- qplot(YM, denguel0, data=dat, geom=c("point", "line"),
            xlab = "t", 
            ylab = "jumlah kasus") + 
  theme_bw()
p1

p2 <- qplot(YM, templ0, data=dat, geom=c("point", "line"),
            xlab = "t", 
            ylab = "suhu") + 
  theme_bw()
p2

p3 <- qplot(YM, rainl0, data=dat, geom=c("point", "line"),
            xlab = "t", 
            ylab = "curah hujan") + 
  theme_bw()
p3

multiplot(p1, p2, p3, cols=1)

names(dat) <- c("year","MoY","cases","temp","rain","term","YM")
dat$MoY <- as.factor(dat$MoY)

# https://www.cedricscherer.com/2019/05/17/the-evolution-of-a-ggplot-ep.-1/

ggplot(dat, aes(x = MoY, y = cases)) +
  geom_boxplot()

g <- ggplot(dat, aes(x = MoY, y = cases))

multiplot(g+geom_boxplot(), g+geom_violin(), g+geom_point(size=1), cols=1)

p1 <- ggplot(dat, aes(x = MoY, y = cases)) +
  geom_boxplot()
p2 <- ggplot(dat, aes(x = MoY, y = temp)) +
  geom_boxplot()
p3 <- ggplot(dat, aes(x = MoY, y = rain)) +
  geom_boxplot()

multiplot(p1, p2, p3, cols=1)

g +
  geom_boxplot(color = "gray60", outlier.alpha = 0) +
  geom_point(size = 3, alpha = 0.15) +
  theme_minimal()

set.seed(2019)

ggplot(dat, aes(x = MoY, y = cases, col = MoY)) +
  theme_minimal() + 
  geom_jitter(size = 2, width = 0.2) + 
  theme(legend.position="none")

cases_avg <-
  dat %>%
  summarize(avg = mean(cases, na.rm = TRUE)) %>%
  pull(avg)

ggplot(dat, aes(x = MoY, y = cases, col = MoY)) +
  geom_hline(aes(yintercept = cases_avg), color = "gray70", size = 0.6) +
  geom_jitter(size = 2, alpha = 0.25, width = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 5) +
  theme_minimal() +
  theme(legend.position="none")

##

# library
library(ggplot2)
library(dplyr)
library(hrbrthemes)

# Build dataset with different distributions

dat %>%
  ggplot( aes(x=cases)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

dat %>%
  ggplot( aes(x=temp)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

dat %>%
  ggplot( aes(x=rain)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

##

p <- dat %>%
  ggplot( aes(x=cases, fill=as.factor(term))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

q <- dat %>%
  ggplot( aes(x=temp, fill=as.factor(term))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

r <- dat %>%
  ggplot( aes(x=rain, fill=as.factor(term))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

multiplot(p, q, r, cols=3)

##

plot(dat[,c(3:5)], pch=20 , cex=1.5 , col="#69b3a2")

library(GGally)
ggpairs(dat[,c(3:5)], title="") 

