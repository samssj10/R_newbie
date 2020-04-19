#load required libraries
x=c("ggplot2","scales","psych","gplots")
lapply(x,require,character.only=TRUE)
library(DataCombine)
rmExcept("marketing_train")
  
#bar plot
ggplot(marketing_train,aes_string(x=marketing_train$profession)) + 
  geom_bar(stat="count",fill="DarkSlateBlue") + theme_bw() + xlab("Profession") + ylab("Count") +
  scale_y_continuous(breaks = pretty_breaks(n=10)) + ggtitle("marketing_campaign_analysis") + theme(text=element_text(size=15))

#histogram
ggplot(marketing_train,aes_string(x=marketing_train$custAge))+
  geom_histogram(fill="cornsilk",colour="black") + geom_density() +
  scale_y_continuous(breaks = pretty_breaks(n=10)) +
  scale_x_continuous(breaks = pretty_breaks(n=10)) +
  theme_bw() + xlab("Customer Age") + ylab("Frequency") + ggtitle("marketing_data : Age") + 
  theme(text=element_text(size=15))

#box plot
ggplot(marketing_train,aes_string(x=marketing_train$responded,y=marketing_train$custAge,
      fill=marketing_train$responded)) +
      geom_boxplot(outlier.colour ="red",outlier.size = 3) +
      scale_y_continuous(breaks = pretty_breaks(n=10)) +
      guides(fill=FALSE) + theme_bw() + xlab("Responded") + ylab("Customer Age") + ggtitle("Outlier Analysis") +
      theme(text=element_text(size=15))

#multivariate scatter plot
ggplot(marketing_train,aes_string(x=marketing_train$campaign,y=marketing_train$custAge)) +
      geom_point(aes_string(colour = marketing_train$responded,shape = marketing_train$profession),size = 4) +
      theme_bw() + xlab("Campaign") + ylab("Customer Age") + ggtitle("scatter plot analysis") +
      theme(text=element_text(size=15)) +
      scale_y_continuous(breaks = pretty_breaks(n=10)) +
      scale_x_continuous(breaks = pretty_breaks(n=10)) +
      scale_colour_discrete(name = "Responded")
      scale_shape_discrete(name = "Profession")
      
      


  
  