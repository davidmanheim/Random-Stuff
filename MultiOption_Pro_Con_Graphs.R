# Test multiple options for preference ordering, re: @Stucchio's https://www.chrisstucchio.com/blog/2014/equal_weights.html

# These graphs were used for a post on Medium.com

library(gtools) #has rdirichlet distribution

#How many dimensions is the feature vector?
dims = c(1, 2,3,4,5,7,10,15,20,30,50)

result_data=data.frame(Dims=1,Choices=1,Samples=0,Correct=1,Truncated_Correct=1)

#max_choices = 20
#choices = 2:max_choices
choices = c(2:6,8,10,15,20,25,30,35,40,45,50)
for (d in dims){
for (c in choices){ #Must be at least 2.

#How many samples am I running?
n=10000
# I need to loop this n times...
results = 0
truncated_results = 0
for (trial in 1:n){
  #The "true" / "unit" weights:
  h = rdirichlet(1,as.vector(rep(1,d)))
  u = as.vector(rep(1.0/d,d))
  truncated_u = ifelse(h>1/(5*d),1.0/d,0)
  #The candidates' features, 1 row for each candidate:
  features <- matrix(sample(x=c(0,1),size=d*c,replace=TRUE, prob=c(.50,.50)), nrow=c, ncol=d)
  true_ranks = features%*%t(h)
  binary_ranks = features%*%u
  truncated_binary_ranks = features%*%t(truncated_u)
  
  #winner in each case?
  result_t = match(max(true_ranks),true_ranks);
  result_b = match(max(binary_ranks),binary_ranks);
  result_tb = match(max(truncated_binary_ranks),truncated_binary_ranks);
  results = results + identical(result_b,result_t);
  truncated_results = truncated_results + identical(result_tb,result_t);
  #print(paste("true ranks;", true_ranks, "binary ranks", binary_ranks));
  #print(paste("t",result_t,"b",result_b, "overall",results));
  }


pct_correct=results/n
truncated_pct_correct=truncated_results/n
result_data = rbind(result_data, c(d,c,n,pct_correct,truncated_pct_correct))
print(paste("dimensions=",d,"choices=",c, "correct=", pct_correct,"truncated correct=", truncated_pct_correct))
}
}

library(ggplot2)
library(reshape2)
result_data <- result_data[2:dim(result_data)[1],c(1,2,4,5)] #Remove initial row.
result_data[,c(1,2,4,5)]

plottable=result_data[,c(1,2,3)]
names(plottable)=c("Dimensions","variable","value")
plottable[2] <- lapply(plottable[2], function(x) if(is.numeric(x)) as.factor(x) else x)

library(scales)

ggplot(subset(plottable,as.integer(variable)<3), aes(x=Dimensions, y=value, colour=variable, ymin=0, ymax=1)) + theme(legend.title=element_blank(), panel.background = element_rect(fill = 'white'),  panel.grid.major = element_line(colour = "gray75")) + geom_line(size=2)  + labs(x="Number of Features", y="Percentage Correct") +ggtitle("Performance of Binary Classifier for 2 or 3 choices") + scale_y_continuous(labels=percent)

ggplot(subset(plottable,variable %in% c(2,3,5,25,50)), aes(x=Dimensions, y=value, colour=variable, ymin=0, ymax=1)) + theme(legend.title=element_blank(), panel.background = element_rect(fill = 'white'),  panel.grid.major = element_line(colour = "gray75")) + geom_line(size=2)  + labs(x="Number of Features", y="Percentage Correct") +ggtitle("Performance given more choices") + scale_y_continuous(labels=percent)


plottable_comparison=melt(result_data, direction="long",id.vars=c("Dims","Choices"), measure.vars=c("Correct","Truncated_Correct"))
names(plottable_comparison)=c("Dimensions","Choices","variable","value")
plottable_comparison[2:3] <- lapply(plottable_comparison[2:3], function(x) if(is.numeric(x)) as.factor(x) else x)
levels(plottable_comparison$variable) <- c("All Features","Ignore Lowest Weigh")

ggplot(subset(plottable_comparison,Choices==3), aes(x=Dimensions, y=value, colour=variable, ymin=0, ymax=1)) + theme(legend.title=element_blank(), panel.background = element_rect(fill = 'white'),  panel.grid.major = element_line(colour = "gray75")) + geom_line(size=2)  + labs(x="Number of Features", y="Percentage Correct") +ggtitle("Performance given 3 choices") + scale_y_continuous(labels=percent)

#Switch Pallette so that these are clearly distinct;
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

plottable_comparison[2] <- lapply(plottable_comparison[2], function(x) as.numeric(as.character(x)))

ggplot(subset(plottable_comparison,Dimensions==5)[2:4], aes(x=Choices, y=value, colour=variable, ymin=0, ymax=1)) + scale_colour_manual(values=cbPalette) + theme(legend.title=element_blank(), panel.background = element_rect(fill = 'white'),  panel.grid.major = element_line(colour = "gray75")) + geom_line(size=2)  + labs(x="Number of Choices", y="Percentage Correct") +ggtitle("Performance by number of choices (5 features)") + scale_y_continuous(labels=percent)

ggplot(subset(plottable_comparison,Dimensions==50)[2:4], aes(x=Choices, y=value, colour=variable, ymin=0, ymax=1)) + scale_colour_manual(values=cbPalette) + theme(legend.title=element_blank(), panel.background = element_rect(fill = 'white'),  panel.grid.major = element_line(colour = "gray75")) + geom_line(size=2)  + labs(x="Number of Choices", y="Percentage Correct") +ggtitle("Performance by number of choices (50 features)") + scale_y_continuous(labels=percent)

