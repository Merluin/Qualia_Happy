dv = c(2,4,2,3,4,1,1,1,3,5,
       3,4,2,4,5,6,7,7,4,3,
       8,9,2,8,10,10,4,6,7,8)

group = c('A','A','A','A','A','A','A','A','A','A',
          'B','B','B','B','B','B','B','B','B','B',
          'C','C','C','C','C','C','C','C','C','C')

dat = data.frame(dv, group)

library(psych)
#Save descriptive statistics by group in data frame called 'descriptives'
descriptives = describeBy(x = dat$dv, group = dat$group)

#Save vectors of group names, and the group means/se's from 'descriptives'.
#Store them in 'plotdat'
group = c('A', 'B', 'C')
means = c(descriptives$A$mean, descriptives$B$mean,descriptives$C$mean)
se = c(descriptives$A$se, descriptives$B$se,descriptives$C$se)
plotdat=data.frame(group, means, se)

library(ggplot2)

apatheme=theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text=element_text(family='Arial'),
        legend.title=element_blank(),
        legend.position=c(.7,.8),
        axis.line.x = element_line(color='black'),
        axis.line.y = element_line(color='black'))

#Use data frame of summary statistics ('plotdat')
#and map group to x-axis and means to y-axis
p1 = ggplot(data = plotdat, aes(x = group, y = means))+
  #Insert bean plot based on data frame of raw data ('dat')
  #and map group to x-axis and raw dv scores to y-axis
  geom_violin(data= dat, aes(x = group, y = dv))+
  #Likewise, add raw data points (jittered) with same mappings
  geom_jitter(data= dat, aes(x = group, y = dv), shape = 1, width = .1)+
  #Add data points (with no unique mapping manually specified, the data
  #points will fall back on reflecting our originally-specified data,
  #the means of each group)
  geom_point(size = 3)+
  #Add error bars for 95% CIs of each group mean
  geom_errorbar(ymax= means+(1.96*se), ymin=means+(-1.96*se), width = 0.25)+
  #Apply the APA-format theme object
  apatheme
