df <- read.delim("clipboard",header = TRUE)
df$TRAT <- as.factor(df$TRAT)

dfc <- summarySE(df, measurevar="inflorescencia", groupvars=c("TRAT"))
dfc

library(ggplot2)
library(Rmisc)


ggplot(dfc, aes(x=TRAT, y=inflorescencia)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=inflorescencia-se, ymax=inflorescencia+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))  +
  xlab(expression(bold(paste('Cd (mg.kg'^"-1"*')'))))+
  ylab(expression(bold(paste('Cd (mg.kg MS'^"-1"*')'))))+
  ggtitle("Inflorescencia") +
  theme_light() + 
  annotate("text", x=1, y=3, label= "b") +
  annotate("text", x=2, y=5, label= "a") +
  annotate("text", x=3, y=5.5, label= "a") +
  annotate("text", x=4, y=5.5, label= "a") +
  annotate("text", x=5, y=5.5, label= "a")
