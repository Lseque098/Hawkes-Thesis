library(ggplot2)
sequence <- seq(1,50, by =1)
pos = 0
history = NULL
for (x in sequence){
  prob = runif(1)
  if (prob < .5)
    pos = pos + 1
  else
    pos = pos -1
  history = c(history, pos)
}
history <- c(0, history)
tiempos <- c(0, sequence)

df <- data.frame(cbind(tiempos,history))

df.colnames <- c('x', 'P1')

ggplot(data=df, aes(x=tiempos, y=history)) +
  geom_line(color="black") + xlab("T") + ylab("S") + theme_classic() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14)) 
