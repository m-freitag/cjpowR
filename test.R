df = cjpowr_amce(amce = 0.05, power = 0.8, levels = 5)

# For example, for a conjoint with 2 profiles and 4 tasks, n becomes:
df$n/(2*4)

#This gives the power (type S, E(type M)):
cjpowr_amce(amce = 0.05, n = 7829.258, levels = 5)

#Generating an interactive plot for type M error:
d <- expand.grid(
    amce = c(0.01, 0.02, 0.03, 0.05), 
    n = seq(from = 100, to = 50000, length.out = 1000), 
    alpha = 0.05, 
    levels = 2,
    treat.prob = 0.5,
    sims = 10000)

df <- list2DF(do.call(cjpowr_amce, d))


d <- expand.grid(
    amce = c(0.01, 0.02, 0.03, 0.05), 
    power = seq(from = 0, to = 1, length.out = 1000),
    alpha = 0.05, 
    levels = 2,
    treat.prob = 0.5,
    sims = 10000 # set to 0 if you want to plot something else than Type M error
    )

df <- list2DF(do.call(cjpowr_amce, d))


library(plotly)
plot_ly(df, x = ~power, y = ~n, type = 'scatter', mode = 'lines', linetype = ~amce) %>%
 layout(
   xaxis = list(title = "Effective Sample Size",
                zeroline = F,
                hoverformat = '.0f'),
   yaxis = list(title = "n",
                range = c(0,10),
                zeroline = F,
                hoverformat = '.2f'),
   legend=list(title=list(text='<b> AMCE </b>')),
   hovermode = "x unified"
 )


d <- expand.grid(
    delta3 = c(0.01, 0.02, 0.03, 0.05), 
    n = seq(from = 100, to = 50000, length.out = 1000),
    delta0 = 0.5, 
    delta1 = 0,
    alpha = 0.05,
    levels1 = 3, 
    levels2 = 4,
    p00 = 0.4, p10 = 0.4, p01 = 0.1, p11 = 0.1,
    sims = 10000
    )

df <- list2DF(do.call(cjpowr_amcie, d))

cjpowr_amcie(delta3 = 0.05, levels1=3, levels2=3, power=0.8)


plot_ly(df, x = ~n, y = ~exp_typeM, type = 'scatter', mode = 'lines', linetype = ~delta3) %>%
 layout(
   xaxis = list(title = "Effective Sample Size",
                zeroline = F,
                hoverformat = '.0f'),
   yaxis = list(title = "Exaggeration Ratio",
                range = c(0,10),
                zeroline = F,
                hoverformat = '.2f'),
   legend=list(title=list(text='<b> AMCIE </b>')),
   hovermode = "x unified"
 )

