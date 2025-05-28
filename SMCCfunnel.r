library(metafor)
library(readxl)

df <- read_excel("SMCC0.xlsx")
summary(df)  

df$r[is.na(df$r)] <- 0.3 #To vary (0.5 ; 0.7)

df_withES <- escalc(measure = "SMCC", 
                    m1i = df$x_self, sd1i = df$sd_self, 
                    m2i = df$x_other, sd2i = df$sd_other, 
                    ri = df$r, ni = df$n, 
                    data = df)

df_withES <- df_withES[order(df_withES$yi), ]

colors_vector <- sapply(1:nrow(df_withES), function(i) {
  age <- as.character(df_withES$age_group[i])
  task <- as.character(df_withES$task[i])
  
  if (age == "children" && task == "implicit") "steelblue"
  else if (age == "children" && task == "explicit") "lightcoral"
  else if (age == "adults" && task == "implicit") "darkblue"
  else if (age == "adults" && task == "explicit") "darkred"
  else "gray"  
})

res1 <- rma(yi, vi, data=df_withES, slab=df_withES$study)  
res3 <- rma(yi, vi, mods = ~ factor(age_group), data=df_withES)  
res4 <- rma(yi, vi, mods = ~ factor(age_group) * factor(task), data=df_withES)  

egger_test <- regtest(res1)

forest(df_withES$yi, df_withES$vi, slab=df_withES$study, col=colors_vector, pch=16) #pch = circle
addpoly(res1, row= -5, mlab="Overall Global", col="red", efac=2)  

legend("topright", legend=c("Children - Implicit", "Children - Explicit", 
                            "Adults - Implicit", "Adults - Explicit"),
       col=c("steelblue", "lightcoral", "darkblue", "darkred"), pch=16, cex=0.7)

mtext(paste("Egger's Test: p =", round(egger_test$pval, 3)), side=1, line=3, adj=1, cex=0.8)

forest(res3, col=colors_vector)  
legend("topright", legend=c("Children - Implicit", "Children - Explicit", 
                            "Adults - Implicit", "Adults - Explicit"),
       col=c("steelblue", "lightcoral", "darkblue", "darkred"), pch=16, cex=0.7)

forest(res4, col=colors_vector)  
legend("topright", legend=c("Children - Implicit", "Children - Explicit", 
                            "Adults - Implicit", "Adults - Explicit"),
       col=c("steelblue", "lightcoral", "darkblue", "darkred"), pch=16, cex=0.7)

funnel(df_withES$yi, df_withES$vi, col=colors_vector)
legend("topright", legend=c("Children - Implicit", "Children - Explicit", 
                            "Adults - Implicit", "Adults - Explicit"),
       col=c("steelblue", "lightcoral", "darkblue", "darkred"), pch=16, cex=0.7)

funnel(res1, col=colors_vector)
legend("topright", legend=c("Children - Implicit", "Children - Explicit", 
                            "Adults - Implicit", "Adults - Explicit"),
       col=c("steelblue", "lightcoral", "darkblue", "darkred"), pch=16, cex=0.7)