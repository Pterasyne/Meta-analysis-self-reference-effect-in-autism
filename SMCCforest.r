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

egger_test <- regtest(res1)

forest(df_withES$yi, df_withES$vi, slab=df_withES$study, col=colors_vector, pch=15)
addpoly(res1, row= -1, mlab="Overall Global", col="red", efac=2)

legend(x = 1, y = 35, 
       legend=c("Children - Implicit", "Children - Explicit", 
                "Adults - Implicit", "Adults - Explicit"),
       col=c("steelblue", "lightcoral", "darkblue", "darkred"), pch=15, cex=0.6, xpd=TRUE)

mtext(paste("Egger's Test: p =", round(egger_test$pval, 3)), side=1, line=3, adj=1, cex=0.8)

forest(df_withES$yi, df_withES$vi, slab=df_withES$study, col=colors_vector, pch=15)
addpoly(res1, row= -1, mlab="Overall Global", col="red", efac=2)
legend(x = 1, y = 35, legend=c("Children - Implicit", "Children - Explicit", 
                               "Adults - Implicit", "Adults - Explicit"),
       col=c("steelblue", "lightcoral", "darkblue", "darkred"), pch=15, cex=0.6, xpd=TRUE)

subset1 <- subset(df_withES, age_group == "children" & task == "implicit")
if (nrow(subset1) > 0) {
    res_subset1 <- rma(yi, vi, data=subset1)
    forest(subset1$yi, subset1$vi, slab=subset1$study, col="steelblue", pch=15, main="Children - Implicit")
    addpoly(res_subset1, row= -1, mlab="Subgroup Effect", col="steelblue", efac=2)
    legend(x = 1, y = 35, legend="Children - Implicit", col="steelblue", pch=15, cex=0.6, xpd=TRUE)
}

subset2 <- subset(df_withES, age_group == "children" & task == "explicit")
if (nrow(subset2) > 0) {
    res_subset2 <- rma(yi, vi, data=subset2)
    forest(subset2$yi, subset2$vi, slab=subset2$study, col="lightcoral", pch=15, main="Children - Explicit")
    addpoly(res_subset2, row= -1, mlab="Subgroup Effect", col="lightcoral", efac=2)
    legend(x = 1, y = 35, legend="Children - Explicit", col="lightcoral", pch=15, cex=0.6, xpd=TRUE)
}

subset3 <- subset(df_withES, age_group == "adults" & task == "implicit")
if (nrow(subset3) > 0) {
    res_subset3 <- rma(yi, vi, data=subset3)
    forest(subset3$yi, subset3$vi, slab=subset3$study, col="darkblue", pch=15, main="Adults - Implicit")
    addpoly(res_subset3, row= -1, mlab="Subgroup Effect", col="darkblue", efac=2)
    legend(x = 1, y = 35, legend="Adults - Implicit", col="darkblue", pch=15, cex=0.6, xpd=TRUE)
}

subset4 <- subset(df_withES, age_group == "adults" & task == "explicit")
if (nrow(subset4) > 0) {
    res_subset4 <- rma(yi, vi, data=subset4)
    forest(subset4$yi, subset4$vi, slab=subset4$study, col="darkred", pch=15, main="Adults - Explicit")
    addpoly(res_subset4, row= -1, mlab="Subgroup Effect", col="darkred", efac=2)
    legend(x = 1, y = 35, legend="Adults - Explicit", col="darkred", pch=15, cex=0.6, xpd=TRUE)
}