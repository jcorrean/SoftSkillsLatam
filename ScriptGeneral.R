
library(ergm.multi)
data("Goeyvaerts")

G <- Goeyvaerts %>% keep(`%n%`, "included")
G.wd <- G %>% keep(`%n%`, "weekday")
f.wd <- Networks(G.wd) ~ N(~edges)
fit.wd <- ergm(f.wd, control=snctrl(seed=123))
fit.wd
summary(fit.wd)
