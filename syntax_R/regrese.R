library(nnet)
library(marginaleffects)
library(performance)
library(glmmTMB)
library(MatchIt)
library(ordinal)
library(rms)
library(MASS)

vars <- c("nQ65_r1","nQ67_r1","nQ69_r1","nQ71_r1","nQ73_r1","nQ75_r1")



mult <- multinom(nQ77_r1 ~ nQ65_r1 + nQ67_r1 + nQ69_r1 + nQ71_r1 +
                   nQ73_r1 + nQ75_r1 + vzd3 + vek4 + nQ88_r1 + celk_spotr_filtr_5kat, data = data)



avg_slopes(mult, variables = "nQ65_r1")
avg_slopes(mult)
avg_slopes(mult, variables = c("nQ65_r1", "nQ67_r1"))

plot_predictions(mult, condition = c("nQ75_r1", "group"), type = "probs") +
  facet_wrap(~group)+
  theme(legend.position = "none")





get_avg_probs <- function(model, var, data, outcome = "Daří se mi to dlouhodobě") {
  levs <- levels(data[[var]])
  probs_list <- lapply(levs, function(l) {
    newdata <- data
    newdata[[var]] <- factor(l, levels = levels(data[[var]]))
    p <- predict(model, newdata = newdata, type = "probs")
    if (is.null(dim(p))) p <- t(as.matrix(p))  
    mean(p[, outcome], na.rm = TRUE) * 100
  })
  data.frame(level = levs, prob = unlist(probs_list))
}


get_avg_probs(mult, "nQ65_r1", data)
get_avg_probs(mult, "nQ67_r1", data)
get_avg_probs(mult, "nQ69_r1", data)
get_avg_probs(mult, "nQ71_r1", data)
get_avg_probs(mult, "nQ73_r1", data)
get_avg_probs(mult, "nQ75_r1", data)








strategy_vars <- c("nQ65_r1","nQ67_r1","nQ69_r1","nQ71_r1","nQ73_r1","nQ75_r1")
outcome_label <- "Daří se mi to dlouhodobě"

res_list <- lapply(strategy_vars, function(v) {
  df <- get_avg_probs(mult, v, data, outcome = outcome_label)
  prob_never <- df$prob[df$level == "Ne, nikdy"]
  prob_more  <- df$prob[df$level == "Ano, vícekrát"]
  prob_once  <- df$prob[df$level == "Ano, jednou"]
  data.frame(strategy = v,
             prob_never = prob_never,
             prob_once  = prob_once,
             prob_more  = prob_more,
             diff_more_minus_never = prob_more - prob_never)
})

res <- do.call(rbind, res_list)
res[order(-res$diff_more_minus_never), ]