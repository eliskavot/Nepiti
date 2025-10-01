library(nnet)
library(marginaleffects)
library(performance)
library(glmmTMB)
library(MatchIt)
library(ordinal)
library(rms)
library(MASS)
library(stringr)

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

#nQ65_r1 = Stanovil/a jsem si maximální množství alkoholu, které mohu vypít při jedné příležitosti,
#nQ67_r1 = Stanovil/a jsem si maximální počet dnů v týdnu, během kterých mohu pít alkohol,
#nQ69_r1 = Vyhýbal/a jsem se určitému druhu alkoholu,
#nQ71_r1 = Stanovil/a jsem si nějaký interval mezi dny, kdy piji alkohol,
#nQ73_r1 = Pil jsem jen při některých vybraných příležitostech,
#nQ75_r1 = Přestal/a jsem pít při některých příležitostech, při nichž jsem předtím pil/a




# graf s pravdepodobnostmi pro odpoved "Daří se mi to dlouhodobě------------------------------------------------


res <- data.frame(
  strategy = c("nQ65_r1", "nQ67_r1", "nQ75_r1", "nQ73_r1", "nQ69_r1", "nQ71_r1"),
  prob_never = c(46.54364, 50.14453, 48.86177, 49.28903, 54.58632, 53.51158),
  prob_once  = c(56.48057, 40.38614, 44.93755, 54.12713, 37.39696, 36.93988),
  prob_more  = c(61.20475, 59.79647, 55.49713, 51.38765, 46.56929, 43.43723)
)



res$strategy <- dplyr::recode(as.character(res$strategy),
                              "nQ65_r1" = "Stanovil/a jsem si maximální množství
                                           alkoholu, které mohu vypít při jedné
                                           příležitosti",
                              "nQ67_r1" = "Stanovil/a jsem si maximální počet dnů v
                                           týdnu, během kterých mohu pít alkohol",
                              "nQ69_r1" = "Vyhýbal/a jsem se určitému druhu
                                           alkoholu",
                              "nQ71_r1" = "Stanovil/a jsem si nějaký interval mezi
                                           dny, kdy piji alkohol",
                              "nQ73_r1" = "Pil/a jsem jen při některých vybraných
                                           příležitostech",
                              "nQ75_r1" = "Přestal/a jsem pít při některých
                                           příležitostech, při nichž jsem předtím
                                           pil/a",
                              .default = NA_character_
)


res_long <- res %>%
  pivot_longer(cols = starts_with("prob_"),
               names_to = "group", values_to = "prob") %>%
  mutate(group = dplyr::recode(group,
                        "prob_never" = "Ne, nikdy",
                        "prob_once"  = "Ano, jednou",
                        "prob_more"  = "Ano, vícekrát"))


res_long$group <- factor(
  res_long$group,
  levels = c("Ano, vícekrát", "Ano, jednou", "Ne, nikdy")
)




res_long$strategy <- factor(res_long$strategy,
                            levels = rev(c("Stanovil/a jsem si maximální množství
                                           alkoholu, které mohu vypít při jedné
                                           příležitosti",
                                           "Stanovil/a jsem si maximální počet dnů v
                                           týdnu, během kterých mohu pít alkohol",
                                           "Přestal/a jsem pít při některých
                                           příležitostech, při nichž jsem předtím
                                           pil/a",
                                           "Pil/a jsem jen při některých vybraných
                                           příležitostech",
                                           "Vyhýbal/a jsem se určitému druhu
                                           alkoholu",
                                           "Stanovil/a jsem si nějaký interval mezi
                                           dny, kdy piji alkohol")))


haha = ggplot(res_long, aes(x = prob, y = strategy, color = group)) +
  geom_point(size = 5, alpha = 1) +
  scale_color_manual(values = rev(seq_pallet3)) +   
  labs(
    x = "",
    y = "",
    color = ""
  ) +
  scale_y_discrete(labels = label_wrap(40))+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1, scale = 1))+
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    legend.position = "top",
    legend.text = element_text(size = 14),
    panel.grid.minor.x = element_blank()
  )

ggsave(plot = haha, filename = "prav. Daří se mi to dlouhodobě.png", path = "grafy",
       device = ragg::agg_png, units = "cm", width = 26.5, height = 15, scaling = 1)





