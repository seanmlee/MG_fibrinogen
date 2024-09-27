
# load libraries ---------------------------------------------------------------
library(tidyverse)
library(ggsignif)
library(ggbeeswarm)


# critval ----------------------------------------------------------------------
critval <- 1.96


# fga ##########################################################################
# newdata_control --------------------------------------------------------------
newdata_control <- data.frame(
  
  treatment = "control"
  
)

preds_control <- predict(
  
  mod_fga, 
  newdata = newdata_control, 
  type = "response", 
  se.fit = TRUE
  
)

fit_link_control <- preds_control$fit

fit_response <- exp(fit_link_control)

upr_link_control <- preds_control$fit + (critval * preds_control$se.fit)

lwr_link_control <- preds_control$fit - (critval * preds_control$se.fit)

upr_response <- exp(upr_link_control)

lwr_response <- exp(lwr_link_control)

fit_control <- as.data.frame(
  
  cbind(
    newdata_control,
    fit_response, 
    upr_response,
    lwr_response
  )
  
)


# newdata_mg -------------------------------------------------------------------
newdata_mg <- data.frame(
  
  treatment = "mg"
  
)

preds_mg <- predict(
  
  mod_fga, 
  newdata = newdata_mg, 
  type = "response", 
  se.fit = TRUE
  
)

fit_link_mg <- preds_mg$fit

fit_response <- exp(fit_link_mg)

upr_link_mg <- preds_mg$fit + (critval * preds_mg$se.fit)

lwr_link_mg <- preds_mg$fit - (critval * preds_mg$se.fit)

upr_response <- exp(upr_link_mg)

lwr_response <- exp(lwr_link_mg)

fit_mg <- as.data.frame(
  
  cbind(
    newdata_mg,
    fit_response, 
    upr_response,
    lwr_response
  )
  
)


# bind -------------------------------------------------------------------------
fit <- rbind(fit_control,fit_mg)

fit$treatment <- factor(
  fit$treatment, 
  levels = c("control", "mg")
)

fit_fga <- fit
fit_fga$variable <- "fga"


# fgb ##########################################################################
# newdata_control --------------------------------------------------------------
newdata_control <- data.frame(
  
  treatment = "control"
  
)

preds_control <- predict(
  
  mod_fgb, 
  newdata = newdata_control, 
  type = "response", 
  se.fit = TRUE
  
)

fit_link_control <- preds_control$fit

fit_response <- exp(fit_link_control)

upr_link_control <- preds_control$fit + (critval * preds_control$se.fit)

lwr_link_control <- preds_control$fit - (critval * preds_control$se.fit)

upr_response <- exp(upr_link_control)

lwr_response <- exp(lwr_link_control)

fit_control <- as.data.frame(
  
  cbind(
    newdata_control,
    fit_response, 
    upr_response,
    lwr_response
  )
  
)


# newdata_mg -------------------------------------------------------------------
newdata_mg <- data.frame(
  
  treatment = "mg"
  
)

preds_mg <- predict(
  
  mod_fgb, 
  newdata = newdata_mg, 
  type = "response", 
  se.fit = TRUE
  
)

fit_link_mg <- preds_mg$fit

fit_response <- exp(fit_link_mg)

upr_link_mg <- preds_mg$fit + (critval * preds_mg$se.fit)

lwr_link_mg <- preds_mg$fit - (critval * preds_mg$se.fit)

upr_response <- exp(upr_link_mg)

lwr_response <- exp(lwr_link_mg)

fit_mg <- as.data.frame(
  
  cbind(
    newdata_mg,
    fit_response, 
    upr_response,
    lwr_response
  )
  
)


# bind -------------------------------------------------------------------------
fit <- rbind(fit_control,fit_mg)

fit$treatment <- factor(
  fit$treatment, 
  levels = c("control", "mg")
)

fit_fgb <- fit
fit_fgb$variable <- "fgb"


# fgg ##########################################################################
# newdata_control --------------------------------------------------------------
newdata_control <- data.frame(
  
  treatment = "control"
  
)

preds_control <- predict(
  
  mod_fgg, 
  newdata = newdata_control, 
  type = "response", 
  se.fit = TRUE
  
)

fit_link_control <- preds_control$fit

fit_response <- exp(fit_link_control)

upr_link_control <- preds_control$fit + (critval * preds_control$se.fit)

lwr_link_control <- preds_control$fit - (critval * preds_control$se.fit)

upr_response <- exp(upr_link_control)

lwr_response <- exp(lwr_link_control)

fit_control <- as.data.frame(
  
  cbind(
    newdata_control,
    fit_response, 
    upr_response,
    lwr_response
  )
  
)


# newdata_mg -------------------------------------------------------------------
newdata_mg <- data.frame(
  
  treatment = "mg"
  
)

preds_mg <- predict(
  
  mod_fgg, 
  newdata = newdata_mg, 
  type = "response", 
  se.fit = TRUE
  
)

fit_link_mg <- preds_mg$fit

fit_response <- exp(fit_link_mg)

upr_link_mg <- preds_mg$fit + (critval * preds_mg$se.fit)

lwr_link_mg <- preds_mg$fit - (critval * preds_mg$se.fit)

upr_response <- exp(upr_link_mg)

lwr_response <- exp(lwr_link_mg)

fit_mg <- as.data.frame(
  
  cbind(
    newdata_mg,
    fit_response, 
    upr_response,
    lwr_response
  )
  
)


# bind -------------------------------------------------------------------------
fit <- rbind(fit_control,fit_mg)

fit$treatment <- factor(
  fit$treatment, 
  levels = c("control", "mg")
)

fit_fgg <- fit
fit_fgg$variable <- "fgg"


# plot #########################################################################
# bind--------------------------------------------------------------------------
fit <- rbind(fit_fga, fit_fgb, fit_fgg)


# filter raw -------------------------------------------------------------------
abg <- fib %>% 
  
  filter(
    variable == "fga" | variable == "fgb" | variable == "fgg"
  )


# plot -------------------------------------------------------------------------
fit %>%
  
  ggplot(
    
    aes(
      x = variable, 
      y = fit_response,
      shape = treatment
    )
    
  ) +
  
  geom_jitter(
    
    data = abg,
    aes(
      x = variable,
      y = value,
      shape = treatment
    ),
    alpha = 1,
    size = 3,
    position = position_dodge(width = 0.5)
  ) +
  
  geom_errorbar(
    
    aes(
      ymin = lwr_response,
      ymax = upr_response
    ),
    width = 0.4,
    size = 0.5,
    position = position_dodge(width = 0.5)
    
  ) +
  
  geom_point(
    
    size = 7,
    fill = "white",
    position = position_dodge(width = 0.5)
    
  ) +
  
  scale_y_continuous(
    
    labels = scales::comma,
    limits = c(10, 22),
    breaks = seq(10, 22, 2)
    
  ) +
  
  ggtitle("") +
  
  xlab("") +
  
  ylab("Relative Abundance") +
  
  labs(shape = "") +
  
  scale_x_discrete(
    labels = c("fga" = "Fibrinogen α", "fgb" = "Fibrinogen β", "fgg" = "Fibrinogen δ")
  ) +
  
  scale_shape_manual(
    values = c("control" = 21, "mg" = 22),
    labels = c("Control", "MG")
  ) +
  
  theme_bw() +
  
  theme(
    
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15)
    
  ) +
  
  geom_signif(
    
    y_position = 21.7, 
    xmin = 0.875,
    xmax = 1.125,
    annotation = "n.s.", 
    tip_length = 0.005,
    color = "black",
    textsize = 7
    
  ) +
  
  geom_signif(
    
    y_position = 21.7, 
    xmin = 1.875,
    xmax = 2.125,
    annotation = "p=0.002", 
    tip_length = 0.005,
    color = "black",
    textsize = 6
    
  ) +
  
  geom_signif(
    
    y_position = 21.7, 
    xmin = 2.875,
    xmax = 3.125,
    annotation = "p=0.003", 
    tip_length = 0.005,
    color = "black",
    textsize = 6
    
  )


# write .png -------------------------------------------------------------------
ggsave(
  
  "out/plot_protein.png",
  height = 5,
  width = 7.5
  
)
