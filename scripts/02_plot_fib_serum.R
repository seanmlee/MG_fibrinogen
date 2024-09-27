
# load libraries ---------------------------------------------------------------
library(tidyverse)
library(ggsignif)


# critval ----------------------------------------------------------------------
critval <- 1.96


# newdata_control --------------------------------------------------------------
newdata_control <- data.frame(
  
  treatment = "control"
  
)

preds_control <- predict(
  
  mod_fib_serum, 
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
  
  mod_fib_serum, 
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

fit_fib_serum <- fit
fit_fib_serum$variable <- "fib_serum"


# plot -------------------------------------------------------------------------
fit %>%
  
  ggplot(
    
    aes(
      x = treatment, 
      y = fit_response,
      shape = treatment
    )
    
  ) +
  
  geom_jitter(
    
    data = fib_serum,
    aes(
      x = treatment,
      y = value,
      shape = treatment
    ),
    alpha = 1,
    size = 2,
    position = position_dodge(width = 0.5)
    
  ) +
  
  geom_errorbar(
    
    aes(
      ymin = lwr_response,
      ymax = upr_response
    ),
    width = 0.2,
    size = 0.4,
    position = position_dodge(width = 0.5)
    
  ) +
  
  geom_point(
    
    size = 5,
    fill = "white",
    position = position_dodge(width = 0.5)
    
  ) +
  
  scale_y_continuous(
    
    labels = scales::comma,
    limits = c(0, 5000)
    
  ) +
  
  ggtitle("") +
  
  xlab("") +
  
  ylab("Fibrinogen in Serum (ng/mL)") +
  
  labs(shape = "") +
  
  scale_x_discrete(
    labels = c("Control", "MG")
  ) +
  
  scale_shape_manual(
    values = c("control" = 21, "mg" = 22),
    labels = c("Control", "MG")
  ) +
  
  theme_bw() +
  
  theme(
    
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15)
    
  ) +
  
  geom_signif(
    
    y_position = 4800, 
    xmin = 1,
    xmax = 2,
    annotation = "n.s.", 
    tip_length = 0.005,
    color = "black",
    textsize = 5.5,
    vjust = -0.1
    
  )


# write .png -------------------------------------------------------------------
ggsave(
  
  "out/plot_fib_serum.png",
  height = 3.5,
  width = 4
  
)
