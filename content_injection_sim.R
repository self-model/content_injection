# Requires ggplot2 >= 3.4.0 (uses the `linewidth` argument).
library(ggplot2)
library(grid)

# ---------------------------------------------------------------------------
# Figure 1: how partialing injects content into the adjusted scale.
# All numbers in the figure are computed from the simulation below.
# ---------------------------------------------------------------------------

show_syringe <- TRUE        # set to FALSE to remove the syringes in the adjusted panels
out_stub     <- "figure_1"

# ---- Simulation ------------------------------------------------------------
set.seed(3)
n <- 100000
dep_effect <- -0.2          # effect of the depression latent on observed task confidence

# latents 
pp         <- rnorm(n)      # shared psychopathology latent
depression <- rnorm(n)      # latent depression 
OC         <- rnorm(n)      # latent OCD 
confidence <- rnorm(n)      # latent confidence 
# observed variables 
observed_confidence <- confidence + dep_effect * depression
OCD_scale <- pp + OC
dep_scale   <- pp + depression

# ---- Stats reported in text (toy model paragraph) --------------------------
b_dep_simple   <- coef(lm(observed_confidence ~ dep_scale))[2]
b_ocd_simple   <- coef(lm(observed_confidence ~ OCD_scale))[2]

fit_multiple   <- lm(observed_confidence ~ dep_scale + OCD_scale)
b_dep_multiple <- coef(fit_multiple)["dep_scale"]
b_ocd_multiple <- coef(fit_multiple)["OCD_scale"]

cat("\n--- Stats reported in the text ---\n")
cat(sprintf("Simple:   confidence ~ depression scale   beta = %.2f\n", b_dep_simple))
cat(sprintf("Simple:   confidence ~ OCD scale          beta = %.2f\n", b_ocd_simple))
cat(sprintf("Multiple: depression scale (with OCD)     beta = %.2f\n", b_dep_multiple))
cat(sprintf("Multiple: OCD scale (with depression)     beta = %.2f\n", b_ocd_multiple))

# ---- Quantities used in the figure -----------------------------------------
beta_dep_on_ocd <- coef(lm(dep_scale ~ OCD_scale))[2]
beta_ocd_on_dep <- coef(lm(OCD_scale ~ dep_scale))[2]

dep_scale_resid   <- dep_scale   - beta_dep_on_ocd * OCD_scale
OCD_scale_resid <- OCD_scale - beta_ocd_on_dep * dep_scale
beta_conf_on_dep_adj <- coef(lm(observed_confidence ~ dep_scale_resid))[2]
beta_conf_on_ocd_adj <- coef(lm(observed_confidence ~ OCD_scale_resid))[2]

# Bar lengths for panel B: how much each adjusted scale loads on each latent variable.
# Each scale is a sum of latents:  dep_scale = l.psyc + l.dep,  OCD_scale = l.psyc + l.ocd.
# Adjusting subtracts a multiple (beta) of the other scale, so the adjusted scale becomes, e.g.:
#   dep_scale_resid = (l.psyc + l.dep) - beta * (l.psyc + l.ocd)
#                   = 1 * l.dep  -  beta * l.ocd  +  (1 - beta) * l.psyc
# That gives the weights below (beta is estimated from the data, ~0.5):
#   l.conf:  0          (confidence is in neither scale)
#   l.dep:   1          (the scale's own latent is unchanged)
#   l.ocd:   -beta      (injected: the other scale's unique latent enters with a negative weight)
#   l.psyc:  1 - beta   (the shared latent is reduced, not removed)
# Order of each vector: l.conf, l.dep, l.ocd, l.psyc. The OCD scale is the mirror image.
w_dep_adj <- c(0, 1, -beta_dep_on_ocd, 1 - beta_dep_on_ocd)
w_ocd_adj <- c(0, -beta_ocd_on_dep, 1, 1 - beta_ocd_on_dep)

cat("\n--- Values shown in Figure 1 (from the simulation) ---\n")
cat(sprintf("Simple:   confidence ~ depression  beta = %+.3f\n", b_dep_simple))
cat(sprintf("Simple:   confidence ~ OCD         beta = %+.3f\n", b_ocd_simple))
cat(sprintf("Adjusted: confidence ~ depression  beta = %+.3f\n", beta_conf_on_dep_adj))
cat(sprintf("Adjusted: confidence ~ OCD         beta = %+.3f\n", beta_conf_on_ocd_adj))
cat("Adjusted depression weights (conf, dep, ocd, psyc):", round(w_dep_adj, 3), "\n")
cat("Adjusted OCD weights        (conf, dep, ocd, psyc):", round(w_ocd_adj, 3), "\n\n")

# ---- Style -----------------------------------------------------------------
blue <- "#4A90D9"; red <- "#E53935"
fill_of <- function(f) switch(f, blue = blue, red = red, gray = "grey60", NA)

W <- 14.2; H <- 5.5             # figure size in inches; all coordinates below are in inches
lat_labels <- c("l.conf", "l.dep", "l.ocd", "l.psyc")
row_h  <- 0.30                  # vertical pitch of one bar row
bar_h  <- 0.22                  # height of a bar
unit_w <- 1.20                  # bar length for a weight of 1 (inches)
ax_off <- 1.30                  # distance from the left edge of a panel to its zero line

IN <- function(v) unit(v, "native")   # root viewport uses inches as native units

# ---- Bars ------------------------------------------------------------------
draw_syringe <- function(tip_x, tip_y, angle, len = 0.52, liquid = blue) {
  # vector syringe; needle tip at (tip_x, tip_y) in inches, tail extends along `angle`
  pushViewport(viewport(x = IN(tip_x), y = IN(tip_y), width = unit(len, "in"), height = unit(0.2, "in"),
                        just = c("left", "center"), angle = angle))
  grid.segments(0, 0.5, 0.20, 0.5, gp = gpar(lwd = 1.2))
  grid.rect(0.20, 0.5, 0.05, 0.42, just = "left", gp = gpar(fill = "grey85", lwd = 0.9))
  grid.rect(0.25, 0.5, 0.55, 0.78, just = "left", gp = gpar(fill = "white", lwd = 1.1))
  grid.rect(0.265, 0.5, 0.36, 0.58, just = "left", gp = gpar(fill = liquid, col = NA))
  for (tx in c(0.66, 0.72, 0.78)) grid.segments(tx, 0.89, tx, 0.78, gp = gpar(lwd = 0.6))
  grid.rect(0.80, 0.5, 0.025, 1.0, just = "left", gp = gpar(fill = "white", lwd = 1.1))
  grid.segments(0.825, 0.5, 0.96, 0.5, gp = gpar(lwd = 1.5))
  grid.rect(0.96, 0.5, 0.035, 0.85, just = "left", gp = gpar(fill = "grey30", col = "grey30"))
  popViewport()
}

# x0: left edge of the panel; ybot: y of the bottom axis line
draw_bars <- function(x0, ybot, title, title_col, vals, fills,
                      syringe_row = NULL, syringe_col = blue, syringe_angle = 140) {
  axx  <- x0 + ax_off
  yrow <- ybot + 0.10                      # bottom of the first (lowest) row
  ytop <- yrow + 4 * row_h
  xl <- axx - 0.5 * unit_w                 # position of -0.5
  xr <- axx + unit_w                       # position of 1
  
  # zero line and bottom scale (-0.5, 0, 0.5, 1)
  grid.segments(IN(axx), IN(ybot), IN(axx), IN(ytop), gp = gpar(lwd = 1.6))
  tick_x <- axx + c(-0.5, 0, 0.5, 1) * unit_w
  grid.segments(IN(xl), IN(ybot), IN(xr), IN(ybot), gp = gpar(lwd = 1.2))
  grid.segments(IN(tick_x), IN(ybot), IN(tick_x), IN(ybot - 0.06), gp = gpar(lwd = 1.2))
  grid.text(c("\u22120.5", "0", "0.5", "1"), x = IN(tick_x), y = IN(ybot - 0.19),
            gp = gpar(fontsize = 12.5))
  
  # latent names, outside the scale on the left
  ym_all <- ytop - (1:4 - 0.5) * row_h
  # left-aligned in one column (the longest label ends 0.15 in before the -0.5 tick)
  lab_w <- max(convertWidth(grobWidth(textGrob(lat_labels, gp = gpar(fontsize = 13))), "in", valueOnly = TRUE))
  grid.text(lat_labels, x = IN(xl - 0.15 - lab_w), y = IN(ym_all), just = "left", gp = gpar(fontsize = 13))
  
  # bars
  for (i in 1:4) {
    v <- vals[i]; f <- fills[i]
    if (is.na(f) || abs(v) < 1e-9) next
    yb <- ym_all[i] - bar_h / 2
    bw <- abs(v) * unit_w
    bx <- if (v >= 0) axx else axx - bw
    grid.rect(x = IN(bx), y = IN(yb), width = unit(bw, "in"), height = unit(bar_h, "in"),
              just = c("left", "bottom"), gp = gpar(fill = fill_of(f), col = NA))
    grid.rect(x = IN(bx), y = IN(yb), width = unit(bw, "in"), height = unit(bar_h, "in"),
              just = c("left", "bottom"), gp = gpar(fill = NA, col = "black", lwd = 1.2))
  }
  
  grid.text(title, x = IN(axx + 0.2), y = IN(ytop + 0.12), just = c("center", "bottom"),
            gp = gpar(fontsize = 14, fontface = "bold", col = title_col))
  
  if (!is.null(syringe_row)) {   # needle tip rests on the top edge of the injected bar
    tip_y <- ym_all[syringe_row] + bar_h / 2 + 0.01
    draw_syringe(tip_x = axx - 0.10, tip_y = tip_y, angle = syringe_angle, liquid = syringe_col)
  }
}

# ---- Scatter panels (regression line; beta in the top right) ---------------
fmt_beta <- function(b) {
  r <- round(b, 2)
  if (r == 0) "\u03b2 = 0.00"
  else if (r > 0) sprintf("\u03b2 = +%.2f", r)
  else sprintf("\u03b2 = \u2212%.2f", abs(r))
}

make_scatter <- function(x, y, beta_val, xlab_text, ylab_text) {
  df <- data.frame(x = x, y = y)
  ggplot(df, aes(x, y)) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "black", fullrange = TRUE, linewidth = 1.3) +
    annotate("text", x = 3.9, y = 0.9, label = fmt_beta(beta_val), hjust = 1, size = 5.4) +
    coord_cartesian(xlim = c(-4, 4), ylim = c(-1, 1)) +
    scale_x_continuous(breaks = c(-4, 0, 4), labels = c("\u22124", "0", "4")) +
    scale_y_continuous(breaks = c(-1, 0, 1), labels = c("\u22121", "0", "1")) +
    theme_classic() +
    theme(aspect.ratio = 1,
          axis.title = element_text(size = 12.5, face = "bold"),
          axis.text = element_text(size = 11.5, colour = "black"),
          plot.margin = margin(t = 6, r = 8, b = 6, l = 5),
          plot.background = element_rect(fill = NA, colour = NA),
          panel.background = element_rect(fill = NA, colour = NA)) +
    labs(x = xlab_text, y = ylab_text)
}

draw_gg <- function(p, x0, y0, x1, y1) {
  pushViewport(viewport(x = IN(x0), y = IN(y0), width = unit(x1 - x0, "in"), height = unit(y1 - y0, "in"),
                        just = c("left", "bottom")))
  grid.draw(ggplotGrob(p))
  popViewport()
}

# ---- Assemble the figure ---------------------------------------------------
draw_figure <- function() {
  grid.newpage()
  pushViewport(viewport(xscale = c(0, W), yscale = c(0, H)))
  
  hy <- H - 0.25
  grid.text("A", x = IN(0.10), y = IN(hy), just = "left", gp = gpar(fontsize = 20, fontface = "bold"))
  grid.text("Generating model and simple regressions", x = IN(0.50), y = IN(hy), just = "left",
            gp = gpar(fontsize = 16))
  grid.text("B", x = IN(8.90), y = IN(hy), just = "left", gp = gpar(fontsize = 20, fontface = "bold"))
  grid.text("Each scale adjusted for the other", x = IN(9.35), y = IN(hy), just = "left",
            gp = gpar(fontsize = 16))
  
  # rows: scatter panels span [yb, yb + rh]; the bar blocks' bottom axes sit at bb
  rh  <- 2.1
  yb2 <- 0.15; yb1 <- yb2 + rh + 0.40
  bb1 <- yb1 + 0.66; bb2 <- yb2 + 0.66
  bbm <- (bb1 + bb2) / 2          # Task confidence panel centred between the two rows
  
  # Panel A
  draw_bars(0.20, bbm, "Task confidence", "black",
            c(1, dep_effect, 0, 0), c("gray", "gray", NA, NA))
  draw_bars(3.00, bb1, "Depression scale", blue, c(0, 1, 0, 1), c(NA, "blue", NA, "blue"))
  draw_bars(3.00, bb2, "OCD scale", red, c(0, 0, 1, 1), c(NA, NA, "red", "red"))
  draw_gg(make_scatter(dep_scale,   observed_confidence, b_dep_simple, "Depression scale", "Task confidence"),
          5.90, yb1, 7.90, yb1 + rh)
  draw_gg(make_scatter(OCD_scale, observed_confidence, b_ocd_simple, "OCD scale", "Task confidence"),
          5.90, yb2, 7.90, yb2 + rh)
  
  # Panel B
  draw_bars(8.85, bb1, "Depression scale (adjusted)", blue, w_dep_adj, c(NA, "blue", "red", "blue"),
            syringe_row = if (show_syringe) 3 else NULL, syringe_col = red)
  draw_bars(8.85, bb2, "OCD scale (adjusted)", red, w_ocd_adj, c(NA, "blue", "red", "red"),
            syringe_row = if (show_syringe) 2 else NULL, syringe_col = blue)
  draw_gg(make_scatter(dep_scale_resid,   observed_confidence, beta_conf_on_dep_adj, "Depression scale (adj.)", "Task confidence"),
          11.65, yb1, 13.65, yb1 + rh)
  draw_gg(make_scatter(OCD_scale_resid, observed_confidence, beta_conf_on_ocd_adj, "OCD scale (adj.)", "Task confidence"),
          11.65, yb2, 13.65, yb2 + rh)
  popViewport()
}

#save figure png and cairo pdf 
png(paste0(out_stub, ".png"), width = W, height = H, units = "in", res = 300, type = "cairo", bg = "white")
draw_figure(); invisible(dev.off())
cairo_pdf(paste0(out_stub, ".pdf"), width = W, height = H)
draw_figure(); invisible(dev.off())