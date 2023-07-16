# Ottawa Replication Games
# May, 2023

# Replication of 
# Douenne, Thomas, and Adrien Fabre. "Yellow vests, pessimistic beliefs, and carbon tax aversion." American Economic Journal: Economic Policy 14.1 (2022): 81-110.

# Relevant package
package("tidyverse")
package("rdrobust")
package("memisc")
package("rddensity")
package("rddtools")
package("latex2exp")
package("gt")
package("modelsummary")
package("stargazer")
package("fastDummies")

# For summarising rdrobust models: https://stackoverflow.com/questions/67823100/print-tables-with-3-regression-output-models-from-rdrobust
tidy.rdrobust <- function(model, ...) {
  ret <- data.frame(
    term = row.names(model$coef),
    estimate = model$coef[, 1],
    std.error = model$se[, 1],
    p.value = model$pv[, 1]
  )
  row.names(ret) <- NULL
  ret
}

glance.rdrobust <- function(model, ...) {
  ret <- data.frame(
    Kernel = model$kernel,
    Bandwidth = model$bwselect,
    Polynomial = ifelse(model$p == 1, "Linear", ifelse(model$p == 2, "Quadratic", "OTHER")),
    Observations = sum(model$N_h)
  )
  ret
}


# Load the original csv
# This was created using "write_csv(s, "s_original.csv")" after running preparation.R
s <- read_csv("../replication/original_data/s_original.csv")

# Main outcome variable is "believes does not lose"
s$believes_does_not_lose <- s$gagnant_feedback_categorie != "Perdant"
# Some robustness checks also use "believes wins"
s$believes_wins <- s$gagnant_feedback_categorie == "Gagnant"

# Plot of mis-classified observations
#   Running variable is "simule_gain". This is predicted benefit in euros per year
#   Cutoff is "simule_gagnant". This should happen when simule_gain>0
#   The text (p.23) says "The binary win/lose feedback is a variable
#   \hat{Gamma} that jumps from 0 to 1 when our continuous estimation of respondents'
#   net gains \hat{gamma} exceeds the zero threshold.
ggplot(s %>% filter(simule_gain < 50 & simule_gain > -50) , aes(x=simule_gain , y=simule_gagnant, colour=simule_gain>0, shape=simule_gain>0)) + 
  geom_point() +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  labs(x = expression(paste("simule_gain (", hat(gamma), ")")),
       y = expression(paste("simule_gagnant (", hat(Gamma), ")"))) +
  annotate(geom="text", x=-60, y=0.75, label="Misclassified observations", hjust=0) +
  annotate(geom="segment", x=-30, y=0.8, xend=-15, yend=1,
           arrow = arrow()) +
  geom_vline(xintercept = 0) +
  theme(legend.position="bottom")
ggsave("../figures_tables/misclassified.png", width=6, height=6)

# Table of mis-classified observations
table(s$simule_gagnant,s$simule_gain>0)
# Restricted to -50 to 50
table(s$simule_gagnant[s$simule_gain>-50 & s$simule_gain<50], s$simule_gain[s$simule_gain>-50 & s$simule_gain<50]>0)

# Write the table  
tab_misclass <- s %>% 
  group_by(simule_gain>0, simule_gagnant==1) %>% 
  summarise(n = n(), .groups = "drop")
colnames(tab_misclass) <- c("$\\hat{\\gamma} > 0$", "$\\hat{\\Gamma} = 1$", "n")
sink("../figures_tables/classification_table.tex")
datasummary_df(tab_misclass, output = "latex_tabular", fmt = 0, align = "llc")
sink()


# This plot visualizes the estimation in the paper:
#   *Global* quadratic in the running variable
#   Manual bandwidth choice
#   Uniform Kernel
#   (note there are no covariates included in this estimation)
ggplot(s %>% filter(abs(simule_gain) < 50), aes(x=simule_gain, y=as.numeric(believes_does_not_lose))) + 
  geom_point(aes(colour=simule_gain>0)) +
  geom_smooth(method="lm", formula = y~x+I(x^2) + I(x>0), se=F, colour="black") +
  theme_bw() +
  labs(x=expression(paste("simule_gain (", hat(gamma), ")")),
       y="Probability believes does not lose") +
  theme(legend.position="bottom")
ggsave("../figures_tables/RD_plot_run_in_paper.png", width=6, height=6)


# Here is the rdplot corresponding to the estimation
#   Full support of running variable
#   4th degree *local* polynomial
#   Triangular kernel
#   (note that there are no covariates included in this estimation)
rdplot(y=s$believes_does_not_lose, x=s$simule_gain, c=0)
# With weights
rdplot(y=s$believes_does_not_lose, x=s$simule_gain, c=0, weights = s$weight,
       x.label = expression(paste("simule_gain (", hat(gamma), ")")),
       y.label = "Probability believes does not lose")
ggsave("../figures_tables/rd_plot.png", width = 6, height = 6)

#Plot dropping low-level outliers
s_within <- s %>% filter(simule_gain > -300)
rdplot(y=s_within$believes_does_not_lose, x=s_within$simule_gain, c=0, weights=s_within$weight,
       x.label = expression(paste("simule_gain (", hat(gamma), ")")),
       y.label = "Probability believes does not lose")
ggsave("../figures_tables/rd_plot_within.png", width = 6, height = 6)

# Density of observations
ggplot(s, aes(x=simule_gain)) +
  geom_density(colour="firebrick") +
  theme_bw() +
  labs(x = expression(paste("simule_gain (", hat(gamma), ")")))
ggsave("../figures_tables/simule_gain_density.png", width=6, height=6)


# Table with and without covariates//with and without corrected cutoff
# Their column (1)
c1 <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ simule_gagnant + tax_acceptance +
           Simule_gain + Simule_gain2 + single + hausse_depenses_par_uc +
           sexe + statut_emploi + csp + region + diplome4 + taille_menage +
           nb_14_et_plus + nb_adultes + fume + actualite + taille_agglo +
           uc + niveau_vie + age_18_24 + age_25_34 + age_35_49 + age_50_64 +
           percentile_revenu + I(pmin(percentile_revenu - 20, 0)) +
           I(pmin(percentile_revenu - 70, 0)) + percentile_revenu_conjoint +
           I(pmin(percentile_revenu_conjoint - 20, 0)) + I(pmin(percentile_revenu_conjoint -
                                                                  70, 0)) ,
         data = s, 
         subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
         weights = weight)

#summary(c1)$coef[, "Pr(>|t|)"]


# Their column (1) with no covariates
c2 <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ simule_gagnant +
           Simule_gain + Simule_gain2  ,
         data = s, 
         subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
         weights = weight)
#summary(c1)$coef[, "Pr(>|t|)"]
# Their column (1) with correctly coded win/lose
#summary(c1)$coef[, "Pr(>|t|)"]
c3 <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ (simule_gain>0) + tax_acceptance +
           Simule_gain + Simule_gain2 + single + hausse_depenses_par_uc +
           sexe + statut_emploi + csp + region + diplome4 + taille_menage +
           nb_14_et_plus + nb_adultes + fume + actualite + taille_agglo +
           uc + niveau_vie + age_18_24 + age_25_34 + age_35_49 + age_50_64 +
           percentile_revenu + I(pmin(percentile_revenu - 20, 0)) +
           I(pmin(percentile_revenu - 70, 0)) + percentile_revenu_conjoint +
           I(pmin(percentile_revenu_conjoint - 20, 0)) + I(pmin(percentile_revenu_conjoint -
                                                                  70, 0)) ,
         data = s, 
         subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
         weights = weight)

summary(c1)$coef[, "Pr(>|t|)"]

# Their column (1) with correctly coded win/lose and no covariates
c4 <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ (simule_gain>0) +
           Simule_gain + Simule_gain2  ,
         data = s, 
         subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
         weights = weight)
# Write to a table
tab42_v1_gof <- tibble::tribble(~raw,        ~clean,         ~fmt,
                                "nobs",      "Observations", 0,
                                "r.squared", "$R^2$",        3)
tab42_v1_rows <- tibble::tribble(~term,        ~c1, ~c2, ~c3, ~c4,
                                 "Covariates", "X", "",  "X", "")
attr(tab42_v1_rows, 'position') <- 7
sink("../figures_tables/table42_v1.tex")
modelsummary(list(c1,c2,c3,c4), output = "latex_tabular",
             fmt = 3, 
             statistic =c("({std.error})", "[{p.value}]"),
             coef_map = c("simule_gagnant" = "Coded as predicted winner ($\\hat{\\Gamma} = 1$)", 
                          "simule_gain > 0TRUE" = "Predicted winner ($\\hat{\\gamma} > 0$)"),
             gof_map = tab42_v1_gof, add_rows = tab42_v1_rows, escape = FALSE)
sink()


# Now implement as a RDD
rdd1 <- rdrobust(y=s$believes_does_not_lose, x=s$simule_gain, c=0, weights = s$weight)
# c4 <- RD with manual bandwidth and uniform kernel
rdd2 <- rdrobust(y=s$believes_does_not_lose, x=s$simule_gain, c=0, weights = s$weight, h=50, kernel = "uniform")
# uniform kernel 
rdd3 <- rdrobust(y=s$believes_does_not_lose, x=s$simule_gain, c=0, weights = s$weight, kernel="uniform")
# local quadratic
rdd4 <- rdrobust(y=s$believes_does_not_lose, x=s$simule_gain, c=0, weights = s$weight, p=2)

# code with covariates
s_cov <-  s %>%
  mutate(pr1 = (pmin(percentile_revenu - 20, 0)),
         pr2 = (pmin(percentile_revenu - 70, 0)),
         pr3 = (pmin(percentile_revenu_conjoint - 20, 0)),
         pr4 = (pmin(percentile_revenu_conjoint - 70, 0))) %>% 
  dplyr::select( tax_acceptance,
                 single,  hausse_depenses_par_uc, 
                 sexe,  statut_emploi,  csp,  region,  diplome4,  taille_menage, 
                 nb_14_et_plus, nb_adultes, fume, actualite, taille_agglo,
                 uc , niveau_vie , age_18_24 , age_25_34 , age_35_49 , age_50_64 ,
                 percentile_revenu , percentile_revenu_conjoint, pr1, pr2, pr3, pr4, weight)
# code as dummies
s_cov <- s_cov %>%
  dummy_cols()  %>%
  dplyr::select(-where(is.character))

rdd5 <- rdrobust(y=s$believes_does_not_lose, x=s$simule_gain, c=0, weights = s$weight, covs = s_cov)

# Use only observations that are correctly assigned to treatment
s_correct <- s %>%
  filter(as.numeric(simule_gain > 0) == simule_gagnant)
rdplot(y=s_correct$believes_does_not_lose, x=s_correct$simule_gain, c=0)
rdd6 <- rdrobust(y=s_correct$believes_does_not_lose, x=s_correct$simule_gain, c=0, weights = s_correct$weight)

# Write to a table
tab42_v2_rows <- tibble::tribble(~term,                ~rdd1,    ~rdd2,    ~rdd3,    ~rdd4,       ~rdd5,    ~rdd6,
                                 "Covariates",         "",       "",       "",       "",          "X",      "",
                                 "Drop misclassified", "",       "",       "",       "",          "",       "X")
attr(tab42_v2_rows, 'position') <- 13:14
sink("../figures_tables/table42_v2.tex")
modelsummary(list(rdd1,rdd2,rdd3,rdd4,rdd5,rdd6), output = "latex_tabular",
             fmt = 3, statistic =c("({std.error})", "[{p.value}]"),
             add_rows = tab42_v2_rows)
sink()


# What about placebo outcomes?
s$male <- s$sexe == "Masculin"
rdd_placebo1 <- rdrobust(y=s$male, x=s$simule_gain, c=0)
s$young <- s$age < 30
rdd_placebo2 <- rdrobust(y=s$young, x=s$simule_gain, c=0)
s$old <- s$age >= 65 
rdd_placebo3 <- rdrobust(y=s$old, x=s$simule_gain, c=0)
rdd_placebo4 <- rdrobust(y=s$taille_menage, x=s$simule_gain, c=0)
# Write to a table
sink("../figures_tables/table42_v3.tex")
modelsummary(list(rdd_placebo1,rdd_placebo2,rdd_placebo3,rdd_placebo4), output = "latex_tabular",
             fmt = 3, statistic =c("({std.error})", "[{p.value}]"))
sink()

# What about density? (a la mcrary)
rddensity(s$simule_gain, c=0) %>% summary()

# Placebo cutoffs 
pp <- rdd_data(y=s$believes_does_not_lose, x=s$simule_gain, cutpoint=0)
ppp <- rdd_reg_np(rdd_object=pp)
p1 <- plotPlacebo(ppp, plot = FALSE)
p1_placeb <- subset(p1, position!="True")
p1_true <- subset(p1, position=="True")
p1_last_left <- nrow(subset(p1_placeb, position=="left"))
p1_W <- diff(p1_placeb[c(p1_last_left, p1_last_left+1), "cutpoint"])/5
qplot(x=cutpoint, y=LATE, data=p1_placeb, geom="line", colour=position) +
  geom_smooth(aes(ymin=CI_low, ymax=CI_high), data=p1_placeb, stat="identity") +
  geom_hline(yintercept=0) +
  geom_point(aes(x=cutpoint, y=LATE), data=p1_true) +
  geom_errorbar(aes(ymin=CI_low, ymax=CI_high), data=p1_true, width=p1_W) +
  theme_bw() +
  theme(legend.position="none")
ggsave("../figures_tables/placebo_cutoffs.png", width = 6, height = 6)


# Replicate Table 4 column 3 -- heterogeneity analysis
# RDD on subset of yellow vests
s_gj <- s %>%
  filter(gilets_jaunes_dedans==1)
rdd1_gj <- rdrobust(y=s_gj$believes_does_not_lose, x=s_gj$simule_gain, c=0, weights = s_gj$weight)

# Replicate their column 3 of table 4 -- note that simule_gagnant is incorrectly coded here.
# This is pretty close to what they report
c3 <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ simule_gagnant + tax_acceptance +
           Simule_gain + Simule_gain2 + single + hausse_depenses_par_uc +
           sexe + statut_emploi + csp + region + diplome4 + taille_menage +
           nb_14_et_plus + nb_adultes + fume + actualite + taille_agglo +
           uc + niveau_vie + age_18_24 + age_25_34 + age_35_49 + age_50_64 +
           percentile_revenu + I(pmin(percentile_revenu - 20, 0)) +
           I(pmin(percentile_revenu - 70, 0)) + percentile_revenu_conjoint +
           I(pmin(percentile_revenu_conjoint - 20, 0)) + I(pmin(percentile_revenu_conjoint -
                                                                  70, 0)) +
           # These are the additional controls for column 3
           simule_gagnant * (gilets_jaunes > 0) +
           simule_gagnant * tax_acceptance +
           simule_gagnant * (gagnant_categorie!='Perdant') ,
         data = s, 
         subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
         weights = weight)
summary(c3)

# Add interactions between the interaction variables and the running variable
c3a <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ simule_gagnant + tax_acceptance +
            Simule_gain + Simule_gain2 + single + hausse_depenses_par_uc +
            sexe + statut_emploi + csp + region + diplome4 + taille_menage +
            nb_14_et_plus + nb_adultes + fume + actualite + taille_agglo +
            uc + niveau_vie + age_18_24 + age_25_34 + age_35_49 + age_50_64 +
            percentile_revenu + I(pmin(percentile_revenu - 20, 0)) +
            I(pmin(percentile_revenu - 70, 0)) + percentile_revenu_conjoint +
            I(pmin(percentile_revenu_conjoint - 20, 0)) + I(pmin(percentile_revenu_conjoint -
                                                                   70, 0)) +
            # These are the additional controls for column 3
            simule_gagnant * (gilets_jaunes > 1) +
            simule_gagnant * tax_acceptance +
            simule_gagnant * (gagnant_categorie!='Perdant') +
            # These are interactions between the sub group variables and running variables
            Simule_gain * (gilets_jaunes > 1) +
            Simule_gain2 * (gilets_jaunes > 1) +
            Simule_gain * tax_acceptance +
            Simule_gain2 * tax_acceptance +
            Simule_gain * (gagnant_categorie!='Perdant') +
            Simule_gain2 * (gagnant_categorie!='Perdant') 
          
          ,
          data = s, 
          subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
          weights = weight)
summary(c3a)

c3_all_covariates <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ simule_gagnant + tax_acceptance +
           Simule_gain + Simule_gain2 + single + hausse_depenses_par_uc +
           sexe + statut_emploi + csp + region + diplome4 + taille_menage +
           nb_14_et_plus + nb_adultes + fume + actualite + taille_agglo +
           uc + niveau_vie + age_18_24 + age_25_34 + age_35_49 + age_50_64 +
           percentile_revenu + I(pmin(percentile_revenu - 20, 0)) +
           I(pmin(percentile_revenu - 70, 0)) + percentile_revenu_conjoint +
           I(pmin(percentile_revenu_conjoint - 20, 0)) + I(pmin(percentile_revenu_conjoint -
                                                                  70, 0)) +
           # These are the additional controls for column 3
           simule_gagnant * (gilets_jaunes > 0) +
           simule_gagnant * tax_acceptance +
           simule_gagnant * (gagnant_categorie!='Perdant') ,
         data = s, 
         subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
         weights = weight)
summary(c3_all_covariates)

c3_only_acceptance <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ simule_gagnant + tax_acceptance +
           Simule_gain + Simule_gain2 + single + hausse_depenses_par_uc +
           sexe + statut_emploi + csp + region + diplome4 + taille_menage +
           nb_14_et_plus + nb_adultes + fume + actualite + taille_agglo +
           uc + niveau_vie + age_18_24 + age_25_34 + age_35_49 + age_50_64 +
           percentile_revenu + I(pmin(percentile_revenu - 20, 0)) +
           I(pmin(percentile_revenu - 70, 0)) + percentile_revenu_conjoint +
           I(pmin(percentile_revenu_conjoint - 20, 0)) + I(pmin(percentile_revenu_conjoint -
                                                                  70, 0)) +
           simule_gagnant * tax_acceptance +
           simule_gagnant * (gagnant_categorie!='Perdant') ,
         data = s, 
         subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
         weights = weight)
summary(c3_only_acceptance)

c3_only_yv <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ simule_gagnant + tax_acceptance +
                           Simule_gain + Simule_gain2 + single + hausse_depenses_par_uc +
                           sexe + statut_emploi + csp + region + diplome4 + taille_menage +
                           nb_14_et_plus + nb_adultes + fume + actualite + taille_agglo +
                           uc + niveau_vie + age_18_24 + age_25_34 + age_35_49 + age_50_64 +
                           percentile_revenu + I(pmin(percentile_revenu - 20, 0)) +
                           I(pmin(percentile_revenu - 70, 0)) + percentile_revenu_conjoint +
                           I(pmin(percentile_revenu_conjoint - 20, 0)) + I(pmin(percentile_revenu_conjoint -
                                                                                  70, 0)) +
                           simule_gagnant * (gilets_jaunes > 0) +
                           simule_gagnant * (gagnant_categorie!='Perdant') ,
                         data = s, 
                         subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
                         weights = weight)
summary(c3_only_yv)


c3a_all_covariates <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ simule_gagnant + tax_acceptance +
            Simule_gain + Simule_gain2 + single + hausse_depenses_par_uc +
            sexe + statut_emploi + csp + region + diplome4 + taille_menage +
            nb_14_et_plus + nb_adultes + fume + actualite + taille_agglo +
            uc + niveau_vie + age_18_24 + age_25_34 + age_35_49 + age_50_64 +
            percentile_revenu + I(pmin(percentile_revenu - 20, 0)) +
            I(pmin(percentile_revenu - 70, 0)) + percentile_revenu_conjoint +
            I(pmin(percentile_revenu_conjoint - 20, 0)) + I(pmin(percentile_revenu_conjoint -
                                                                   70, 0)) +
            # These are the additional controls for column 3
            simule_gagnant * (gilets_jaunes > 0) +
            simule_gagnant * tax_acceptance +
            simule_gagnant * (gagnant_categorie!='Perdant') +
            # These are interactions between the sub group variables and running variables
            Simule_gain * (gilets_jaunes > 0) +
            Simule_gain2 * (gilets_jaunes > 0) +
            Simule_gain * tax_acceptance +
            Simule_gain2 * tax_acceptance +
            Simule_gain * (gagnant_categorie!='Perdant') +
            Simule_gain2 * (gagnant_categorie!='Perdant') 
          
          ,
          data = s, 
          subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
          weights = weight)
summary(c3a_all_covariates)

# Add interactions between the interaction variables and the running variable
c3a_only_acceptance <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ simule_gagnant + tax_acceptance +
            Simule_gain + Simule_gain2 + single + hausse_depenses_par_uc +
            sexe + statut_emploi + csp + region + diplome4 + taille_menage +
            nb_14_et_plus + nb_adultes + fume + actualite + taille_agglo +
            uc + niveau_vie + age_18_24 + age_25_34 + age_35_49 + age_50_64 +
            percentile_revenu + I(pmin(percentile_revenu - 20, 0)) +
            I(pmin(percentile_revenu - 70, 0)) + percentile_revenu_conjoint +
            I(pmin(percentile_revenu_conjoint - 20, 0)) + I(pmin(percentile_revenu_conjoint -
                                                                   70, 0)) +
            # These are the additional controls for column 3
            simule_gagnant * tax_acceptance +
            simule_gagnant * (gagnant_categorie!='Perdant') +
            # These are interactions between the sub group variables and running variables
            Simule_gain * tax_acceptance +
            Simule_gain2 * tax_acceptance +
            Simule_gain * (gagnant_categorie!='Perdant') +
            Simule_gain2 * (gagnant_categorie!='Perdant') 
          
          ,
          data = s, 
          subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
          weights = weight)
summary(c3a_only_acceptance)

# Add interactions between the interaction variables and the running variable
c3a_only_yv <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ simule_gagnant + tax_acceptance +
            Simule_gain + Simule_gain2 + single + hausse_depenses_par_uc +
            sexe + statut_emploi + csp + region + diplome4 + taille_menage +
            nb_14_et_plus + nb_adultes + fume + actualite + taille_agglo +
            uc + niveau_vie + age_18_24 + age_25_34 + age_35_49 + age_50_64 +
            percentile_revenu + I(pmin(percentile_revenu - 20, 0)) +
            I(pmin(percentile_revenu - 70, 0)) + percentile_revenu_conjoint +
            I(pmin(percentile_revenu_conjoint - 20, 0)) + I(pmin(percentile_revenu_conjoint -
                                                                   70, 0)) +
            # These are the additional controls for column 3
            simule_gagnant * (gilets_jaunes > 0) +
            simule_gagnant * (gagnant_categorie!='Perdant') +
            # These are interactions between the sub group variables and running variables
            Simule_gain * (gilets_jaunes > 0) +
            Simule_gain2 * (gilets_jaunes > 0) +
            Simule_gain * (gagnant_categorie!='Perdant') +
            Simule_gain2 * (gagnant_categorie!='Perdant') 
          
          ,
          data = s, 
          subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
          weights = weight)
summary(c3a_only_yv)


# Replace quadratic with local linear
c3b <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ simule_gagnant + tax_acceptance +
            simule_gain * simule_gagnant + single + hausse_depenses_par_uc +
            sexe + statut_emploi + csp + region + diplome4 + taille_menage +
            nb_14_et_plus + nb_adultes + fume + actualite + taille_agglo +
            uc + niveau_vie + age_18_24 + age_25_34 + age_35_49 + age_50_64 +
            percentile_revenu + I(pmin(percentile_revenu - 20, 0)) +
            I(pmin(percentile_revenu - 70, 0)) + percentile_revenu_conjoint +
            I(pmin(percentile_revenu_conjoint - 20, 0)) + I(pmin(percentile_revenu_conjoint -
                                                                   70, 0)) +
            # These are the additional controls for column 3
            simule_gagnant * (gilets_jaunes > 1) +
            simule_gagnant * tax_acceptance +
            simule_gagnant * (gagnant_categorie!='Perdant') +
            # These are interactions between the sub group variables and running variables
            (simule_gain * simule_gagnant) * (gilets_jaunes > 1) +
            (simule_gain * simule_gagnant) * tax_acceptance +
            (simule_gain * simule_gagnant) * (gagnant_categorie!='Perdant') 
          
          ,
          data = s, 
          subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
          weights = weight)
summary(c3b)

c3b_all_covariates <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ simule_gagnant + tax_acceptance +
            simule_gain * simule_gagnant + single + hausse_depenses_par_uc +
            sexe + statut_emploi + csp + region + diplome4 + taille_menage +
            nb_14_et_plus + nb_adultes + fume + actualite + taille_agglo +
            uc + niveau_vie + age_18_24 + age_25_34 + age_35_49 + age_50_64 +
            percentile_revenu + I(pmin(percentile_revenu - 20, 0)) +
            I(pmin(percentile_revenu - 70, 0)) + percentile_revenu_conjoint +
            I(pmin(percentile_revenu_conjoint - 20, 0)) + I(pmin(percentile_revenu_conjoint -
                                                                   70, 0)) +
            # These are the additional controls for column 3
            simule_gagnant * (gilets_jaunes > 0) +
            simule_gagnant * tax_acceptance +
            simule_gagnant * (gagnant_categorie!='Perdant') +
            # These are interactions between the sub group variables and running variables
            (simule_gain * simule_gagnant) * (gilets_jaunes > 0) +
            (simule_gain * simule_gagnant) * tax_acceptance +
            (simule_gain * simule_gagnant) * (gagnant_categorie!='Perdant') 
          
          ,
          data = s, 
          subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
          weights = weight)
summary(c3b_all_covariates)

# Replace quadratic with local linear
c3b_only_acceptance <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ simule_gagnant + tax_acceptance +
            simule_gain * simule_gagnant + single + hausse_depenses_par_uc +
            sexe + statut_emploi + csp + region + diplome4 + taille_menage +
            nb_14_et_plus + nb_adultes + fume + actualite + taille_agglo +
            uc + niveau_vie + age_18_24 + age_25_34 + age_35_49 + age_50_64 +
            percentile_revenu + I(pmin(percentile_revenu - 20, 0)) +
            I(pmin(percentile_revenu - 70, 0)) + percentile_revenu_conjoint +
            I(pmin(percentile_revenu_conjoint - 20, 0)) + I(pmin(percentile_revenu_conjoint -
                                                                   70, 0)) +
            # These are the additional controls for column 3
            simule_gagnant * tax_acceptance +
            simule_gagnant * (gagnant_categorie!='Perdant') +
            # These are interactions between the sub group variables and running variables
            (simule_gain * simule_gagnant) * tax_acceptance +
            (simule_gain * simule_gagnant) * (gagnant_categorie!='Perdant') 
          
          ,
          data = s, 
          subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
          weights = weight)
summary(c3b_only_acceptance)

# Replace quadratic with local linear
c3b_only_yv <- lm(formula =  gagnant_feedback_categorie != "Perdant" ~ simule_gagnant + tax_acceptance +
            simule_gain * simule_gagnant + single + hausse_depenses_par_uc +
            sexe + statut_emploi + csp + region + diplome4 + taille_menage +
            nb_14_et_plus + nb_adultes + fume + actualite + taille_agglo +
            uc + niveau_vie + age_18_24 + age_25_34 + age_35_49 + age_50_64 +
            percentile_revenu + I(pmin(percentile_revenu - 20, 0)) +
            I(pmin(percentile_revenu - 70, 0)) + percentile_revenu_conjoint +
            I(pmin(percentile_revenu_conjoint - 20, 0)) + I(pmin(percentile_revenu_conjoint -
                                                                   70, 0)) +
            # These are the additional controls for column 3
            simule_gagnant * (gilets_jaunes > 0) +
            simule_gagnant * (gagnant_categorie!='Perdant') +
            # These are interactions between the sub group variables and running variables
            (simule_gain * simule_gagnant) * (gilets_jaunes > 0) +
            (simule_gain * simule_gagnant) * (gagnant_categorie!='Perdant') 
          
          ,
          data = s, 
          subset = variante_taxe_info == 'f' & abs(simule_gain) < 50,
          weights = weight)
summary(c3b_only_yv)

# Write to a table
tab42_v4_gof <- tibble::tribble(~raw,        ~clean,         ~fmt,
                                "nobs",      "Observations", 0,
                                "r.squared", "$R^2$",        3)
tab42_v4_rows <- tibble::tribble(~term,                         ~c3,                ~c3a,                ~c3b,
                                 "Polynomial",                  "Global quadratic", "Global quadratic",  "Local linear",
                                 "Interactions with polynomial", "",                 "X",                 "X")
attr(tab42_v4_rows, 'position') <- 19:20
sink("../replication/figures_tables/table42_v4.tex")
modelsummary(list(c3,c3a,c3b), output = "latex_tabular",
             fmt = 3, statistic =c("({std.error})", "[{p.value}]"),
             coef_map = c("simule_gagnant" = "Predicted winner ($\\hat{\\Gamma} == 1$)", 
                          "tax_acceptanceTRUE" = "Initial tax acceptance ($A^0$)",
                          "gilets_jaunes > 0TRUE" = "Yellow Vests supporter",
                          "simule_gagnant:tax_acceptanceTRUE" = "$\\hat{\\Gamma} \\times A^0$",
                          "simule_gagnant:gilets_jaunes > 0TRUE" = "$\\hat{\\Gamma} \\times $ Yellow Vests supporter",
                          "simule_gagnant:gagnant_categorie != \"Perdant\"TRUE" = "$\\hat{\\Gamma} \\times G$"),
             gof_map = tab42_v4_gof, add_rows = tab42_v4_rows, escape = FALSE)
sink()

# Create rdplots for heterogeneity
rdplot(y=s_within$believes_does_not_lose, x=s_within$simule_gain, c=0, weights=s_within$weight,
       subset = s$tax_acceptance==TRUE,
       x.label = expression(paste("simule_gain (", hat(gamma), ")")),
       y.label = "Probability believes does not lose", y.lim = c(0, 1),
       title = "RD Plot - Respondents who initially accept tax")
ggsave("../figures_tables/rd_plot_het_tax.png", width = 6, height = 6)
rdplot(y=s_within$believes_does_not_lose, x=s_within$simule_gain, c=0, weights=s_within$weight,
       subset = s$tax_acceptance!=TRUE,
       x.label = expression(paste("simule_gain (", hat(gamma), ")")),
       y.label = "Probability believes does not lose", y.lim = c(0, 1),
       title = "RD Plot - Respondents who initially do not accept tax")
ggsave("../figures_tables/rd_plot_het_notax.png", width = 6, height = 6)
rdplot(y=s_within$believes_does_not_lose, x=s_within$simule_gain, c=0, weights=s_within$weight,
       subset = s$gilets_jaunes > 0,
       x.label = expression(paste("simule_gain (", hat(gamma), ")")),
       y.label = "Probability believes does not lose", y.lim = c(0, 1),
       title = "RD Plot - Respondents who support Yellow Vests")
ggsave("../figures_tables/rd_plot_het_gj.png", width = 6, height = 6)
rdplot(y=s_within$believes_does_not_lose, x=s_within$simule_gain, c=0, weights=s_within$weight,
       subset = s$gilets_jaunes <= 0,
       x.label = expression(paste("simule_gain (", hat(gamma), ")")),
       y.label = "Probability believes does not lose", y.lim = c(0, 1),
       title = "RD Plot - Respondents who do not support Yellow Vests")
ggsave("../figures_tables/rd_plot_het_nogj.png", width = 6, height = 6)
rdplot(y=s_within$believes_does_not_lose, x=s_within$simule_gain, c=0, weights=s_within$weight,
       subset = s$gagnant_categorie!='Perdant',
       x.label = expression(paste("simule_gain (", hat(gamma), ")")),
       y.label = "Probability believes does not lose", y.lim = c(0, 1),
       title = "RD Plot - Respondents who initially believe they do not lose")
ggsave("../figures_tables/rd_plot_het_nolose.png", width = 6, height = 6)
rdplot(y=s_within$believes_does_not_lose, x=s_within$simule_gain, c=0, weights=s_within$weight,
       subset = s$gagnant_categorie=='Perdant',
       x.label = expression(paste("simule_gain (", hat(gamma), ")")),
       y.label = "Probability believes does not lose", y.lim = c(0, 1),
       title = "RD Plot - Respondents who initially believe they lose")
ggsave("../figures_tables/rd_plot_het_lose.png", width = 6, height = 6)
