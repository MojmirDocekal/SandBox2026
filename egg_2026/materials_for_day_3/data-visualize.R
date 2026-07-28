#| echo: false
source("_common.R")

#| label: setup
library(tidyverse)

#| eval: false
# install.packages("tidyverse")
library(tidyverse)

library(palmerpenguins)
library(ggthemes)

penguins

glimpse(penguins)

attach(penguins)

View(penguins)

#| echo: false
#| warning: false
#| fig-alt: |
#|   A scatterplot of body mass vs. flipper length of penguins, with a
#|   best fit line of the relationship between these two variables
#|   overlaid. The plot displays a positive, fairly linear, and relatively
#|   strong relationship between these two variables. Species (Adelie,
#|   Chinstrap, and Gentoo) are represented with different colors and
#|   shapes. The relationship between body mass and flipper length is
#|   roughly the same for these three species, and Gentoo penguins are
#|   larger than penguins from the other two species.
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)",
    y = "Body mass (g)",
    color = "Species",
    shape = "Species"
  ) +
  scale_color_colorblind()

# let's add a model (lme4)

library(lme4)
# install.packages("lme4")

# Fit an LMM for body mass vs flipper length with species as a grouping factor.
penguins_lmm <- penguins |>
  select(body_mass_g, flipper_length_mm, species) |>
  drop_na() |>
  mutate(flipper_length_c = flipper_length_mm - mean(flipper_length_mm))

mod_bodymass_flipper <- lmer(
  body_mass_g ~ flipper_length_c + (1 | species),
  data = penguins_lmm
)

summary(mod_bodymass_flipper)

# 95% CI for fixed effects (Wald CI; fast and commonly reported for lme4).
confint(mod_bodymass_flipper, parm = "beta_", method = "Wald")

# Optional: allow the flipper-length slope to vary by species.
mod_bodymass_flipper_rs <- lmer(
  body_mass_g ~ flipper_length_c + (flipper_length_c | species),
  data = penguins_lmm
)

summary(mod_bodymass_flipper_rs)

# 95% CI for fixed effects in the random-slope model.
confint(mod_bodymass_flipper_rs, parm = "beta_", method = "Wald")

# Compare models with a likelihood-ratio test (fit with ML for comparison).
mod_bodymass_flipper_ml <- update(mod_bodymass_flipper, REML = FALSE)
mod_bodymass_flipper_rs_ml <- update(mod_bodymass_flipper_rs, REML = FALSE)

anova(mod_bodymass_flipper_ml, mod_bodymass_flipper_rs_ml)


#| fig-alt: |
#|   A blank, gray plot area.
ggplot(data = penguins)

#| fig-alt: |
#|   The plot shows flipper length on the x-axis, with values that range from
#|   170 to 230, and body mass on the y-axis, with values that range from 3000
#|   to 6000.
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
)

#| fig-alt: |
#|   A scatterplot of body mass vs. flipper length of penguins. The plot
#|   displays a positive, linear, and relatively strong relationship between
#|   these two variables.
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point()

#| warning: false
#| fig-alt: |
#|   A scatterplot of body mass vs. flipper length of penguins. The plot
#|   displays a positive, fairly linear, and relatively strong relationship
#|   between these two variables. Species (Adelie, Chinstrap, and Gentoo)
#|   are represented with different colors.
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point()

#| warning: false
#| fig-alt: |
#|   A scatterplot of body mass vs. flipper length of penguins. Overlaid
#|   on the scatterplot are three smooth curves displaying the
#|   relationship between these variables for each species (Adelie,
#|   Chinstrap, and Gentoo). Different penguin species are plotted in
#|   different colors for the points and the smooth curves.
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point() +
  geom_smooth(method = "lm")

#| warning: false
#| fig-alt: |
#|   A scatterplot of body mass vs. flipper length of penguins. Overlaid
#|   on the scatterplot is a single line of best fit displaying the
#|   relationship between these variables for each species (Adelie,
#|   Chinstrap, and Gentoo). Different penguin species are plotted in
#|   different colors for the points only.
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species)) +
  geom_smooth(method = "lm")

#| warning: false
#| fig-alt: |
#|   A scatterplot of body mass vs. flipper length of penguins. Overlaid
#|   on the scatterplot is a single line of best fit displaying the
#|   relationship between these variables for each species (Adelie,
#|   Chinstrap, and Gentoo). Different penguin species are plotted in
#|   different colors and shapes for the points only.
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm")

#| warning: false
#| fig-alt: |
#|   A scatterplot of body mass vs. flipper length of penguins, with a
#|   line of best fit displaying the relationship between these two variables
#|   overlaid. The plot displays a positive, fairly linear, and relatively
#|   strong relationship between these two variables. Species (Adelie,
#|   Chinstrap, and Gentoo) are represented with different colors and
#|   shapes. The relationship between body mass and flipper length is
#|   roughly the same for these three species, and Gentoo penguins are
#|   larger than penguins from the other two species.
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)",
    y = "Body mass (g)",
    color = "Species",
    shape = "Species"
  ) +
  scale_color_colorblind()

#| eval: false
ggplot(data = penguins) +
  geom_point()

#| echo: false
#| warning: false
#| fig-alt: |
#|   A scatterplot of body mass vs. flipper length of penguins, colored
#|   by bill depth. A smooth curve of the relationship between body mass
#|   and flipper length is overlaid. The relationship is positive,
#|   fairly linear, and moderately strong.
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = bill_depth_mm)) +
  geom_smooth()

#| eval: false
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point() +
  geom_smooth(se = FALSE)

#| eval: false
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
  geom_smooth(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  )

#| eval: false
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point()

#| eval: false
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

#| eval: false
penguins |>
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

#| fig-alt: |
#|   A bar chart of frequencies of species of penguins: Adelie
#|   (approximately 150), Chinstrap (approximately 90), Gentoo
#|   (approximately 125).
ggplot(penguins, aes(x = species)) +
  geom_bar()

#| fig-alt: |
#|   A bar chart of frequencies of species of penguins, where the bars are
#|   ordered in decreasing order of their heights (frequencies): Adelie
#|   (approximately 150), Gentoo (approximately 125), Chinstrap
#|   (approximately 90).
ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()

#| warning: false
#| fig-alt: |
#|   A histogram of body masses of penguins. The distribution is unimodal
#|   and right skewed, ranging between approximately 2500 to 6500 grams.
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)

#| warning: false
#| layout-ncol: 2
#| fig-width: 3
#| fig-alt: |
#|   Two histograms of body masses of penguins, one with binwidth of 20
#|   (left) and one with binwidth of 2000 (right). The histogram with binwidth
#|   of 20 shows lots of ups and downs in the heights of the bins, creating a
#|   jagged outline. The histogram  with binwidth of 2000 shows only three bins.
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 20)
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 2000)

#| fig-alt: |
#|   A density plot of body masses of penguins. The distribution is unimodal
#|   and right skewed, ranging between approximately 2500 to 6500 grams.
ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()

#| eval: false
ggplot(penguins, aes(x = species)) +
  geom_bar(color = "red")

ggplot(penguins, aes(x = species)) +
  geom_bar(fill = "red")

#| label: fig-eda-boxplot
#| echo: false
#| fig-cap: |
#|   Diagram depicting how a boxplot is created.
#| fig-alt: |
#|   A diagram depicting how a boxplot is created following the steps outlined
#|   above.
knitr::include_graphics("images/EDA-boxplot.png")

#| warning: false
#| fig-alt: |
#|   Side-by-side box plots of distributions of body masses of Adelie,
#|   Chinstrap, and Gentoo penguins. The distribution of Adelie and
#|   Chinstrap penguins' body masses appear to be symmetric with
#|   medians around 3750 grams. The median body mass of Gentoo penguins
#|   is much higher, around 5000 grams, and the distribution of the
#|   body masses of these penguins appears to be somewhat right skewed.
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()

#| warning: false
#| fig-alt: |
#|   A density plot of body masses of penguins by species of penguins. Each
#|   species (Adelie, Chinstrap, and Gentoo) is represented with different
#|   colored outlines for the density curves.
ggplot(penguins, aes(x = body_mass_g, color = species)) +
  geom_density(linewidth = 0.75)

#| warning: false
#| fig-alt: |
#|   A density plot of body masses of penguins by species of penguins. Each
#|   species (Adelie, Chinstrap, and Gentoo) is represented in different
#|   colored outlines for the density curves. The density curves are also
#|   filled with the same colors, with some transparency added.
ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 0.5)

#| fig-alt: |
#|   Bar plots of penguin species by island (Biscoe, Dream, and Torgersen)
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar()

#| fig-alt: |
#|   Bar plots of penguin species by island (Biscoe, Dream, and Torgersen)
#|   the bars are scaled to the same height, making it a relative frequencies
#|   plot. The y-axis is labeled "count" by default.
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")

#| fig-alt: |
#|   Bar plots of penguin species by island (Biscoe, Dream, and Torgersen)
#|   the bars are scaled to the same height, making it a relative frequencies
#|   plot. The y-axis is labeled "proportion".
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill") +
  labs(y = "proportion")

#| warning: false
#| fig-alt: |
#|   A scatterplot of body mass vs. flipper length of penguins. The plot
#|   displays a positive, linear, relatively strong relationship between
#|   these two variables.
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()

#| warning: false
#| fig-alt: |
#|   A scatterplot of body mass vs. flipper length of penguins. The plot
#|   displays a positive, linear, relatively strong relationship between
#|   these two variables. The points are colored based on the species of the
#|   penguins and the shapes of the points represent islands (round points are
#|   Biscoe island, triangles are Dream island, and squared are Torgersen
#|   island). The plot is very busy and it's difficult to distinguish the shapes
#|   of the points.
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = island))

#| warning: false
#| fig-width: 8
#| fig-asp: 0.33
#| fig-alt: |
#|   A scatterplot of body mass vs. flipper length of penguins. The shapes and
#|   colors of points represent species. Penguins from each island are on a
#|   separate facet. Within each facet, the relationship between body mass and
#|   flipper length is positive, linear, relatively strong.
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)

#| warning: false
#| fig-show: hide
ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm,
    y = bill_depth_mm,
    color = species,
    shape = species
  )
) +
  geom_point() +
  labs(color = "Species")

#| fig-show: hide
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")
ggplot(penguins, aes(x = species, fill = island)) +
  geom_bar(position = "fill")

#| fig-show: hide
#| warning: false
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
ggsave(filename = "penguin-plot.png")

#| include: false
file.remove("penguin-plot.png")

#| eval: false
ggplot(mpg, aes(x = class)) +
  geom_bar()
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point()
ggsave("mpg-plot.png")

#| eval: false
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))
