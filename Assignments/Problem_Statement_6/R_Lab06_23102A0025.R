install.packages(c(
  "palmerpenguins",
  "dplyr",
  "ggplot2",
  "moments",
  "car",
  "effectsize"
))

library(palmerpenguins)
library(dplyr)
library(ggplot2)
library(moments)
library(car)
library(effectsize)

data(penguins)
head(penguins)
dim(penguins)
str(penguins)
summary(penguins)

colSums(is.na(penguins))
penguins_clean <- penguins %>%
  filter(
    !is.na(body_mass_g),
    !is.na(flipper_length_mm),
    !is.na(species),
    !is.na(sex)
  )
dim(penguins_clean)

mean(penguins_clean$body_mass_g)
median(penguins_clean$body_mass_g)
min(penguins_clean$body_mass_g)
max(penguins_clean$body_mass_g)
var(penguins_clean$body_mass_g)
sd(penguins_clean$body_mass_g)

quantile(
  penguins_clean$body_mass_g,
  probs = c(0.25, 0.50, 0.75)
)
IQR(penguins_clean$body_mass_g)

skewness(penguins_clean$body_mass_g)

kurtosis(penguins_clean$body_mass_g)

descriptive_stats <- data.frame(
  Mean = mean(penguins_clean$body_mass_g),
  Median = median(penguins_clean$body_mass_g),
  Minimum = min(penguins_clean$body_mass_g),
  Maximum = max(penguins_clean$body_mass_g),
  Variance = var(penguins_clean$body_mass_g),
  SD = sd(penguins_clean$body_mass_g),
  Q1 = quantile(penguins_clean$body_mass_g, 0.25),
  Q3 = quantile(penguins_clean$body_mass_g, 0.75),
  IQR = IQR(penguins_clean$body_mass_g),
  Skewness = skewness(penguins_clean$body_mass_g),
  Kurtosis = kurtosis(penguins_clean$body_mass_g)
)

descriptive_stats

species_stats <- penguins_clean %>%
  group_by(species) %>%
  summarise(
    Mean = mean(body_mass_g),
    Median = median(body_mass_g),
    Minimum = min(body_mass_g),
    Maximum = max(body_mass_g),
    Variance = var(body_mass_g),
    SD = sd(body_mass_g),
    Q1 = quantile(body_mass_g, 0.25),
    Q3 = quantile(body_mass_g, 0.75),
    IQR = IQR(body_mass_g),
    Skewness = skewness(body_mass_g),
    Kurtosis = kurtosis(body_mass_g)
  )

species_stats

ggplot(penguins_clean, aes(x = body_mass_g)) +
  geom_histogram(bins = 30, color = "black") +
  labs(
    title = "Distribution of Penguin Body Mass",
    x = "Body Mass (g)",
    y = "Frequency"
  ) +
  theme_minimal()

ggplot(
  penguins_clean,
  aes(x = species, y = body_mass_g)
) +
  geom_boxplot() +
  labs(
    title = "Body Mass by Penguin Species",
    x = "Species",
    y = "Body Mass (g)"
  ) +
  theme_minimal()
ggplot(
  penguins_clean,
  aes(x = body_mass_g, fill = species)
) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Density Distribution of Body Mass by Species",
    x = "Body Mass (g)",
    y = "Density"
  ) +
  theme_minimal()

ggplot(
  penguins_clean,
  aes(x = sex, y = body_mass_g)
) +
  geom_boxplot() +
  labs(
    title = "Body Mass by Sex",
    x = "Sex",
    y = "Body Mass (g)"
  ) +
  theme_minimal()

shapiro.test(
  penguins_clean$body_mass_g[
    penguins_clean$sex == "male"
  ]
)
shapiro.test(
  penguins_clean$body_mass_g[
    penguins_clean$sex == "female"
  ]
)


qqnorm(
  penguins_clean$body_mass_g[
    penguins_clean$sex == "male"
  ],
  main = "QQ Plot - Male Body Mass"
)

qqline(
  penguins_clean$body_mass_g[
    penguins_clean$sex == "male"
  ]
)

qqnorm(
  penguins_clean$body_mass_g[
    penguins_clean$sex == "female"
  ],
  main = "QQ Plot - Female Body Mass"
)

qqline(
  penguins_clean$body_mass_g[
    penguins_clean$sex == "female"
  ]
)

t_test_result <- t.test(
  body_mass_g ~ sex,
  data = penguins_clean
)

t_test_result

t_test_result$conf.int

cohens_d <- cohens_d(
  body_mass_g ~ sex,
  data = penguins_clean
)

cohens_d


ggplot(
  penguins_clean,
  aes(x = species, y = body_mass_g)
) +
  geom_boxplot() +
  labs(
    title = "Distribution of Body Mass Across Species",
    x = "Species",
    y = "Body Mass (g)"
  ) +
  theme_minimal()

shapiro.test(
  penguins_clean$body_mass_g[
    penguins_clean$species == "Adelie"
  ]
)


shapiro.test(
  penguins_clean$body_mass_g[
    penguins_clean$species == "Chinstrap"
  ]
)

shapiro.test(
  penguins_clean$body_mass_g[
    penguins_clean$species == "Gentoo"
  ]
)


ggplot(
  penguins_clean,
  aes(sample = body_mass_g)
) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~species) +
  labs(
    title = "QQ Plots of Body Mass by Species"
  ) +
  theme_minimal()

leveneTest(
  body_mass_g ~ species,
  data = penguins_clean
)

anova_model <- aov(
  body_mass_g ~ species,
  data = penguins_clean
)

summary(anova_model)

tukey_result <- TukeyHSD(anova_model)

tukey_result

plot(tukey_result)

kruskal_result <- kruskal.test(
  body_mass_g ~ species,
  data = penguins_clean
)

kruskal_result

two_way_model <- aov(
  body_mass_g ~ species * sex,
  data = penguins_clean
)

summary(two_way_model)

ggplot(
  penguins_clean,
  aes(
    x = species,
    y = body_mass_g,
    fill = sex
  )
) +
  geom_boxplot() +
  labs(
    title = "Body Mass by Species and Sex",
    x = "Species",
    y = "Body Mass (g)",
    fill = "Sex"
  ) +
  theme_minimal()



flipper_stats <- penguins_clean %>%
  group_by(species) %>%
  summarise(
    Mean = mean(flipper_length_mm),
    Median = median(flipper_length_mm),
    SD = sd(flipper_length_mm),
    Minimum = min(flipper_length_mm),
    Maximum = max(flipper_length_mm),
    IQR = IQR(flipper_length_mm)
  )

flipper_stats

ggplot(
  penguins_clean,
  aes(
    x = species,
    y = flipper_length_mm
  )
) +
  geom_boxplot() +
  labs(
    title = "Flipper Length Across Penguin Species",
    x = "Species",
    y = "Flipper Length (mm)"
  ) +
  theme_minimal()

flipper_anova <- aov(
  flipper_length_mm ~ species,
  data = penguins_clean
)

summary(flipper_anova)

TukeyHSD(flipper_anova)

ggplot(
  penguins_clean,
  aes(
    x = flipper_length_mm,
    fill = species
  )
) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Flipper Length Distribution by Species",
    x = "Flipper Length (mm)",
    y = "Density"
  ) +
  theme_minimal()

ggplot(
  penguins_clean,
  aes(
    x = species,
    y = body_mass_g
  )
) +
  geom_boxplot() +
  geom_jitter(
    width = 0.15,
    alpha = 0.3
  ) +
  labs(
    title = "Body Mass Comparison Among Penguin Species",
    x = "Penguin Species",
    y = "Body Mass (g)"
  ) +
  theme_minimal()

