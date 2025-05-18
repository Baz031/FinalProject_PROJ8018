library(dplyr)
library(readxl)
library(stringr)
library(vtable)
library(ggplot2)
library(gtsummary)
library(effectsize)
library(MQMF)

results1 <- read.csv("C:/Users/barry/Documents/Barry/DkIT HDip Data Analytics/Data Analytics Project/Football Reports in ChatGPT/Evaluation/scores.csv")
results2 <- read_xlsx("C:/Users/barry/Downloads/Questionnaire_ AI Matchday Reporter(1-8).xlsx")

results_completeness <- read.csv("C:/Users/barry/Documents/Barry/DkIT HDip Data Analytics/Data Analytics Project/Football Reports in ChatGPT/Evaluation/completeness.csv")

results2_article1 <- results2 %>% select("Interest in football", "Article ID", "Enjoyment", "Excitement", "Clarity", "Quality", "Author")
results2_article2 <- results2 %>% select("Interest in football", "Article ID2", "Enjoyment2", "Excitement2", "Clarity2", "Quality2", "Author2")
results2_article3 <- results2 %>% select("Interest in football", "Article ID3", "Enjoyment3", "Excitement3", "Clarity3", "Quality3", "Author3")


names(results2_article2) <- names(results2_article1) # https://stackoverflow.com/a/12019514
names(results2_article3) <- names(results2_article1)

results2all <- rbind(results2_article1,results2_article2, results2_article3)
names(results2all) <- names(results1)

final_results <- rbind(results1, results2all)


# Exploratory Analysis and Cleaning

str(final_results)
colSums(is.na(final_results))

final_results <- final_results %>% 
  mutate(InterestFootballClean = if_else(is.na(InterestFootball), round(mean(InterestFootball, na.rm = TRUE)), InterestFootball)) %>% 
  select(-InterestFootball) %>% 
  rename(InterestFootball = "InterestFootballClean")

# Article Column
unique(final_results$Article)
final_results$Article <- as.numeric(str_extract(final_results$Article, "[0-9]+")) # https://stackoverflow.com/a/38340803

# Add Article type factor
final_results <- final_results %>% mutate(Type = if_else(Article < 200, "1", if_else(Article < 300, "2", "3")))
final_results$Type <- as.factor(final_results$Type)

final_results %>% group_by(Type)

# Factorise Article column
final_results$Article <- as.factor(final_results$Article)
unique(final_results$Article)  

# Combine Author from 4 to 2 categories
final_results$Author <- if_else(final_results$Author == "AI", "C", if_else(final_results$Author == "Human", "H", final_results$Author))
final_results$Author <- as.factor(final_results$Author)


# Add actual Authors
final_results <- final_results %>% mutate(ActualAuthor = if_else(Type %in% c("1", "2"), "C", "H"))
final_results$ActualAuthor <- as.factor(final_results$ActualAuthor)

# Add column for correct guess
final_results <- final_results %>% mutate(GuessedCorrect = if_else(Author == ActualAuthor, "T", "F"))
final_results$GuessedCorrect <- as.factor(final_results$GuessedCorrect)


# Add column for Report Set

final_results <- final_results %>% mutate(ReportSet = if_else(Article %in% c(101,204,301), "1",if_else(Article %in% c(102,202,303), "2", if_else(Article %in% c(103,205,302), "3", if_else(Article %in% c(201,104,304), "4", if_else(Article %in% c(105,209,305), "5", if_else(Article %in% c(106,210,306),"6",if_else(Article %in% c(107,203,310), "7",if_else(Article %in% c(108,208,308), "8",if_else(Article %in% c(109,207,307), "9", "10"))))))))))

final_results$ReportSet <- as.factor(final_results$ReportSet)
str(final_results)

# Percentage correct author
# Overall
overall_guess_pct <- round(final_results %>% filter(GuessedCorrect == "T") %>% nrow() / final_results %>% nrow() * 100,2)
overall_guess_pct


# Exploratory Analysis
par(mfrow = c(2,2), cex.names = 1.5, cex.lab = 1.5, cex.main = 1.5)
hist(final_results$Enjoyment, main = "Histogram of Enjoyment Ratings", xlab = "Enjoyment")
hist(final_results$Excitement, main = "Histogram of Excitement Ratings", xlab = "Excitement")
hist(final_results$Clarity, main = "Histogram of Clarity Ratings", xlab = "Clarity")
hist(final_results$Quality, main = "Histogram of Quality Ratings", xlab = "Quality")

par(mfrow = c(1,1))

final_results %>% select(InterestFootball, Enjoyment, Excitement, Clarity, Quality) %>% sumtable(title = "Summary of Continuous Variables", add.median = TRUE, out = "kable")


# Summary table of continuous variables according to article type
final_results %>% select(Type, Enjoyment, Excitement, Clarity, Quality) %>% tbl_summary(by = Type, statistic = list(all_continuous() ~ "{mean} ({sd})"), type = list( Enjoyment ~ "continuous", Excitement ~ "continuous", Clarity ~ "continuous", Quality ~ "continuous")) %>% 
  modify_header(list(
  stat_1 ~ "**Zero-Shot**",
  stat_2~ "**Few_Shot**",
  stat_3 ~ "**Human**"), label = "")

# Summary table of categorical variables according to article type
final_results %>% select(Type, Author, GuessedCorrect) %>% tbl_summary(by = Type) %>% 
  modify_header(list(
      stat_1 ~ "**Zero-Shot**",
      stat_2 ~ "**Few_Shot**",
      stat_3 ~ "**Human**"), label = "")

# Guessed correct only
final_results %>% select(Type, GuessedCorrect) %>% tbl_summary(by = Type) %>% 
  modify_header(list(
    stat_1 ~ "**Zero-Shot**",
    stat_2 ~ "**Few_Shot**",
    stat_3 ~ "**Human**"), label = "")


final_results %>% select(InterestFootball, Enjoyment, Excitement, Clarity, Quality) %>% pairs()

# Ratings vs Article Type
final_results %>% group_by(Type) %>% summarise(Enjoyment = mean(Enjoyment), Excitement = mean(Excitement), Clarity = mean(Clarity), Quality = mean(Quality))


# Ratings by Article Type

# Enjoyment

par(mfrow = c(2,2), cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
boxplot(final_results$Enjoyment ~ final_results$Type, xlab = "Article Type", ylab = "Enjoyment Rating", main = "Enjoyment Rating vs. Article Type", names = c("Zero-Shot", "Few-Shot", "Human"))

# Excitement
boxplot(final_results$Excitement ~ final_results$Type, xlab = "Article Type", ylab = "Excitement Rating", main = "Excitement Rating vs. Article Type", names = c("Zero-Shot", "Few-Shot", "Human"))

# Clarity
boxplot(final_results$Clarity ~ final_results$Type, xlab = "Article Type", ylab = "Clarity Rating", main = "Clarity Rating vs. Article Type", names = c("Zero-Shot", "Few-Shot", "Human"))

# Quality
boxplot(final_results$Quality ~ final_results$Type, xlab = "Article Type", ylab = "Quality Rating", main = "Quality Rating vs. Article Type", names = c("Zero-Shot", "Few-Shot", "Human"))
                     
par(mfrow = c(1,1))

# Ratings by perceived author

par(mfrow = c(2,2), cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
boxplot(final_results$Enjoyment ~ final_results$Author, xlab = "Perceived Author", ylab = "Enjoyment Rating", main = "Enjoyment Rating vs. Perceived Author", names = c("ChatGPT", "Human"))

# Excitement
boxplot(final_results$Excitement ~ final_results$Author, xlab = "Perceived Author", ylab = "Excitement Rating", main = "Excitement Rating vs. Perceived Author", names = c("ChatGPT", "Human"))

# Clarity
boxplot(final_results$Clarity ~ final_results$Author, xlab = "Perceived Author", ylab = "Clarity Rating", main = "Clarity Rating vs. Perceived Author", names = c("ChatGPT", "Human"))

# Quality
boxplot(final_results$Quality ~ final_results$Author, xlab = "Perceived Author", ylab = "Quality Rating", main = "Quality Rating vs. Perceived Author", names = c("ChatGPT", "Human"))

par(mfrow = c(1,1))



# Analysis

# Author prediction accuracy according to Type
t1_true <- final_results %>% filter(Type == "1") %>% filter(GuessedCorrect == "T") %>% nrow()
t1_false <- final_results %>% filter(Type == "1") %>% filter(GuessedCorrect == "F") %>% nrow()

t2_true <- final_results %>% filter(Type == "2") %>% filter(GuessedCorrect == "T") %>% nrow()
t2_false <- final_results %>% filter(Type == "2") %>% filter(GuessedCorrect == "F") %>% nrow()

t3_true <- final_results %>% filter(Type == "3") %>% filter(GuessedCorrect == "T") %>% nrow()
t3_false <- final_results %>% filter(Type == "3") %>% filter(GuessedCorrect == "F") %>% nrow()

t1 <- c(t1_true, t1_false)
t2 <- c(t2_true, t2_false)
t3 <- c(t3_true, t3_false)

frequency_table <- as.table(rbind(t1, t2, t3))
colnames(frequency_table) = c("Yes", "No")
frequency_table

chisq.test(frequency_table)

# One sided z-test proportions (Guessed correct vs random chance)
prop.test(25, 39, 0.5)
prop.test(21, 39, 0.5)
prop.test(23,39,0.5)

# Author prediction accuracy according to Actual Source

human_true <- final_results %>% filter(Type == "3") %>% filter(GuessedCorrect == "T") %>% nrow()
human_false <- final_results %>% filter(Type == "3") %>% filter(GuessedCorrect == "F") %>% nrow()

gpt_true <- final_results %>% filter(Type == "1"| Type == "2") %>% filter(GuessedCorrect == "T") %>% nrow()
gpt_false <- final_results %>% filter(Type == "1"| Type == "2") %>% filter(GuessedCorrect == "F") %>% nrow()

human <- c(human_true, human_false)
gpt <- c(gpt_true, gpt_false)

frequency_table <- as.table(rbind(human, gpt))
colnames(frequency_table) = c("Yes", "No")
frequency_table

prop.test(frequency_table)

# Enjoyment by Type, ReportSet 2-way ANOVA

# Anova Model
model_aov <- aov(Enjoyment ~ Type + ReportSet,
    data = final_results)
summary(model_aov)
TukeyHSD(model_aov)
eta_squared(model_aov, partial = TRUE)

# Lm model
model_lm <- lm(Enjoyment ~ Type + ReportSet, data = final_results)
summary(model_lm)

# Plot residuals - check assumptions
par(mfrow=c(2,1))
plot(model_lm, which = 1:2)
par(mfrow = c(1,1))


# Cohens d
type_1_2 <- final_results %>% filter(!Type == "3")
type_1_3 <- final_results %>% filter(!Type == "2")
type_2_3 <- final_results %>% filter(!Type == "1")

cohens_d(Enjoyment ~ Type, data = type_1_2)
cohens_d(Enjoyment ~ Type, data = type_1_3)
cohens_d(Enjoyment ~ Type, data = type_2_3)

# Excitement by Type, ReportSet 2-way ANOVA
model_aov <- aov(Excitement ~ Type + ReportSet,
             data = final_results)
summary(model_aov)
TukeyHSD(model_aov)
eta_squared(model_aov, partial = TRUE)

# Lm model
model_lm <- lm(Excitement ~ Type + ReportSet,
             data = final_results)
summary(model_lm)

# Plot residuals - check assumptions
par(mfrow=c(2,1))
plot(model_lm, which = 1:2)
par(mfrow = c(1,1))

# Cohens d

cohens_d(Excitement ~ Type, data = type_1_2)
cohens_d(Excitement ~ Type, data = type_1_3)
cohens_d(Excitement ~ Type, data = type_2_3)

# Clarity by Type, ReportSet 2-way ANOVA
model_aov <- aov(Clarity ~ Type + ReportSet,
             data = final_results)
summary(model_aov)
TukeyHSD(model_aov)
eta_squared(model_aov, partial = TRUE)

# Lm model
model_lm <- lm(Clarity ~ Type + ReportSet,
            data = final_results)
summary(model_lm)

# Plot residuals - check assumptions
par(mfrow=c(2,1))
plot(model_lm, which = 1:2)
par(mfrow = c(1,1))

# Cohens d

cohens_d(Clarity ~ Type, data = type_1_2)
cohens_d(Clarity ~ Type, data = type_1_3)
cohens_d(Clarity ~ Type, data = type_2_3)

# Quality by Type, ReportSet 2-way ANOVA
model_aov <- aov(Quality ~ Type + ReportSet,
             data = final_results)
summary(model_aov)
TukeyHSD(model_aov)
eta_squared(model_aov, partial = TRUE)

# Lm model
model_lm <- lm(Quality ~ Type + ReportSet,
            data = final_results)
summary(model_lm)

# Plot residuals - check assumptions
par(mfrow=c(2,1))
plot(model_lm, which = 1:2)
par(mfrow = c(1,1))

# Cohens d

cohens_d(Quality ~ Type, data = type_1_2)
cohens_d(Quality ~ Type, data = type_1_3)
cohens_d(Quality ~ Type, data = type_2_3)


# Second set of models - including perceived author as predictor

# Enjoyment - 3 way ANOVA
model_aov <- aov(Enjoyment ~ Type + ReportSet + Author,
                 data = final_results)
summary(model_aov)
TukeyHSD(model_aov)
eta_squared(model_aov, partial = TRUE)

# No interaction effects

# Lm model
model_lm <- lm(Enjoyment ~ Type + ReportSet + Author, data = final_results)
summary(model_lm)

# Plot residuals - check assumptions
par(mfrow=c(2,1))
plot(model_lm, which = 1:2)
par(mfrow = c(1,1))

# Cohens d

cohens_d(Enjoyment ~ Type, data = type_1_2)
cohens_d(Enjoyment ~ Type, data = type_1_3)
cohens_d(Enjoyment ~ Type, data = type_2_3)

cohens_d(Enjoyment ~ Author, data = final_results)

# Excitement by Type, ReportSet, Author 3-way ANOVA
model_aov <- aov(Excitement ~ Type + ReportSet + Author,
             data = final_results)
summary(model_aov)
TukeyHSD(model_aov)
eta_squared(model_aov, partial = TRUE)

# Lm model
model_lm <- lm(Excitement ~ Type + ReportSet + Author,
            data = final_results)
summary(model_lm)

# Plot residuals - check assumptions
par(mfrow=c(2,1))
plot(model_lm, which = 1:2)
par(mfrow = c(1,1))

# Cohens d

cohens_d(Excitement ~ Type, data = type_1_2)
cohens_d(Excitement ~ Type, data = type_1_3)
cohens_d(Excitement ~ Type, data = type_2_3)

cohens_d(Excitement ~ Author, data = final_results)


# Clarity by Type, ReportSet, Author 3-way ANOVA
model_aov <- aov(Clarity ~ Type + ReportSet + Author,
             data = final_results)
summary(model_aov)
TukeyHSD(model_aov)
eta_squared(model_aov, partial = TRUE)

# Lm model
model_lm <- lm(Clarity ~ Type + ReportSet + Author,
            data = final_results)
summary(model_lm)

# Plot residuals - check assumptions
par(mfrow=c(2,1))
plot(model_lm, which = 1:2)
par(mfrow = c(1,1))

# Cohens d

cohens_d(Clarity ~ Type, data = type_1_2)
cohens_d(Clarity ~ Type, data = type_1_3)
cohens_d(Clarity ~ Type, data = type_2_3)

cohens_d(Clarity ~ Author, data = final_results)


# Quality by Type, ReportSet, Author 3-way ANOVA
model_aov <- aov(Quality ~ Type + ReportSet + Author,
             data = final_results)
summary(model_aov)
TukeyHSD(model_aov)
eta_squared(model_aov, partial = TRUE)

# Lm model
model_lm <- lm(Quality ~ Type + ReportSet + Author,
            data = final_results)
summary(model_lm)

# Plot residuals - check assumptions
par(mfrow=c(2,1))
plot(model_lm, which = 1:2)
par(mfrow = c(1,1))

# Cohens d

cohens_d(Quality ~ Type, data = type_1_2)
cohens_d(Quality ~ Type, data = type_1_3)
cohens_d(Quality ~ Type, data = type_2_3)

cohens_d(Quality ~ Author, data = final_results)


# Descriptive Statistics - Completeness and Accuracy

results_completeness %>% select(Type, Accuracy, Speculativeness, Combined.Accuracy, Completeness) %>%
  tbl_summary(by = Type,digits = list(Accuracy = c(2,2), Speculativeness = c(2,2), Accuracy = c(2,2), Combined.Accuracy = c(2,2), Completeness = c(2,2)), statistic = list(all_continuous() ~ "{mean} ({sd})"), type = list(Accuracy ~ "continuous", Speculativeness ~ "continuous", Combined.Accuracy ~ "continuous", Completeness ~ "continuous")) %>% modify_header(list(
    stat_1 ~ "**Zero-Shot**",
    stat_2 ~ "**Few_Shot**", label = ""))

final_results %>% select(Enjoyment, Excitement, Clarity, Quality) %>% pairs(main = "Pairs plot of Rating Variables", upper.panel = panel.cor)
