if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("reshape2", quietly = TRUE)) install.packages("reshape2")

library(ggplot2)
library(reshape2)

cor_matrix <- matrix(c(
  1, 0.811, 0.794, 0.759, 0.738,
  0.811, 1, 0.778, 0.783, 0.769,
  0.794, 0.778, 1, 0.818, 0.797,
  0.759, 0.783, 0.818, 1, 0.813,
  0.738, 0.769, 0.797, 0.813, 1
), 
nrow = 5, ncol = 5, byrow = TRUE)

rownames(cor_matrix) <- c("Energy", "Mood", "Focus", "Engagement", "Clarity")
colnames(cor_matrix) <- c("Energy", "Mood", "Focus", "Engagement", "Clarity")

cor_df <- melt(cor_matrix)

heatmap_plot <- ggplot(data = cor_df, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0.5, limit = c(0, 1), space = "Lab", name = "Correlation") +
  geom_text(aes(label = sprintf("%.3f", value)), color = "black", size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap", x = "", y = "")

print(heatmap_plot)



Correlations


Energy
Mood
Focus
Engagement
Clarity
Energy
Pearson Correlation
1
.811**
  .794**
  .759**
  .738**
  Sig. (2-tailed)


<.001
<.001
<.001
<.001
N
24653
24652
24648
24641
24638
Mood
Pearson Correlation
.811**
  1
.778**
  .783**
  .769**
  Sig. (2-tailed)
<.001


<.001
<.001
<.001
N
24652
24657
24652
24645
24642
Focus
Pearson Correlation
.794**
  .778**
  1
.818**
  .797**
  Sig. (2-tailed)
<.001
<.001


<.001
<.001
N
24648
24652
24653
24642
24639
Engaged
Pearson Correlation
.759**
  .783**
  .818**
  1
.813**
  Sig. (2-tailed)
<.001
<.001
<.001


<.001
N
24641
24645
24642
24646
24638
Clarity
Pearson Correlation
.738**
  .769**
  .797**
  .813**
  1
Sig. (2-tailed)
<.001
<.001
<.001
<.001


N
24638
24642
24639
24638
24644
**. Correlation is significant at the 0.01 level (2-tailed).


