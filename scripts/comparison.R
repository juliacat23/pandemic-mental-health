source("Data_Clean.R")

# before and after composite
composite.long <- gather(composite, test, score, stress:total_score, factor_key=TRUE)
dev.off()
ggplot(composite.long, aes(x = test, y = score, fill = date)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values=c("gray", "black"), name = "", labels = c("Before", "After")) + 
  scale_x_discrete(labels = c('PSS','GAD-7','CES-R', "Composite")) + 
  geom_text(aes(label=score), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3) + 
  labs(x = "Test", y = "Score") + 
  theme_apa() +
  theme(
    axis.text.x = element_text(size = 7),
    axis.title.y = element_text(size = 10),
    axis.title.x = element_blank())
ggsave("comparison.png", width = 4, height = 4, units = "in") 