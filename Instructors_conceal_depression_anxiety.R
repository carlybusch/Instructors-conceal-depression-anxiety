####Script for data analysis and figures presented in: Most college science and engineering instructors do not reveal that they have depression or anxiety to undergraduates
### Carly Busch and Margaret Barstow, Sara Brownell, Katelyn Cooper
### August 16, 2023

library(dplyr)
library(tidyr)
library(ggplot2)
library(car)

my_data <- read.csv("23aug_instructors_conceal_depression_anxiety.csv")

### relevel race column----
my_data$race5 <- factor(my_data$race5, 
                        levels = c("asian", "peer",
                                   "white"))
my_data$race5 <- relevel(my_data$race5, ref = "white")

### relevel appointment column ----
my_data$appointment <- factor(my_data$appointment, 
                        levels = c("instructor", "tenuretrack",
                                   "tenured"))
my_data$appointment <- relevel(my_data$appointment, ref = "tenured")

### relevel course column ----
my_data$course.level <- factor(my_data$course.level, 
                              levels = c("intro", "upper",
                                         "all"))
my_data$course.level <- relevel(my_data$course.level, ref = "intro")


###RQ1a: To what extent do instructors report depression and/or anxiety?----

table(my_data$depression)

table(my_data$anxiety)

table(my_data$depression, my_data$anxiety)

report_depanx <- as.data.frame(matrix(NA, nrow = 4, ncol = 2))
colnames(report_depanx) <- c("condition", "count")
report_depanx$condition <- c("depression (no or decline anx)", "anxiety (no or decline dep)", "yes dep and anx", "no dep or anx")

report_depanx[report_depanx$condition == "yes dep and anx",]$count <- table(my_data$depression, my_data$anxiety)[4,4]
report_depanx[report_depanx$condition == "no dep or anx",]$count <- table(my_data$depression, my_data$anxiety)[3,3]
report_depanx[report_depanx$condition == "depression (no or decline anx)",]$count <- sum(table(my_data$depression, my_data$anxiety)[4,2:3])
report_depanx[report_depanx$condition == "anxiety (no or decline dep)",]$count <- sum(table(my_data$depression, my_data$anxiety)[2:3,4])

report_depanx$pct <- report_depanx$count/2013

###RQ1b: Demographic differences in reporting depression or anxiety ----
#model with only demographics depression
rep_dep.m <- glm(depression2 ~ gender4 + race5 + age_42yr + lgbtq2,
                 data = my_data)
summary(rep_dep.m)

#model with only demographics anxiety
rep_anx.m <- glm(anxiety2 ~ gender4 + race5 + age_42yr + lgbtq2,
                 data = my_data)
summary(rep_anx.m)


#demographics only model
depress_mod_df <- as.data.frame(summary(rep_dep.m)$coefficients[,-3])
depress_mod_df$predictor <- rownames(depress_mod_df)
rownames(depress_mod_df) <- NULL
depress_mod_df$stdest <- beta(rep_dep.m)$coefficients[,1]
depress_mod_df <- depress_mod_df %>%
  dplyr::rename(est = Estimate, se = `Std. Error`, pval = `Pr(>|t|)`) %>%
  as.data.frame()
depress_mod_df$or <- exp(depress_mod_df$est)
depress_mod_df <- depress_mod_df %>% dplyr::select(predictor, everything())
depress_mod_df$outcome <- NA
depress_mod_df$outcome <- "depression"

depress_mod_df %>% filter(pval < .05)

#demographics only model
anxiety_mod_df <- as.data.frame(summary(rep_anx.m)$coefficients[,-3])
anxiety_mod_df$predictor <- rownames(anxiety_mod_df)
rownames(anxiety_mod_df) <- NULL
anxiety_mod_df$stdest <- beta(rep_anx.m)$coefficients[,1]
anxiety_mod_df <- anxiety_mod_df %>%
  dplyr::rename(est = Estimate, se = `Std. Error`, pval = `Pr(>|t|)`) %>%
  as.data.frame()
anxiety_mod_df$or <- exp(anxiety_mod_df$est)
anxiety_mod_df <- anxiety_mod_df %>% dplyr::select(predictor, everything())
anxiety_mod_df$outcome <- NA
anxiety_mod_df$outcome <- "anxiety"

anxiety_mod_df %>% filter(pval < .05)

report_model_combined <- rbind(depress_mod_df, anxiety_mod_df)
rm(depress_mod_df, anxiety_mod_df)

###RQ2a: To what extent do instructors reveal depression or anxiety to undergraduates? ----
depress_reveal_tbl <- as.data.frame(table(my_data$depress.reveal.ugs))
colnames(depress_reveal_tbl) <- c("extent", "count")
depress_reveal_tbl <- depress_reveal_tbl[depress_reveal_tbl$extent != "",]
depress_reveal_tbl$denom <- sum(depress_reveal_tbl$count)
depress_reveal_tbl$pct <- depress_reveal_tbl$count/depress_reveal_tbl$denom
depress_reveal_tbl$id <- "depression"

anxiety_reveal_tbl <- as.data.frame(table(my_data$anxiety.reveal.ugs))
colnames(anxiety_reveal_tbl) <- c("extent", "count")
anxiety_reveal_tbl <- anxiety_reveal_tbl[anxiety_reveal_tbl$extent != "",]
anxiety_reveal_tbl$denom <- sum(anxiety_reveal_tbl$count)
anxiety_reveal_tbl$pct <- anxiety_reveal_tbl$count/anxiety_reveal_tbl$denom
anxiety_reveal_tbl$id <- "anxiety"

combined_reveal_df <- rbind(depress_reveal_tbl, anxiety_reveal_tbl)
rm(depress_reveal_tbl, anxiety_reveal_tbl)

###RQ2b: Demographic differences in revealing depression or anxiety----
#model with only demographics depression
rev_dep.m <- glm(depress.reveal.ugs.binary ~ gender4 + race5 + age_42yr + lgbtq2 + appointment,
                 data = my_data)
summary(rev_dep.m)

#model with only demographics anxiety
rev_anx.m <- glm(anxiety.reveal.ugs.binary ~ gender4 + race5 + age_42yr + lgbtq2 + appointment,
                 data = my_data)
summary(rev_anx.m)

#demographics only model
depress_rev_df <- as.data.frame(summary(rev_dep.m)$coefficients[,-3])
depress_rev_df$predictor <- rownames(depress_rev_df)
rownames(depress_rev_df) <- NULL
depress_rev_df$stdest <- beta(rev_dep.m)$coefficients[,1]
depress_rev_df <- depress_rev_df %>%
  dplyr::rename(est = Estimate, se = `Std. Error`, pval = `Pr(>|t|)`) %>%
  as.data.frame()
depress_rev_df$or <- exp(depress_rev_df$est)
depress_rev_df <- depress_rev_df %>% dplyr::select(predictor, everything())
depress_rev_df$outcome <- NA
depress_rev_df$outcome <- "depression"

depress_rev_df %>% filter(pval < .05)


#demographics only model
anxiety_rev_df <- as.data.frame(summary(rev_anx.m)$coefficients[,-3])
anxiety_rev_df$predictor <- rownames(anxiety_rev_df)
rownames(anxiety_rev_df) <- NULL
anxiety_rev_df$stdest <- beta(rev_anx.m)$coefficients[,1]
anxiety_rev_df <- anxiety_rev_df %>%
  dplyr::rename(est = Estimate, se = `Std. Error`, pval = `Pr(>|t|)`) %>%
  as.data.frame()
anxiety_rev_df$or <- exp(anxiety_rev_df$est)
anxiety_rev_df <- anxiety_rev_df %>% dplyr::select(predictor, everything())
anxiety_rev_df$outcome <- NA
anxiety_rev_df$outcome <- "anxiety"

anxiety_rev_df %>% filter(pval < .05)

reveal_model_demo_combined <- rbind(depress_rev_df, anxiety_rev_df)
rm(depress_rev_df, anxiety_rev_df)

#vif for demographic characteristics
vif(rev_dep.m)

#model with only course characteristics depression
rev_dep.m2 <- glm(depress.reveal.ugs.binary ~ course.size2 + course.subject + course.level,
                 data = my_data)
summary(rev_dep.m2)

#model with only course characteristics anxiety
rev_anx.m2 <- glm(anxiety.reveal.ugs.binary ~ course.size2 + course.subject + course.level,
                 data = my_data)
summary(rev_anx.m2)

#depression course characteristics only model
depress_rev_df2 <- as.data.frame(summary(rev_dep.m2)$coefficients[,-3])
depress_rev_df2$predictor <- rownames(depress_rev_df2)
rownames(depress_rev_df2) <- NULL
depress_rev_df2$stdest <- beta(rev_dep.m2)$coefficients[,1]
depress_rev_df2 <- depress_rev_df2 %>%
  dplyr::rename(est = Estimate, se = `Std. Error`, pval = `Pr(>|t|)`) %>%
  as.data.frame()
depress_rev_df2$or <- exp(depress_rev_df2$est)
depress_rev_df2 <- depress_rev_df2 %>% dplyr::select(predictor, everything())
depress_rev_df2$outcome <- NA
depress_rev_df2$outcome <- "depression"

depress_rev_df2 %>% filter(pval < .05)

#anxiety course characteristics only model
anxiety_rev_df2 <- as.data.frame(summary(rev_anx.m2)$coefficients[,-3])
anxiety_rev_df2$predictor <- rownames(anxiety_rev_df2)
rownames(anxiety_rev_df2) <- NULL
anxiety_rev_df2$stdest <- beta(rev_anx.m2)$coefficients[,1]
anxiety_rev_df2 <- anxiety_rev_df2 %>%
  dplyr::rename(est = Estimate, se = `Std. Error`, pval = `Pr(>|t|)`) %>%
  as.data.frame()
anxiety_rev_df2$or <- exp(anxiety_rev_df2$est)
anxiety_rev_df2 <- anxiety_rev_df2 %>% dplyr::select(predictor, everything())
anxiety_rev_df2$outcome <- NA
anxiety_rev_df2$outcome <- "anxiety"

anxiety_rev_df2 %>% filter(pval < .05)

reveal_model_course_combined <- rbind(depress_rev_df2, anxiety_rev_df2)
rm(depress_rev_df2, anxiety_rev_df2)

#vif for course characteristics
vif(rev_dep.m2)

###RQ3: What are instructors' reasons for revealing depression or anxiety?-----

#vector of reasons
reasons_reveal<-c("I felt like I had a personal relationship with the students in the course",
                  "students in this course was appropriate",
                  "I typically share",
                  "relevant to the students in this course",
                  "relevant to the course content",
                  "I knew others in the department",
                  "live authentically or be open",
                  "I wanted to be an example to my students",
                  "I wanted to serve as a mentor",
                  "I wanted to be known as a supporter",
                  "could make me more relatable",
                  "could make students more comfortable",
                  "could help students understand me or my circumstances better",
                  "course material by making a connection")



reveal_counts <-do.call(rbind, lapply(c("depress.reveal.why",
                                        "anxiety.reveal.why"),
                                      function(i){
                                        tmp_heatmap<-do.call(rbind,
                                                             lapply(reasons_reveal,
                                                                    function(x){
                                                                      tmp<-as.data.frame(length(which(str_detect(my_data[[i]],x))))
                                                                      colnames(tmp) <- "count"
                                                                      tmp$reason<-x
                                                                      tmp$csi<-i
                                                                      tmp$csi<-str_split(tmp$csi, "\\.")[[1]][1]
                                                                      return(tmp)
                                                                    }))
                                        return(tmp_heatmap)
                                      }))

reveal_counts <- reveal_counts %>% dplyr::select(csi, everything())

reveal_counts$saw_question <- NA
reveal_counts[reveal_counts$csi == "depress",]$saw_question <- length(which(
  my_data[["depress.reveal.why"]] != ""))
reveal_counts[reveal_counts$csi == "anxiety",]$saw_question <- length(which(
  my_data[["anxiety.reveal.why"]] != ""))

reveal_counts$pct <- reveal_counts$count/reveal_counts$saw_question

reveal_counts$count <- as.numeric(reveal_counts$count)


###RQ4a: Do instructors perceive students would benefit from revealing depression or anxiety?----
table(my_data$depress.reveal.benefit)
#percents out of number of instructors with depression NOT out to ALL
table(my_data$depress.reveal.benefit)/440*100

table(my_data$depress.reveal.ugs)

table(my_data$depress.reveal.benefit, my_data$depress.reveal.ugs)
#percents out of number of instructors with depression OUT TO SOME ugs
table(my_data$depress.reveal.benefit, my_data$depress.reveal.ugs)/108*100
#percents out of number of instructors with depression OUT TO NO ugs
table(my_data$depress.reveal.benefit, my_data$depress.reveal.ugs)/332*100


table(my_data$anxiety.reveal.benefit)
#percents out of number of instructors with anxiety NOT out to ALL
table(my_data$anxiety.reveal.benefit)/550*100

table(my_data$anxiety.reveal.ugs)

table(my_data$anxiety.reveal.benefit, my_data$anxiety.reveal.ugs)
#percents out of number of instructors with anxiety OUT TO SOME ugs
table(my_data$anxiety.reveal.benefit, my_data$anxiety.reveal.ugs)/165*100
#percents out of number of instructors with anxiety OUT TO NO ugs
table(my_data$anxiety.reveal.benefit, my_data$anxiety.reveal.ugs)/385*100


###RQ4b: What are instructors' reasons for concealing depression or anxiety?----
reasons_conceal <- c("I did not feel like I had a personal enough relationship with the students in this course",
                     "to all undergraduates in this course was inappropriate",
                     "I typically do not share",
                     "was relevant to the students in this course",
                     "was relevant to the course content",
                     "I did not know others in the department, such as other faculty or instructors, who had revealed a similar identity to people in the department",
                     "I had never thought about",
                     "I was concerned students would have a negative opinion", 
                     "result in poor course evaluations",
                     "I was concerned that I would be subjected to departmental disciplinary action",
                     "I was concerned I could be fired",
                     "would waste class time")

conceal_counts <-do.call(rbind, lapply(c("depress.conceal.why",
                                         "anxiety.conceal.why"),
                                       function(i){
                                         tmp_heatmap<-do.call(rbind,
                                                              lapply(reasons_conceal,
                                                                     function(x){
                                                                       tmp<-as.data.frame(length(which(str_detect(my_data[[i]],x))))
                                                                       colnames(tmp) <- "count"
                                                                       tmp$reason<-x
                                                                       tmp$csi<-i
                                                                       tmp$csi<-str_split(tmp$csi, "\\.")[[1]][1]
                                                                       return(tmp)
                                                                     }))
                                         return(tmp_heatmap)
                                       }))

conceal_counts <- conceal_counts %>% dplyr::select(csi, everything())

conceal_counts$saw_question <- NA
conceal_counts[conceal_counts$csi == "depress",]$saw_question <- length(which(
  my_data[["depress.conceal.why"]] != ""))
conceal_counts[conceal_counts$csi == "anxiety",]$saw_question <- length(which(
  my_data[["anxiety.conceal.why"]] != ""))

conceal_counts$pct <- conceal_counts$count/conceal_counts$saw_question

conceal_counts$count <- as.numeric(conceal_counts$count)
###RQ4c: Reasons to conceal depression or anxiety by anticipated student benefit (y/n)----
reasons_conceal <- c("I did not feel like I had a personal enough relationship with the students in this course",
                     "to all undergraduates in this course was inappropriate",
                     "I typically do not share",
                     "was relevant to the students in this course",
                     "was relevant to the course content",
                     "I did not know others in the department, such as other faculty or instructors, who had revealed a similar identity to people in the department",
                     "I had never thought about",
                     "I was concerned students would have a negative opinion", 
                     "result in poor course evaluations",
                     "I was concerned that I would be subjected to departmental disciplinary action",
                     "I was concerned I could be fired",
                     "would waste class time")


conceal_counts_noben <-do.call(rbind, lapply(c("depress",
                                              "anxiety"),
                                            function(i){
                                              tmp_heatmap<-do.call(rbind,
                                                                   lapply(reasons_conceal,
                                                                          function(x){
                                                                            tmp<-as.data.frame(length(which(str_detect(my_data[[paste0(i, ".conceal.why")]],x) & my_data[[paste0(i, ".reveal.benefit")]] == "no")))
                                                                            colnames(tmp) <- "count"
                                                                            tmp$benefit <- "no"
                                                                            tmp$reason<-x
                                                                            tmp$csi<-i
                                                                            tmp$csi<-str_split(tmp$csi, "\\.")[[1]][1]
                                                                            return(tmp)
                                                                          }))
                                              return(tmp_heatmap)
                                            }))

conceal_counts_noben <- conceal_counts_noben %>% dplyr::select(csi, everything())

conceal_counts_noben$saw_question <- NA
conceal_counts_noben[conceal_counts_noben$csi == "depress",]$saw_question <- nrow(
  my_data[my_data$depress.conceal.why != "" & my_data$depress.reveal.benefit == "no",])
conceal_counts_noben[conceal_counts_noben$csi == "anxiety",]$saw_question <- nrow(
  my_data[my_data$anxiety.conceal.why != "" & my_data$anxiety.reveal.benefit == "no",])

conceal_counts_yben <-do.call(rbind, lapply(c("depress",
                                              "anxiety"),
                                            function(i){
                                              tmp_heatmap<-do.call(rbind,
                                                                   lapply(reasons_conceal,
                                                                          function(x){
                                                                            tmp<-as.data.frame(length(which(str_detect(my_data[[paste0(i, ".conceal.why")]],x) & my_data[[paste0(i, ".reveal.benefit")]] == "yes")))
                                                                            colnames(tmp) <- "count"
                                                                            tmp$benefit <- "yes"
                                                                            tmp$reason<-x
                                                                            tmp$csi<-i
                                                                            tmp$csi<-str_split(tmp$csi, "\\.")[[1]][1]
                                                                            return(tmp)
                                                                          }))
                                              return(tmp_heatmap)
                                            }))

conceal_counts_yben <- conceal_counts_yben %>% dplyr::select(csi, everything())

conceal_counts_yben$saw_question <- NA
conceal_counts_yben[conceal_counts_yben$csi == "depress",]$saw_question <- nrow(
  my_data[my_data$depress.conceal.why != "" & my_data$depress.reveal.benefit == "yes",])
conceal_counts_yben[conceal_counts_yben$csi == "anxiety",]$saw_question <- nrow(
  my_data[my_data$anxiety.conceal.why != "" & my_data$anxiety.reveal.benefit == "yes",])

conceal_counts_ben_disagg <- rbind(conceal_counts_noben, conceal_counts_yben)
rm(conceal_counts_noben, conceal_counts_yben)

conceal_counts_ben_disagg$pct <- conceal_counts_ben_disagg$count/conceal_counts_ben_disagg$saw_question
conceal_counts_ben_disagg$count <- as.numeric(conceal_counts_ben_disagg$count)

###RQ4d: Demographic differences for reasons to conceal depression or anxiety ----
why_conceal_cols <- c("depress.conceal.why_not.personal","depress.conceal.why_not.relevant.content","depress.conceal.why_not.relevant.students","depress.conceal.why_no.others","depress.conceal.why_never.thought","depress.conceal.why_inappropriate","depress.conceal.why_dont.share","depress.conceal.why_negative.opinion","anxiety.conceal.why_not.personal","anxiety.conceal.why_not.relevant.content","anxiety.conceal.why_not.relevant.students","anxiety.conceal.why_no.others","anxiety.conceal.why_never.thought","anxiety.conceal.why_inappropriate","anxiety.conceal.why_dont.share","anxiety.conceal.why_negative.opinion") #only for reasons >20% selected

why_conceal <- do.call(rbind, lapply(why_conceal_cols, function(x){
  tmp.mod <- as.data.frame(summary(glm(formula = as.formula(paste0(x, "~ gender4 + race5 + age_42yr + lgbtq2 + appointment")), 
                                       data = my_data, family = binomial))$coefficients[,-3])
  tmp.mod$reason <- x
  tmp.mod$predictor <- rownames(tmp.mod)
  tmp.mod$stdest <- beta(glm(formula = as.formula(paste0(x, "~ gender4 + race5 + age_42yr + lgbtq2 + appointment")),
                             data = my_data))$coefficients[,1]
  return(tmp.mod)
})) 
why_conceal <- why_conceal %>%
  dplyr::rename(est = `Estimate`, se = `Std. Error`, pval = `Pr(>|z|)`) %>%
  as.data.frame()
rownames(why_conceal) <- NULL
why_conceal$or<- exp(why_conceal$est)
why_conceal$qval <- NA
predictors_reasons <- c("(Intercept)","gender4woman.nb","race5asian","race5peer","age_42yr43+",
                        "lgbtq2","appointmentinstructor","appointmenttenuretrack")
why_conceal_depress <- why_conceal[str_detect(why_conceal$reason, "depress"),]
why_conceal_anxiety <- why_conceal[str_detect(why_conceal$reason, "anxiety"),]

for(i in 1:length(predictors_reasons)){
  why_conceal_depress[why_conceal_depress$predictor == predictors_reasons[i],]$qval<-p.adjust(p = why_conceal_depress[why_conceal_depress$predictor == predictors_reasons[i],]$pval, method = "fdr")
}

for(i in 1:length(predictors_reasons)){
  why_conceal_anxiety[why_conceal_anxiety$predictor == predictors_reasons[i],]$qval<-p.adjust(p = why_conceal_anxiety[why_conceal_anxiety$predictor == predictors_reasons[i],]$pval, method = "fdr")
}

why_conceal <- rbind(why_conceal_depress, why_conceal_anxiety)
rm(why_conceal_depress, why_conceal_anxiety)

why_conceal |>
  filter(qval<.05) |>
  filter(predictor != "(Intercept)")

###FIGURE 1a: bar graph reporting depression and/or anxiety----

fig1a <- report_depanx %>%
  ggplot(aes(y=pct, x=reorder(condition, pct))) + 
  geom_col(fill = "gray40") +
  geom_text(aes(y = pct,
                label = scales::percent(pct,
                                        accuracy = .1)),
            hjust = -.02,
            fontface = "bold", size = 3, color = "black",
            show.legend = F) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0), limits = c(0, .65))+
  scale_x_discrete(labels = c("Report only depression", "Report only anxiety",
                              "Report depression\nand anxiety", "Report neither\ndepression nor anxiety")) +
  coord_flip()+
  labs(title = "" , x="", y="Percent (%)") +
  theme_classic()+
  theme(legend.position = "right", 
        axis.text.x = element_text(family="Helvetica", color = "black", size = 10),
        axis.text.y = element_text(family = "Helvetica", color = "black", size = 10, face = "bold"),
        axis.title = element_text(family="Helvetica", color = "black", size = 10, face = "bold"), 
        axis.ticks.y = element_blank())

fig1a

###FIGURE 1b: Forest plot demographic differences reporting ------

fig1b <- report_model_combined %>%
  filter(predictor!="(Intercept)")%>%
  mutate(predictor = fct_relevel(predictor, "lgbtq2", "age_42yr43+",
                                 "race5peer", "race5asian", "gender4woman.nb")) %>%
  ggplot(aes(y = predictor, color = outcome)) +
  geom_point(aes(x = exp(est)), size = 3, position=ggstance::position_dodgev(height = 0.5)) +
  geom_vline(aes(xintercept = 1), linetype="dashed", linewidth=.7, color = "grey25") + 
  geom_errorbarh(aes(xmin = exp(est - 1.96*se), xmax = exp(est + 1.96*se)), 
                 position=ggstance::position_dodgev(height=0.5), linewidth = 1) +  
  labs(x = "log10(Odds ratio ± 95% CI)", y = "", title = "") + 
  scale_x_log10(limits = c(.8, 1.5), breaks = seq(.8, 1.5, .2)) +
  scale_y_discrete(labels = c("LGBTQ+", "Age 43+", "PEER", "Asian", "Woman or\nNon-binary")) +
  scale_color_manual(values = c("#CB6D51", "#82CAFF"), breaks = c("depression", "anxiety"),
                     labels = c("Depression", "Anxiety")) +
  theme_classic() +
  theme(legend.position = "right", 
        axis.title.x = element_text(family="Helvetica", color = "black", size = 10, face = "bold"), 
        axis.text.x = element_text(family="Helvetica", color = "black", size = 10),
        axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 10, face = "bold"), 
        legend.key.height = unit(1.25, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(family="Helvetica", color = "black", size = 10, face = "bold"),
        plot.title = element_text(family="Helvetica", color = "black", size = 10, face = "bold"))

fig1b

###FIGURE 2a: clustered bar graph extent of reveal----

fig2a <- combined_reveal_df%>%
  mutate(id = fct_relevel(id, "depression", "anxiety"))%>%
  ggplot(aes(fill=id, y=pct, x=reorder(extent, pct))) + 
  geom_bar(position="dodge", stat="identity")+
  scale_x_discrete(labels=c("All", "Some", "None"))+
  labs(title = "", y = "Percent (%)", x = "Extent revealed to undergraduates")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0), limits = c(0, .8))+
  scale_fill_manual(values=c("#CB6D51", "#82CAFF"), name = "", labels = c("Depression", "Anxiety"))+
  theme_classic()+
  theme(legend.position = "right", 
        axis.text = element_text(family="Helvetica", color = "black", size = 10),
        axis.ticks.x = element_blank(),
        axis.title = element_text(family="Helvetica", color = "black", size = 10, face = "bold"), 
        legend.key.height = unit(1, "cm"),
        legend.text = element_text(family="Helvetica", color = "black", size = 10, face = "bold"),
        plot.title = element_text(family="Helvetica", color = "black", size = 10, face = "bold"))

fig2a

###FIGURE 2b: forest plot extent of reveal----

fig2b <- reveal_model_demo_combined %>%
  filter(predictor!="(Intercept)")%>%
  mutate(predictor = fct_relevel(predictor, "appointmenttenuretrack",
                                 "appointmentinstructor", "lgbtq2", "age_42yr43+",
                                 "race5peer", "race5asian", "gender4woman.nb")) %>%
  ggplot(aes(y = predictor, color = outcome)) +
  geom_point(aes(x = exp(est)), size = 3, position=ggstance::position_dodgev(height = 0.5)) +
  geom_vline(aes(xintercept = 1), linetype="dashed", linewidth=.7, color = "grey25") + 
  geom_errorbarh(aes(xmin = exp(est - 1.96*se), xmax = exp(est + 1.96*se)), 
                 position=ggstance::position_dodgev(height=0.5), linewidth = 1) +  
  labs(x = "log10(Odds ratio ± 95% CI)", y = "", title = "") + 
  scale_x_log10(limits = c(.6, 1.5), breaks = seq(.6, 1.5, .2)) +
  scale_y_discrete(labels = c("Appointment:\nTenure-track", "Appointment:\nLecturer",
                              "LGBTQ+", "Age 43+", "PEER", "Asian", "Woman or\nNon-binary")) +
  scale_color_manual(values = c("#CB6D51", "#82CAFF"), breaks = c("depression", "anxiety"),
                     labels = c("Depression", "Anxiety")) +
  theme_classic() +
  theme(legend.position = "right", 
        axis.title.x = element_text(family="Helvetica", color = "black", size = 10, face = "bold"), 
        axis.text.x = element_text(family="Helvetica", color = "black", size = 10),
        axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_text(family="Helvetica", color = "black", size = 10, face = "bold"), 
        legend.key.height = unit(1.25, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(family="Helvetica", color = "black", size = 10, face = "bold"),
        plot.title = element_text(family="Helvetica", color = "black", size = 10, face = "bold"))

fig2b

###FIGURE 3: horizontal bar graph reasons to reveal----

fig3 <- reveal_counts %>%
  mutate(csi = fct_relevel(csi, "anxiety", "depress"))%>%
  ggplot(aes(fill=csi, y=pct, x=reorder(reason, pct))) + 
  geom_bar(position="dodge", stat="identity")+
  scale_x_discrete(labels = function(x) str_wrap(c("Knew others in the department", "To connect with course material",
                              "Perceive personal relationships with students", "Perceive as relevant to content",
                              "To help students gain an understanding of the instructor", 
                              "Typically share", "To be relatable",
                              "To serve as a mentor", "To be authentic",
                              "To make students more comfortable", "Perceive as relevant to students", 
                              "Perceive as appropriate","To be a supporter",
                              "To be an example"), width = 22))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0), limits = c(0,.8))+
  scale_fill_manual(breaks = c("depress", "anxiety"),
                    name = "", values = c("#CB6D51", "#82CAFF"), labels = c("Depression", "Anxiety"))+
  coord_flip()+
  labs(title = "" , x="Reasons to reveal depression or anxiety to all undergraduates", y="Percent (%)") +
  theme_classic()+
  theme(legend.position = "right", 
        axis.text = element_text(family="Helvetica", color = "black", size = 10),
        axis.title = element_text(family="Helvetica", color = "black", size = 10, face = "bold"), 
        axis.ticks.y = element_blank(),
        legend.key.height = unit(1, "cm"),
        legend.text = element_text(family="Helvetica", color = "black", size = 10, face = "bold"),
        plot.title = element_text(family="Helvetica", color = "black", size = 10, face = "bold"))

fig3

###FIGURE 4: horizontal bar graph reasons to conceal----

fig4 <- conceal_counts %>%
  mutate(csi = fct_relevel(csi, "anxiety", "depress"))%>%
  ggplot(aes(fill=csi, y=pct, x=reorder(reason, pct))) + 
  geom_bar(position="dodge", stat="identity")+
  scale_x_discrete(labels = function(x) str_wrap(c("Concerned about being fired", 
                                                   "Concerned about disciplinary action", 
                              "Perceive as a waste of class time", "Concerned about course evaluations", 
                              "Did not know others who revealed", 
                              "Concerned students would have a negative opinion", "Perceived as irrelevant to students",
                              "Did not have a personal relationship with students", 
                              "Perceive as inappropriate", "Had never thought about revealing", 
                              "Perceive as irrelevant to course content", "Do not typically share"), width = 22))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0), limits = c(0,.8))+
  scale_fill_manual(breaks = c("depress", "anxiety"),
                    name = "", values = c("#CB6D51", "#82CAFF"), labels = c("Depression", "Anxiety"))+
  coord_flip() +
  labs(title = "" , x="Reasons to conceal depression or anxiety from undergraduates", y="Percent (%)") +
  theme_classic() +
  theme(legend.position = "right", 
        axis.text = element_text(family="Helvetica", color = "black", size = 10),
        axis.title = element_text(family="Helvetica", color = "black", size = 10, face = "bold"), 
        axis.ticks.y = element_blank(),
        legend.key.height = unit(1, "cm"),
        legend.text = element_text(family="Helvetica", color = "black", size = 10, face = "bold"),
        plot.title = element_text(family="Helvetica", color = "black", size = 10, face = "bold"))

fig4

###FIGURE S1: horizontal bar graph reasons to conceal disaggregated by perceive benefit----

figs1 <- conceal_counts_ben_disagg |>
  mutate(csi = fct_relevel(csi, "depress", "anxiety")) |>
  mutate(reason = dplyr::recode(reason, "I did not feel like I had a personal enough relationship with the students in this course" = "Did not have a personal\nrelationship with\nstudents",
                         "to all undergraduates in this course was inappropriate" = "Perceive as\ninappropriate",
                         "I typically do not share" = "Do not typically share",    
                         "was relevant to the students in this course" = "Perceived as irrelevant\nto students",
                         "was relevant to the course content" = "Perceive as irrelevant\nto course content",
                         "I did not know others in the department, such as other faculty or instructors, who had revealed a similar identity to people in the department" = "Did not know others who\nrevealed",
                         "I had never thought about" = "Had never thought about\nrevealing",
                         "I was concerned students would have a negative opinion" = "Concerned students would\nhave a negative opinion",
                         "result in poor course evaluations" = "Concerned about course\nevaluations",
                         "I was concerned that I would be subjected to departmental disciplinary action" = "Concerned about\ndisciplinary action",
                         "I was concerned I could be fired" = "Concerned about being\nfired",
                         "would waste class time" = "Perceive as a waste of\nclass time")) |>
  ggplot(aes(fill=csi, y=pct, x=reorder(benefit, pct))) + 
  geom_bar(position="dodge", stat="identity")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), expand = c(0,0))+
  scale_fill_manual(breaks = c("depress", "anxiety"),
                    name = "", values = c("#CB6D51", "#82CAFF"), labels = c("Depression", "Anxiety"))+
  labs(title = "" , x="Perceive potential student benefit to disclosure", y="Percent (%)") +
  theme_classic() +
  theme(legend.position = "right", 
        axis.text = element_text(family="Helvetica", color = "black", size = 10),
        axis.ticks.x = element_blank(),
        axis.title = element_text(family="Helvetica", color = "black", size = 10, face = "bold"), 
        axis.ticks.y = element_blank(),
        legend.key.height = unit(1, "cm"),
        legend.text = element_text(family="Helvetica", color = "black", size = 10, face = "bold"),
        strip.text = element_text(family="Helvetica", color = "black", size = 8),
        strip.background = element_blank(),
        plot.title = element_text(family="Helvetica", color = "black", size = 10, face = "bold")) +
  facet_wrap(.~reorder(reason, -pct)) # ,labeller = labeller(reason = label_wrap_gen(25))

figs1

### saving figures ----
ggsave(fig1a, file = "~/Desktop/fig1a_depanx.pdf", device = "pdf", units = "in", width = 5, height = 3)
ggsave(fig1b, file = "~/Desktop/fig1b_depanx.pdf", device = "pdf", units = "in", width = 5, height = 3)
ggsave(fig2a, file = "~/Desktop/fig2a_depanx.pdf", device = "pdf", units = "in", width = 5, height = 3)
ggsave(fig2b, file = "~/Desktop/fig2b_depanx.pdf", device = "pdf", units = "in", width = 5, height = 3)
ggsave(fig3, file = "~/Desktop/fig3_depanx.pdf", device = "pdf", units = "in", width = 5, height = 6)
ggsave(fig4, file = "~/Desktop/fig4_depanx.pdf", device = "pdf", units = "in", width = 5, height = 6)
ggsave(figs1, file = "~/Desktop/figs1_depanx.pdf", device = "pdf", units = "in", width = 8, height = 6)
