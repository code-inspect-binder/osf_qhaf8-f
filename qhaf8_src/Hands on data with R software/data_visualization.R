#code to performe plots presented in the main paper
##left right pupil size correlation
library(ggpubr)

all$DiameterPupilLeftEye = as.numeric(as.character(all$PupilSizeLeft))
all$DiameterPupilRightEye = as.numeric(as.character(all$PupilSizeRight))
all = all[all$PupilSizeLeft >0 & all$PupilSizeRight>0,]

all %>% mutate(Color = ifelse(DiameterPupilLeftEye <=3 | DiameterPupilLeftEye >=5 | DiameterPupilRightEye <=3 | DiameterPupilRightEye >=5,"gray", "black")) %>%
  ggplot(aes(x = DiameterPupilLeftEye, y= DiameterPupilRightEye, color = Color))+
  geom_point(alpha= 0.5)+ xlab("pupil size left eye in millimeters") + ylab("pupil size right eye in millimeters") +
  scale_color_identity() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))

#ggsave("correlation_eyes.jpg")

#Plot AOI

plot(as.numeric(all$GazePointXLeft), all$GazePointYLeft)
abline(v=c(250,1250),h=c(200,800),col="red")
ggplot(all, aes(x=GazePointXLeft))+
  geom_density(color="darkgray", fill="lightgray")  +theme_bw()

#plot pupil variations vs changes across time
library(ggplot2)
library(gridExtra)
a = grid.arrange(
  ggplot(universe_one[[2]][[1]], aes(time,baseline,familiarization_type, colour =familiarization_type)) +
    labs(x = "time (ms)",y = "averaged pupil size (mm)", colour = NULL)+
    geom_smooth(se = F) + 
    geom_hline(yintercept = 0) +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                        panel.background = element_blank(), axis.line = element_line(colour = "black"))
  + ggtitle("no baseline"), #+ facet_wrap("TrialId"),
  
  ggplot(universe_two[[2]][[2]], aes(time,baseline,familiarization_type, colour =familiarization_type)) +
    labs(x = "time (ms)",y = "baseline corrected pupil size (mm)", colour = NULL)+
    geom_smooth(se = F) + #facet_wrap("id") + 
    geom_hline(yintercept = 0) +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                        panel.background = element_blank(), axis.line = element_line(colour = "black"))
  + ggtitle("baseline 100ms"), #+ facet_wrap("TrialId"),
  
  ggplot(universe_three[[2]][[2]], aes(time,baseline,familiarization_type, colour =familiarization_type)) +
    labs(x = "time (ms)",y = "baseline corrected pupil size (mm)", colour = NULL)+
    geom_smooth(se = F) + #facet_wrap("id") + 
    geom_hline(yintercept = 0) +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                        panel.background = element_blank(), axis.line = element_line(colour = "black"))
  + ggtitle("baseline 200ms"),nrow = 2) # + facet_wrap("TrialId")

ggsave("baseline.jpg",a, height = 5 , width = 13)