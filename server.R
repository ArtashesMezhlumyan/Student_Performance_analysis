library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(ggExtra)
library(readxl)
library(viridis)
library(tidyverse)
library(scales)
library(ggpubr)
library(devtools)
library(ggthemes)
library(reshape)
library(ggthemr)
library(Dict)


shinyServer(function(input,output){
  
    output$matrix <- renderPlot({
      
      data_por <- read.csv('student-por.csv', sep=";")
      data_por <- transform(data_por, avrg_G=(G1+G2+G3)/3)
      
      data_cor<-data[c("G1","G2","G3","absences","Walc","Dalc","health","goout","freetime","famrel","failures","studytime","traveltime","age")]
      
      corr_mat <- round(cor(data_cor),2)
      melted_corr_mat <- melt(corr_mat)
      
      ggplot(data = melted_corr_mat, aes(x=X1, y=X2, fill=value)) + geom_tile() +
        geom_text(aes(X2, X1, label = value), color = "black", size = 4) + theme(axis.text.x = element_text(angle = 90))
    })
    
    ## Plot 1
    output$plot1 <- renderPlot({
    data <- read.csv('student-por.csv', sep=";")
    data <- transform(data, avrg_G=(G1+G2+G3)/3)
    address.labs <- c("Rural", "Urban")
    male_female <- c("Female", "Male")
    names(address.labs) <- c("R", "U")
    ggplot(data, aes(x=sex, y=avrg_G, fill=sex)) + 
      geom_boxplot(notch=TRUE,colour = "#020427") + 
      facet_grid(~address, labeller = labeller(address = address.labs))  + 
      labs(title = "Student Performance by geographical location and gender",
           y = "Average grade") +
      scale_x_discrete(labels= male_female) + theme_stata() +
      scale_fill_stata(name = "Gender", labels = c("Female", "Male")) +
      theme(axis.title.x = element_blank()) 
  })
    ## Plot 2
    output$plot2 <- renderPlot({
      data <- read.csv('student-por.csv', sep=";")
      data <- transform(data, avrg_G=(G1+G2+G3)/3)
      quality_rel <- c("Very bad", "Bad","Average","Good","Extremely good")
      ggplot(data_por, aes(x=as.character(famrel), y=avrg_G, color = famrel)) + 
        geom_jitter(position=position_jitter(0.2)) +
        labs(title = "Student Performance Based on Quality of Relationship with Family",
             y="Average Grade", x="Quality of Relationship with Family") +
        scale_x_discrete(labels= quality_rel)  + scale_color_viridis() + theme_clean()+
        theme(legend.position = "none") + stat_summary(fun = mean, geom = "point", 
                                                       shape = 18,size=4,color="red",
                                                       fill = "red")
    })
    ## Plot 3
    output$plot3 <- renderPlot({
      Medu.labs <- c("Primary education","5th to 9th grade"
                     ,"Secondary education ","Higher education")
      names(Medu.labs) <- c("1","2","3","4")
      Fedu.labs <- c("Primary education","5th to 9th grade"
                     ,"Secondary education ","Higher education")
      names(Fedu.labs) <- c("1","2","3","4")
      
      means1_por <- aggregate(avrg_G ~ Medu, data = data_por[data_por['Medu'] != "0",], FUN=mean)
      means2_por <- aggregate(avrg_G ~ Fedu, data = data_por[data_por['Fedu'] != "0",], FUN=mean)
      
      pe_por<-ggplot(data_por[data_por["Medu"] != "0",], aes(x = avrg_G,fill= Medu)) + geom_histogram(bins = 25,alpha = 0.8) + 
        labs(x = "Average Grade", y = "Frequency",
             title = "Student’s performance based on Mothers’s education") +
        facet_grid( ~ Medu, scales = "free",labeller = labeller(Medu = Medu.labs)) +
        scale_fill_viridis(option = "plasma") + theme_hc() +
        theme(legend.position = "none") + scale_x_continuous(limits=c(0,20)) +
        geom_vline(data =means1_por, aes(xintercept=avrg_G), colour="red") 
      
      pe2_por<-ggplot(data_por[data_por["Fedu"] != "0",], aes(x = avrg_G,fill= Fedu)) + geom_histogram(bins = 25,alpha = 0.8) + 
        labs(x = "Average Grade", y = "Frequency",
             title = "Student’s performance based on Father’s education") +
        facet_grid( ~ Fedu, scales = "free",labeller = labeller(Fedu = Fedu.labs)) +
        scale_fill_viridis(option = "plasma") + theme_hc() +
        theme(legend.position = "none") + scale_x_continuous(limits=c(0,20)) +
        geom_vline(data =means2_por, aes(xintercept=avrg_G), colour="red") 
      
      figure_pe_por <- ggarrange(pe_por, pe2_por, ncol = 1, nrow = 2)
      figure_pe_por
    })
    ## Corr matrix
  
    
    output$plot4 <- renderPlot({
      ggthemr("light")
      ggplot(data_por,aes(x = higher, y =avrg_G , fill = higher)) + 
        geom_boxplot() + geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5)+
        labs(title = "Performance of students by ambition for higher education",
             x = "Inclination for higher education",y = "Average Grade",fill="")
    })
    
    output$plot5 <- renderPlot({
      data_por$concatenated<-paste(data_por$schoolsup,data_por$famsup,data_por$paid)
      agg_concat_por <- aggregate(avrg_G~concatenated,data =data_por, FUN = mean)
      ggplot(agg_concat_por, aes(x=concatenated, y=round(avrg_G,2), fill=concatenated)) +
        geom_bar(stat="identity", position = 'dodge') + 
        labs(fill = "EES/ FES /EPC ", 
             title = "Student performance based in exta educational support",
             subtitle = "(Extra Educational Support / Family Educational Suport / Extra Paid Classes)",
             y = "Average Grade", x = "Combination of educational help") +
        theme(axis.text.x = element_blank())
    })
    
    output$plot6 <- renderPlot({
      agg_data <- aggregate(avrg_G ~ Dalc, data = data_por, FUN=mean)
      ggplot(agg_data, aes(x = Dalc, y = round(avrg_G,2))) + geom_line(size=2, color='orange') + 
        geom_point(color = 'red', size=3) + labs(x = 'Daily Alcohol Consumption', y = "Average Grade of Exams") +
        geom_text(aes(label=round(avrg_G,2)), position=position_dodge(width=0.9), vjust=-1, size = 4) + 
        theme() + scale_fill_manual(values=c('salmon','darkturquoise'), labels=c('Female', 'Male')) + guides(fill=guide_legend("Gender")) +
        theme_bw() + ggtitle('Average grade of students based on daily alcohol consumption') + ylim(8,12.5) + scale_x_continuous(breaks=c(1,2,3,4,5), labels=c("Very low","Low","Medium","High", "Very high"))
    })
    
    output$plot7 <- renderPlot({
      agg_data <- aggregate(avrg_G ~ romantic + sex, data = data_por, FUN=mean)
      ggplot(agg_data, aes(x = romantic, y = round(avrg_G,2), fill = sex)) + geom_bar(stat='identity', position = 'dodge') +
        labs(x = 'In a romantic relationship', y = "Average Grade of Exams") + geom_text(aes(label=round(avrg_G,2)), position=position_dodge(width=0.9), vjust=-0.3, size = 4) +
        scale_fill_manual(values=c('salmon','darkturquoise'), labels=c('Female', 'Male')) + guides(fill=guide_legend("Gender")) + theme_few() + ggtitle('Average grade of students based on romantic relationship')+ 
        ylim(0,12.5) + theme(panel.background = element_rect(fill = "#FFC4C47D")) +  scale_x_discrete(breaks=c("no","yes"), labels=c("No", "Yes"))
      
    })
    
    output$plot8 <- renderPlot({
      summarised <- data_por %>% group_by(Pstatus) %>% summarise(mean = mean(avrg_G), sd = sd(avrg_G))
      ggplot(summarised, aes(x = Pstatus, y = mean, ymin=mean-sd, ymax = mean+sd)) + geom_errorbar(size=0.5) + geom_point(colour = 'Red', size = 4) + labs(x = 'Cohabitation status', y = "Expected grade CI") + ggtitle("Confidence interval of the expected grade based on cohabitation with parents") +  scale_x_discrete(breaks=c("A","T"), labels=c("Apart", "Together")) + theme_hc()
      
      ##data_por$Pstatus2 <- data_por$Pstatus
      
      ##data_por["Pstatus2"][data_por["Pstatus2"]=='A']<-0
      ##data_por["Pstatus2"][data_por["Pstatus2"]=='T']<-1
      ##anova <- aov(Pstatus2 ~ avrg_G , data = data_por)
      
      ##summary(anova)
      
    })
    
    output$plot9 <- renderPlot({
      agg_data <- aggregate(avrg_G ~ goout, data = data_por, FUN=mean)
      
      agg_data[2,2] <- (agg_data[2,2] + agg_data[3,2])/2
      agg_data[3,2] <- (agg_data[4,2] + agg_data[5,2])/2
      agg_data<- agg_data[c(-4,-5),]
      
      ggplot(agg_data, aes(x = goout, y = round(avrg_G,2))) +
        geom_segment(aes(x=goout, xend=goout, y=0, yend = avrg_G), size=1) + 
        geom_point(size=5, color="red", fill=alpha("orange"), alpha=1, shape=21, stroke=2) + 
        labs(x = 'Hangout with friends', y = "Average Grade of Exams") + geom_text(aes(label=round(avrg_G,2)), position=position_dodge(width=0.9), vjust=-1.2, size = 4) + 
        scale_fill_brewer(palette = 'Set2', labels=c('Little','Medium', 'High')) + guides(fill=guide_legend("Hangout")) + scale_x_discrete(limits=c("1","2","3"),labels = c("Low","Medium","High")) + theme_classic() +
        ggtitle('Average grade of students based on student hangout') + ylim(0,13) 
    })
    
    output$plot10 <- renderPlot({
      correlation <- cor(data_por$failures, data_por$absences)
      
      ggplot(data_por, aes(x = failures, y = absences)) + geom_point() + geom_jitter(height = 0.1, width = 0.1) + 
        geom_smooth(formula=y~x,method = 'lm', se=FALSE) + theme_classic() + labs(x = 'Failures', y = "Absences", caption = paste0('Correlation coefficient ', round(correlation,3))) + 
        ggtitle('Student absences vs failures')
    })
    
    output$plot11 <- renderPlot({
      ggplot(data = data_por, aes(x = studytime, y=avrg_G, group = studytime, fill = factor(studytime))) + 
        geom_violin() + geom_boxplot(width = 0.3) + ggtitle('The distribution boxplot of average grade based on study time of the students') +
        labs(x='Study hours (weekly)', y = 'Average grade') + theme_hc() +  theme(legend.position = "none") + scale_fill_brewer(palette='Pastel2') + 
        scale_x_discrete(limits=c("1","2","3","4"),labels = c("<2","2-5","5-10",">10"))
      
    })
    
    output$matrixmath <- renderPlot({
      data <- read.csv('student-mat.csv', sep=";")
      data <- transform(data, avrg_G=(G1+G2+G3)/3)
      
      
      data_cor<-data[c("G1","G2","G3","absences","Walc","Dalc","health","goout","freetime","famrel","failures","studytime","traveltime","age")]
      
      corr_mat <- round(cor(data_cor),2)
      melted_corr_mat <- melt(corr_mat)
      
      ggplot(data = melted_corr_mat, aes(x=X1, y=X2, fill=value)) + geom_tile() +
        geom_text(aes(X2, X1, label = value), color = "black", size = 4) + theme(axis.text.x = element_text(angle = 90))
    })
    
    output$plotmath2 <- renderPlot({
      data <- read.csv('student-mat.csv', sep=";")
      data <- transform(data, avrg_G=(G1+G2+G3)/3)
      address.labs <- c("Rural", "Urban")
      male_female <- c("Female", "Male")
      names(address.labs) <- c("R", "U")
      ggplot(data, aes(x=sex, y=avrg_G, fill=sex)) + 
        geom_boxplot(notch=TRUE,colour = "#020427") + 
        facet_grid(~address, labeller = labeller(address = address.labs))  + 
        labs(title = "Student Performance by geographical location and gender",
             y = "Average grade") +
        scale_x_discrete(labels= male_female) + theme_stata() +
        scale_fill_stata(name = "Gender", labels = c("Female", "Male")) +
        theme(axis.title.x = element_blank()) 
    })
    
    
    output$plotmath3 <- renderPlot({
      data <- read.csv('student-mat.csv', sep=";")
      data <- transform(data, avrg_G=(G1+G2+G3)/3)
      quality_rel <- c("Very bad", "Bad","Average","Good","Extremely good")
      ggplot(data, aes(x=as.character(famrel), y=avrg_G, color = famrel)) + 
        geom_jitter(position=position_jitter(0.2)) +
        labs(title = "Student Performance Based on Quality of Relationship with Family",
             y="Average Grade", x="Quality of Relationship with Family") +
        scale_x_discrete(labels= quality_rel)  + scale_color_viridis() + theme_clean()+
        theme(legend.position = "none") + stat_summary(fun = mean, geom = "point", 
                                                       shape = 18,size=4,color="red",
                                                       fill = "red")
    })
    
    
    output$plotmath4 <- renderPlot({
      data <- read.csv('student-mat.csv', sep=";")
      data <- transform(data, avrg_G=(G1+G2+G3)/3)
      Medu.labs <- c("Primary education","5th to 9th grade"
                     ,"Secondary education ","Higher education")
      names(Medu.labs) <- c("1","2","3","4")
      Fedu.labs <- c("Primary education","5th to 9th grade"
                     ,"Secondary education ","Higher education")
      names(Fedu.labs) <- c("1","2","3","4")
      
      means1 <- aggregate(avrg_G ~ Medu, data = data[data['Medu'] != "0",], FUN=mean)
      means2 <- aggregate(avrg_G ~ Fedu, data = data[data['Fedu'] != "0",], FUN=mean)
      
      pe<-ggplot(data[data["Medu"] != "0",], aes(x = avrg_G,fill= Medu)) +
        geom_histogram(bins = 25,alpha = 0.8) + 
        labs(x = "Average Grade", y = "Frequency",
             title = "Student’s performance based on Mothers’s education") +
        facet_grid( ~ Medu, scales = "free",labeller = labeller(Medu = Medu.labs)) +
        scale_fill_viridis(option = "plasma") + theme_hc() +
        theme(legend.position = "none") + scale_x_continuous(limits=c(0,20)) +
        geom_vline(data =means1, aes(xintercept=avrg_G), colour="red") 
      
      pe2<-ggplot(data[data["Fedu"] != "0",], aes(x = avrg_G,fill= Fedu)) + geom_histogram(bins = 25,alpha = 0.8) + 
        labs(x = "Average Grade", y = "Frequency",
             title = "Student’s performance based on Father’s education") +
        facet_grid( ~ Fedu, scales = "free",labeller = labeller(Fedu = Fedu.labs)) +
        scale_fill_viridis(option = "plasma") + theme_hc() +
        theme(legend.position = "none") + scale_x_continuous(limits=c(0,20)) +
        geom_vline(data =means2, aes(xintercept=avrg_G), colour="red")
      
      figure_pe <- ggarrange(pe, pe2, ncol = 1, nrow = 2)
      figure_pe
    })
    
    
    output$plotmath5 <- renderPlot({
      data <- read.csv('student-mat.csv', sep=";")
      data <- transform(data, avrg_G=(G1+G2+G3)/3)
      ggthemr("light")
      ggplot(data,aes(x = higher, y =avrg_G , fill = higher)) + 
        geom_boxplot() + geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5)+
        labs(title = "Performance of students by ambition for higher education",
             x = "Inclination for higher education",y = "Average Grade",fill="")
    })
    
    output$plotmath6 <- renderPlot({
      data <- read.csv('student-mat.csv', sep=";")
      data <- transform(data, avrg_G=(G1+G2+G3)/3)
      data$concatenated<-paste(data$schoolsup,data$famsup,data$paid)
      agg_concat <- aggregate(avrg_G~concatenated,data =data, FUN = mean)
      ggplot(agg_concat, aes(x=concatenated, y=round(avrg_G,2), fill=concatenated)) +
        geom_bar(stat="identity", position = 'dodge') + 
        labs(fill = "EES/ FES /EPC ", 
             title = "Student performance based in exta educational support",
             subtitle = "(Extra Educational Support / Family Educational Suport / Extra Paid Classes)",
             y = "Average Grade", x = "Combination of educational help") +
        theme(axis.text.x = element_blank())
    })
    
    output$plotmath7 <- renderPlot({
      data <- read.csv('student-mat.csv', sep=";")
      data <- transform(data, avrg_G=(G1+G2+G3)/3)
      agg_data <- aggregate(avrg_G ~ Dalc, data = data, FUN=mean)
      
      ggplot(agg_data, aes(x = Dalc, y = round(avrg_G,2))) + geom_line(size=2, color = 'orange') + geom_point(size=3, color = 'red') + 
        labs(x = 'Daily Alcohol Consumption', y = "Average Grade of Exams") + 
        geom_text(aes(label=round(avrg_G,2)), position=position_dodge(width=0.9), vjust=-1, size = 4, color = "black") + theme() + 
        scale_fill_discrete(labels=c('Female', 'Male')) + guides(fill=guide_legend("Gender")) + theme_bw() + 
        ggtitle('Average grade of students based on daily alcohol consumption') + ylim(8,12.5) + 
        scale_x_continuous(breaks=c(1,2,3,4,5), labels=c("Very low","Low","Medium","High", "Very high"))
    })
    
    output$plotmath8 <- renderPlot({
      data <- read.csv('student-mat.csv', sep=";")
      data <- transform(data, avrg_G=(G1+G2+G3)/3)
      agg_data <- aggregate(avrg_G ~ romantic + sex, data = data, FUN=mean)
      
      ggplot(agg_data, aes(x = romantic, y = round(avrg_G,2), fill = sex)) + geom_bar(stat='identity', position = 'dodge') + 
        labs(x = 'In a romantic relationship', y = "Average Grade of Exams") + 
        geom_text(aes(label=round(avrg_G,2)), position=position_dodge(width=0.9), vjust=-0.3, size = 4, color = "black") + 
        scale_fill_manual(values=c('salmon','darkturquoise'), labels=c('Female', 'Male')) + guides(fill=guide_legend("Gender")) + 
        theme_few() + ggtitle('Average grade of students based on romantic relationship') + ylim(0,12.5) + 
        theme(panel.background = element_rect(fill = "#FFC4C47D")) +  scale_x_discrete(breaks=c("no","yes"), labels=c("No", "Yes"))
      
      
    })
    
    output$plotmath9 <- renderPlot({
      data <- read.csv('student-mat.csv', sep=";")
      data <- transform(data, avrg_G=(G1+G2+G3)/3)
      
      summarised <- data %>% group_by(Pstatus) %>% summarise(mean = mean(avrg_G), sd = sd(avrg_G))
      
      ggplot(summarised, aes(x = Pstatus, y = mean, ymin=mean-sd, ymax = mean+sd)) + geom_errorbar(size=0.5) + geom_point(colour = 'Red', size = 4) + labs(x = 'Cohabitation status', y = "Expected grade CI") + ggtitle("Confidence interval of the expected grade based on cohabitation with parents") +  scale_x_discrete(breaks=c("A","T"), labels=c("Apart", "Together")) + theme_hc()
      
      ##data$Pstatus2 <- data$Pstatus
      
      ##data["Pstatus2"][data["Pstatus2"]=='A']<-0
      ##data["Pstatus2"][data["Pstatus2"]=='T']<-1
      
      ##anova <- aov(avrg_G~Pstatus2 , data = data)
      
      ##summary(anova)
    })
    
    output$plotmath10 <- renderPlot({
      data <- read.csv('student-mat.csv', sep=";")
      data <- transform(data, avrg_G=(G1+G2+G3)/3)
      
      agg_data <- aggregate(avrg_G ~ goout, data = data, FUN=mean)
      agg_data[2,2] <- (agg_data[2,2] + agg_data[3,2])/2
      agg_data[3,2] <- (agg_data[4,2] + agg_data[5,2])/2
      agg_data<- agg_data[c(-4,-5),]
      
      ggplot(agg_data, aes(x = goout, y = round(avrg_G,2))) + geom_segment(aes(x=goout, xend=goout, y=0, yend = avrg_G), size=1) +
        geom_point(size=5, color="red", fill=alpha("orange"), alpha=1, shape=21, stroke=2) + 
        labs(x = 'Hangout with friends', y = "Average Grade of Exams") + 
        geom_text(aes(label=round(avrg_G,2)), position=position_dodge(width=0.9), vjust=-1.2, size = 4) + 
        scale_fill_brewer(palette = 'Set2', labels=c('Little','Medium', 'High')) + guides(fill=guide_legend("Hangout")) + 
        scale_x_discrete(limits=c("1","2","3"),labels = c("Low","Medium","High")) + theme_classic() +
        ggtitle('Average grade of students based on student hangout') + ylim(0,12) 
    })
    
    output$plotmath11 <- renderPlot({
      data <- read.csv('student-mat.csv', sep=";")
      data <- transform(data, avrg_G=(G1+G2+G3)/3)
      correlation <- cor(data$failures, data$absences)
      
      ggplot(data, aes(x = failures, y = absences)) + geom_point() + geom_jitter(height = 0.1, width = 0.1) + 
        geom_smooth(formula=y~x,method = 'lm', se=FALSE) + theme_classic() + 
        labs(x = 'Failures', y = "Absences", caption = paste0('Correlation coefficient ', round(correlation,3))) + 
        ggtitle('Student absences vs failures')
      
    })
    output$plotmath12 <- renderPlot({
    ggplot(data = data, aes(x = studytime, y=avrg_G, group = studytime, fill = factor(studytime))) + 
      geom_violin() + geom_boxplot(width = 0.3) + ggtitle('The distribution boxplot of average grade based on study time of the students') + 
      labs(x='Study hours (weekly)', y = 'Average grade') + theme_hc() +  theme(legend.position = "none") + scale_fill_brewer(palette='Pastel2') + 
      scale_x_discrete(limits=c("1","2","3","4"),labels = c("<2","2-5","5-10",">10"))
  })
    output$mathdata <- DT::renderDataTable({
      mathdata <- read.csv('student-mat.csv', sep=";")
      mathdata <- transform(data, avrg_G=(G1+G2+G3)/3)
      DT::datatable(mathdata, escape=FALSE, 
                    options = list(
                      pageLength = 10, autoWidth = TRUE,
                      columnDefs = list(list( targets = 1)),
                      scrollX = TRUE
                    ))
      
    })
      
    output$pordata <- DT::renderDataTable({
      pordata <- read.csv('student-por.csv', sep=";")
      pordata <- transform(data_por, avrg_G=(G1+G2+G3)/3)
      DT::datatable(pordata, escape=FALSE, 
                    options = list(
                      pageLength = 10, autoWidth = TRUE,
                      columnDefs = list(list( targets = 1)),
                      scrollX = TRUE
                    ))
      
    })
    
    output$interplot <- renderPlot({
 
      names <- Dict$new(
        'school'='School',
        'sex'='Gender',
        'age'='Age',
        'address'='Address',
        'famsize'='Family Size',
        'Pstatus'="Parent's cohabitation",
        'Medu' = "Mother's education",
        'Fedu' = "Father's education",
        "Mjob" = "Mother's job",
        "Fjob" = "Father's job",
        "reason" = "Reason",
        "guardian" = "Guardian",
        "traveltime" = "Home to school time",
        "studytime" = "Weekly study time",
        "failures" = "Past class failures",
        "schoolsup" = "Educational support",
        "famsup" = "Family educational support",
        "paid" = "Extra paid classes",
        "activities" = "Extra-curricular activities",
        "nursery" = "Attended nursery school",
        "higher" = "Intention for higher education",
        "internet" = "Internet at home",
        "romantic" = "Romantic relationship",
        'famrel' = 'Family Relationship',
        'freetime' = "Freetime",
        "goout" = "Hangout with friends",
        "Dalc" = "Workday alcohol consumption",
        "Walc" = "Weekend alcohol consumption",
        "health" = "Health status",
        "absences" = "Num. of school absences",
        "G1" = "Grade 1",
        "G2" = "Grade 2",
        "G3" = "Grade 3",
        "avrg_G" = "Average Grade",
        .class = "character",
        .overwrite = TRUE
      )
      
      
      th <- data[ ,c(input$VarX, 'famsup')]
      print(th)
      ggplot(data = th, aes(x=th[,1]))+
        geom_bar(fill = "#FF9933")+
        labs(x=names[colnames(th)[1]], 
             title = paste("Count of Unique Values of", names[input$VarX]))
        
      
      
      
        
        
      
    })
    
})