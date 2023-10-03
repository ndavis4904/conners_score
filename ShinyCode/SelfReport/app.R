
library(shiny)
library(ggplot2)

urlfile <- "https://raw.githubusercontent.com/ndavis4904/conners_score/main/Data/Conners_Self_Updated.csv"
self <- read.csv(url(urlfile))


years <- function(first.date, second.date) {
    floor((second.date - first.date)/365)[[1]]
}
months <- function(first.date, second.date) {
    temp.hold <- (second.date - first.date)[[1]]
    temp.age <- temp.hold/365
    floor((temp.age-floor(temp.age))*12)
}
prob_function <- function(score) {
    ifelse(score == 0, 26, 
           ifelse(score == 1, 35,
                  ifelse(score == 2, 44,
                         ifelse(score == 3, 52,
                                ifelse(score == 4, 59,
                                       ifelse(score == 5, 66,
                                              ifelse(score == 6, 73,
                                                     ifelse(score == 7, 78,
                                                            ifelse(score == 8, 83,
                                                                   ifelse(score == 9, 87,
                                                                          ifelse(score == 10, 91,
                                                                                 ifelse(score == 11, 94,
                                                                                        ifelse(score == 12, 96,
                                                                                               ifelse(score == 13, 97,
                                                                                                      ifelse(score == 14, 98,
                                                                                                             ifelse(score == 15, 98,
                                                                                                                    ifelse(score == 16, 99,
                                                                                                                           ifelse(score == 17, 99,
                                                                                                                                  ifelse(score == 18, 99,
                                                                                                                                         ifelse(score == 19, 99,
                                                                                                                                                ifelse(score == 20, 99))))
                                                                                                                    )))))))))))))))))
}

ui <- fluidPage(
    
    # Application title
    titlePanel("Self Report Form"),
    
    # Sidebar with a demographc input of the examinee 
    sidebarLayout(
        sidebarPanel(
            dateInput("dob", label = "Student's Date of Birth", format = "m/d/yyyy"),
            
            dateInput("doe", label = "Date of Evaluation", format = "m/d/yyyy"),
            
            radioButtons("norms", label = "Which Norm Table?",
                         choices = c("Male", "Female", "Total")),
            
            textOutput("Age")
            
        ),
        
        # Input self responses
        mainPanel(
            tabsetPanel(
                tabPanel("Input",
                         inputPanel(
                             numericInput("Q1", label = "Question 1", value = 0),
                             numericInput("Q2", label = "Question 2", value = 0),
                             numericInput("Q3", label = "Question 3", value = 0),
                             numericInput("Q4", label = "Question 4", value = 0),
                             numericInput("Q5", label = "Question 5", value = 0),
                             numericInput("Q6", label = "Question 6", value = 0),
                             numericInput("Q7", label = "Question 7", value = 0),
                             numericInput("Q8", label = "Question 8", value = 0),
                             numericInput("Q9", label = "Question 9", value = 0),
                             numericInput("Q10", label = "Question 10", value = 0),
                             numericInput("Q11", label = "Question 11", value = 0),
                             numericInput("Q12", label = "Question 12", value = 0),
                             numericInput("Q13", label = "Question 13", value = 0),
                             numericInput("Q14", label = "Question 14", value = 0),
                             numericInput("Q15", label = "Question 15", value = 0),
                             numericInput("Q16", label = "Question 16", value = 0),
                             numericInput("Q17", label = "Question 17", value = 0),
                             numericInput("Q18", label = "Question 18", value = 0),
                             numericInput("Q19", label = "Question 19", value = 0),
                             numericInput("Q20", label = "Question 20", value = 0),
                             numericInput("Q21", label = "Question 21", value = 0),
                             numericInput("Q22", label = "Question 22", value = 0),
                             numericInput("Q23", label = "Question 23", value = 0),
                             numericInput("Q24", label = "Question 24", value = 0),
                             numericInput("Q25", label = "Question 25", value = 0),
                             numericInput("Q26", label = "Question 26", value = 0),
                             numericInput("Q27", label = "Question 27", value = 0),
                             numericInput("Q28", label = "Question 28", value = 0),
                             numericInput("Q29", label = "Question 29", value = 0),
                             numericInput("Q30", label = "Question 30", value = 0),
                             numericInput("Q31", label = "Question 31", value = 0),
                             numericInput("Q32", label = "Question 32", value = 0),
                             numericInput("Q33", label = "Question 33", value = 0),
                             numericInput("Q34", label = "Question 34", value = 0),
                             numericInput("Q35", label = "Question 35", value = 0),
                             numericInput("Q36", label = "Question 36", value = 0),
                             numericInput("Q37", label = "Question 37", value = 0),
                             numericInput("Q38", label = "Question 38", value = 0),
                             numericInput("Q39", label = "Question 39", value = 0),
                             numericInput("Q40", label = "Question 40", value = 0),
                             numericInput("Q41", label = "Question 41", value = 0),
                             numericInput("Q42", label = "Question 42", value = 0),
                             numericInput("Q43", label = "Question 43", value = 0),
                             numericInput("Q44", label = "Question 44", value = 0),
                             numericInput("Q45", label = "Question 45", value = 0),
                             numericInput("Q46", label = "Question 46", value = 0),
                             numericInput("Q47", label = "Question 47", value = 0),
                             numericInput("Q48", label = "Question 48", value = 0),
                             numericInput("Q49", label = "Question 49", value = 0),
                             numericInput("Q50", label = "Question 50", value = 0),
                             numericInput("Q51", label = "Question 51", value = 0),
                             numericInput("Q52", label = "Question 52", value = 0),
                             numericInput("Q53", label = "Question 53", value = 0),
                             numericInput("Q54", label = "Question 54", value = 0),
                             numericInput("Q55", label = "Question 55", value = 0),
                             numericInput("Q56", label = "Question 56", value = 0),
                             numericInput("Q57", label = "Question 57", value = 0),
                             numericInput("Q58", label = "Question 58", value = 0),
                             numericInput("Q59", label = "Question 59", value = 0),
                             numericInput("Q60", label = "Question 60", value = 0),
                             numericInput("Q61", label = "Question 61", value = 0),
                             numericInput("Q62", label = "Question 62", value = 0),
                             numericInput("Q63", label = "Question 63", value = 0),
                             numericInput("Q64", label = "Question 64", value = 0),
                             numericInput("Q65", label = "Question 65", value = 0),
                             numericInput("Q66", label = "Question 66", value = 0),
                             numericInput("Q67", label = "Question 67", value = 0),
                             numericInput("Q68", label = "Question 68", value = 0),
                             numericInput("Q69", label = "Question 69", value = 0),
                             numericInput("Q70", label = "Question 70", value = 0),
                             numericInput("Q71", label = "Question 71", value = 0),
                             numericInput("Q72", label = "Question 72", value = 0),
                             numericInput("Q73", label = "Question 73", value = 0),
                             numericInput("Q74", label = "Question 74", value = 0),
                             numericInput("Q75", label = "Question 75", value = 0),
                             numericInput("Q76", label = "Question 76", value = 0),
                             numericInput("Q77", label = "Question 77", value = 0),
                             numericInput("Q78", label = "Question 78", value = 0),
                             numericInput("Q79", label = "Question 79", value = 0),
                             numericInput("Q80", label = "Question 80", value = 0),
                             numericInput("Q81", label = "Question 81", value = 0),
                             numericInput("Q82", label = "Question 82", value = 0),
                             numericInput("Q83", label = "Question 83", value = 0),
                             numericInput("Q84", label = "Question 84", value = 0),
                             numericInput("Q85", label = "Question 85", value = 0),
                             numericInput("Q86", label = "Question 86", value = 0),
                             numericInput("Q87", label = "Question 87", value = 0),
                             numericInput("Q88", label = "Question 88", value = 0),
                             numericInput("Q89", label = "Question 89", value = 0),
                             numericInput("Q90", label = "Question 90", value = 0),
                             numericInput("Q91", label = "Question 91", value = 0),
                             numericInput("Q92", label = "Question 92", value = 0),
                             numericInput("Q93", label = "Question 93", value = 0),
                             numericInput("Q94", label = "Question 94", value = 0),
                             numericInput("Q95", label = "Question 95", value = 0),
                             numericInput("Q96", label = "Question 96", value = 0),
                             numericInput("Q97", label = "Question 97", value = 0)
                         ),
                         
                         h3(" ") #Adds whitespace to bottom
                ),
                
                #Run validity analysis on response consistency and positive or negative responses
                tabPanel("Validity Check",
                         
                         h4("Inconsistency Index"),
                         plotOutput("Differences_graph"),
                         htmlOutput("diff_phrase"),
                         
                         h4("Positive Impression"),
                         textOutput("PI_1"),
                         htmlOutput("PI_2"),
                         
                         h4("Negative Impression"),
                         textOutput("NI_1"),
                         htmlOutput("NI_2"),
                         
                         h3(" ") #Adds whitespace to bottom
                ),
                
                #Report the symptom counts based on the DSM-5 criteria
                tabPanel("DSM-5 Symptom Counts",
                         
                         h3("ADHD Inattentive"),
                         htmlOutput("symp_in"),
                         
                         h3("ADHD Hyperactive-Impulsive"),
                         htmlOutput("symp_hy"),
                         
                         h3("ADHD Combined"),
                         htmlOutput("symp_adhdComb"),
                         
                         h3("Conduct Disorder"),
                         htmlOutput("symp_cd"),
                         
                         h3("Oppositional Defiant Disorder"),
                         htmlOutput("symp_od"),
                         
                         h3("Conners 3 ADHD Index Probability Score"),
                         htmlOutput("prob_calc"),
                         
                         h3(" ") #Adds whitespace to bottom
                         
                ),
                
                #Report calculated T-Scores for each area measured by the Conners 3
                tabPanel("T-Scores",
                         h3("Inattention"),
                         textOutput("IN"),
                         
                         h3("Hyperactivity/Impulsivity"),
                         textOutput("HY"),
                         
                         h3("Learning Problems"),
                         textOutput("LP"),
                         
                         h3("Defiance/Aggression"),
                         textOutput("AG"),
                         
                         h3("Family Relations"),
                         textOutput("FR"),
                         
                         h3("DSM-5 ADHD Inattentive"),
                         textOutput("AN"),
                         
                         h3("DSM-5 ADHD Hyperactive-Impulsive"),
                         textOutput("AH"),
                         
                         h3("DSM-5 Conduct Disorder"),
                         textOutput("CD"),
                         
                         h3("DSM-5 Oppositional Defiant Disorder"),
                         textOutput("OD"),
                         
                         h3(" ") #Adds whitespace to bottom
                )
            )
        )))

server <- function(input, output) {
    
    output$Age <- renderText(paste0(years(input$dob, input$doe), " Years, and ",
                                    months(input$dob, input$doe), " Months"))
    
    #Functions for data on the validity check tab
    
    #Creating the graph of critical differences
    output$Differences_graph <- renderPlot(qplot(c(abs(diff(c(input$Q73, input$Q85))), abs(diff(c(input$Q50, input$Q60))), 
                                                   abs(diff(c(input$Q81, input$Q79))), abs(diff(c(input$Q46, input$Q90))),
                                                   abs(diff(c(input$Q17, input$Q30))), abs(diff(c(input$Q56, input$Q70))),
                                                   abs(diff(c(input$Q42, input$Q63))), abs(diff(c(input$Q69, input$Q83))),
                                                   abs(diff(c(input$Q43, input$Q61))), abs(diff(c(input$Q25, input$Q38))))) +
                                               geom_bar(fill = "skyblue", color = "black", width = 1) +
                                               theme(panel.background = element_blank()) +
                                               labs(x = "") +
                                               geom_hline(yintercept = 2, col = "firebrick", linewidth = 1.5) +
                                               scale_x_continuous(breaks = c(0, 1, 2, 3)))
    
    output$diff_phrase <- renderText(paste0("Note: If the total number of differences of 2s and 3s (seen above in the graph) are greater than 2,",
                                            "<b>"," AND ", "</b>", sum(c(abs(diff(c(input$Q73, input$Q85))), abs(diff(c(input$Q50, input$Q60))), 
                                                                         abs(diff(c(input$Q81, input$Q79))), abs(diff(c(input$Q46, input$Q90))),
                                                                         abs(diff(c(input$Q17, input$Q30))), abs(diff(c(input$Q56, input$Q70))),
                                                                         abs(diff(c(input$Q42, input$Q63))), abs(diff(c(input$Q69, input$Q83))),
                                                                         abs(diff(c(input$Q43, input$Q61))), abs(diff(c(input$Q25, input$Q38))))),
                                            " is greater than 9, there may be a question of consistency in responses."))
    
    #Calculating positive response style
    output$PI_1 <- renderText(paste0("Total Positive Impressions is ", sum(ifelse(input$Q12 == 0, 1, 0),
                                                                           ifelse(input$Q37 == 3, 1, 0),
                                                                           ifelse(input$Q48 == 3, 1, 0),
                                                                           ifelse(input$Q54 == 3, 1, 0),
                                                                           ifelse(input$Q75 == 3, 1, 0),
                                                                           ifelse(input$Q93 == 0, 1, 0)), "."))
    output$PI_2 <- renderText(paste0("If ", "<b>", sum(ifelse(input$Q12 == 0, 1, 0),
                                                       ifelse(input$Q37 == 3, 1, 0),
                                                       ifelse(input$Q48 == 3, 1, 0),
                                                       ifelse(input$Q54 == 3, 1, 0),
                                                       ifelse(input$Q75 == 3, 1, 0),
                                                       ifelse(input$Q93 == 0, 1, 0)), "</b>",
                                     " is greater than 4, there is a possible positive response style indicated."))
    
    #Calculating negative response style
    output$NI_1 <- renderText(paste0("Total Negative Impressions is ", sum(ifelse(input$Q10 < 2, 1, 0),
                                                                           ifelse(input$Q11 < 2, 1, 0),
                                                                           ifelse(input$Q19 < 2, 1, 0),
                                                                           ifelse(input$Q28 > 1, 1, 0),
                                                                           ifelse(input$Q40 < 2, 1, 0),
                                                                           ifelse(input$Q41 < 2, 1, 0)),
                                     "."))
    output$NI_2 <- renderText(HTML(paste0("If ", "<b>", sum(ifelse(input$Q10 < 2, 1, 0),
                                                            ifelse(input$Q11 < 2, 1, 0),
                                                            ifelse(input$Q19 < 2, 1, 0),
                                                            ifelse(input$Q28 > 1, 1, 0),
                                                            ifelse(input$Q40 < 2, 1, 0),
                                                            ifelse(input$Q41 < 2, 1, 0)), "</b>",
                                          " is greater than 5, there is a possible positive response style indicated.")))
    
    #Calclating dsm-5 symptom counts
    
    #ADHD Inatentive
    output$symp_in <- renderText(paste0("Symptom criteria probably met if ", "<b>", sum(c(
        ifelse(input$Q31 > 1 | input$Q39 > 1,TRUE,FALSE),
        ifelse(input$Q63 > 1,TRUE,FALSE),
        ifelse(input$Q42 > 1,TRUE,FALSE),
        ifelse(input$Q61 > 1 & input$Q17 > 1,TRUE,FALSE),
        ifelse(input$Q21 > 1,TRUE,FALSE),
        ifelse(input$Q51 > 1,TRUE,FALSE),
        ifelse(input$Q5 > 1,TRUE,FALSE),
        ifelse(input$Q77 > 1,TRUE,FALSE),
        ifelse(input$Q32 > 1,TRUE,FALSE)
    )), "</b>",
    " greater than 6."))
    
    #ADHD Hyperactive-Impulsive
    output$symp_hy <- renderText(paste0("Symptom criteria probably met if ", "<b>", sum(c(
        ifelse(input$Q60 > 1,TRUE,FALSE),
        ifelse(input$Q64 > 1,TRUE,FALSE),
        ifelse(input$Q20 > 1 | input$Q7 > 1,TRUE,FALSE),
        ifelse(input$Q84 > 1,TRUE,FALSE),
        ifelse(input$Q66 > 0 | input$Q55 > 1,TRUE,FALSE),
        ifelse(input$Q34 > 1,TRUE,FALSE),
        ifelse(input$Q9 > 1,TRUE,FALSE),
        ifelse(input$Q27 > 1,TRUE,FALSE),
        ifelse(input$Q6 > 1, TRUE, FALSE)
    )), "</b>",
    " greater than 6."))
    
    #ADHD Combined
    output$symp_adhdComb <- renderText(paste0("Symptom criteria probably met if 
                                             symptom criteria probably met for ADHD Inattentive ", "<b>", "AND", "</b>", " ADHD Hyperactive-Impulsive"))
    
    #Conduct Disorder
    output$symp_cd <- renderText(paste0("Symptom criteria probably met if ", "<b>", sum(c(
        ifelse(input$Q25 > 0,TRUE,FALSE),
        ifelse(input$Q38 > 0,TRUE,FALSE),
        ifelse(input$Q59 > 0,TRUE,FALSE),
        ifelse(input$Q86 > 0,TRUE,FALSE),
        ifelse(input$Q47 > 0,TRUE,FALSE),
        ifelse(input$Q13 > 0,TRUE,FALSE),
        ifelse(input$Q72 > 0,TRUE,FALSE),
        ifelse(input$Q82 > 0,TRUE,FALSE),
        ifelse(input$Q78 > 0,TRUE,FALSE),
        ifelse(input$Q16 > 1,TRUE,FALSE),
        ifelse(input$Q52 > 0,TRUE,FALSE),
        ifelse(input$Q91 > 0,TRUE,FALSE),
        ifelse(input$Q8 > 0,TRUE,FALSE),
        ifelse(input$Q33 > 0,TRUE,FALSE)
    )), "</b>", " is greater than 3."))
    
    #Oppositional Defiant Disorder
    output$symp_od <- renderText(paste0("Symptom criteria probably met if ", "<b>", sum(c(
        ifelse(input$Q67 > 1,TRUE,FALSE),
        ifelse(input$Q24 > 1,TRUE,FALSE),
        ifelse(input$Q1 < 2,TRUE,FALSE),
        ifelse(input$Q3 > 1,TRUE,FALSE),
        ifelse(input$Q62 > 1,TRUE,FALSE),
        ifelse(input$Q74 > 1,TRUE,FALSE),
        ifelse(input$Q87 > 1,TRUE,FALSE),
        ifelse(input$Q94 > 1,TRUE,FALSE)
    )), "</b>", " is greater than 4."))
    
    #Calculate probability of diagnosis of ADHD
    output$prob_calc <- renderText(paste0("There is ", "<b>", prob_function(sum(c(
        if(input$Q6 == 0) {
            0
        } else if(input$Q6 == 1) {
            0
        } else if(input$Q6 == 2) {
            1
        } else if(input$Q6 == 3) {
            2
        },
        if(input$Q9 == 0) {
            0
        } else if(input$Q9 == 1) {
            0
        } else if(input$Q9 == 2) {
            1
        } else if(input$Q9 == 3) {
            2
        },
        if(input$Q15 == 0) {
            0
        } else if(input$Q15 == 1) {
            0
        } else if(input$Q15 == 2) {
            1
        } else if(input$Q15 == 3) {
            2
        },
        if(input$Q21 == 0) {
            0
        } else if(input$Q21 == 1) {
            0
        } else if(input$Q21 == 2) {
            1
        } else if(input$Q21 == 3) {
            2
        },
        if(input$Q34 == 0) {
            0
        } else if(input$Q34 == 1) {
            0
        } else if(input$Q34 == 2) {
            1
        } else if(input$Q34 == 3) {
            1
        },
        if(input$Q35 == 0) {
            0
        } else if(input$Q35 == 1) {
            0
        } else if(input$Q35 == 2) {
            2
        } else if(input$Q35 == 3) {
            2
        },
        if(input$Q43 == 0) {
            0
        } else if(input$Q43 == 1) {
            0
        } else if(input$Q43 == 2) {
            1
        } else if(input$Q43 == 3) {
            2
        },
        if(input$Q50 == 0) {
            0
        } else if(input$Q50 == 1) {
            0
        } else if(input$Q50 == 2) {
            1
        } else if(input$Q50 == 3) {
            1
        },
        if(input$Q61 == 0) {
            0
        } else if(input$Q61 == 1) {
            0
        } else if(input$Q61 == 2) {
            2
        } else if(input$Q61 == 3) {
            2
        },
        if(input$Q63 == 0) {
            0
        } else if(input$Q63 == 1) {
            0
        } else if(input$Q63 == 2) {
            1
        } else if(input$Q63 == 3) {
            2
        }
    ))), "</b>", "% chance of diagnosis of ADHD."))
    
    #Calculating t-scores based on norm data
    #Formula is 50 + (10*(Raw Score - Mean)/SD)
    
    #inattention
    output$IN <- renderPrint(cat(round(50 + 10 * ((sum(input$Q17, input$Q30, input$Q31, input$Q43, input$Q49,
                                                       input$Q53, input$Q63, input$Q71, input$Q77, input$Q79,
                                                       input$Q81) - 
                                                       subset(self, Age == years(input$dob, input$doe) & 
                                                                  Scale == "IN" & NormGrp == input$norms)[[3]]) / 
                                                      subset(self, Age == years(input$dob, input$doe) & 
                                                                 Scale == "IN" & NormGrp == input$norms)[[4]]))))
    #hyperactivity/impulsivity
    output$HY <- renderPrint(cat(round(50 + 10 * ((sum(input$Q4, input$Q6, input$Q7, input$Q9, input$Q20,
                                                       input$Q34, input$Q50, input$Q57, input$Q60, input$Q64,
                                                       input$Q66, input$Q84, input$Q88, input$Q92) - 
                                                       subset(self, Age == years(input$dob, input$doe) & 
                                                                  Scale == "HY" & NormGrp == input$norms)[[3]]) / 
                                                      subset(self, Age == years(input$dob, input$doe) & 
                                                                 Scale == "HY" & NormGrp == input$norms)[[4]]))))
    #learning problems
    output$LP <- renderPrint(cat(round(50 + 10 * (sum(input$Q15, input$Q35, input$Q45, input$Q56, input$Q58,
                                                      input$Q65, input$Q70, input$Q76) - 
                                                      subset(self, Age == years(input$dob, input$doe) & 
                                                                 Scale == "LP" & NormGrp == input$norms)[[3]]) / 
                                           subset(self, Age == years(input$dob, input$doe) & 
                                                      Scale == "LP" & NormGrp == input$norms)[[4]])))
    #defiance/aggression
    output$AG <- renderPrint(cat(round(50 + 10 * (sum(input$Q8, input$Q16, input$Q18, input$Q22, input$Q25,
                                                      input$Q33, input$Q38, input$Q52, input$Q59, input$Q62,
                                                      input$Q72, input$Q82, input$Q86, input$Q91, input$Q94) -
                                                      subset(self, Age == years(input$dob, input$doe) & 
                                                                 Scale == "AG" & NormGrp == input$norms)[[3]]) / 
                                           subset(self, Age == years(input$dob, input$doe) & 
                                                      Scale == "AG" & NormGrp == input$norms)[[4]])))
    #family relations
    output$FR <- renderPrint(cat(round(50 + 10 * (sum(input$Q14, input$Q23, input$Q26, input$Q69, input$Q73,
                                                      input$Q83, input$Q85, input$Q89) - 
                                                      subset(self, Age == years(input$dob, input$doe) & 
                                                                 Scale == "FR" & NormGrp == input$norms)[[3]]) / 
                                           subset(self, Age == years(input$dob, input$doe) & 
                                                      Scale == "FR" & NormGrp == input$norms)[[4]])))
    #dsm-5 inattentive
    output$AN <- renderPrint(cat(round(50 + 10 * (sum(input$Q5, input$Q17, input$Q21, input$Q31, input$Q32,
                                                      input$Q39, input$Q42, input$Q51, input$Q61, input$Q63,
                                                      input$Q77) -
                                                      subset(self, Age == years(input$dob, input$doe) & 
                                                                 Scale == "AN" & NormGrp == input$norms)[[3]]) / 
                                           subset(self, Age == years(input$dob, input$doe) & 
                                                      Scale == "AN" & NormGrp == input$norms)[[4]])))
    #dsm-5 hyperactive-impulsive
    output$AH <- renderPrint(cat(round (50 + 10 * (sum(input$Q6, input$Q7, input$Q9, input$Q20, input$Q27,
                                                       input$Q34, input$Q55, input$Q60, input$Q64, input$Q66,
                                                       input$Q84) -
                                                       subset(self, Age == years(input$dob, input$doe) & 
                                                                  Scale == "AH" & NormGrp == input$norms)[[3]]) / 
                                            subset(self, Age == years(input$dob, input$doe) & 
                                                       Scale == "AH" & NormGrp == input$norms)[[4]])))
    #dsm-5 conduct disorder
    output$CD <- renderPrint(cat(round(50 + 10 * (sum(input$Q8, input$Q13, input$Q16, input$Q25, input$Q33,
                                                      input$Q38, input$Q47, input$Q52, input$Q59, input$Q72,
                                                      input$Q78, input$Q82, input$Q86, input$Q91) - 
                                                      subset(self, Age == years(input$dob, input$doe) & 
                                                                 Scale == "CD" & NormGrp == input$norms)[[3]]) / 
                                           subset(self, Age == years(input$dob, input$doe) & 
                                                      Scale == "CD" & NormGrp == input$norms)[[4]])))
    #dsm-5 oppositional defiant disorder
    output$OD <- renderPrint(cat(round(50 + 10 * (sum(if(input$Q1 == 0) {3} else if(input$Q1 == 1) {2}
                                                      else if(input$Q1 == 2) {1} else {0}, input$Q3, input$Q24,
                                                      input$Q62, input$Q67, input$Q74, input$Q87, input$Q94) - 
                                                      subset(self, Age == years(input$dob, input$doe) & 
                                                                 Scale == "OD" & NormGrp == input$norms)[[3]]) / 
                                           subset(self, Age == years(input$dob, input$doe) & 
                                                      Scale == "OD" & NormGrp == input$norms)[[4]])))
}

# Run the application 
shinyApp(ui = ui, server = server)
