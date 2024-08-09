#Parent Form
library(shiny)
library(ggplot2)

urlfile <- "https://raw.githubusercontent.com/ndavis4904/conners_score/main/Data/Conners_Parent_Updated.csv"
parent <- read.csv(url(urlfile))


years <- function(first.date, second.date) {
    floor((second.date - first.date)/365)[[1]]
}
months <- function(first.date, second.date) {
    temp.hold <- (second.date - first.date)[[1]]
    temp.age <- temp.hold/365
    floor((temp.age-floor(temp.age))*12)
}
prob_function <- function(score) {
    ifelse(score == 0, 11, 
           ifelse(score == 1, 29,
                  ifelse(score == 2, 41,
                         ifelse(score == 3, 51,
                                ifelse(score == 4, 56,
                                       ifelse(score == 5, 64,
                                              ifelse(score == 6, 71,
                                                     ifelse(score == 7, 77,
                                                            ifelse(score == 8, 82,
                                                                   ifelse(score == 9, 87,
                                                                          ifelse(score == 10, 91,
                                                                                 ifelse(score == 11, 94,
                                                                                        ifelse(score == 12, 97,
                                                                                               ifelse(score == 13, 98,
                                                                                                      ifelse(score == 14, 99,
                                                                                                             ifelse(score == 15, 99,
                                                                                                                    ifelse(score == 16, 99,
                                                                                                                           ifelse(score == 17, 99,
                                                                                                                                  ifelse(score == 18, 99,
                                                                                                                                         ifelse(score == 19, 99,
                                                                                                                                                ifelse(score == 20, 99))))
                                                                                                                    )))))))))))))))))
}

ui <- fluidPage(
    
    # Application title
    titlePanel("Parent Form"),
    
    # Sidebar with a demographc input of the examinee 
    sidebarLayout(
        sidebarPanel(
            dateInput("dob", label = "Student's Date of Birth", format = "m/d/yyyy"),
            
            dateInput("doe", label = "Date of Evaluation", format = "m/d/yyyy"),
            
            radioButtons("norms", label = "Which Norm Table?",
                         choices = c("Male", "Female", "Total")),
            
            textOutput("Age")
            
        ),
        
        # Input parent responses
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
                             numericInput("Q97", label = "Question 97", value = 0),
                             numericInput("Q98", label = "Question 98", value = 0),
                             numericInput("Q99", label = "Question 99", value = 0),
                             numericInput("Q100", label = "Question 100", value = 0),
                             numericInput("Q101", label = "Question 101", value = 0),
                             numericInput("Q102", label = "Question 102", value = 0),
                             numericInput("Q103", label = "Question 103", value = 0),
                             numericInput("Q104", label = "Question 104", value = 0),
                             numericInput("Q105", label = "Question 105", value = 0),
                             numericInput("Q106", label = "Question 106", value = 0),
                             numericInput("Q107", label = "Question 107", value = 0),
                             numericInput("Q108", label = "Question 108", value = 0)
                         ),
                         
                         h3(" ") #Adds whitespace to bottom
                ),
                
                #Run validity analysis on response consistency and positive or negative responses
                tabPanel("Validity Check",
                         
                         h4("Inconsistency Index"),
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
                         
                         h3("Executive Functioning"),
                         textOutput("EF"),
                         
                         h3("Defiance/Aggression"),
                         textOutput("AG"),
                         
                         h3("Peer Relations"),
                         textOutput("PR"),
                         
                         h3("Conners 3 Global Index"),
                         textOutput("GI"),
                         
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
    
    #Counting comparisons for the inconsistency index
    output$diff_phrase <- renderText(paste0("Note: If the total number of differences of 2s and 3s (which is ",
                                            sum(grepl(2, c(abs(diff(c(input$Q44, input$Q67))), abs(diff(c(input$Q12, input$Q23))), 
                                                           abs(diff(c(input$Q36, input$Q60))), abs(diff(c(input$Q14, input$Q81))),
                                                           abs(diff(c(input$Q19, input$Q98))), abs(diff(c(input$Q45, input$Q99))),
                                                           abs(diff(c(input$Q94, input$Q102))), abs(diff(c(input$Q75, input$Q79))),
                                                           abs(diff(c(input$Q13, input$Q92))), abs(diff(c(input$Q39, input$Q83)))))) +
                                            sum(grepl(3, c(abs(diff(c(input$Q44, input$Q67))), abs(diff(c(input$Q12, input$Q23))), 
                                                           abs(diff(c(input$Q36, input$Q60))), abs(diff(c(input$Q14, input$Q81))),
                                                           abs(diff(c(input$Q19, input$Q98))), abs(diff(c(input$Q45, input$Q99))),
                                                           abs(diff(c(input$Q94, input$Q102))), abs(diff(c(input$Q75, input$Q79))),
                                                           abs(diff(c(input$Q13, input$Q92))), abs(diff(c(input$Q39, input$Q83))))))
                                            ,") are greater than 2,",
                                            "<b>"," AND ", "</b>", sum(c(abs(diff(c(input$Q44, input$Q67))), abs(diff(c(input$Q12, input$Q23))), 
                                                                         abs(diff(c(input$Q36, input$Q60))), abs(diff(c(input$Q14, input$Q81))),
                                                                         abs(diff(c(input$Q19, input$Q98))), abs(diff(c(input$Q45, input$Q99))),
                                                                         abs(diff(c(input$Q94, input$Q102))), abs(diff(c(input$Q75, input$Q79))),
                                                                         abs(diff(c(input$Q13, input$Q92))), abs(diff(c(input$Q39, input$Q83))))),
                                            " is greater than 7, there may be a question of consistency in responses."))
    
    
    #Calculating positive response style
    output$PI_1 <- renderText(paste0("Total Positive Impressions is ", sum(ifelse(input$Q31 == 0, 1, 0),
                                                                           ifelse(input$Q33 == 3, 1, 0),
                                                                           ifelse(input$Q38 == 0, 1, 0),
                                                                           ifelse(input$Q74 == 3, 1, 0),
                                                                           ifelse(input$Q80 == 3, 1, 0),
                                                                           ifelse(input$Q105 == 3, 1, 0)), "."))
    output$PI_2 <- renderText(paste0("If ", "<b>", sum(ifelse(input$Q31 == 0, 1, 0),
                                                       ifelse(input$Q33 == 3, 1, 0),
                                                       ifelse(input$Q38 == 0, 1, 0),
                                                       ifelse(input$Q74 == 3, 1, 0),
                                                       ifelse(input$Q80 == 3, 1, 0),
                                                       ifelse(input$Q105 == 3, 1, 0)), "</b>",
                                     " is greater than 5, there is a possible positive response style indicated."))
    
    #Calculating negative response style
    output$NI_1 <- renderText(paste0("Total Negative Impressions is ", sum(ifelse(input$Q1 < 2, 1, 0),
                                                                           ifelse(input$Q8 < 2, 1, 0),
                                                                           ifelse(input$Q18 > 1, 1, 0),
                                                                           ifelse(input$Q26 > 1, 1, 0),
                                                                           ifelse(input$Q32 > 1, 1, 0),
                                                                           ifelse(input$Q42 > 1, 1, 0)),
                                     "."))
    output$NI_2 <- renderText(HTML(paste0("If ", "<b>", sum(ifelse(input$Q1 < 2, 1, 0),
                                                            ifelse(input$Q8 < 2, 1, 0),
                                                            ifelse(input$Q18 > 1, 1, 0),
                                                            ifelse(input$Q26 > 1, 1, 0),
                                                            ifelse(input$Q32 > 1, 1, 0),
                                                            ifelse(input$Q42 > 1, 1, 0)), "</b>",
                                          " is greater than 5, there is a possible positive response style indicated.")))
    
    #Calclating dsm-5 symptom counts
    
    #ADHD Inatentive
    output$symp_in <- renderText(paste0("Symptom criteria probably met if ", "<b>", sum(c(
        ifelse(input$Q47 > 1,TRUE,FALSE),
        ifelse(input$Q95 > 1,TRUE,FALSE),
        ifelse(input$Q35 > 1,TRUE,FALSE),
        ifelse(input$Q68 > 1 & input$Q79 > 1,TRUE,FALSE),
        ifelse(input$Q84 > 2,TRUE,FALSE),
        ifelse(input$Q28 > 1,TRUE,FALSE),
        ifelse(input$Q97 > 1,TRUE,FALSE),
        ifelse(input$Q101 > 1,TRUE,FALSE),
        ifelse(input$Q2 > 1,TRUE,FALSE)
    )), "</b>",
    " greater than ", ifelse(years(input$dob, input$doe) > 16, 5, 6), "."))
    
    #ADHD Hyperactive-Impulsive
    output$symp_hy <- renderText(paste0("Symptom criteria probably met if ", "<b>", sum(c(
        ifelse(input$Q98 > 1,TRUE,FALSE),
        ifelse(input$Q93 > 1,TRUE,FALSE),
        ifelse(input$Q69 > 0 | input$Q99 > 0,TRUE,FALSE),
        ifelse(input$Q71 > 1,TRUE,FALSE),
        ifelse(input$Q54 > 0 | input$Q45 > 0,TRUE,FALSE),
        ifelse(input$Q3 > 1,TRUE,FALSE),
        ifelse(input$Q43 > 1,TRUE,FALSE),
        ifelse(input$Q61 > 1,TRUE,FALSE),
        ifelse(input$Q104 > 1, TRUE, FALSE)
    )), "</b>",
    " greater than ", ifelse(years(input$dob, input$doe) > 16, 5, 6), "."))
    
    #ADHD Combined
    output$symp_adhdComb <- renderText(paste0("Symptom criteria probably met if 
                                             symptom criteria probably met for ADHD Inattentive ", "<b>", "AND", "</b>", " ADHD Hyperactive-Impulsive"))
    
    #Conduct Disorder
    output$symp_cd <- renderText(paste0("Symptom criteria probably met if ", "<b>", sum(c(
        ifelse(input$Q16 > 0,TRUE,FALSE),
        ifelse(input$Q30 > 0,TRUE,FALSE),
        ifelse(input$Q27 > 0,TRUE,FALSE),
        ifelse(input$Q39 > 0,TRUE,FALSE),
        ifelse(input$Q41 > 0,TRUE,FALSE),
        ifelse(input$Q96 > 0,TRUE,FALSE),
        ifelse(input$Q11 > 0,TRUE,FALSE),
        ifelse(input$Q78 > 0,TRUE,FALSE),
        ifelse(input$Q65 > 0,TRUE,FALSE),
        ifelse(input$Q89 > 0,TRUE,FALSE),
        ifelse(input$Q56 > 1,TRUE,FALSE),
        ifelse(input$Q58 > 0,TRUE,FALSE),
        ifelse(input$Q91 > 0,TRUE,FALSE),
        ifelse(input$Q76 > 0,TRUE,FALSE),
        ifelse(input$Q6 > 0,TRUE,FALSE)
    )), "</b>", " is greater than 3."))
    
    #Oppositional Defiant Disorder
    output$symp_od <- renderText(paste0("Symptom criteria probably met if ", "<b>", sum(c(
        ifelse(input$Q14 > 1,TRUE,FALSE),
        ifelse(input$Q73 > 1,TRUE,FALSE),
        ifelse(input$Q48 > 0,TRUE,FALSE),
        ifelse(input$Q102 > 1,TRUE,FALSE),
        ifelse(input$Q94 > 1,TRUE,FALSE),
        ifelse(input$Q59 > 1,TRUE,FALSE),
        ifelse(input$Q21 > 1,TRUE,FALSE),
        ifelse(input$Q57 > 0,TRUE,FALSE)
    )), "</b>", " is greater than 4."))
    
    #Calculate probability of diagnosis of ADHD
    output$prob_calc <- renderText(paste0("There is ", "<b>", prob_function(sum(c(        #responses to specific questions need to be recoded for probability conversion
        if(input$Q19 == 0) {
            0
        } else if(input$Q19 == 1) {
            0
        } else if(input$Q19 == 2) {
            1
        } else if(input$Q19 == 3) {
            2
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
        if(input$Q47 == 0) {
            0
        } else if(input$Q47 == 1) {
            0
        } else if(input$Q47 == 2) {
            1
        } else if(input$Q47 == 3) {
            2
        },
        if(input$Q67 == 0) {
            0
        } else if(input$Q67 == 1) {
            0
        } else if(input$Q67 == 2) {
            1
        } else if(input$Q67 == 3) {
            2
        },
        if(input$Q84 == 0) {
            0
        } else if(input$Q84 == 1) {
            0
        } else if(input$Q84 == 2) {
            1
        } else if(input$Q84 == 3) {
            2
        },
        if(input$Q88 == 0) {
            0
        } else if(input$Q88 == 1) {
            0
        } else if(input$Q88 == 2) {
            2
        } else if(input$Q88 == 3) {
            2
        },
        if(input$Q98 == 0) {
            0
        } else if(input$Q98 == 1) {
            0
        } else if(input$Q98 == 2) {
            2
        } else if(input$Q98 == 3) {
            2
        },
        if(input$Q99 == 0) {
            0
        } else if(input$Q99 == 1) {
            0
        } else if(input$Q99 == 2) {
            1
        } else if(input$Q99 == 3) {
            2
        },
        if(input$Q101 == 0) {
            0
        } else if(input$Q101 == 1) {
            0
        } else if(input$Q101 == 2) {
            1
        } else if(input$Q101 == 3) {
            2
        },
        if(input$Q104 == 0) {
            0
        } else if(input$Q104 == 1) {
            0
        } else if(input$Q104 == 2) {
            1
        } else if(input$Q104 == 3) {
            2
        }
    ))), "</b>", "% chance of diagnosis of ADHD."))
    
    #Calculating t-scores based on norm data
    #Formula is 50 + (10*(Raw Score - Mean)/SD)
    
    #inattention
    output$IN <- renderPrint(cat(round(50 + 10 * ((sum(input$Q12, input$Q23, input$Q28, input$Q44, input$Q47,
                                                       input$Q49, input$Q67, input$Q77, input$Q88, input$Q95) - 
                                                       subset(parent, Age == years(input$dob, input$doe) & 
                                                                  Scale == "IN" & NormGrp == input$norms)[[3]]) /      #extract mean
                                                      subset(parent, Age == years(input$dob, input$doe) & 
                                                                 Scale == "IN" & NormGrp == input$norms)[[4]]))))      #extract sd
    #hyperactivity/impulsivity
    output$HY <- renderPrint(cat(round(50 + 10 * ((sum(input$Q19, input$Q43, input$Q45, input$Q50, input$Q52,
                                                       input$Q54, input$Q55, input$Q61, input$Q69, input$Q71,
                                                       input$Q93, input$Q98, input$Q99, input$Q104) - 
                                                       subset(parent, Age == years(input$dob, input$doe) & 
                                                                  Scale == "HY" & NormGrp == input$norms)[[3]]) /       #extract mean
                                                      subset(parent, Age == years(input$dob, input$doe) & 
                                                                 Scale == "HY" & NormGrp == input$norms)[[4]]))))       #extract sd
    #learning problems
    output$LP <- renderPrint(cat(round(50 + 10 * (sum(input$Q5, input$Q7, 
                                                      if(input$Q9 == 0){3} else if(input$Q9 == 1) {2} 
                                                      else if(input$Q9 == 2) {1} else {0},
                                                      input$Q15, input$Q36, input$Q51, input$Q53, input$Q60,
                                                      input$Q87) - 
                                                      subset(parent, Age == years(input$dob, input$doe) & 
                                                                 Scale == "LP" & NormGrp == input$norms)[[3]]) /       #extract mean
                                           subset(parent, Age == years(input$dob, input$doe) & 
                                                      Scale == "LP" & NormGrp == input$norms)[[4]])))                  #extract sd
    #executive functioning
    output$EF <- renderPrint(cat(round(50 + 10 * (sum(input$Q34, input$Q37, input$Q63,
                                                      if(input$Q72 == 0){3} else if(input$Q72 == 1) {2} 
                                                      else if(input$Q72 == 2) {1} else {0}, 
                                                      input$Q75, input$Q79, input$Q84, input$Q90, input$Q97) - 
                                                      subset(parent, Age == years(input$dob, input$doe) & 
                                                                 Scale == "EF" & NormGrp == input$norms)[[3]]) /        #extract mean
                                           subset(parent, Age == years(input$dob, input$doe) & 
                                                      Scale == "EF" & NormGrp == input$norms)[[4]])))                   #extract sd
    #defiance/aggression
    output$AG <- renderPrint(cat(round(50 + 10 * (sum(input$Q16, input$Q22, input$Q27, input$Q30, input$Q39,
                                                      input$Q46, input$Q48, input$Q57, input$Q58, input$Q65,
                                                      input$Q83, input$Q86, input$Q94, input$Q102) -
                                                      subset(parent, Age == years(input$dob, input$doe) & 
                                                                 Scale == "AG" & NormGrp == input$norms)[[3]]) /       #extract mean
                                           subset(parent, Age == years(input$dob, input$doe) & 
                                                      Scale == "AG" & NormGrp == input$norms)[[4]])))                  #extract sd
    #peer relations
    output$PR <- renderPrint(cat(round(50 + 10 * (sum(input$Q10, input$Q13, input$Q24, input$Q62,
                                                      if(input$Q64 == 0){3} else if(input$Q64 == 1) {2} 
                                                      else if(input$Q64 == 2) {1} else {0}, 
                                                      input$Q92) - 
                                                      subset(parent, Age == years(input$dob, input$doe) &  
                                                                 Scale == "PR" & NormGrp == input$norms)[[3]]) /        #extract mean
                                           subset(parent, Age == years(input$dob, input$doe) & 
                                                      Scale == "PR" & NormGrp == input$norms)[[4]])))                   #extract sd
    #conners 3 global index
    output$GI <- renderPrint(cat(round(50 + 10 * (sum(input$Q19, input$Q25, input$Q29, input$Q34, input$Q40,
                                                      input$Q50, input$Q67, input$Q81, input$Q85, input$Q99) - 
                                                      subset(parent, Age == years(input$dob, input$doe) & 
                                                                 Scale == "GI" & NormGrp == input$norms)[[3]]) /        #extract mean
                                           subset(parent, Age == years(input$dob, input$doe) & 
                                                      Scale == "GI" & NormGrp == input$norms)[[4]])))                   #extract sd
    #dsm-5 inattentive
    output$AN <- renderPrint(cat(round(50 + 10 * (sum(input$Q2, input$Q28, input$Q35, input$Q47, input$Q68,
                                                      input$Q79, input$Q84, input$Q95, input$Q97, input$Q101) -
                                                      subset(parent, Age == years(input$dob, input$doe) & 
                                                                 Scale == "AN" & NormGrp == input$norms)[[3]]) /        #extract mean
                                           subset(parent, Age == years(input$dob, input$doe) & 
                                                      Scale == "AN" & NormGrp == input$norms)[[4]])))                   #extract sd
    #dsm-5 hyperactive-impulsive
    output$AH <- renderPrint(cat(round (50 + 10 * (sum(input$Q3, input$Q43, input$Q45, input$Q54, input$Q61,
                                                       input$Q69, input$Q71, input$Q93, input$Q98, input$Q99,
                                                       input$Q104) -
                                                       subset(parent, Age == years(input$dob, input$doe) & 
                                                                  Scale == "AH" & NormGrp == input$norms)[[3]]) /         #extract mean
                                            subset(parent, Age == years(input$dob, input$doe) & 
                                                       Scale == "AH" & NormGrp == input$norms)[[4]])))                    #extract sd
    #dsm-5 conduct disorder
    output$CD <- renderPrint(cat(round(50 + 10 * (sum(input$Q6, input$Q11, input$Q16, input$Q27, input$Q30,
                                                      input$Q39, input$Q41, input$Q56, input$Q58, input$Q65,
                                                      input$Q76, input$Q78, input$Q89, input$Q91, input$Q96) - 
                                                      subset(parent, Age == years(input$dob, input$doe) & 
                                                                 Scale == "CD" & NormGrp == input$norms)[[3]]) /         #extract mean
                                           subset(parent, Age == years(input$dob, input$doe) & 
                                                      Scale == "CD" & NormGrp == input$norms)[[4]])))                    #extract sd
    #dsm-5 oppositional defiant disorder
    output$OD <- renderPrint(cat(round(50 + 10 * (sum(input$Q14, input$Q21, input$Q48, input$Q57, input$Q59,
                                                      input$Q73, input$Q94, input$Q102) - 
                                                      subset(parent, Age == years(input$dob, input$doe) & 
                                                                 Scale == "OD" & NormGrp == input$norms)[[3]]) /         #extract mean
                                           subset(parent, Age == years(input$dob, input$doe) & 
                                                      Scale == "OD" & NormGrp == input$norms)[[4]])))                    #extract sd
}

# Run the application 
shinyApp(ui = ui, server = server)
