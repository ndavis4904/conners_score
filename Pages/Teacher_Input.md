---
title: Conners Teacher Form
output:
  git_document:
    code_folding: hide
    theme: cerulean
runtime: shiny
---

```{r, include = FALSE}
library(shiny)
library(readxl)
library(ggplot2)
library(here)


teacher <- read_excel(here("Data/Conners_Teacher_Updated.xlsx"))

years <- function(first.date, second.date) {
  floor((second.date - first.date)/365)[[1]]
}
months <- function(first.date, second.date) {
  temp.hold <- (second.date - first.date)[[1]]
  temp.age <- temp.hold/365
  floor((temp.age-floor(temp.age))*12)
}

```

### Student Information

```{r}
dateInput("dob", label = "Student's Date of Birth")

dateInput("doe", label = "Date of Evaluation")

radioButtons("norms", label = "Which Norm Table?",
             choices = c("Male", "Female", "Total"))

year.old <- reactive(years(input$dob, input$doe))
month.old <- reactive(months(input$dob, input$doe))

```

`r year.old` Years, and `r month.old` Months


### Data Input

```{r}
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
  numericInput("Q108", label = "Question 108", value = 0),
  numericInput("Q109", label = "Question 109", value = 0),
  numericInput("Q110", label = "Question 110", value = 0),
  numericInput("Q111", label = "Question 111", value = 0),
  numericInput("Q112", label = "Question 112", value = 0),
  numericInput("Q113", label = "Question 113", value = 0)
)
```

### Output

##### Validity Check

```{r}
#Visual Representation of critical differences
renderPlot(qplot(c(abs(diff(c(input$Q97, input$Q100))), abs(diff(c(input$Q52, input$Q63))), 
              abs(diff(c(input$Q4, input$Q77))), abs(diff(c(input$Q7, input$Q13))),
              abs(diff(c(input$Q26, input$Q29))), abs(diff(c(input$Q98, input$Q105))),
              abs(diff(c(input$Q25, input$Q57))), abs(diff(c(input$Q23, input$Q44))),
              abs(diff(c(input$Q34, input$Q89))), abs(diff(c(input$Q47, input$Q71))))) +
             geom_bar(fill = "skyblue", color = "black", width = 1) +
             theme(panel.background = element_blank()) +
             labs(x = "") +
             geom_hline(yintercept = 2, col = "firebrick", size = 1.5) +
             scale_x_continuous(breaks = c(0, 1, 2, 3)))

#Numeric representation of critical differences
differences <- reactive(sum(c(abs(diff(c(input$Q97, input$Q100))), abs(diff(c(input$Q52, input$Q63))), 
              abs(diff(c(input$Q4, input$Q77))), abs(diff(c(input$Q7, input$Q13))),
              abs(diff(c(input$Q26, input$Q29))), abs(diff(c(input$Q98, input$Q105))),
              abs(diff(c(input$Q25, input$Q57))), abs(diff(c(input$Q23, input$Q44))),
              abs(diff(c(input$Q34, input$Q89))), abs(diff(c(input$Q47, input$Q71))))))


#Positive and negative answering pattern data
posimp <- reactive(sum(ifelse(input$Q15 == 3, 1, 0),
                       ifelse(input$Q28 == 3, 1, 0),
                       ifelse(input$Q30 == 0, 1, 0),
                       ifelse(input$Q36 == 0, 1, 0),
                       ifelse(input$Q55 == 3, 1, 0),
                       ifelse(input$Q109 == 3, 1, 0)))
negimp <- reactive(sum(ifelse(input$Q5 > 1, 1, 0),
                       ifelse(input$Q81 < 2, 1, 0),
                       ifelse(input$Q93 > 1, 1, 0),
                       ifelse(input$Q101 > 1, 1, 0),
                       ifelse(input$Q107 < 2, 1, 0),
                       ifelse(input$Q110 > 1, 1, 0)))

```

$\text{Note: If the total number of differences of 2s and 3s (seen above in the graph) are } \geq \text{ 2, AND }$ `r differences` $\geq \text{ 6, there may be a question of validity.}$


**Positive Impression:**

Total Positive Impressions is `r posimp`. 

If `r posimp` is $\geq$ 5, there is a possible positive response style indicated.


**Negative Impression:**

Total Negative Impressions is `r negimp`. 

If `r negimp` is $\geq$ 5, there is a possible negative response style indicated.


##### T-Score Calculations

```{r}
#Sum of questions relating to Inattention
inattention <- renderPrint(round(50 + 10 * ((sum(input$Q3, input$Q37, input$Q41, input$Q44, input$Q60,
                            input$Q86, input$Q97, input$Q100, input$Q108, input$Q111) - 
                              subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "IN" & NormGrp == input$norms)[[3]]) / 
                              subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "IN" & NormGrp == input$norms)[[4]])))

#Sum of questions relating to Hyperactivity/Impulsivity
hyper <- renderPrint(round(50 + 10 * ((sum(input$Q1, input$Q2, input$Q4, input$Q7, input$Q9, input$Q13, input$Q17,
                      input$Q24, input$Q26, input$Q29, input$Q32, input$Q39, input$Q50, input$Q76,
                      input$Q77, input$Q78, input$Q83, input$Q91) - 
                        subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "HY" & NormGrp == input$norms)[[3]]) / 
                              subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "HY" & NormGrp == input$norms)[[4]])))

#Sum of questions relating to Learning Problems/Executive Functioning composite
learningExec <- renderPrint(round(50 + 10 * (sum(input$Q6, input$Q11, input$Q12, input$Q16, input$Q18,
                                                 input$Q20, input$Q25, input$Q45,
                             input$Q52, input$Q63, input$Q65, 
                             if(input$Q66 == 0) {
                               3
                             } else if(input$Q66 == 1){
                               2
                             } else if(input$Q66 == 2) {
                               1
                             } else {
                               0
                             }, input$Q72, input$Q94, input$Q99, input$Q106) -
                               subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "LE" & NormGrp == input$norms)[[3]]) / 
                              subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "LE" & NormGrp == input$norms)[[4]]))

#Sum of questions relating to Learning Problems
learning <- renderPrint(round(50 + 10 * (sum(input$Q12, input$Q45, input$Q52, input$Q63, input$Q65, input$Q72) - 
                                           subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "LP" & NormGrp == input$norms)[[3]]) / 
                              subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "LP" & NormGrp == input$norms)[[4]]))

#Sum of questions relating to Executive Functioning
exec <- renderPrint(round(50 + 10 * (sum(input$Q6, input$Q11, input$Q16, input$Q20, input$Q25, 
                             if(input$Q66 == 0) {
                               3
                             } else if(input$Q66 == 1){
                               2
                             } else if(input$Q66 == 2) {
                               1
                             } else {
                               0
                             }, input$Q106) - 
                      subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "EF" & NormGrp == input$norms)[[3]]) / 
                              subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "EF" & NormGrp == input$norms)[[4]]))

#Sum of questions relating to Defiance/Aggression
agg <- renderPrint(round(50 + 10 * (sum(input$Q8, input$Q10, input$Q35, input$Q38, input$Q40, input$Q43,
                                        input$Q47, input$Q51,input$Q59, input$Q62, input$Q68, input$Q70,
                                        input$Q71, input$Q85, input$Q96, input$Q98, input$Q102, input$Q105) -
                                      subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "AG" & NormGrp == input$norms)[[3]]) / 
                              subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "AG" & NormGrp == input$norms)[[4]]))

#Sum of questions relating to Peer Relations
peer <- renderPrint(round(50 + 10 * (sum(input$Q19, input$Q34, input$Q42, if(input$Q74 == 0) {3} 
                                         else if(input$Q74 == 1) {2}
                        else if(input$Q74 == 2) {1} else {0}, input$Q80, input$Q89, input$Q104) - 
                          subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "PR" & NormGrp == input$norms)[[3]]) / 
                              subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "PR" & NormGrp == input$norms)[[4]]))

#Sum of questions relating to the Conners 3 Global Index
globin <- renderPrint(round(50 + 10 * (sum(input$Q7, input$Q13, input$Q22, input$Q25, input$Q46, 
                                           input$Q48, input$Q75, input$Q77,
                          input$Q84, input$Q100) - 
                            subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "GI" & NormGrp == input$norms)[[3]]) / 
                              subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "GI" & NormGrp == input$norms)[[4]]))

#Sum of questions relating to ADHD Inattentive according to the DSM-V
dsm_in <- renderPrint(round(50 + 10 * (sum(input$Q23, input$Q37, input$Q57, input$Q60, input$Q69, 
                                           input$Q73, input$Q88, input$Q92,
                          input$Q103, input$Q111) -
                            subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "AN" & NormGrp == input$norms)[[3]]) / 
                              subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "AN" & NormGrp == input$norms)[[4]]))

#Sum of questions relating to Hyperactive/Impulsive according to the DSM-V
dsm_hy <- renderPrint(round (50 + 10 * (sum(input$Q1, input$Q4, input$Q7, input$Q9, input$Q17, input$Q24, 
                                            input$Q29, input$Q32,
                          input$Q50, input$Q76, input$Q78) -
                            subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "AH" & NormGrp == input$norms)[[3]]) / 
                              subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "AH" & NormGrp == input$norms)[[4]]))

#Sum of questions relating to Conduct Disorder according to the DSM-V
dsm_cd <- renderPrint(round(50 + 10 * (sum(input$Q10, input$Q14, input$Q21, input$Q27, input$Q31, 
                                           input$Q33, input$Q35, input$Q40,
                          input$Q54, input$Q61, input$Q90, input$Q98, input$Q105) - 
                            subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "CD" & NormGrp == input$norms)[[3]]) / 
                              subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "CD" & NormGrp == input$norms)[[4]]))

#Sum of questions relating to Oppositional Defiant Disorder according to the DSM-V
dsm_od <- renderPrint(round(50 + 10 * (sum(input$Q38, input$Q47, input$Q51, input$Q56, input$Q59, 
                                           input$Q62, input$Q64, input$Q71) - 
                                         subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "OD" & NormGrp == input$norms)[[3]]) / 
                              subset(teacher, Age == years(input$dob, input$doe) & 
                                       Scale == "OD" & NormGrp == input$norms)[[4]]))

```


| IN | HY | LE | LP | EF | AG | PR | GI | AN | AH | CD | OD |
|----------------|-----------|------------------|--------------|----------|---------|----------|------------|-----------------|------------|------------|------------|
|`r inattention` | `r hyper` | `r learningExec` | `r learning` | `r exec` | `r agg` | `r peer` | `r globin` | `r dsm_in` | `r dsm_hy` | `r dsm_cd` | `r dsm_od` |


### DSM-5 Symptom Counts

```{r}
#Determining which threshold to use according to age
symp.in <- reactive(if(years(input$dob, input$doe) > 16) {
  5
} else {
  6
})

symp.hy <- reactive(if(years(input$dob, input$doe) > 16) {
  5
} else {
  6
})
```

##### ADHD Inattention

```{r}
adhd.in <- reactive(sum(c(
  ifelse(input$Q37 > 1,TRUE,FALSE),
  ifelse(input$Q111 > 1,TRUE,FALSE),
  ifelse(input$Q69 > 1,TRUE,FALSE),
  ifelse(input$Q73 + input$Q57 > 3,TRUE,FALSE),
  ifelse(input$Q103 > 1,TRUE,FALSE),
  ifelse(input$Q60 > 1,TRUE,FALSE),
  ifelse(input$Q92 > 1,TRUE,FALSE),
  ifelse(input$Q23 > 1,TRUE,FALSE),
  ifelse(input$Q88 > 1,TRUE,FALSE)
)))
```

Symptom Criteria met if Total Symptom Count $\geq$ `r symp.in`

`r adhd.in`

##### ADHD Hyperactive/Impulsive

```{r}
adhd.hy <- reactive(sum(c(
  ifelse(input$Q4 > 1,TRUE,FALSE),
  ifelse(input$Q1 > 1,TRUE,FALSE),
  ifelse(input$Q24 + input$Q7 > 0,TRUE,FALSE),
  ifelse(input$Q32 > 1,TRUE,FALSE),
  ifelse(input$Q17 + input$Q78 > 0,TRUE,FALSE),
  ifelse(input$Q50 > 1,TRUE,FALSE),
  ifelse(input$Q9 > 1,TRUE,FALSE),
  ifelse(input$Q76 > 1,TRUE,FALSE),
  ifelse(input$Q29 > 1, TRUE, FALSE)
)))
```

Symptom Criteria met if Total Symptom Count $\geq$ `r symp.hy`

`r adhd.hy`

##### Conduct Disorder

```{r}
con.dis <- reactive(sum(c(
  ifelse(input$Q98 > 0,TRUE,FALSE),
  ifelse(input$Q105 > 0,TRUE,FALSE),
  ifelse(input$Q14 > 0,TRUE,FALSE),
  ifelse(input$Q35 > 0,TRUE,FALSE),
  ifelse(input$Q21 > 0,TRUE,FALSE),
  ifelse(input$Q27 > 0,TRUE,FALSE),
  ifelse(input$Q33 > 0,TRUE,FALSE),
  ifelse(input$Q61 > 0,TRUE,FALSE),
  ifelse(input$Q10 > 0, TRUE, FALSE),
  ifelse(input$Q90 > 0,TRUE,FALSE),
  ifelse(input$Q40 > 1,TRUE,FALSE),
  ifelse(input$Q31 > 0,TRUE,FALSE),
  ifelse(input$Q54 > 0,TRUE,FALSE)
)))
```

Symptom Criteria met if Total Symptom Count $\geq$ 4

`r con.dis`

##### Oppositional Defiant Disorder

```{r}
opp.def <- reactive(sum(c(
  ifelse(input$Q62 > 0,TRUE,FALSE),
  ifelse(input$Q56 > 1,TRUE,FALSE),
  ifelse(input$Q38 > 0,TRUE,FALSE),
  ifelse(input$Q47 > 1,TRUE,FALSE),
  ifelse(input$Q71 > 0,TRUE,FALSE),
  ifelse(input$Q59 > 0,TRUE,FALSE),
  ifelse(input$Q64 > 0,TRUE,FALSE),
  ifelse(input$Q51 > 0,TRUE,FALSE)
)))
```

Symptom Criteria met if Total Symptom Count $\geq$ 4

`r opp.def`

##### Conners 3 ADHD Index

```{r}
#Summing scores related to probability of ADHD identification
converted <- reactive(sum(c(
  if(input$Q4 == 0) {
    0
  } else if(input$Q4 == 1) {
    0
  } else if(input$Q4 == 2) {
    1
  } else if(input$Q4 == 3) {
    2
  },
  if(input$Q7 == 0) {
    0
  } else if(input$Q7 == 1) {
    0
  } else if(input$Q7 == 2) {
    1
  } else if(input$Q7 == 3) {
    2
  },
  if(input$Q13 == 0) {
    0
  } else if(input$Q13 == 1) {
    0
  } else if(input$Q13 == 2) {
    1
  } else if(input$Q13 == 3) {
    2
  },
  if(input$Q23 == 0) {
    0
  } else if(input$Q23 == 1) {
    0
  } else if(input$Q23 == 2) {
    1
  } else if(input$Q23 == 3) {
    2
  },
  if(input$Q44 == 0) {
    0
  } else if(input$Q44 == 1) {
    0
  } else if(input$Q44 == 2) {
    1
  } else if(input$Q44 == 3) {
    2
  },
  if(input$Q57 == 0) {
    0
  } else if(input$Q57 == 1) {
    0
  } else if(input$Q57 == 2) {
    1
  } else if(input$Q57 == 3) {
    2
  },
  if(input$Q60 == 0) {
    0
  } else if(input$Q60 == 1) {
    0
  } else if(input$Q60 == 2) {
    1
  } else if(input$Q60 == 3) {
    2
  },
  if(input$Q69 == 0) {
    0
  } else if(input$Q69 == 1) {
    0
  } else if(input$Q69 == 2) {
    1
  } else if(input$Q69 == 3) {
    2
  },
  if(input$Q97 == 0) {
    0
  } else if(input$Q97 == 1) {
    0
  } else if(input$Q97 == 2) {
    1
  } else if(input$Q97 == 3) {
    2
  },
  if(input$Q100 == 0) {
    0
  } else if(input$Q100 == 1) {
    0
  } else if(input$Q100 == 2) {
    1
  } else if(input$Q100 == 3) {
    2
  }
)))



```

Probability of a classification of ADHD

*The sum is reported in the upper left of the table. You'll need to find the corresponding score across the top and locate the probability of an ADHD diagnosis underneath.*

| Raw Probability Score: `r converted` | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 |
|--------------------------------------|---|---|---|---|---|---|---|---|---|---|----|----|----|----|----|----|----|---|----|----|----|
|        Probability of ADHD           | 19 | 39 | 51 | 52 | 58 | 64 | 69 | 73 | 77 | 81 | 84 | 87 | 89 | 91 | 92 | 93 | 95 | 96 | 97 | 98 | 99 |