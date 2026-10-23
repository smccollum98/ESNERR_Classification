
# library(shiny)
# runExample("01_hello")


# 
# ui <- page_sidebar(
#   title = "title panel",
#   sidebar = sidebar("sidebar"),
#   "maincontents"
# )
# 
# server <- function(input, output){
#   
# }
# 
# shinyApp(ui = ui, server = server)

## Olfsson sample calculator

# U = users accuracy
# Wi = mapped proportion of area of class i
# Si = Std of stratum (sqrt(U*(1-U)))
# S(O) = Standard error of estimated overall accuracy to achieve

# n = ((SUM(Wi*Si))/)^2


## Create table of estimated area and estimate user accuracy for each class

t <- data.frame(name = c("unvegetated", "juicyPickleweed", "woodyPickleweed", "wetSediment"),
                    area = c(0.42, 0.07, 0.18, 0.33),
                    U = c(.94, .78, .85, .64))

targetErrors <- c(0.01, 0.015, 0.02, 0.03, 0.04, 0.05)

sampleCount <- lapply(targetErrors, function(e){
  
     ((sum(t %>% 
            mutate(s = sqrt((U*(1-U)))) %>% 
            mutate(ws = area*s) %>% 
            select(ws)))/e)^2
  
})

print(data.frame(cbind(targetErrors, sampleCount)))

## Then calculate number of samples for each class (min 50 for rare, proportional for rest.)



## Function that accepts a table
   #function is unnecessary
accuracySampleCount <- function(x){
  
  t <- x %>% 
    mutate(S = sqrt((U*(1-U)))) %>% 
    mutate(WS = area*S)
  
  sum <- sum(t$WS)
  
  n = (sum(t$WS)/targetError)^2
  
  print(n)
  # print((numerator[1]/targetError)^2)
  # numerator <- summarise(sum(WS))
  
}

accuracySampleCount(t)



