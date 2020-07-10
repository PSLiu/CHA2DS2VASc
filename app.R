library(shiny)

# Define UI 
ui <- fluidPage(
    
    # header
    h1("CHA2DS2-VASc Score計算機",
       style = "text-align: center"),
    
    # information
    h5(p("App作者: Peter Pin-Sung Liu"), 
       p("版本: 2020.05.17 v.0.1"),
       style = "text-align: right"),
    
    # tabset
    tabsetPanel(
        
        # tab - 1
        tabPanel(
            title = "計算機",
            h2(p("請輸入病人資訊"),style = "text-align: center"),
            radioButtons(
                inputId = "fsex",
                label = "請問您的性別是?",
                choices = c("男性" = 1, "女性" = 2),
                selected = 1,
                inline = TRUE
            ),
            radioButtons(
                inputId = "fage",
                label = "請問您今年幾歲",
                choices = c("64(含)以下" = 0, "65-74歲" = 1, "75歲(含)以上" = 2),
                selected = 0,
                inline = TRUE
            ),
            checkboxGroupInput(
                inputId = "fcom",
                label = "請問您是否有以下疾病史?",
                choices = c("中風(Stroke)" = 1, 
                            "心臟衰竭(Congestive heart failure, CHF)" = 2, 
                            "糖尿病(Diabetes Mellitus, DM)" = 3, 
                            "高血壓(Hypertension, HTN)" = 4,
                            "周邊動脈阻塞性疾病(PAOD)" = 5,
                            "心肌梗塞(Myocardial Infarction, MI)" = 6,
                            "動脈粥狀硬化(Aortic plaque)" = 7)
            ),
            h2(p("您的風險分數為", textOutput("cfrst", inline = TRUE), "分"))
            ),
        
        # tab - 2
        tabPanel(
            title = "研究文獻",
            h3(strong("Refining clinical risk stratification for predicting stroke and thromboembolism in atrial fibrillation using a novel risk factor-based approach: the euro heart survey on atrial fibrillation.")),
            h4(em("Lip GY, Nieuwlaat R, Pisters R, Lane DA, Crijns HJ.")),
            h5("Chest. 2010 Feb;137(2):263-72. doi: 10.1378/chest.09-1584. Epub 2009 Sep 17."),
            h5(a(href = "https://www.ncbi.nlm.nih.gov/pubmed/19762550", "Pubmed", target="_blank"))
            ),
        
        # tab - 3
        tabPanel(
            title = "關於", 
            h3("作者：", strong("劉品崧 Peter Pin-Sung Liu")),
            h4(strong("email"),"psliu520@gmail.com"),
            h4("statistician / programmer"),
            h4("coffee / volleyball / dogs / cats"),
            h3("Contact information"),
            h4(a(href = "https://www.facebook.com/liu.sung", img(src="facebook.png", width="100", height="100"), target="_blank"),
               a(href = "https://www.researchgate.net/profile/Peter_Liu9", img(src="ResearchGate.png", width="100", height="100"), target="_blank"),
               a(href = "https://github.com/PSLiu", img(src="GitHub.png", width="100", height="100"), target="_blank")
               )
            )
    )
)

# Define server logic 
server <- function(input, output) {
    
    # 計算分數，()是給function辨識用，{}是將多行指令的結果合併處理用
    output$cfrst <- renderText({
        ifelse(input$fsex == 2, 1, 0) + 
            as.numeric(input$fage) +
            ifelse(any(input$fcom %in% 1), 2, 0) +
            ifelse(any(input$fcom %in% 2), 1, 0) +
            ifelse(any(input$fcom %in% 3), 1, 0) +
            ifelse(any(input$fcom %in% 4), 1, 0) +
            ifelse(any(input$fcom %in% c(5, 6, 7)), 1, 0)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



# Peter Pin-Sung Liu
# 2020.07.09
# psliu520@gmail.com
# One more editor