rm(list = ls())

options(repos = c(CRAN = "https://cran.rstudio.com/"))

install_and_load <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(paste("Menginstall package:", pkg, "...\n"))
    install.packages(pkg, quiet = FALSE)
    library(pkg, character.only = TRUE)
    cat(paste("✓ Package", pkg, "berhasil diinstall\n"))
  } else {
    cat(paste("✓ Package", pkg, "sudah terinstall\n"))
  }
}




install_and_load("shiny")
install_and_load("ggplot2")
install_and_load("dplyr")
install_and_load("readxl")


# Baca data
file_excel <- "tugas_r3/data_set_tugas3.xlsx"

if (file.exists(file_excel)) {
  data <- read_excel(file_excel, sheet = "weather")
  cat("✓ Data loaded:", nrow(data), "rows,", ncol(data), "columns\n")
} else {
  cat("File not found:", file_excel, "\n")
  cat("Working directory:", getwd(), "\n")
  cat("Available Excel files:", list.files(pattern = "\\.xlsx$"), "\n")
  stop("Pastikan file data_set_tugas3.xlsx berada di folder yang sama")
}

# Variabel
numeric_vars <- names(data)[sapply(data, is.numeric)]
categorical_vars <- names(data)[sapply(data, function(x) is.character(x) | is.factor(x))]

# UI
ui <- fluidPage(
  titlePanel("Analisis dan Visualisasi Data Cuaca"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_type", "Jenis Plot:",
                  choices = c("Scatter Plot" = "scatter",
                              "Line Plot" = "line",
                              "Bar Plot" = "bar",
                              "Tabel Data" = "table")),
      
      conditionalPanel(
        condition = "input.plot_type == 'scatter' || input.plot_type == 'line'",
        selectInput("x_var", "Variabel X:", choices = numeric_vars),
        selectInput("y_var", "Variabel Y:", choices = numeric_vars)
      ),
      
      conditionalPanel(
        condition = "input.plot_type == 'bar'",
        selectInput("cat_var", "Variabel Kategorik:", choices = categorical_vars),
        selectInput("bar_type", "Tipe Bar:",
                    choices = c("Frekuensi" = "count", "Rata-rata" = "mean")),
        conditionalPanel(
          condition = "input.bar_type == 'mean'",
          selectInput("num_bar", "Variabel Numerik:", choices = numeric_vars)
        )
      ),
      
      hr(),
      p("Jumlah data:", nrow(data)),
      p("Jumlah variabel:", ncol(data))
    ),
    
    mainPanel(
      h3(textOutput("title")),
      conditionalPanel(
        condition = "input.plot_type != 'table'",
        plotOutput("plot", height = "500px")
      ),
      conditionalPanel(
        condition = "input.plot_type == 'table'",
        tableOutput("table")
      ),
      hr(),
      h4("Ringkasan Statistik"),
      verbatimTextOutput("summary")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  output$title <- renderText({
    switch(input$plot_type,
           "scatter" = paste("Scatter Plot:", input$x_var, "vs", input$y_var),
           "line" = paste("Line Plot:", input$x_var, "vs", input$y_var),
           "bar" = paste("Bar Plot -", input$cat_var),
           "table" = "Tabel Data")
  })
  
  output$plot <- renderPlot({
    if(input$plot_type == "scatter") {
      ggplot(data, aes_string(x = input$x_var, y = input$y_var)) +
        geom_point(color = "steelblue", alpha = 0.6, size = 3) +
        geom_smooth(method = "lm", se = TRUE, color = "red") +
        theme_minimal()
    }
    else if(input$plot_type == "line") {
      data_sorted <- data %>% arrange(!!sym(input$x_var))
      ggplot(data_sorted, aes_string(x = input$x_var, y = input$y_var)) +
        geom_line(color = "steelblue", size = 1.2) +
        geom_point(color = "darkblue", size = 2) +
        theme_minimal()
    }
    else if(input$plot_type == "bar") {
      if(input$bar_type == "count") {
        plot_df <- data %>%
          group_by(!!sym(input$cat_var)) %>%
          summarise(Frekuensi = n(), .groups = 'drop')
        
        ggplot(plot_df, aes_string(x = input$cat_var, y = "Frekuensi", fill = input$cat_var)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = Frekuensi), vjust = -0.5) +
          theme_minimal() +
          theme(legend.position = "none")
      } else {
        plot_df <- data %>%
          group_by(!!sym(input$cat_var)) %>%
          summarise(Rata_rata = mean(!!sym(input$num_bar), na.rm = TRUE), .groups = 'drop')
        
        ggplot(plot_df, aes_string(x = input$cat_var, y = "Rata_rata", fill = input$cat_var)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = round(Rata_rata, 2)), vjust = -0.5) +
          theme_minimal() +
          theme(legend.position = "none")
      }
    }
  })
  
  output$table <- renderTable({
    head(data, 30)
  }, bordered = TRUE)
  
  output$summary <- renderPrint({
    cat("\n=== RINGKASAN STATISTIK ===\n\n")
    cat("Data:", nrow(data), "observasi,", ncol(data), "variabel\n\n")
    
    for(var in numeric_vars[1:min(5, length(numeric_vars))]) {
      cat(var, ":\n")
      cat("  Min  :", min(data[[var]], na.rm=TRUE), "\n")
      cat("  Mean :", round(mean(data[[var]], na.rm=TRUE), 2), "\n")
      cat("  Max  :", max(data[[var]], na.rm=TRUE), "\n")
      cat("  NA   :", sum(is.na(data[[var]])), "\n\n")
    }
    
    if(length(categorical_vars) > 0) {
      cat("Distribusi Kategorik:\n")
      for(var in categorical_vars[1:min(3, length(categorical_vars))]) {
        cat("\n", var, ":\n")
        print(table(data[[var]], useNA = "ifany"))
      }
    }
  })
}

# Jalankan aplikasi
cat("\nMenjalankan aplikasi Shiny...\n")
shinyApp(ui = ui, server = server)