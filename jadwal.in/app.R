# ==================================================
# JADWAL.in
# dibuat oleh ikanx101.com
# ==================================================

# dimulai dari hati yang suci
rm(list=ls())

library(ggplot2)
library(dplyr)
library(shinydashboard)
library(shiny)
library(shinymanager)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

# credentials
credentials <- data.frame(
    user = c("ikanx_server", "sains"), # mandatory
    password = c("ahnaf", "komputasi"), # mandatory
    admin = c(TRUE, TRUE),
    comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
    stringsAsFactors = FALSE
)

# data base hari
sel_hari = c(5,10,15,20)

# ==================================================
# user interface
# header
header = dashboardHeader(title = "JADWAL.in",
                         titleWidth = 200)

# sidebar
sidebar = dashboardSidebar(width = 200,
                           sidebarMenu(
                               menuItem(tabName = 'program',
                                        text = 'Buat Jadwal',icon = icon('database'),
                                        badgeLabel = "New!", badgeColor = "blue")
                               )
                           )

# panel perolehan responden
panel1 = tabItem(tabName = 'program',
                 fluidRow(
                   box(width = 6,
                       fluidRow(
                         box(width = 6,
                             sliderInput("siswa", "Banyak Siswa:",
                                         min = 1, max = 30, value = 25)
                         ),
                         box(width = 6,
                             sliderInput("capacity", 
                                         "Kapasitas Kelas:",
                                         min = 1, 
                                         max = 30, 
                                         value = c(5,10)
                             )
                       )),
                       fluidRow(
                         box(width = 6,
                             sliderInput("frek", 
                                         "Frekuensi kunjungan siswa per periode:",
                                         min = 1, 
                                         max = 15, 
                                         value = c(4,12)
                             )
                         ),
                         box(width = 6,
                             radioButtons("hari",
                                          "Pilih periode penjadwalan: (dalam hari)",
                                          sel_hari,
                                          selected = sel_hari[4])
                         )
                       )
                       ),
                 box(width = 6,
                     h1("JADWAL.in"),
                     h3("Merupakan web apps yang digunakan guru atau administrasi sekolah untuk membuat jadwal kunjungan siswa pada masa Pembelajaran Tatap Muka Terbatas pandemi Covid 19."),
                     br(),
                     h3("Silakan mengisi sesuai dengan kondisi di sekolah Anda."),
                     br(),
                     h4("Catatan: Siswa diberikan jeda sehari untuk bisa masuk ke sekolah sejak kedatangannya di suatu hari tertentu.")
                     )
                 ),
                 fluidRow(
                   column(width = 3,
                          textOutput("stat"))
                 )
)
        

# gabung
body = dashboardBody(tabItems(panel1))

# make ui happens
ui = secure_app(dashboardPage(skin = "black",header,sidebar,body))

# server
server <- function(input, output, session) {
    
    # call the server part
    # check_credentials returns a function to authenticate users
    res_auth <- secure_server(
        check_credentials = check_credentials(credentials)
    )
    
    # n siswa
    n_sis = reactive({
      input$siswa
    })  
    # banyak hari
    n_har = reactive({
      input$hari
    })   
    # max kelas
    max_cap = reactive({
      max(input$capacity)
    })
    # min kelas
    min_cap = reactive({
      min(input$capacity)
    })
    # min frek
    max_frek = reactive({
      max(input$frek)
    })
    # max frek
    min_frek = reactive({
      min(input$frek)
    })
    

    # process
    model_sol = reactive({
      n_sis = n_sis() %>% as.numeric()
      n_har = n_har() %>% as.numeric()
      min_cap = min_cap() %>% as.numeric()
      max_cap = max_cap() %>% as.numeric()
      min_frek = min_frek() %>% as.numeric()
      max_frek = max_frek() %>% as.numeric()
      
      hasil = 
        MIPModel() %>%
        # menambah variabel
        add_variable(x[i,j],
                     i = 1:n_sis,
                     j = 1:n_har,
                     type = "binary",
                     lb = 0) %>%
        # membuat objective function
        set_objective(sum_expr(x[i,j],
                               i = 1:n_sis,
                               j = 1:n_har),
                      "max") %>%
        # menambah constraints
        # max kapasitas kelas
        add_constraint(sum_expr(x[i,j],i = 1:n_sis) >= min_cap,
                       j = 1:n_har) %>%
        add_constraint(sum_expr(x[i,j],i = 1:n_sis) <= max_cap,
                       j = 1:n_har) %>%
        # frek kunjungan siswa
        add_constraint(sum_expr(x[i,j],j = 1:n_har) >= min_frek,
                       i = 1:n_sis) %>%
        add_constraint(sum_expr(x[i,j],j = 1:n_har) <= max_frek,
                       i = 1:n_sis) %>%
        # jeda sehari
        add_constraint(x[i,j] + x[i,j+1] <= 1,
                       i = 1:n_sis,
                       j = 1:(n_har-1)) %>%
        solve_model(with_ROI(solver = "glpk",
                             verbose = T))
      
      return(hasil)
      
    })
    
    output$stat = renderText({
      model_sol()$status %>% print()
    })
    
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
