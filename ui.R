############################################################
################### FRONT-END ##############################
###########################################################


# Define the main page of the App
fluidPage(

    # Application title
    titlePanel("RH PROJECT"),

        mainPanel(
            plotOutput("distPlot")
        )
    )

