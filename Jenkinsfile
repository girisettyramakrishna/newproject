pipeline {
    agent any

    environment {
        R_HOME = '/usr/local/lib/R'                     // Adjust if your R is in a different location
        PATH = "${env.PATH}:${R_HOME}/bin"             // Make R available in PATH
        R_LIBS_USER = "${env.HOME}/Rlibs"              // Use user-writable library
        R_ENVIRON_USER = "${WORKSPACE}/.Renviron"      // Optional .Renviron if needed
    }

    stages {
        stage('Clone Repository') {
            steps {
                git 'https://github.com/girisettyramakrishna/newproject.git'
            }
        }

        stage('Install R Packages') {
            steps {
                sh '''
                mkdir -p "$R_LIBS_USER"
                Rscript -e 'install.packages(c("rsconnect", "shiny", "ggplot2", "dplyr"), repos="https://cloud.r-project.org")'
                '''
            }
        }

        stage('Deploy to shinyapps.io') {
            environment {
                SHINYAPPS_NAME = credentials('SHINYAPPS_NAME')     // Jenkins string credential (e.g., psmlabs)
                SHINYAPPS_TOKEN = credentials('SHINYAPPS_TOKEN')   // Jenkins secret text credential
                SHINYAPPS_SECRET = credentials('SHINYAPPS_SECRET') // Jenkins secret text credential
            }
            steps {
                sh '''
                Rscript -e '
                    rsconnect::setAccountInfo(
                        name = Sys.getenv("SHINYAPPS_NAME"),
                        token = Sys.getenv("SHINYAPPS_TOKEN"),
                        secret = Sys.getenv("SHINYAPPS_SECRET")
                    )
                    rsconnect::deployApp(".", appName = "demandforecasting", account = Sys.getenv("SHINYAPPS_NAME"))
                '
                '''
            }
        }
    }

    post {
        always {
            echo 'Pipeline finished.'
        }
        failure {
            echo 'Pipeline failed!'
        }
    }
}

