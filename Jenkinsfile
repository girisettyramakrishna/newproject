pipeline {
    agent any

    environment {
        R_HOME = '/usr/local/lib/R'  // Adjust if your R is in a different location
        PATH = "${env.PATH}:${R_HOME}/bin"
        R_ENVIRON_USER = "${WORKSPACE}/.Renviron"
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
                Rscript -e 'install.packages(c("rsconnect", "shiny", "ggplot2", "dplyr"), repos="https://cloud.r-project.org")'
                '''
            }
        }

        stage('Deploy to shinyapps.io') {
            steps {
                sh '''
                Rscript -e '
                    rsconnect::setAccountInfo(name = Sys.getenv("psmlabs"),
                                              token = Sys.getenv("8FAB74BDBB46C9CA05D75B7102711773"),
                                              secret = Sys.getenv("9a6AwJZn3QacJOlVCdzWv6Ptj34mOsuIRE3OuNDI"))
                    rsconnect::deployApp(".", appName = "demandforecasting", account = Sys.getenv("psmlabs"))
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

