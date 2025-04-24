pipeline {
    agent any

    environment {
        R_HOME = '/usr/local/lib/R'  // Adjust if needed
        PATH = "${env.PATH}:${R_HOME}/bin"
        R_ENVIRON_USER = "${WORKSPACE}/.Renviron"

        // These should match Jenkins credentials (secret text or environment variables)
        SHINYAPPS_NAME = credentials('shinyapps_name')
        SHINYAPPS_TOKEN = credentials('shinyapps_token')
        SHINYAPPS_SECRET = credentials('shinyapps_secret')
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
                Rscript -e "
                    rsconnect::setAccountInfo(
                        name='${SHINYAPPS_NAME}',
                        token='${SHINYAPPS_TOKEN}',
                        secret='${SHINYAPPS_SECRET}'
                    )
                    rsconnect::deployApp('.', appName='demandforecasting', account='${SHINYAPPS_NAME}')
                "
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

