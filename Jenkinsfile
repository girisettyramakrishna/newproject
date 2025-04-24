pipeline {
    agent any

    environment {
        R_HOME = "/usr/local/lib64/R" // Confirm this with `R RHOME` as you did
        R_LIBS_USER = "${HOME}/R/x86_64-pc-linux-gnu-library/4.3"
    }

    stages {
        stage('Clone Repository') {
            steps {
                git url: 'https://github.com/girisettyramakrishna/newproject.git'
            }
        }

        stage('Install R Packages') {
            steps {
                sh '''
                    Rscript -e "packages <- c('shiny', 'rsconnect', 'curl'); new.packages <- packages[!(packages %in% installed.packages())]; if(length(new.packages)) install.packages(new.packages, repos='https://cloud.r-project.org/')"
                '''
            }
        }

        stage('Test Shiny App Launch') {
            steps {
                sh '''
                    Rscript -e "shiny::runApp('.', port=8081, launch.browser=FALSE)" & \
                    sleep 5 && curl -f http://localhost:8081 || (echo 'App did not start properly'; exit 1)
                '''
            }
        }

        stage('Deploy to shinyapps.io') {
            steps {
                sh '''
                    Rscript -e "rsconnect::setAccountInfo(name='psmlabs', token='8FAB74BDBB46C9CA05D75B7102711773', secret='9a6AwJZn3QacJOlVCdzWv6Ptj34mOsuIRE3OuNDI')"
                    Rscript -e "rsconnect::deployApp(appDir='.', appName='DF_v1')"
                '''
            }
        }
    }

    post {
        success {
            echo ' Deployment successful to shinyapps.io!'
        }
        failure {
            echo ' Pipeline failed. Check logs for more details.'
        }
    }
}

