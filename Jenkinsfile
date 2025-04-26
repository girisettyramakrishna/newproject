pipeline {
    agent any

    environment {
        REMOTE_USER = 'deployuser'
        REMOTE_HOST = '54.227.205.64'
        REMOTE_DIR  = '/opt/shinyapps/DF_v1'
    }

    stages {
        stage('Clone Repo') {
            steps {
                git 'https://github.com/your-username/DF_v1.git'
            }
        }

        stage('Copy Files to Deployment Server') {
            steps {
                sh '''
                scp -r . ${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_DIR}
                '''
            }
        }
    }
}
