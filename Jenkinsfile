pipeline {
  agent { label 'docker' }
  environment {
    VERSION   = '0.1.0'
    LONG_REV  = """${sh(returnStdout: true, script: 'git rev-parse HEAD').trim()}"""
    SHORT_REV = """${sh(returnStdout: true, script: 'git rev-parse --short=7 HEAD').trim()}"""
    K_VERSION = """${sh(returnStdout: true, script: 'cd deps/k && git tag --points-at HEAD | cut --characters=2-').trim()}"""
  }
  options { ansiColor('xterm') }
  stages {
    stage('Init title') {
      when { changeRequest() }
      steps { script { currentBuild.displayName = "PR ${env.CHANGE_ID}: ${env.CHANGE_TITLE}" } }
    }
    stage('Build and Test') {
      agent {
        dockerfile {
          additionalBuildArgs '--build-arg K_COMMIT="${K_VERSION}" --build-arg USER_ID=$(id -u) --build-arg GROUP_ID=$(id -g)'
          reuseNode true
        }
      }
      stages {
        stage('Build') { steps { sh 'make build' } }
        stage('Test') {
          failFast true
          options { timeout(time: 20, unit: 'MINUTES') }
          parallel {
            stage('Conformance') { steps { sh 'echo test-conformance' } }
          }
        }
        stage('Test Interactive') {
          options { timeout(time: 20, unit: 'MINUTES') }
          steps {
          sh '''
            export PATH=$(pwd)/.build/usr/bin:$PATH
            kplutus --help
            kplutus --version
          '''
          }
        }
      }
    }
  }
}
