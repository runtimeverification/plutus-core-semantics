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
        stage('Build')            { steps { sh 'make build -j4'   } }
        stage('Test kplutus-pyk') { steps { sh 'make kplutus-pyk' } }
        stage('Test') {
          failFast true
          options { timeout(time: 20, unit: 'MINUTES') }
          parallel {
            stage('proof tests')                   { steps { sh 'make test-prove -j4 --output-sync=recurse'                         } }
            stage('functional unit tests')         { steps { sh 'make test-unit-tests -j4 --output-sync=recurse'                    } }
            stage('simple')                        { steps { sh 'make test-simple -j4 --output-sync=recurse'                        } }
            stage('uplc-examples')                 { steps { sh 'make test-uplc-examples -j4 --output-sync=recurse'                 } }
            stage('benchmark-validation-examples') { steps { sh 'make test-benchmark-validation-examples -j4 --output-sync=recurse' } }
            stage('nofib-exe-examples')            { steps { sh 'make test-nofib-exe-examples -j4 --output-sync=recurse'            } }
            stage('flat')                          { steps { sh 'make test-flat -j4 --output-sync=recurse'                          } }
            stage('new-syntax')                    { steps { sh 'make test-new-syntax -j4 --output-sync=recurse'                    } }
            stage('error')                         { steps { sh 'make test-error -j4 --output-sync=recurse'                         } }
          }
        }
        stage('Test Interactive') {
          options { timeout(time: 20, unit: 'MINUTES') }
          steps {
          sh '''
            export PATH=$(pwd)/.build/usr/bin:$PATH
            kplc --help
            kplc --version
          '''
          }
        }
      }
    }
  }
}
