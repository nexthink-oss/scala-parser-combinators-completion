node {
  stage('checkout') {
    git credentialsId: '354fe79e-2aec-4bf2-9341-61d42352ecb0', url: 'ssh://git@bitbucket.intra.nexthink.com:7999/nxs/scala-parser-combinators-completion.git'
  }

  stage('build'){
    withCredentials([usernamePassword(credentialsId: 'fbd2607e-27f3-4b53-aa36-66d6896d72d2', usernameVariable: 'USERNAME', passwordVariable: 'PASSWORD')]) {
      echo "My build number is: ${env.BUILD_NUMBER}"

      sh "${tool name: 'sbt-0.13.15', type: 'org.jvnet.hudson.plugins.SbtPluginBuilder$SbtInstallation'}/bin/sbt -Dsbt.log.noformat=true clean test publish -Pnexus.username=${USERNAME} -Pnexus.password=${PASSWORD} -PBUILD_NUMBER=${env.BUILD_NUMBER}"
    }
  }

  stage('publish'){
    junit allowEmptyResults: true, testResults: 'build/test-results/*.xml'
  }
}
