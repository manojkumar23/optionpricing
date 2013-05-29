import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "openoptions"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
   //   "org.apache.commons" % "commons-math3" % "3.2",
      "net.gadgil" % "finance.portfolio_2.10" % "0.2.0-SNAPSHOT",
    jdbc,
    anorm
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here      
      //resolvers += "JBoss repository" at "https://repository.jboss.org/nexus/content/repositories/"
      resolvers += "Chetan's repository" at "https://github.com/cgadgil/optionpricing/tree/master/finance.portfolio/releases"
  
  )

}
