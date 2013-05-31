package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import net.gadgil.finance.portfolio._
import play.api.libs.json.Json

case class PortfolioInput(definition: String)

object Application extends Controller {

  val portfolioInputForm = Form(
    mapping("definition" -> text)(PortfolioInput.apply)(PortfolioInput.unapply))

  def index = Action {
    //portfolioInputForm.
    Ok(views.html.index("Enter portfolio definition", portfolioInputForm))
  }

  def simulate = Action { implicit request =>
    //val theData = Map("definition" -> "")
    portfolioInputForm.bindFromRequest.fold(hasErrors => {
      println("error: " + hasErrors)
      Ok(hasErrors.errorsAsJson)
    },
      success => {
        println("success")
        val y = PortfolioProcessor.simulatePortfolio(success.definition)
        val indexValues = y.map { p =>
        	//Map("date" -> Json.toJson(p.date), "index" -> Json.toJson(p.indexValue), "your portfolio" -> Json.toJson(p.positionValue))
          Map("label" -> Json.toJson(p.date), "y" -> Json.toJson(p.indexValue))
        }
        val positionValues = y.map { p =>
        	//Map("date" -> Json.toJson(p.date), "index" -> Json.toJson(p.indexValue), "your portfolio" -> Json.toJson(p.positionValue))
          Map("label" -> Json.toJson(p.date), "y" -> Json.toJson(p.positionValue))
        }
        //Ok(Json.toJson(y.map { dpi => dpi.positionValue }))
        val xxxx = Json.toJson(indexValues)
        val yyyy = Json.toJson(positionValues)
        //println(xxxx)
        Ok(views.html.portfolioPerformance("performance details", xxxx.toString, yyyy.toString))
        //Ok("")
      })
  }
}
