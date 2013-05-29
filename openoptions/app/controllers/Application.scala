package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

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
      Ok(success.definition)
    })
  }
}
