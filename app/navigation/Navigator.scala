package navigation


import pages.Page
import play.api.mvc.Call
trait Navigator {
  private type RouteMapping = PartialFunction[Page, String => Option[Call]]

  protected def normalRoutes: RouteMapping

  private def handleCall(ua: String, call: String => Option[Call]) =
    call(ua) match {
      case Some(onwardRoute) => onwardRoute
      case None              => ???
    }

  def nextPage(page: Page, userAnswers: String): Call =
    normalRoutes.lift(page) match {
      case None       => ???
      case Some(call) => handleCall(userAnswers, call)
    }

}
