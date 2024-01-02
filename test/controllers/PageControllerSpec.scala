package controllers

import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.test._
import play.api.test.Helpers._
import routing.AnswerWithRoute

class PageControllerSpec
    extends PlaySpec
    with GuiceOneAppPerTest
    with Injecting
    with CommonTestObjects {

  "PageController" should {

    "respond with a valid page when showing a valid page" in {
      val controller = inject[PageController]

      val result = controller.showPage(6)(FakeRequest())

      status(result) mustBe OK
      contentAsString(result) must include("Lamp&lt;br&gt;plugged in?")
    }

    "respond with NotFound when showing an invalid page" in {
      val controller = inject[PageController]

      val result = controller.showPage(999)(FakeRequest())

      status(result) mustBe NOT_FOUND
      contentAsString(result) must include(
        "IdNotFoundInDiagramNodes: The page with id: 999 was not found in the diagramNodes"
      )

    }

    "redirect to the next page when processing a valid answer" in {
      val controller = inject[PageController]

      val result = controller.processAnswer(6,Seq(ansYes,ansNo))(
        FakeRequest(POST, "/page/6/answer")
          .withFormUrlEncodedBody("answers[]" -> "Yes")
      )

      status(result) mustBe SEE_OTHER
      redirectLocation(result) mustBe Some(
        routes.PageController.showPage(10).url
      )
    }

    "respond with NotFound when processing an invalid answer" in {
      val controller = inject[PageController]

      val result = controller.processAnswer(6, Seq(ansInvalid))(
        FakeRequest(POST, "/page/6/answer")
          .withFormUrlEncodedBody("answers[]" -> "InvalidAnswer")
      )

      status(result) mustBe NOT_FOUND
    }

    "return 404 for non-existing page" in {
      val request = FakeRequest(GET, "/page/100")
      val result = route(app, request).get

      status(result) mustBe NOT_FOUND
    }

    "display a page with a question" in {
      val request = FakeRequest(GET, "/page/6")
      val result = route(app, request).get

      status(result) mustBe OK
      contentAsString(result) must include("Lamp&lt;br&gt;plugged in?")
    }

    "process a single answer and redirect to the next page" in {
      val request = FakeRequest(POST, "/page/6/answer")
        .withFormUrlEncodedBody("answers[]" -> "Yes")
      val result = route(app, request).get

      status(result) mustBe SEE_OTHER
      redirectLocation(result) mustBe Some("/page/10")
    }

    "process a single answer and display the final result page" in {
      val request = FakeRequest(POST, "/page/10/answer")
        .withFormUrlEncodedBody("answers[]" -> "Yes")

      val result = route(app, request).get

      status(result) mustBe SEE_OTHER
      redirectLocation(result) mustBe Some("/page/12")
    }

    "return 404 for non-existing answer" in {
      val request = FakeRequest(POST, "/page/6/answer")
        .withFormUrlEncodedBody("answers[]" -> "InvalidAnswer")
      val result = route(app, request).get

      status(result) mustBe NOT_FOUND
    }

    "return 404 for more than one answer selected" in {
      val request = FakeRequest(POST, "/page/6/answer")
        .withFormUrlEncodedBody("answers[]" -> "Yes", "answers[]" -> "No")
      val result = route(app, request).get

      status(result) mustBe NOT_FOUND
      contentAsString(result) must include(
        "MoreThanOneAnswersError: Did not find a single answer for id: 6. Found Yes,No"
      )
    }

    "return 404 for out of bounds id page" in {
      val request = FakeRequest(POST, "/page/99/answer")
        .withFormUrlEncodedBody("answers[]" -> "Yes")
      val result = route(app, request).get

      status(result) mustBe NOT_FOUND
      contentAsString(result) must include(
        "IdNotFoundInPageRoutingData: The page with id: 99 was not found in the PageRoutingAndQuestions"
      )
    }

  }
}

trait CommonTestObjects {
  val ansYes = AnswerWithRoute("Yes", 10)
  val ansNo = AnswerWithRoute("No", 6)
  val ansInvalid = AnswerWithRoute("InvalidAnswer", 600)
}
