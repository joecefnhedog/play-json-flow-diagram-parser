package pages

trait Page

object Page {

  implicit def toString(page: Page): String =
    page.toString
}