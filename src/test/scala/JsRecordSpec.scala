package jsrecord

import org.scalatest.{ FunSpec, Matchers }

import scala.scalajs.js

import shapeless._
import shapeless.record.Record
import shapeless.syntax.singleton._
import shapeless.test._

class JSRecordSpec extends FunSpec with Matchers {

  describe("apply(Record)") {
    it("should construct empty object for empty record") {
      val x = JSRecord[HNil](HNil)
      typed[JSRecord[Record.` `.T]](x)
      assertJsEq(x, js.Dynamic.literal())
    }
    it("should construct object of expected structure") {
      val x = JSRecord(
        'foo ->> 123 ::
          'bar ->> "hello" ::
          HNil
      )
      typed[JSRecord[Record.`'foo -> Int, 'bar -> String`.T]](x)
      assertJsEq(x, js.Dynamic.literal(foo = 123, bar = "hello"))
    }
    it("shouldn't add undefined fields") {
      val x = JSRecord(
        'foo ->> (123: js.UndefOr[Int]) ::
          'bar ->> (js.undefined: js.UndefOr[String]) ::
          HNil
      )
      typed[JSRecord[Record.`'foo -> js.UndefOr[Int], 'bar -> js.UndefOr[String]`.T]](x)
      assertJsEq(x, js.Dynamic.literal(foo = 123))
    }
  }

  // Check that two values have identical JS representation
  def assertJsEq(a: js.Any, b: js.Any): Unit = (a, b) match {
    case (a0: js.Object, b0: js.Object) =>
      assert(js.Object.getPrototypeOf(a0) == js.Object.getPrototypeOf(b0))
      val a = a0.asInstanceOf[js.Dictionary[js.Any]]
      val b = b0.asInstanceOf[js.Dictionary[js.Any]]
      assert(a.keys.toSet == b.keys.toSet)
      a.keys.foreach( k => assertJsEq(a(k), b(k)) )
    case _ => a == b
  }

  def debug(x: js.Any) = {
    js.Dynamic.global.console.log(
      js.Dynamic.global.JSON.stringify(
        x
      )
    )
  }
}
