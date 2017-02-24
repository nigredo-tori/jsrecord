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
      val res = jsEq(x, js.Dynamic.literal())
      assert(res)
    }
    it("should construct object of expected structure") {
      val x = JSRecord(
        'foo ->> 123 ::
          'bar ->> "hello" ::
          HNil
      )
      typed[JSRecord[Record.`'foo -> Int, 'bar -> String`.T]](x)
      val res = jsEq(x, js.Dynamic.literal(foo = 123, bar = "hello"))
      assert(res)
    }
  }

  def jsEq(a: js.Any, b: js.Any): Boolean = (a, b) match {
    case (a0: js.Object, b0: js.Object) =>
      val a = a0.asInstanceOf[js.Dictionary[js.Any]]
      val b = b0.asInstanceOf[js.Dictionary[js.Any]]
      a.keySet == b.keySet &&
        a.keys.forall( k => jsEq(a(k), b(k)) )
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
