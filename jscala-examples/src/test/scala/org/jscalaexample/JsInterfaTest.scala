package org.jscalaexample

import org.scalatest.FunSuite
import org.jscala._

/**
 * @author Trond Bjerkestrand
 */

class JsInterfaceTest extends FunSuite {

  object L extends JsInterface {
    val $ = new JsDynamic {}
  }
  val $ = new JsDynamic with JsTop
  val $$ = new JsTop {
    def t(x:Int) = x
  }

  test("L.$.test should produce $.test when L <:< JsInterface") {
    assert(javascript(L.$.test(2)).asString === "$.test(2)")
  }

  test("$.t.x should produce $.t.x when $ <:< JsTop") {
    assert(javascriptDebug($.t.x(2)).asString === "$.t.x(2)")
  }


  test("$$.t.x should produce $$.t.x when $ <:< JsTop") {
    assert(javascript($$.t(2)).asString === "$$.t(2)")
  }

  test("$.test(2) should produce $.test when $ <:< JsTop") {
    assert(javascript($.test(2)).asString === "$.test(2)")
  }

  test("$.test = 2 should produce $.test = 2 when $ <:< JsTop") {
    assert(javascript($.test = 2).asString === "$.test = 2")
  }

  test("""$("#test") = 2 should produce $("#.test") $ <:< JsTop""") {
    assert(javascriptDebug($("#test")).asString === """$("#test")""")
  }

}
