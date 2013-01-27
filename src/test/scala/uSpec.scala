/*
 * This file is part of the scryptography
 *
 * Copyright (c) Eamonn O'Brien-Strain All rights
 * reserved. This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License v1.0 which
 * accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    Eamonn O'Brien-Strain  e@obrain.com - initial author
 */

import org.eamonn.crypto.U

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.util.Random
import USpec._

/** Test the U class */
class USpec extends FlatSpec with ShouldMatchers {


  "A U value" should "be created from a byte array" in {

    val x = U( "iiii".getBytes )
    x.text should equal ("iiii")
    x.n should equal (4*8)

  }

  it should "be created from a hex string" in {
    val x = U("6a6a6a")
    x.text should equal ("jjj")   // 6a is hex of character 'i'
    x.n should equal (3*8)

  }

  it should "support XOR" in {
    val blanks = U("           ".getBytes)
    val x      = U("hello WORLD".getBytes)
    val xor = blanks ^ x

    // XORing with blank toggles case 
    xor should equal (U("HELLO\000world".getBytes))

  }


  it should "allow for very big values" in {
    val N = 8*1000*1000 // one million bytes
    val bytes = new Array[Byte](N/8)
    
    rnd nextBytes bytes
      
    val x = U(bytes)
    x.n           should equal(N) 
    x.text.length should equal(N/8)

  }


  it should "convert text to the correct length" in {
    val N = 80
    val bytes = new Array[Byte](N/8)
    
    for (_ <- 0 until 100) yield {
      rnd nextBytes bytes

      val x = U(bytes)
      x.n           should equal(N) 
      x.text.length should equal(N/8)
      
    }
  }

}

object USpec {
  val rnd = new Random
}
