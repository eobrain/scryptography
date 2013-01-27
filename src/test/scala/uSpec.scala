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
    x.text should equal ("jjj")   // 6a is hex of character 'j'
    x.n should equal (3*8)

  }

  it should "be created from a very big value" in {
    val N = 8*1000*1000 // one million bytes
    val bytes = new Array[Byte](N/8)
    
    rnd nextBytes bytes
      
    val x = U(bytes)
    x.n           should equal(N) 
    x.text.length should equal(N/8)

  }

  it should "XOR with another U value" in {
    val blanks = U("           ".getBytes)
    val x      = U("hello WORLD".getBytes)
    val xor = blanks ^ x

    // XORing with blank toggles case 
    xor should equal (U("HELLO\000world".getBytes))

  }

  "text property" should "be correct length" in {
    val N = 80
    val bytes = new Array[Byte](N/8)
    
    for (_ <- 0 until 100) yield {
      rnd nextBytes bytes

      val x = U(bytes)
      x.n           should equal(N) 
      x.text.length should equal(N/8)
      
    }
  }

  it should "be correctly padded at the beginning" in {

    val x = U("\000\000jjj".getBytes)
    x.n should equal (8*5)
    x.text should equal("»»jjj")

  }

  it should "display control characters as '°'" in {

    val x = U("jj\007j".getBytes)
    x.n should equal (8*4)
    x.text should equal("jj°j")

  }

  "byte property" should "be correct length" in {
    val N = 80
    val bytes = new Array[Byte](N/8)
    
    for (_ <- 0 until 100) yield {
      rnd nextBytes bytes

      val x = U(bytes)
      x.n            should equal(N) 
      x.bytes.length should equal(N/8)
      
    }
  }

  it should "be correctly padded at the beginning" in {

    val x = U("00006a6a6a")
    x.n should equal (8*5)
    x.text should equal("»»jjj")
    x.bytes should equal("\000\000jjj".getBytes)

  }

}

object USpec {
  val rnd = new Random
}
