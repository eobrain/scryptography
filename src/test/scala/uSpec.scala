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

    val x = U.ascii( "iiii" )
    x.text should equal ("iiii")
    x.n should equal (4*8)

  }

  it should "be created from a hex string" in {
    val x = U("6a6a6a")
    x.text should equal ("jjj")   // 6a is hex of character 'j'
    x.n should equal (3*8)
    x.toString should equal ("6a6a6a")
  }

  it should "be created from a very big value" in {
    val N = 8*1000*1000 // one million bytes
    val bytes = new Array[Byte](N/8)
    
    rnd nextBytes bytes
      
    val x = U(bytes)
    x.n           should equal(N) 
    x.text.length should equal(N/8)
    x.toString.length should equal (N/4)

  }

  it should "XOR with another U value" in {
    val blanks = U.ascii("           ")
    val x      = U.ascii("hello WORLD")
    val xor = blanks ^ x

    // XORing with blank toggles case 
    xor should equal (U.ascii("HELLO\000world"))

  }

  it should "be splittable into blocks" in {
    val x = U("00010203040506070809")
    ( x blocks 5*8) should equal( Seq(
      U("0001020304"),
      U("0506070809")) )
  }

  it should "be creatable by concatenating a sequence" in {
    val seq = Seq(
      U("0A0B0C"),
      U("0E0F"),
      U("101112131415"))
    U(seq) should equal( U("0A0B0C0E0F101112131415") )
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

  it should "be correctly zerod at the beginning" in {

    val x = U.ascii("\000\000jjj")
    x.n should equal (8*5)
    x.text should equal("°°jjj")
    x.toString should equal ("00006a6a6a")

  }
/*
  it should "be correctly padded at the beginning" in {

    val x = U.ascii("\000jjj")
    x.text should equal("»jjj")
    x.n should equal (8*5)

  }
  */ 
/*
  it should "be correctly zerod at the beginning" in {

    val x = U.ascii("\000\000jjj")
    x.n should equal (8*5)
    x.text should equal("°°jjj")

  }
*/
  /*it should "display control characters as '°'" in {

    val x = U.ascii("jj\007j")
    x.n should equal (8*4)
    x.text should equal("jj°j")

  }*/

  it should "handle byte with sign bit set " in {
    val x = U("f0")
    x.n        should equal(8)
    x.bytes    should equal( Array( 0xf0.asInstanceOf[Byte] ) )
    x.toString should equal("f0")
  }

  it should "handle large value with sign bit set " in {
    val hex = "f5d3d58503b9699de785895a96fdbaaf"
    val x = U(hex)
    hex.length should equal(32)
    x.n        should equal(128)
    x.toString should equal(hex)
  }

  "hex property" should "handle hex values beginning with zeros" in {
    val x = U(
      "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f")
    x.toString should equal(
      "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f")
  }

  "byte property" should "be correct length" in {
    val N = 80
    val bytes = new Array[Byte](N/8)
    
    for (_ <- 0 until 100) yield {
      rnd nextBytes bytes

      val x = U(bytes)
      x.n            should equal(N) 
      x.bytes.length should equal(N/8)
      x.toString.length should equal (N/4)
      
    }
  }

  /*it should "be correctly padded at the beginning" in {

    val x = U("00006a6a6a")
    x.n should equal (8*5)
    x.text should equal("»»jjj")
    x.bytes should equal("\000\000jjj" getBytes "ASCII")

  }*/

  it should "handle byte with sign bit set " in {
    val x = U("f0")
    x.n            should equal(8)
    x.bytes.length should equal(1)
    x.toString should equal ("f0")
  }

  it should "have 128-bit value with sign bit set" in {
    val hex = "ae2d8a571e03ac9c9eb76fac45af8e51"
    val x = U(hex)
    hex.length     should equal(32)
    x.n            should equal(128)
    x.bytes.length should equal(16)
    x.toString should equal ("ae2d8a571e03ac9c9eb76fac45af8e51")
  }


}

object USpec {
  val rnd = new Random
}
