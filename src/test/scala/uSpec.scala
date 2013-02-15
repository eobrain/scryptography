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

import scala.util.Random
import USpec._
import org.specs2.mutable.Specification

/** Test the U class */
class USpec extends Specification {


  "A U value" should {
    "be created from a byte array" in {
      
      val x = U.ascii( "iiii" )
      x.ascii === "iiii"
      x.n     ===  4*8
      
    }

    "be created from a hex string" in {
      val x = U("6a6a6a")
      x.ascii    === "jjj"   // 6a is hex of character 'j'
      x.n        ===  3*8
      x.toString === "6a6a6a"
    }

    "be created from a very big value" in {
      val N = 8*1000*1000 // one million bytes
      val bytes = new Array[Byte](N/8)
      
      rnd nextBytes bytes
      
      val x = U(bytes)
      x.n               === N 
      x.ascii.length    === N/8
      x.toString.length === N/4
      
    }

    "XOR with another U value" in {
      val blanks = U.ascii("           ")
      val x      = U.ascii("hello WORLD")
      val xor = blanks ^ x
      
      // XORing with blank toggles case 
      xor === U.ascii("HELLO\000world")
      
    }

    "XOR with another U value of different length" in {
      val blanks = U.ascii("           ")
      val x      = U.ascii("hello WORLD----all this ignored----")
      val xor = blanks ^ x
      
      // XORing with blank toggles case 
      xor === U.ascii("HELLO\000world")
      
    }

    "be splittable into blocks" in {
      val x = U("00010203040506070809")
      (x blocks 5*8) === Seq(
	U("0001020304"),
	U("0506070809"))
    }

    "be splittable into block with remainders" in {
      val x = U("00010203040506070809ffff")
      (x blocks 5*8) === Seq(
	U("0001020304"),
	U("0506070809"),
	U("ffff"))
    }

    "be splittable at an arbitrary position" in {
      val x = U.ascii("hello world!!!")
      val Seq(a,b) = x partitionAt 5*8
      a.ascii === "hello"
      b.ascii === " world!!!"
    }
    
    "be creatable by concatenating a sequence" in {
      val seq = Seq(
	U("0A0B0C"),
	U("0E0F"),
	U("101112131415"))
      U(seq) === U("0A0B0C0E0F101112131415")
    }

  }

  "addition" should {

    "work in normal case" in {
      U("00")     + 1 == U("01")
      U("222222") + 5 == U("222227")
      U("11111111222222223333333344444444555555556666666677777777") + 0x100 ===
	U("11111111222222223333333344444444555555556666666677777877")
    }

    "handles leading zeros" in {
      U("0001") + 3 == U("0004")
    }
    "handles overflow" in {
      U("FFFF") + 6 == U("0005")
    }

    "handles longs" in {
      U("222222222222222222222222222222") + 0x111111111111111L ===
	U("222222222222222333333333333333")
    }

  }

  "ascii property" should {

    "be correct length" in {
      val N = 80
      val bytes = new Array[Byte](N/8)
      
      for (_ <- 0 until 100) yield {
	rnd nextBytes bytes
	
	val x = U(bytes)
	x.n            === N 
	x.ascii.length === N/8
	
      }
    }
    
    "be correctly zerod at the beginning" in {
      
      val x = U.ascii("\000\000jjj")
      x.n        === 8*5
      x.ascii    === "°°jjj"
      x.toString === "00006a6a6a"
      
    }
    
    "be correctly padded at the beginning" in {
      
      val x = U.ascii("\000jjj")
      x.ascii === "°jjj"
      x.n     === 8*4
    }
    

    "display control characters as '°'" in {
      
      val x = U.ascii("jj\007j")
      x.n     ===  8*4
      x.ascii === "jj°j"
      
    }

    "handle byte with sign bit set " in {
      val x = U("f0")
      x.n        ===  8
      x.bytes    ===  Array( 0xf0.asInstanceOf[Byte] )
      x.toString === "f0"
    }
    
    "handle large value with sign bit set " in {
      val hex = "f5d3d58503b9699de785895a96fdbaaf"
      val x = U(hex)
      hex.length === 32
      x.n        === 128
      x.toString === hex
    }
  }

  "hex property" should {

    "handle hex values beginning with zeros" in {
      val x = U(
	"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f")
      x.toString === 
	"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
    }
  }

  "byte property" should {

    "be correct length" in {
      val N = 80
      val bytes = new Array[Byte](N/8)
      
      for (_ <- 0 until 100) yield {
	rnd nextBytes bytes
	
	val x = U(bytes)
	x.n               === N 
	x.bytes.length    === N/8
	x.toString.length === N/4
	
      }
    }

    "be correctly padded at the beginning" in {
      
      val x = U("00006a6a6a")
      x.n     ===  8*5
      x.ascii ===  "°°jjj"
      x.bytes === ("\000\000jjj" getBytes "ASCII")
      
    }
    
    "handle byte with sign bit set " in {
      val x = U("f0")
      x.n            ===  8
      x.bytes.length ===  1
      x.toString     === "f0"
    }
    
    "have 128-bit value with sign bit set" in {
      val hex = "ae2d8a571e03ac9c9eb76fac45af8e51"
      val x = U(hex)
      hex.length     ===  32
      x.n            === 128
      x.bytes.length ===  16
      x.toString     === "ae2d8a571e03ac9c9eb76fac45af8e51"
    }
  }
    
  
}

object USpec {
  val rnd = new Random
}
