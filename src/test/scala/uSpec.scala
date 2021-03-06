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
import java.io.{ByteArrayInputStream, FileInputStream, File}

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

 
    "XOR with all ones inverts" in {
      val a =    U("00112233445566778899aabbccddeeff")
      val b =    U("ffeeddccbbaa99887766554433221100")
      val ones = U("ffffffffffffffffffffffffffffffff")

      val aes00 = U("66e94bd4ef8a2c3b884cfa59ca342b2e")
      println( a )
      println( a ^ ones )
      println( aes00 )
      println( aes00 ^ ones )

      (a ^ ones) === b
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

  "sha256" should {

    "be 256 bits long" in {
      U.ascii("some data").sha256.n === 256
    }

    "never collide" in {
      var hashes = Set[U]()
      for( i <- 0 until 1000 ){
        var hash = U.random(1000).sha256
        hashes = hashes + hash
      }
      hashes.size === 1000  
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

  "stream reading" should {
    "handle general case" in {
      val in = new ByteArrayInputStream("Hello world!".getBytes)
      val blocks:Seq[U] = U(in, 5*8)
      val expected:Seq[U] = Seq( "d!", " worl", "Hello" ) map {U.ascii(_)}

      blocks === expected
    }
    "handle the exact multiple case" in {
      val in = new ByteArrayInputStream("Hello world!".getBytes)
      val blocks:Seq[U] = U(in, 6*8)
      val expected:Seq[U] = Seq( "world!", "Hello " ) map {U.ascii(_)}

      blocks === expected
    }

    "can read this file" in {
      val file = new File("src/test/scala/uSpec.scala")
      val fileLen = file.length
      val blocks = U(new FileInputStream(file),1024*8)
      blocks.length      === (fileLen.toFloat/1024).ceil
      blocks.head.n      === (fileLen % 1024)*8
      blocks.tail.head.n === 1024*8

    }
  }

  "byte twiddling" should {
    "can get a byte" in {
      val x = U("00112233445566")
      x(3) === 0x33
    }
    "can set a byte" in {
      val x = U("7777777777")
      x.copyWith(2, 0xAB.asInstanceOf[Byte]) === U("7777AB7777")
    }
    "can xor a byte" in {
      val x = U("7777777777")
      x.xorAt(2, 0xEC.asInstanceOf[Byte]) === U("77779B7777")
    }
  }

  "concatenation" should {
    "work" in {
      U("abcd")*3 === U("abcdabcdabcd")
    }
  } 

  "range" should {
    "support until" in {
      U("001122334455667788")(2 until 5) === U("223344")
    } 
    "support to" in {
      U("001122334455667788")(2 to    5) === U("22334455")
    } 
    "support step" in {
      U("001122334455667788")(1 until 7 by 2) === U("113355")
    } 

    "support backwards" in {
      U("001122334455667788")(5 to 2 by -1) === U("55443322")
    }
    "support backwards step" in {
      U("001122334455667788")(8 to 2 by -2) === U("88664422")
    }
 }
  
}

object USpec {
  val rnd = new Random
}
