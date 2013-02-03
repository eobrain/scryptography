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

package org.eamonn.crypto
import java.security.SecureRandom
import U._

/** The set {0,1}^n which is useful in cryptography. Rather than use
this constructor directly it is more convenient to use the 
functions in the object U("af30065d") or U.ascii("hello") */

case class U(n:Int, bigInt: BigInt) {

  require( bigInt >= 0 )

  /** XOR */
  def ^(that:U) = {
    require( this.n == that.n )
    new U(n, this.bigInt ^ that.bigInt)
  }

  /** Convert to ascii, replacing control characters with '°', and any
     leading padding with '»' */
  def text = {
    val s = new String(bytes,"ASCII") map {
      (c:Char)=> if (c.isControl) '°' else c
    }
    "»"*(n/8 - s.length) + s
  }

  /** Convert to bytes always of size n/8 (unlike BigInt)  */
  def bytes = {
    val fromBigInt = bigInt.toByteArray 
    val len = fromBigInt.length
    if( len == n/8 )
      fromBigInt
    else{
      val result = new Array[Byte](n/8)
      if( len > n/8 ){
	assert( len==1+n/8 )
	Array.copy( fromBigInt, 1, result, 0, len-1 )
      }else
	Array.copy( fromBigInt, 0, result, n/8 - len, len )
      result
    }
  }

  /** Return as hex string */
  override def toString = bigInt toString 16
}



object U{

  //create from hex-encoded string
  def apply(hex: String) = new U(4*hex.length, BigInt(hex,16))

  //create from an array of 16 bytes
  def apply(bytes: Array[Byte]) = new U(8*bytes.length, BigInt(1, bytes))

  //Create from a string, converted to bytes using ASCII encoding
  def ascii(ascii: String) = apply(ascii getBytes "ASCII")

  //create random
  def random(n:Int) = {
    val result = new Array[Byte](n)
    gen nextBytes result
    U(result)
  }

  val gen = new SecureRandom

}  
