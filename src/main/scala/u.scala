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

//case class U(n:Int, bigInt: BigInt) {
case class U(bytes: Array[Byte]) {

  //val bigInt = BigInt(1, bytes)

  val n = 8 * bytes.length

  override def equals(other:Any):Boolean = other match {
    case that: U => {
      if( that.n != this.n )
	false
      else {
	for( i <- 0 until n/8 )
	  if( this.bytes(i) != that.bytes(i) )
	    return false
	true
      }
    }
    case _ => false
  }

  //require( bigInt >= 0 )

  /** XOR */
  def ^(that:U) = {
    require( this.n == that.n, "can only XOR values of same length" )
    val bs = new Array[Byte](n/8)
    for( i <- 0 until n/8 )
      bs(i) = (this.bytes(i) ^ that.bytes(i)).asInstanceOf[Byte]
    new U(bs)
  }

  /* * Concatenate, but pads each component to even number of bytes  */
  /*def ++ (that:U) = {
    val thisB = this.bytes
    val thatB = that.bytes
    val buf = new Array[Byte](thisB.length + thatB.length)
    Array.copy( thisB, 0, buf, 0,            thisB.length )
    Array.copy( thatB, 0, buf, thisB.length, thatB.length )
    U(buf)
  }*/

  /** Convert to ascii, replacing control characters with '°', and any
     leading padding with '»' */
  def text = {
    val s = new String(bytes,"ASCII") map {
      (c:Char)=> if (c.isControl) '°' else c
    }
    "»"*(n/8 - s.length) + s
  }

  /* * Convert to bytes always of size n/8 (unlike BigInt)  */
  /*def bytes = {
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
  }*/


  def blocks(bitsPerBlock:Int):Seq[U] = {
    val bs = bytes
    val bytesPerBlock = bitsPerBlock/8; 
    val n = bs.length/bytesPerBlock
    (0 until n) map { (i:Int) =>
      val buf = new Array[Byte](bytesPerBlock)
      Array.copy( bs, i * bytesPerBlock, buf, 0, bytesPerBlock )
      new U(buf)		     
    }
  }

  /** Return as hex string */
  override def toString = (bytes map { "%02x" format _ }).mkString
}



object U{

  def hex2bytes(hex: String) = {
    require( hex.length % 2 == 0, "hex must have even number of chars" )
    val length = hex.length/2
    val bytes = new Array[Byte](length)
    for( i <- 0 until length ){
      val byteHex = hex.substring( 2*i, 2*(i+1 ))
      assert( byteHex.length==2, "length should be 2 for i="+i+" in "+hex)
      bytes(i) = Integer.parseInt( byteHex, 16 ).asInstanceOf[Byte]
    }
    bytes
  }

  //create from hex-encoded string
  def apply(hex: String) = new U(hex2bytes(hex))

  //Create from a string, converted to bytes using ASCII encoding
  def ascii(ascii: String) = new U(ascii getBytes "ASCII")

  //By concatenating a sequence of Us, padding each up to next byte
  def apply(uSeq: Seq[U]):U = {
    val bytesSeq = uSeq map {_.bytes}
    val nSeq = bytesSeq map {_.length}
    val nTot = nSeq reduce {_ + _}
    val buf = new Array[Byte](nTot) 
    var insert = 0;
    for( bytes <- bytesSeq ){
      val n = bytes.length
      //println( "Array.copy( ["+bytes.length+"], 0, ["+buf.length+"], "+insert+", "+n+")" )
      Array.copy( bytes, 0, buf, insert, n )
      insert += n
    }
    new U(buf)
  }

  //create random
  def random(n:Int) = {
    val result = new Array[Byte](n)
    gen nextBytes result
    new U(result)
  }

  val gen = new SecureRandom

}  
