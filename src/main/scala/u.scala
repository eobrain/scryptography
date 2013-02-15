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

  /** XOR, truncating to shorter length if lengths are different */
  def ^(that:U) = {
    var length = ( this.n min that.n )/8
    val bs = new Array[Byte](length)
    for( i <- 0 until length )
      bs(i) = (this.bytes(i) ^ that.bytes(i)).asInstanceOf[Byte]
    new U(bs)
  }

  /** addition */
  def +(that:Long) = {
    val bs = (BigInt(bytes) + BigInt(that)).toByteArray
    val length = bs.length
    if( length == n/8 )
      new U(bs)
    else{
      val good = new Array[Byte](n/8)
      if (length < good.length)
	//leading zeros
	Array.copy(bs,0                 , good, good.length -length, length)
      else
	//overflow
	Array.copy(bs,length-good.length, good, 0                  , length)
      new U(good)
    }
  }

  /** Concatenate  */
  def ++ (that:U) = {
    val buf = new Array[Byte](this.bytes.length + that.bytes.length)
    Array.copy( this.bytes, 0, buf, 0,                 this.bytes.length )
    Array.copy( that.bytes, 0, buf, this.bytes.length, that.bytes.length )
    U(buf)
  }

  /** Partition into two at given bit position */
  def partitionAt(i:Int) = {
    require( i>=0 && i<n && i % 8 == 0)
    val a = new Array[Byte](i/8)
    val b = new Array[Byte]((n-i)/8)
    Array.copy(bytes, 0,        a, 0, a.length )
    Array.copy(bytes, a.length, b, 0, b.length )  
    Seq( new U(a), new U(b) )
  }

  /** Return new U with PKCS5 padding removed from end */
  def unpad = {
    val length = bytes.length
    val padVal = bytes(length - 1)
    assert( padVal <= length )
    val buf = new Array[Byte]( length - padVal )
    Array.copy( bytes, 0, buf, 0, buf.length )
    new U(buf)
  }

  /** Convert to ascii, replacing control characters with '°' */
  def ascii = {
    val s = new String(bytes,"ASCII") map {
      (c:Char)=> if (c.isControl) '°' else c
    }
    assert( s.length == n/8 )
    s
    //"»"*(n/8 - s.length) + s
  }

  /** splits into blocks od given size, with last one possibly shorter */
  def blocks(bitsPerBlock:Int):Seq[U] = {
    val bs = bytes
    val bytesPerBlock = bitsPerBlock/8; 
    val fullBlockCount = bs.length/bytesPerBlock
    val fullBlocks = (0 until fullBlockCount) map { (i:Int) =>
      val buf = new Array[Byte](bytesPerBlock)
      Array.copy( bs, i * bytesPerBlock, buf, 0, bytesPerBlock )
      new U(buf)		     
    }
    if( bitsPerBlock * fullBlockCount == n )
      fullBlocks
    else {
      val partialBlockSize =  n -  bitsPerBlock * fullBlockCount
      assert( partialBlockSize > 0 )
      val buf = new Array[Byte](partialBlockSize/8)
      Array.copy( bytes, n/8-buf.length, buf, 0, buf.length)
      fullBlocks :+ new U(buf)
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
