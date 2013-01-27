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

/** The set {0,1}^n which is useful in cryptography. Rather than use
this constructor directly it is more convenient to use the apply
functiona in the object U("af30065d") or U("hello".getBytes) */

case class U(n:Int, bigInt: BigInt) {

  /** XOR */
  def ^(that:U) = {
    require( this.n == that.n )
    new U(n, this.bigInt ^ that.bigInt)
  }

  //Conver to ascii, replacing control characters with °, and any leading padding with »
  def text = {
    val s = new String(bigInt.toByteArray) map {(c:Char)=> if (c.isControl) '°' else c}
    "»"*(n/8 - s.length) + s
  }
 
}



object U{

  //from hex-encoded string
  def apply(hex: String) = new U(4*hex.length, BigInt(hex,16))

  def apply(bytes: Array[Byte]) = new U(8*bytes.length, BigInt(bytes))

}  
