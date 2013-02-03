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
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec

/** A 128-bit block */
class U128(bigInt: BigInt) extends U(128, bigInt) {

  /** encrypt using single-block AES */
  def e(k:U128) = aes(Cipher.ENCRYPT_MODE, k)

  /** encrypt using single-block AES */
  def d(k:U128) = aes(Cipher.DECRYPT_MODE, k)

  private def aes(opmode:Int, k:U128) = {
    val aes = Cipher getInstance "AES/ECB/NoPadding"
    aes.init(opmode, k.asAesKey)
    def result = new Array[Byte](16)
    U128( aes doFinal bytes )
  }

  /** As key to pass into AUE */
  def asAesKey = new SecretKeySpec(bytes, "AES")

}

object U128{

  //from a hex-encoded string of length 32
  def apply(hex: String) = {
    require( hex.length == 128/4 )
    new U128(BigInt(hex,16))
  }

  //from an array of 16 bytes
  def apply(bytes: Array[Byte]) = {
    require( bytes.length == 128/8 )
    new U128(BigInt(1,bytes))
  }

  //Create from a string, converted to bytes using ASCII encoding
  def ascii(ascii: String) = apply(ascii getBytes "ASCII")

  //create random
  def random = {
    val result = new Array[Byte](16)
    U.gen nextBytes result
    U128(result)
  }
  
}  
