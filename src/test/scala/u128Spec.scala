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

import org.eamonn.crypto.U128
import org.specs2.mutable.Specification

/** Test the U128 class */
class U128Spec extends Specification {

  /* See
   http://www.inconteam.com/software-development/41-encryption/55-aes-test-vectors#aes-ecb
   */

  val vectors = Array(
    ("6bc1bee22e409f96e93d7e117393172a","3ad77bb40d7a3660a89ecaf32466ef97"),
    ("ae2d8a571e03ac9c9eb76fac45af8e51","f5d3d58503b9699de785895a96fdbaaf"),
    ("30c81c46a35ce411e5fbc1191a0a52ef","43b1cd7f598ece23881b00e3ed030688"),
    ("f69f2445df4f9b17ad2b417be66c3710","7b0c785e27e8ad3f8223207104725dd4")
  )

  val vecKey = U128("2b7e151628aed2a6abf7158809cf4f3c")

  "aes encruption" should {
    "encrypt test vector 0" in {
      (U128(vectors(0)._1) e vecKey) === (U128(vectors(0)._2) )
    }
    "encrypt test vector 1" in {
      (U128(vectors(1)._1) e vecKey) === (U128(vectors(1)._2) )
    }
    "encrypt test vector 2" in {
      (U128(vectors(2)._1) e vecKey) === (U128(vectors(2)._2) )
    }
    "encrypt test vector 3" in {
      (U128(vectors(3)._1) e vecKey) === (U128(vectors(3)._2) )
    }
    "decrypt test vector 0" in {
      (U128(vectors(0)._2) d vecKey) === (U128(vectors(0)._1) )
    }
    "decrypt test vector 1" in {
      (U128(vectors(1)._2) d vecKey) === (U128(vectors(1)._1) )
    }
    "decrypt test vector 2" in {
      (U128(vectors(2)._2) d vecKey) === (U128(vectors(2)._1) )
    }
    "decrypt test vector 3" in {
      (U128(vectors(3)._2) d vecKey) === (U128(vectors(3)._1) )
    }
  }
  
  "U128 value" should {
    "encrypt to a 128 bit block" in {
      val k = U128.random
      val m  = U128.ascii("Hello World!____")
      val c = m e k
      c.bytes.length === (16)
    }
    
    "be same as encrypt followed by decrypt" in {
      val k = U128.random
      val m  = U128.ascii("Hello World!____")
      val c = m e k
      new String((c d k).bytes) === ("Hello World!____")
    }
  }

}

