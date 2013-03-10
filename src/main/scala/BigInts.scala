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

import org.jscience.mathematics.number.LargeInteger

object BigInts {

  class BigIntPimped(i:BigInt){
    val li = LargeInteger valueOf i.bigInteger
    def sqrt = BigInt(li.sqrt.toText.toString)
    def sqrtAbove = {
      def s = sqrt
      val ss = s*s
      if( ss <= i )
        s + 1
      else
        s
    }

    def isPrime = i isProbablePrime 64
  }

  implicit def pimp(i:BigInt) = new BigIntPimped(i)


}
