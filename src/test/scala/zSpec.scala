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

import org.eamonn.crypto.{Z,ZP}
import org.specs2.mutable.Specification

/** Test the Z class */
class ZSpec extends Specification {

  "Z set" should {
    "supports modular aritmatic" in {
      val Z12 = new Z(12)
      Z12(9) + Z12(8) === Z12(5)  
      Z12(5) * Z12(7) === Z12(11)  
      Z12(5) - Z12(7) === Z12(10)  
    }
    "arihtmetic works as expected" in {
      val Z55 = Z(55)
      val x = Z55.random
      val y = Z55.random
      val z = Z55.random
      x*(y+z) === x*y + x*z
    }

    "exponentiation works as expected" in {
       val Z12 = new Z(12)
      (Z12(2) pow Z12(3)) === Z12(8)
      (Z12(2) pow Z12(4)) === Z12(4)
     }

    "has expected inverses" in {
      val Z12 = new Z(12)
      val x1 = Z12(1)
      val x5 = Z12(5)
      val x7 = Z12(7)
      val x11 = Z12(11)
      x1*x1       === x1
      x1.inverse  === Some(x1)
      x5*x5       === x1
      x5.inverse  === Some(x5)
      x7*x7       === x1
      x7.inverse  === Some(x7)
      x11*x11     === x1
      x11.inverse === Some(x11)
      Z12(2).inverse === None
      Z12(3).inverse === None
    }

  }

  "Zstar set" should {

    val Zp = ZP random 2000
    val Zp_* = Zp.*

   "have inverse when small" in {
      val ZZ = Z(55)
      val ZZ_* = ZZ.*
      val x = ZZ_*.random
      x * x.inverse.get === ZZ(1)
    } 

   "have inverse when large" in {
      val x = Zp_*.random
      x * x.inverse.get === Zp(1)
    } 

   "contains all invertible objects" in {
      val x = Zp_*.random
      x.inverse.get * x == Zp(1)
    }

    "obeys Fermat's theorem" in {
      val x = Zp_*.random
      (x pow Zp.maxValue) === Zp(1)
    }

    "have expected members" in {
      Z(12).*.toSet == Set(1,5,7,11)
    }

  }

  "generators" should {

    "generate sets" in {
      val Z_* = Z(7).*
      Z_*(3).get.generate === ( Set(1,3,2,6,4,5) map { Z_*(_).get  } )
      Z_*(2).get.generate === ( Set(1,2,4)       map { Z_*(_).get  } )
    }

    "have an order" in {
      val Z_* = Z(7).*
      Z_*(3).get.order === 6
      Z_*(2).get.order === 3
    }

    def divides(i:Int, n:Int) = {
      val a = n/i
      a*i == n
    }

    "obeys Lagrange Theorem" in {
      val p = 7
      val Zp_* = ZP(p).*
      divides(Zp_*(1).get.order, p-1) === true 
      divides(Zp_*(2).get.order, p-1) === true 
      divides(Zp_*(3).get.order, p-1) === true 
      divides(Zp_*(4).get.order, p-1) === true 
      divides(Zp_*(5).get.order, p-1) === true 
      divides(Zp_*(6).get.order, p-1) === true 
    }

  }

  "Z with prime n" should {

    "have eth roots" in {
      val Z11 = ZP(11)
      (Z11(7) root 3) === Set( Z11(6) )
      (Z11(1) root 1) === Set( Z11(1) )
    }
    
    "have square roots" in {
      val Z11 = ZP(11)
      (Z11(1) root 2) === Set( Z11(1), Z11(10) )
      (Z11(4) root 2) === Set( Z11(2), Z11(9) )
      (Z11(9) root 2) === Set( Z11(3), Z11(8) )
      (Z11(5) root 2) === Set( Z11(4), Z11(7) )
      (Z11(3) root 2) === Set( Z11(5), Z11(6) )
      (Z11(2) root 2) === Set() 
    }
    
  }
}
