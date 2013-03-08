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

package org.eamonn.crypto //import java.math.BigInteger
import java.security.SecureRandom
import java.math.BigInteger
import Z._


/** Multiplicative group of integers modulo n
 * See https://en.wikipedia.org/wiki/Multiplicative_group_of_integers_modulo_n */
case class Z(n:BigInt, p:Option[BigInt]) {


  val one = new ModInt(BigInt(1))

  val maxValue = new ModInt(n-1)

  def apply(i:BigInt) = new ModInt(i)
  def apply(i:Int) = new ModInt(BigInt(i))

  def random:ModInt = {
    val bits = n.bitLength
    while(true){
      val r = new BigInt(new BigInteger(bits,rnd))
      if(r < n ){
        return new ModInt(r)
      }
    }
    throw new Error("never gets here")
  }



/*
  def invertible(x:Int) = {
    val xx = new ModInt(x)
    xx.inverse match {
      case Some(i) => Some(xx)
      case None => None
    }
  }

  def invertible(x:BigInt) = {
    val xx = new ModInt(x)
    xx.inverse match {
      case Some(i) => Some(xx)
      case None => None
    }
  }
 */ 

  def randomInvertible:ModInt = {
    while(true){
      Z.this.random.invertible match {
        case Some(r) => 
          return r.inverse
        case _ =>
      }
    }
    throw new Error("never gets here")
  }
  
  def toInvertibleSet = {
    require( n < Int.MaxValue )
    ((0 until n.toInt) filter {apply(_).invertible != None}).toSet
  }


  case class ModInt (xUnModded:BigInt) {

    var x = xUnModded mod n
    require( x >= 0 )
    require( x < n )

    def unary_- = new ModInt(n-x)

    def * (that:ModInt) = new ModInt(this.x * that.x)
    def + (that:ModInt) = new ModInt(this.x + that.x)
    def - (that:ModInt) = new ModInt(this.x - that.x)
    def / (that:ModInt) = that.invertible match {
      case Some(thatInvertible) => this * thatInvertible.inverse
      case None                 => None
    }
    def pow(that:ModInt) = new ModInt(this.x.modPow(that.x, n)) 

    def * (that:BigInt) = new ModInt(x * that)
    def + (that:BigInt) = new ModInt(x + that)
    def - (that:BigInt) = new ModInt(x - that)
    def / (that:BigInt) = new ModInt(x / that)

    def pow(that:BigInt) = new ModInt(this.x.modPow(that, n)) 

    def * (that:Int) = new ModInt(x * that)
    def + (that:Int) = new ModInt(x + that)
    def - (that:Int) = new ModInt(x - that)
    def / (that:Int) = new ModInt(x / that)

    def pow(that:Int) = new ModInt(this.x.modPow(that, n)) 

    //def equals(that:IModInt) = this.x == that.x
    /*def equals(that:Int)     = {
      System.out println "@@@@@@@@ "+this+".equals("+that+") @@@@@@"
      this.x == that
    }*/


    override def equals(that:Any) = {
      val result = that match {
        case thatModInt:ModInt => this.x == thatModInt.x
        case thatInt:Int        => this.x == thatInt
        case _                  => false
      }
      result
    }

    case class Invertible(inverse:ModInt){
      def generate:Set[ModInt] = {
        require( n < Int.MaxValue )
        var i = one
        var result = Set(one)
        while(true){
          val v =  ModInt.this pow i 
          if( result contains v){
            return result
          }
          result = result + v
          i = i + one
        }
        throw new Error("never gets here")
      }
      
      def order = generate.size

      override def toString = x+" ∈ ℤ*"+n
    }

    def invertible = {
      try{
        Some(new Invertible(new ModInt(x modInverse n)))
      }catch{
        case _ => None
      }
        
        
      /*val (gcd,a,b) = extendedEuclid(x,n)
      if(gcd==1)
        Some(new Invertible(new ModInt(a mod n)))
      else
        None*/
    }
/*
    def inverse = {
      val (gcd,a,b) = extendedEuclid(x,n)
      if(gcd==1)
        Some(new ModInt(a mod n))
      else
        None
    }
*/


    /** returns a Set of two, one, or no values */
    def root(e:ModInt):Set[ModInt] = {
      if( e.x == 2 ){

        //square root
        if( legendreSymbol == 1 ){
          if( (p.get mod 4) == 3 ){
            val pos = this  pow ((p.get+1)/4)
            Set( pos, -pos )
          } else
            throw new Error("not yet implemented")
        }else
          Set()

        
      }else if( (e.x gcd maxValue.x) == 1 ){
        val ee = new Z(maxValue.x, None)(e.x)
        ee.invertible match {
          case Some(invertible) => Set( this pow invertible.inverse.x )
          case None           => Set()
        }
      }else
        Set()  //TODO: check if this is correct
    } 

    def root(e:Int):Set[ModInt] = root(new ModInt(e))

    def legendreSymbol = this pow (maxValue/2)



    
    override def toString = x+" ∈ ℤ"+n
  }




}

object Z{

  def newZ(n:BigInt) = new Z( n, ifPrime(n) )

  def apply(n:Int) = newZ( BigInt(n) )
  def apply(n:BigInt) = newZ(n)

  val rnd = new SecureRandom

  /* * http://introcs.cs.princeton.edu/java/78crypto/ExtendedEuclid.java.html
   returns (gcd, a, b) such that ax + by = gcd
   */
  /*def extendedEuclid__(x:BigInt, y:BigInt) : (BigInt,BigInt,BigInt) = {
      if (y == 0)
         ( x, BigInt(1), BigInt(0) )
      else{
         val (dd,aa,bb) = extendedEuclid__(y, x % y)
         ( dd, bb, aa - (x / y) * aa )
      }
   }
  def extendedEuclid(p:BigInt, q:BigInt) : (BigInt,BigInt,BigInt) = {
      if (q == 0)
         ( p, BigInt(1), BigInt(0) )
      else{
        val (v0,v1,v2) = extendedEuclid(q, p % q);
        val d = v0
        val a = v2
        val b = v1 - (p / q) * v2
        ( d, a, b )
      }
   }*/

  def isPrime(a:BigInt) = a isProbablePrime 64

  def ifPrime(a:BigInt) = if(isPrime(a)) Some(a) else None

  /** Random (probable) prime of this bitlength */
  def randomPrime(bitLength:Int) = {
    val p = new BigInt(BigInteger.probablePrime(bitLength,rnd))
    new Z( p, Some(p) )
    
  }

}


/* * the case where n is a prime number p */
/*case class ZP(p:BigInt) extends Z(p){

  require( p > 2 )
  require( p isProbablePrime 64 )

  case class ModIntOfZP(xP:BigInt) extends ModInt(xP){

    override def toString = x+" ∈ ℤp="+n


  }

  override def apply(i:Int) = new ModIntOfZP(BigInt(i))
  
}*/

/*object ZP {
  //def apply(p:BigInt) = new ZP(p)
  def apply(p:Int)    = new ZP(BigInt(p))


}*/
