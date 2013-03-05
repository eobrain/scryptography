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
//import scala.math.BigInt
import java.math.BigInteger
import java.security.SecureRandom
import Z._

/*trait IMember {
  val x:BigInt
}*/

/** The set of numbers supporting modulus n arithmetic */
case class Z(n:BigInt) {

  case class Member(x:BigInt) /*extends IMember*/ {
    def unary_- = new Member(n-x)

    def * (that:Member) = new Member((this.x * that.x) mod n)
    def + (that:Member) = new Member((this.x + that.x) mod n)
    def - (that:Member) = new Member((this.x - that.x) mod n)
    def / (that:Member) = that.inverse match {
      case Some(thatInverse) => this * thatInverse
      case None              => None
    }
    def pow(that:Member) = new Member(this.x.modPow(that.x, n)) 

    def * (that:BigInt) = new Member((x * that) mod n)
    def + (that:BigInt) = new Member((x + that) mod n)
    def - (that:BigInt) = new Member((x - that) mod n)
    def / (that:BigInt) = new Member((x / that) mod n)

    def pow(that:BigInt) = new Member(this.x.modPow(that, n)) 

    def * (that:Int) = new Member((x * that) mod n)
    def + (that:Int) = new Member((x + that) mod n)
    def - (that:Int) = new Member((x - that) mod n)
    def / (that:Int) = new Member((x / that) mod n)

    def pow(that:Int) = new Member(this.x.modPow(that, n)) 

    //def equals(that:IMember) = this.x == that.x
    /*def equals(that:Int)     = {
      System.out println "@@@@@@@@ "+this+".equals("+that+") @@@@@@"
      this.x == that
    }*/

    override def equals(that:Any) = {
      val result = that match {
        case thatMember:Member => this.x == thatMember.x
        case thatInt:Int        => this.x == thatInt
        case _                  => false
      }
      //System.out println "@@@@@@@@ "+this+".equals("+that+")--> "+result+" @@@@@@"
      result
    }

    def inverse = {
      val (gcd,a,b) = extendedEuclid(x,n)
      if(gcd==1)
        Some(new Member(a mod n))
      else
        None
    }
    override def toString = x+" ∈ ℤ"+n
  }

  private val one = new Member(BigInt(1))

  object *{

    case class Invertible(g:Member) extends Member(g.x){
  
      def generate:Set[Invertible] = {
        require( n < Int.MaxValue )
        var i = one
        var result = Set(new Invertible(i))
        while(true){
          val v =  new Invertible(g pow i) 
          if( result contains v)
            return result
          result = result + v
          i = i + one
        }
        throw new Error("never gets here")
      }

      def order = generate.size

      override def toString = g.x+" ∈ ℤ"+n+"*" 

    }


    def apply(x:Int) = {
      val xx = new Member(x)
      xx.inverse match {
        case Some(i) => Some(new Invertible(xx))
        case None => None
      }
    }

    def apply(x:BigInt) = {
      val xx = new Member(x)
      xx.inverse match {
        case Some(i) => Some(new Invertible(xx))
        case None => None
      }
    }

    def random:Member = {
      while(true){
        Z.this.random.inverse match {
          case Some(r) => return r
          case _ =>
        }
      }
      throw new Error("never gets here")
    }

    def toSet = {
      require( n < Int.MaxValue )
      ((0 until n.toInt) filter {apply(_) != None}).toSet
    }
  }

  val maxValue = new Member(n-1)

  def apply(i:BigInt) = new Member(i)
  def apply(i:Int) = new Member(BigInt(i))

  def random:Member = {
    val bits = n.bitLength
    while(true){
      val r = new BigInt(new BigInteger(bits,rnd))
      if(r < n )
        return new Member(r)
    }
    throw new Error("never gets here")
  }

}

object Z{
  def apply(n:Int) = new Z(BigInt(n))

  val rnd = new SecureRandom

  /** http://introcs.cs.princeton.edu/java/78crypto/ExtendedEuclid.java.html
   returns (gcd, a, b) such that ax + by = gcd
   */
  def extendedEuclid__(x:BigInt, y:BigInt) : (BigInt,BigInt,BigInt) = {
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
   }


}


/** the case where n is a prime number p */
case class ZP(p:BigInt) extends Z(p){

  require( p > 2 )
  require( p isProbablePrime 64 )

  case class MemberOfZP(xP:BigInt) extends Member(xP){

    override def toString = x+" ∈ ℤp="+n

    /** returns a Set of two, one, or no values */
    def root(e:Member):Set[Member] = {
      if( e.x == 2 ){

        //square root
        if( legendreSymbol == 1 ){
          if( (p mod 4) == 3 ){
            val pos = this  pow ((p+1)/4)
            Set( pos, -pos )
          } else
            throw new Error("not yet implemented")
        }else
          Set()

        
      }else if( (e.x gcd maxValue.x) == 1 ){
        val ee = new Z(maxValue.x)(e.x)
        ee.inverse match {
          case Some(eInverse) => Set( this pow eInverse.x )
          case None           => Set()
        }
      }else
        Set()  //TODO: check if this is correct
    } 

    def root(e:Int):Set[Member] = root(new Member(e))

    def legendreSymbol = this pow (maxValue/2)

  }

  override def apply(i:Int) = new MemberOfZP(BigInt(i))
  
}

object ZP {
  //def apply(p:BigInt) = new ZP(p)
  def apply(p:Int)    = new ZP(BigInt(p))

  /** Random (probable) prime of this bitlength */
  def random(bitLength:Int) = new ZP(new BigInt(BigInteger.probablePrime(bitLength,rnd)))

}
