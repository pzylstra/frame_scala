package ffm.geometry

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class CrownPolySpec extends FlatSpec with Matchers {
  val Tol = 1.0e-8

  "Creating a crown poly" should "fail if hp is not greater than hc" in {
    intercept[IllegalArgumentException] {
      CrownPoly(hc=1.0, he=1.5, ht=2.0, hp=1.0, w=1.0)
    }
  }
  
  it should "fail if ht is not greater than or equal to he" in {
    intercept[IllegalArgumentException] {
      CrownPoly(hc=1.0, he=1.5, ht=1.0, hp=2.0, w=1.0)
    }
  }
  
  it should "fail if w is not positive" in {
    intercept[IllegalArgumentException] {
      CrownPoly(hc=1.0, he=1.5, ht=2.5, hp=3.0, w=0.0)
    }
  }
  
  "A valid CrownPoly" should "return the correct dimensions" in {
    val w = 2.0
    val hc = 1.0
    val he = 1.5
    val ht = 2.5
    val hp = 3.0
        
    val poly = CrownPoly(hc=hc, he=he, ht=ht, hp=hp, w=w)
    
    poly.width should be (w +- Tol)
    poly.height should be ((ht.max(hp) - hc.min(he)) +- Tol)
    
    poly.left should be (-w/2 +- Tol)
    poly.right should be (w/2 +- Tol)
    poly.top should be (hp.max(ht) +- Tol)
    poly.bottom should be (hc.min(he) +- Tol)
  }
  
  it should "return the correct centroid" in {
    val poly = CrownPoly(hc=1.0, he=1.5, ht=2.5, hp=3.0, w=2.0)
    val c = poly.centroid
    c.x should be (0.0 +- Tol)
    c.y should be (2.0 +- Tol)
  }
  
  it should "return the correct area" in {
    val poly = CrownPoly(hc=1.0, he=1.5, ht=2.5, hp=3.0, w=2.0)
    poly.area should be (3.0 +- Tol)
  }
  
  it should "return the correct volume (conical)" in {
    // cone with radius 2.0 and height 2.0
    val poly = CrownPoly(hc=1.0, he=1.0, ht=1.0, hp=3.0, w=4.0)
    poly.volume should be (math.Pi * 4.0 * 2.0 / 3 +- Tol)
  }
  
  it should "return the correct volume (cylindrical)" in {
    // cylinder with radius 2.0 and height 2.0
    val poly = CrownPoly(hc=1.0, he=1.0, ht=3.0, hp=3.0, w=4.0)
    poly.volume should be (math.Pi * 4.0 * 2.0 +- Tol)
  }
  
  it should "return the correct volume (hexagonal)" in {
    val poly = CrownPoly(hc=1.0, he=2.0, ht=3.0, hp=4.0, w=4.0)
    
    val endConeVol = math.Pi * 4.0 * 1.0 / 3.0
    val midCylVol = math.Pi * 4.0 * 1.0
    
    poly.volume should be (2*endConeVol + midCylVol +- Tol)
  }
  
  it should "return left-most lower vertex for pointInBase with x <= left" in {
    val he=2.0
    val poly = CrownPoly(hc=1.0, he=he, ht=3.0, hp=4.0, w=2.0)
    
    val c1 = poly.pointInBase(poly.left - 1)
    val c2 = poly.pointInBase(poly.left)
    
    c1.almostEq(c2) should be (true)
    c1 should be (Coord(poly.left, he))
  }
  
  it should "return right-most lower vertex for pointInBase with x >= right" in {
    val he = 2.0
    val poly = CrownPoly(hc=1.0, he=he, ht=3.0, hp=4.0, w=2.0)
    
    val c1 = poly.pointInBase(poly.right + 1)
    val c2 = poly.pointInBase(poly.right)
    
    c1.almostEq(c2) should be (true)
    c1 should be (Coord(poly.right, he))
  }
  
  it should "return the centre lower vertex for pointInBase with x = 0" in {
    val hc = 1.0
    val poly = CrownPoly(hc=hc, he=2.0, ht=3.0, hp=4.0, w=2.0)
    
    poly.pointInBase(0.0) should be (Coord(0, hc))
  }
  
  it should "return the correct edge intersection for pointInBase with intermediate x" in {
    val hc = 1.0
    val he = 2.0
    val w = 2.0
    val poly = CrownPoly(hc, he, ht=he + 1, hp=he + 2, w)

    poly.pointInBase(-w/4) should be ( Coord(-w/4, (hc + he)/2) )
    poly.pointInBase( w/4) should be ( Coord( w/4, (hc + he)/2) )
  }

  it should "return a proper LineSegment when intersected with a Ray that crosses it" in {
    // rectangular poly
    val poly = CrownPoly(hc=1.0, he=1.0, ht=2.0, hp=2.0, w=1.0)
    
    // horizontal ray crossing centre of poly
    val c = poly.centroid 
    val ray = Ray(c.toOffset(-poly.width, 0), angle=0)

    testIntersectionResult( 
        resultOp = poly.intersection(ray), 
        expectedOp = Some( Segment(Coord(poly.left, c.y), Coord(poly.right, c.y)) ) )
  }
  
  it should "return a proper LineSegment when intersected with a Ray from within it" in {
    // rectangular poly
    val poly = CrownPoly(hc=1.0, he=1.0, ht=2.0, hp=2.0, w=1.0)
    
    // horizontal ray crossing centre of poly
    val c = poly.centroid 
    val ray = Ray(c, angle=0)

    testIntersectionResult( 
        resultOp = poly.intersection(ray), 
        expectedOp = Some( Segment(c, Coord(poly.right, c.y)) ) )
  }
  
  it should "return a degenerate LineSegment when intersected with a Ray touching at a vertex" in {
    // rectangular poly
    val top = 2.0
    val width = 1.0
    val poly = CrownPoly(hc=1.0, he=1.0, ht=top, hp=top, w=width)
    
    // ray touching only at vertex
    val topRight = Coord(width / 2, top)
    val ray = Ray(topRight, math.Pi / 4)
    
    testIntersectionResult(
        resultOp = poly.intersection(ray),
        expectedOp = Some( Segment(topRight, topRight) )
    )
  }
  
  def testIntersectionResult(resultOp: Option[Segment], expectedOp: Option[Segment]) = expectedOp match {
    case None =>
      resultOp should be (None)
      
    case Some(expected) =>
      resultOp.isDefined should be (true)
      resultOp.get should be (expected)
  }
}