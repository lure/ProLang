# University of Washington, Programming Languages, Homework 7, 
# hw7testsprovided.rb

require "./hw7.rb"

#  Will not work completely until you implement all the classes and their methods

# Will print only if code has errors; prints nothing if all tests pass

# These tests do NOT cover all the various cases, especially for intersection

#Constants for testing
ZERO = 0.0
ONE = 1.0
TWO = 2.0
THREE = 3.0
FOUR = 4.0
FIVE = 5.0
SIX = 6.0
SEVEN = 7.0
TEN = 10.0

#Point Tests
a = Point.new(THREE, FIVE)
if not (a.x == THREE and a.y == FIVE)
  puts "Point is not initialized properly"
end
if not (a.eval_prog([]) == a)
  puts "Point eval_prog should return self"
end
if not (a.preprocess_prog == a)
  puts "Point preprocess_prog should return self"
end
a1 = a.shift(THREE, FIVE)
if not (a1.x == SIX and a1.y == TEN)
  puts "Point shift not working properly"
end
a2 = a.intersect(Point.new(THREE, FIVE))
if not (a2.x == THREE and a2.y == FIVE)
  puts "Point intersect not working properly"
end
a3 = a.intersect(Point.new(FOUR, FIVE))
if not (a3.is_a? NoPoints)
  puts "Point intersect not working properly"
end

#Line Tests
b = Line.new(THREE, FIVE)
if not (b.m == THREE and b.b == FIVE)
  puts "Line not initialized properly"
end
if not (b.eval_prog([]) == b)
  puts "Line eval_prog should return self"
end
if not (b.preprocess_prog == b)
  puts "Line preprocess_prog should return self"
end

b1 = b.shift(THREE, FIVE)
if not (b1.m == THREE and b1.b == ONE)
  puts "Line shift not working properly"
end

b2 = b.intersect(Line.new(THREE, FIVE))
if not (((b2.is_a? Line)) and b2.m == THREE and b2.b == FIVE)
  puts "Line intersect not working properly"
end
b3 = b.intersect(Line.new(THREE, FOUR))
if not ((b3.is_a? NoPoints))
  puts "Line intersect not working properly"
end

#VerticalLine Tests
c = VerticalLine.new(THREE)
if not (c.x == THREE)
  puts "VerticalLine not initialized properly"
end

if not (c.eval_prog([]) == c)
  puts "VerticalLine eval_prog should return self"
end
if not (c.preprocess_prog == c)
  puts "VerticalLine preprocess_prog should return self"
end
c1 = c.shift(THREE, FIVE)
if not (c1.x == SIX)
  puts "VerticalLine shift not working properly"
end
c2 = c.intersect(VerticalLine.new(THREE))
if not ((c2.is_a? VerticalLine) and c2.x == THREE)
  puts "VerticalLine intersect not working properly"
end
c3 = c.intersect(VerticalLine.new(FOUR))
if not ((c3.is_a? NoPoints))
  puts "VerticalLine intersect not working properly"
end

#LineSegment Tests
d = LineSegment.new(ONE, TWO, -THREE, -FOUR)
if not (d.eval_prog([]) == d)
  puts "LineSegement eval_prog should return self"
end
d1 = LineSegment.new(ONE, TWO, ONE, TWO)
d2 = d1.preprocess_prog
if not ((d2.is_a? Point)and d2.x == ONE and d2.y == TWO)
  puts "LineSegment preprocess_prog should convert to a Point"
  puts "if ends of segment are real_close"
end

d = d.preprocess_prog
if not (d.x1 == -THREE and d.y1 == -FOUR and d.x2 == ONE and d.y2 == TWO)
  puts "LineSegment preprocess_prog should make x1 and y1"
  puts "on the left of x2 and y2"
end

d3 = d.shift(THREE, FIVE)
if not (d3.x1 == ZERO and d3.y1 == ONE and d3.x2 == FOUR and d3.y2 == SEVEN)
  puts "LineSegment shift not working properly"
end

d4 = d.intersect(LineSegment.new(-THREE, -FOUR, ONE, TWO))
if not (((d4.is_a? LineSegment)) and d4.x1 == -THREE and d4.y1 == -FOUR and d4.x2 == ONE and d4.y2 == TWO)
  puts "LineSegment intersect not working properly1"
end
d5 = d.intersect(LineSegment.new(TWO, THREE, FOUR, FIVE))
if not ((d5.is_a? NoPoints))
  puts "LineSegment intersect not working properly2"
end

#Intersect Tests
i = Intersect.new(LineSegment.new(-ONE, -TWO, THREE, FOUR), LineSegment.new(THREE, FOUR, -ONE, -TWO))
i1 = i.preprocess_prog.eval_prog([])
if not (i1.x1 == -ONE and i1.y1 == -TWO and i1.x2 == THREE and i1.y2 == FOUR)
  puts "Intersect eval_prog should return the intersect between e1 and e2"
end

#Var Tests
v = Var.new("a")
v1 = v.eval_prog([["a", Point.new(THREE, FIVE)]])
if not ((v1.is_a? Point) and v1.x == THREE and v1.y == FIVE)
  puts "Var eval_prog is not working properly"
end
if not (v1.preprocess_prog == v1)
  puts "Var preprocess_prog should return self"
end

#Let Tests
l = Let.new("a", LineSegment.new(-ONE, -TWO, THREE, FOUR),
            Intersect.new(Var.new("a"), LineSegment.new(THREE, FOUR, -ONE, -TWO)))
l1 = l.preprocess_prog.eval_prog([])
if not (l1.x1 == -ONE and l1.y1 == -TWO and l1.x2 == THREE and l1.y2 == FOUR)
  puts "Let eval_prog should evaluate e2 after adding [s, e1] to the environment"
end

#Let Variable Shadowing Test
l2 = Let.new("a", LineSegment.new(-ONE, -TWO, THREE, FOUR),
             Let.new("b", LineSegment.new(THREE, FOUR, -ONE, -TWO), Intersect.new(Var.new("a"), Var.new("b"))))
l2 = l2.preprocess_prog.eval_prog([["a", Point.new(0, 0)]])
if not (l2.x1 == -ONE and l2.y1 == -TWO and l2.x2 == THREE and l2.y2 == FOUR)
  puts "Let eval_prog should evaluate e2 after adding [s, e1] to the environment"
end


#Shift Tests
s = Shift.new(THREE, FIVE, LineSegment.new(-ONE, -TWO, THREE, FOUR))
s1 = s.preprocess_prog.eval_prog([])
if not (s1.x1 == TWO and s1.y1 == THREE and s1.x2 == SIX and s1.y2 == 9)
  puts "Shift should shift e by dx and dy"
end




class Point < GeometryValue
  def ==(p)
    self.class == p.class and @x == p.x and @y == p.y
  end
end

class Line < GeometryValue
  def ==(l)
    self.class == l.class and @m == l.m and @b == l.b
  end
end

class VerticalLine < GeometryValue
  def ==(vl)
    self.class == vl.class and @x == vl.x
  end
end

class LineSegment < GeometryValue
  def ==(ls)
    self.class == ls.class and @x1 == ls.x1 and @y1 == ls.y1 and @x2 == ls.x2 and @y2 == ls.y2
  end
end

class Intersect < GeometryExpression
  def ==(i)
    self.class == i.class and @e1 == i.e1 and @e2 == i.e2
  end
end

class Let < GeometryExpression
  def ==(l)
    self.class == l.class and @s == l.s and @e1 == l.e1 and @e2 == l.e2
  end
end

class Var < GeometryExpression
  def ==(v)
    self.class == v.class and @s == v.s
  end
end

class Shift < GeometryExpression
  def ==(s)
    self.class == s.class and @dx == s.dx and @dy == s.dy and @e == s.e
  end
end

if not (Let.new("a", LineSegment.new(3.2,4.1,3.2,4.1), LineSegment.new(3.2,4.1,3.2,4.1)).preprocess_prog == Let.new("a", Point.new(3.2, 4.1), Point.new(3.2, 4.1)))
  puts "Let preprocess_prog test fails"
end

if not (Shift.new(1.0, 2.0, LineSegment.new(3.2,4.1,3.2,4.1)).preprocess_prog == Shift.new(1.0, 2.0, Point.new(3.2, 4.1)))
  puts "Shift preprocess_prog test fails"
end

if not (Intersect.new(LineSegment.new(3.2,4.1,3.2,4.1), LineSegment.new(3.2,4.1,3.2,4.1)).preprocess_prog == Intersect.new(Point.new(3.2, 4.1), Point.new(3.2, 4.1)))
  puts "Intersect preprocess_prog test fails"
end

if not (Shift.new(3.0, 4.0, Point.new(4.0, 4.0)).eval_prog([]) == Point.new(7.0, 8.0))
  puts "Shift eval_prog test fails with Point"
end

if not (Shift.new(3.0, 4.0, Line.new(4.0, 4.0)).eval_prog([]) == Line.new(4.0, -4.0))
  puts "Shift eval_prog test fails with Line"
end

if not (Shift.new(3.0, 4.0, VerticalLine.new(4.0)).eval_prog([]) == VerticalLine.new(7.0))
  puts "Shift eval_prog test fails with VerticalLine"
end

if not (Shift.new(3.0, 4.0, LineSegment.new(4.0, 3.0, 12.0, -2.0)).eval_prog([]) == LineSegment.new(7.0, 7.0, 15.0, 2.0))
  puts "Shift eval_prog test fails with LineSegment"
end

if not (Intersect.new(Point.new(4.0, 4.0), Point.new(4.0, 4.0)).eval_prog([]) == Point.new(4.0, 4.0))
  puts "Intersect test #1 fails for Point/Point"
end
if not (Intersect.new(Point.new(4.0, 4.0), Point.new(4.0, 4.1)).eval_prog([]).class == NoPoints.new.class)
  puts "Intersect test #2 fails for Point/Point"
end
if not (Intersect.new(Point.new(4.0, 4.0), Line.new(4.0, 4.1)).eval_prog([]).class == NoPoints.new.class)
  puts "Intersect test #1 fails for Point/Line"
end
if not (Intersect.new(Point.new(1.0, 8.0), Line.new(4.0, 4.0)).eval_prog([]) == Point.new(1.0, 8.0))
  puts "Intersect test #2 fails for Point/Line"
end
if not (Intersect.new(Point.new(5.0, 4.0), VerticalLine.new(4.0)).eval_prog([]).class == NoPoints.new.class)
  puts "Intersect test #1 fails for Point/VerticalLine"
end
if not (Intersect.new(Point.new(4.0, 4.0), VerticalLine.new(4.0)).eval_prog([]) == Point.new(4.0, 4.0))
  puts "Intersect test #2 fails for Point/VerticalLine"
end
if not (Intersect.new(Point.new(2.0, 2.0), LineSegment.new(1.0, 1.0, 4.0, 4.0)).eval_prog([]) == Point.new(2.0, 2.0))
  puts "Intersect test #1 fails for Point/LineSegment"
end
if not (Intersect.new(Point.new(4.1, 4.1), LineSegment.new(1.0, 1.0, 4.0, 4.0)).eval_prog([]).class == NoPoints.new.class)
  puts "Intersect test #2 fails for Point/LineSegment"
end

if not (Intersect.new(Line.new(4.0, 4.0), Point.new(1.0, 8.0)).eval_prog([]) == Point.new(1.0, 8.0))
  puts "Intersect test #1 fails for Line/Point"
end
if not (Intersect.new(Line.new(4.0, 4.0), Point.new(4.0, 4.1)).eval_prog([]).class == NoPoints.new.class)
  puts "Intersect test #2 fails for Line/Point"
end
if not (Intersect.new(Line.new(4.0, 4.0), Line.new(4.0, 4.1)).eval_prog([]).class == NoPoints.new.class)
  puts "Intersect test #1 fails for Line/Line"
end
if not (Intersect.new(Line.new(1.0, 7.0), Line.new(4.0, 4.0)).eval_prog([]) == Point.new(1.0, 8.0))
  puts "Intersect test #2 fails for Line/Line"
end
if not (Intersect.new(Line.new(4.0, 4.0), VerticalLine.new(4.0)).eval_prog([]) == Point.new(4.0, 20.0))
  puts "Intersect test fails for Line/VerticalLine"
end
if not (Intersect.new(Line.new(-1.0, 1.0), LineSegment.new(1.0, 1.0, 4.0, 4.0)).eval_prog([]).class == NoPoints.new.class)
  puts "Intersect test #1 fails for Line/LineSegment"
end
if not (Intersect.new(Line.new(-1.0, 2.0), LineSegment.new(1.0, 1.0, 4.0, 4.0)).eval_prog([]) == Point.new(1.0, 1.0))
  puts "Intersect test #2 fails for Line/LineSegment"
end

if not (Intersect.new(VerticalLine.new(4.0), Point.new(4.0, 8.0)).eval_prog([]) == Point.new(4.0, 8.0))
  puts "Intersect test #1 fails for VerticalLine/Point"
end
if not (Intersect.new(VerticalLine.new(4.0), Point.new(4.1, 4.0)).eval_prog([]).class == NoPoints.new.class)
  puts "Intersect test #2 fails for VerticalLine/Point"
end
if not (Intersect.new(VerticalLine.new(4.0), Line.new(4.0, 4.0)).eval_prog([]) == Point.new(4.0, 20.0))
  puts "Intersect test fails for VerticalLine/Line"
end
if not (Intersect.new(VerticalLine.new(4.0), VerticalLine.new(4.1)).eval_prog([]).class == NoPoints.new.class)
  puts "Intersect test #1 fails for VerticalLine/VerticalLine"
end
if not (Intersect.new(VerticalLine.new(4.0), VerticalLine.new(4.0)).eval_prog([]) == VerticalLine.new(4.0))
  puts "Intersect test #2 fails for VerticalLine/VerticalLine"
end
if not (Intersect.new(VerticalLine.new(4.1), LineSegment.new(1.0, 1.0, 4.0, 4.0)).eval_prog([]).class == NoPoints.new.class)
  puts "Intersect test #1 fails for VerticalLine/LineSegment"
end
if not (Intersect.new(VerticalLine.new(2.0), LineSegment.new(1.0, 1.0, 4.0, 4.0)).eval_prog([]) == Point.new(2.0, 2.0))
  puts "Intersect test #2 fails for VerticalLine/LineSegment"
end

# Intersection tests with LineSegment and Point/Line/VerticalLine
if not (Intersect.new(LineSegment.new(1.0, 1.0, 4.0, 4.0), Point.new(2.0, 2.0)).eval_prog([]) == Point.new(2.0, 2.0))
  puts "Intersect test #1 fails for LineSegment/Point"
end
if not (Intersect.new(LineSegment.new(1.0, 1.0, 4.0, 4.0), Point.new(4.1, 4.1)).eval_prog([]).class == NoPoints.new.class)
  puts "Intersect test #2 fails for LineSegment/Point"
end
if not (Intersect.new(LineSegment.new(1.0, 1.0, 4.0, 4.0), Line.new(-1.0, 1.0)).eval_prog([]).class == NoPoints.new.class)
  puts "Intersect test #1 fails for LineSegment/Line"
end
if not (Intersect.new(LineSegment.new(1.0, 1.0, 4.0, 4.0), Line.new(-1.0, 2.0)).eval_prog([]) == Point.new(1.0, 1.0))
  puts "Intersect test #2 fails for LineSegment/Line"
end
if not (Intersect.new(LineSegment.new(1.0, 1.0, 4.0, 4.0), VerticalLine.new(4.1)).eval_prog([]).class == NoPoints.new.class)
  puts "Intersect test #1 fails for LineSegment/VerticalLine"
end
if not (Intersect.new(LineSegment.new(1.0, 1.0, 4.0, 4.0), VerticalLine.new(2.0)).eval_prog([]) == Point.new(2.0, 2.0))
  puts "Intersect test #2 fails for LineSegment/VerticalLine"
end

# Intersection between a vertical LineSegment and Point/Line/VerticalLine
if not (Intersect.new(LineSegment.new(1.0, 1.0, 1.0, 4.0), Point.new(1.0, 2.0)).eval_prog([]) == Point.new(1.0, 2.0))
  puts "Intersect test #3 fails for LineSegment/Point"
end
if not (Intersect.new(LineSegment.new(1.0, 1.0, 1.0, 4.0), Point.new(1.0, 4.1)).eval_prog([]).class == NoPoints.new.class)
  puts "Intersect test #4 fails for LineSegment/Point"
end
if not (Intersect.new(LineSegment.new(1.0, 1.0, 1.0, 4.0), Line.new(-1.0, 1.0)).eval_prog([]).class == NoPoints.new.class)
  puts "Intersect test #3 fails for LineSegment/Line"
end
if not (Intersect.new(LineSegment.new(1.0, 1.0, 1.0, 4.0), Line.new(-1.0, 4.0)).eval_prog([]) == Point.new(1.0, 3.0))
  puts "Intersect test #4 fails for LineSegment/Line"
end
if not (Intersect.new(LineSegment.new(1.0, 1.0, 1.0, 4.0), VerticalLine.new(4.1)).eval_prog([]).class == NoPoints.new.class)
  puts "Intersect test #3 fails for LineSegment/VerticalLine"
end
if not (Intersect.new(LineSegment.new(1.0, 1.0, 1.0, 4.0), VerticalLine.new(1.0)).eval_prog([]) == LineSegment.new(1.0, 1.0, 1.0, 4.0))
  puts "Intersect test #4 fails for LineSegment/VerticalLine"
end

# intersection between two oblique LineSegments
if not (Intersect.new(LineSegment.new(1.0, 1.0, 4.0, 4.0), LineSegment.new(4.1, 4.1, 5.0, 5.0)).eval_prog([]).class == NoPoints.new.class)
  puts "Intersect test #1 fails for LineSegment/LineSegment"
end
if not (Intersect.new(LineSegment.new(1.0, 1.0, 4.0, 4.0), LineSegment.new(2.0, 2.0, 3.0, 3.0)).eval_prog([]) == LineSegment.new(2.0, 2.0, 3.0, 3.0))
  puts "Intersect test #2 fails for LineSegment/LineSegment"
end
if not (Intersect.new(LineSegment.new(1.0, 1.0, 4.0, 4.0), LineSegment.new(-1.0, -1.0, 3.0, 3.0)).eval_prog([]) == LineSegment.new(1.0, 1.0, 3.0, 3.0))
  puts "Intersect test #3 fails for LineSegment/LineSegment"
end
if not (Intersect.new(LineSegment.new(1.0, 1.0, 4.0, 4.0), LineSegment.new(2.0, 2.0, 5.0, 5.0)).eval_prog([]) == LineSegment.new(2.0, 2.0, 4.0, 4.0))
  puts "Intersect test #4 fails for LineSegment/LineSegment"
end
if not (Intersect.new(LineSegment.new(1.0, 1.0, 4.0, 4.0), LineSegment.new(4.0, 4.0, 5.0, 5.0)).eval_prog([]) == Point.new(4.0, 4.0))
  puts "Intersect test #5 fails for LineSegment/LineSegment"
end
if not (Intersect.new(LineSegment.new(1.0, 1.0, 4.0, 4.0), LineSegment.new(-4.0, -4.0, 1.0, 1.0)).eval_prog([]) == Point.new(1.0, 1.0))
  puts "Intersect test #6 fails for LineSegment/LineSegment"
end
if not (Intersect.new(LineSegment.new(2.0, 2.0, 3.0, 3.0), LineSegment.new(1.0, 1.0, 5.0, 5.0)).eval_prog([]) == LineSegment.new(2.0, 2.0, 3.0, 3.0))
  puts "Intersect test #7 fails for LineSegment/LineSegment"
end

# intersection between two vertical LineSegments
if not (Intersect.new(LineSegment.new(1.0, 1.0, 1.0, 4.0), LineSegment.new(1.0, 4.1, 1.0, 5.0)).eval_prog([]).class == NoPoints.new.class)
  puts "Intersect test #8 fails for LineSegment/LineSegment"
end
if not (Intersect.new(LineSegment.new(1.0, 1.0, 1.0, 4.0), LineSegment.new(1.0, 2.0, 1.0, 3.0)).eval_prog([]) == LineSegment.new(1.0, 2.0, 1.0, 3.0))
  puts "Intersect test #9 fails for LineSegment/LineSegment"
  puts Intersect.new(LineSegment.new(1.0, 1.0, 1.0, 4.0), LineSegment.new(1.0, 2.0, 1.0, 3.0)).eval_prog([]).class
end
if not (Intersect.new(LineSegment.new(1.0, 1.0, 1.0, 4.0), LineSegment.new(1.0, -1.0, 1.0, 3.0)).eval_prog([]) == LineSegment.new(1.0, 1.0, 1.0, 3.0))
  puts "Intersect test #10 fails for LineSegment/LineSegment"
end
if not (Intersect.new(LineSegment.new(1.0, 1.0, 1.0, 4.0), LineSegment.new(1.0, 2.0, 1.0, 5.0)).eval_prog([]) == LineSegment.new(1.0, 2.0, 1.0, 4.0))
  puts "Intersect test #11 fails for LineSegment/LineSegment"
end
if not (Intersect.new(LineSegment.new(1.0, 1.0, 1.0, 4.0), LineSegment.new(1.0, 4.0, 1.0, 5.0)).eval_prog([]) == Point.new(1.0, 4.0))
  puts "Intersect test #12 fails for LineSegment/LineSegment"
end
if not (Intersect.new(LineSegment.new(1.0, 1.0, 1.0, 4.0), LineSegment.new(1.0, -4.0, 1.0, 1.0)).eval_prog([]) == Point.new(1.0, 1.0))
  puts "Intersect test #136 fails for LineSegment/LineSegment"
end
if not (Intersect.new(LineSegment.new(1.0, 2.0, 1.0, 3.0), LineSegment.new(1.0, 1.0, 1.0, 5.0)).eval_prog([]) == LineSegment.new(1.0, 2.0, 1.0, 3.0))
  puts "Intersect test #14 fails for LineSegment/LineSegment"
end


