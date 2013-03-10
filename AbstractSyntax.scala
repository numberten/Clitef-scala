import java.util._
/*
class Program(val globals:Declarations, val functions:Functions) {
   
   def display:Unit = {
      println("Display of Program Object")
      globals.display(0)
      functions.display(0)
   }
}
*/

class Program(val decpart:Declarations, val body:Block) {
   def display():Unit = {
      println("Display of Program Object")
      decpart.display(0)
      body.display(0)
   }
}

class Declarations() extends java.util.ArrayList[Declaration] {
   
   def display(j:Int):Unit = {
      for (i <- 0 to j) 
         print("\t")
      println("Display of Declarations Object")
      for (i <- 0 until this.size)
         this.get(i).display(j+1)
   }
}

class Declaration(val v:Variable, val t:Type) {
   def display(j:Int):Unit = {
      for (i <- 0 to j)
         print("\t")
      println("Display of Declaration Object")
      v.display(j + 1)
      t.display(j + 1)
   }
}

class Type(val id:String) {
//   val INT:Type = new Type("int")
//   val BOOL:Type = new Type("bool")
//   val CHAR:Type = new Type("char")
//   val FLOAT:Type = new Type("float")

   override def toString():String = { return id}
   def equals(t:Type):Boolean = { return t.id.equals(this.id) }
   def display(j:Int):Unit = {
      for (i <- 0 to j)
         print("\t")
      println("Display of Type Object: "+id)
   }
}
object DefaultType {
   val INT:Type = new Type("int")
   val BOOL:Type = new Type("bool")
   val CHAR:Type = new Type("char")
   val FLOAT:Type = new Type("float")
}

abstract class Statement() {
   def display(j:Int):Unit 
}

class Skip() extends Statement {
   def display(j:Int) {
      for (i <- 0 to j)
         print("\t")
      println("Display of Skip object")
   }
}

class Block() extends Statement {
   val members:java.util.ArrayList[Statement] = new ArrayList[Statement]()
   def display(j:Int):Unit = {
      for (i <- 0 to j)
         print("\t")
      println("Display of Block Object")
      for (i <- 0 until members.size)
         members.get(i).display(j+1)
   }
}

class Assignment(val target:Variable, val source:Expression) extends Statement {
   def display(j:Int):Unit = {
      for (i <- 0 to j)
         print("\t")
      println("Display of Assignment Object")
      target.display(j+1)
      source.display(j+1)
   }
}

class Conditional(val test:Expression, val thenbranch:Statement, val elsebranch:Statement) extends Statement {
   def this(t:Expression, tb:Statement) = this(t, tb, new Skip())

   def display(j:Int):Unit = {
      for (i <- 0 to j)
         print("\t")
      println("Display of Conditional Object")
      test.display(j+1)
      thenbranch.display(j+1)
      elsebranch.display(j+1)
   }
}

class Loop(val test:Expression, val body:Statement) extends Statement {
   def display(j:Int):Unit = {
      for (i <- 0 to j)
         print("\t")
      println("Display of Loop Object")
      test.display(j+1)
      body.display(j+1)
   }
}

abstract class Expression() {
   def display(j:Int):Unit
}

class Variable(val id:String) extends Expression {
   override def toString():String = { return id }
   override def equals(obj:Any):Boolean = {
      val varobj = obj match {
         case v: Variable => v
         case _ => throw new ClassCastException
      }
      id.equals(varobj.id)
   }
   def display(j:Int):Unit = {
      for (i <- 0 to j)
         print("\t")
      println("Display of Variable Object: "+id)
   }
   override def hashCode():Int = { return id.hashCode() }
}

abstract class Value extends Expression {
   var mytype:Type
   var undef:Boolean = true

   def intValue():Int = {
      assert(false, "should never reach here")
      return 0
   }
   def boolValue():Boolean = {
      assert(false, "should never reach here")
      return false
   }
   def charValue():Char = {
      assert(false, "should never reach here")
      return ' ';
   }
   def floatValue():Float = {
      assert(false, "should never reach here")
      return 0.0f
   }
   def isUndef():Boolean = { return undef }
   def `type`():Type = { return mytype }
    
    def mkValue(t:Type):Value = {
      if (t.equals(DefaultType.INT)) return new IntValue()
      if (t.equals(DefaultType.BOOL)) return new BoolValue()
      if (t.equals(DefaultType.CHAR)) return new CharValue()
      if (t.equals(DefaultType.FLOAT)) return new FloatValue()
      throw new IllegalArgumentException("Illegal type in mkValue")
   }
}

import DefaultType._

class IntValue(val value:Int) extends Value { 
   var mytype = DefaultType.INT
   def this() = { this(0);undef = false }
   override def intValue():Int = {
      assert(!undef, "reference to undefined int value")
      return value
   }
   override def toString():String = {
      if (undef)
         return "undef"
      return "" + value
   }
   def display(j:Int):Unit = {
      for (i <- 0 to j)
         print("\t")
      println("Display of IntValue Object: "+value)
   }
}

class BoolValue(val value:Boolean) extends Value {
   var mytype = DefaultType.BOOL
   def this() = { this(false);undef = false }
   override def boolValue():Boolean = {
      assert(!undef, "reference to undefined bool value")
      return value
   }
   override def intValue():Int = {
      assert(!undef, "reference to undefined bool value")
      if (value)
         return 1
      else
         return 0
   }
   override def toString():String = {
      if (undef) return "undef"
      return "" + value
   }

   def display(j:Int):Unit = {
      for (i <- 0 to j)
         print("\t")
      println("Display of BoolValue object: "+value)
   }
}

class CharValue(val value:Char) extends Value {
   var mytype = DefaultType.CHAR
   def this() = { this(' ');undef = false }
   override def charValue():Char = {
      assert(!undef, "reference to undefined char value")
      return value
   }
   override def toString():String = {
      if (undef) return "undef"
      return "" + value
   }
   def display(j:Int):Unit = {
      for (i <- 0 to j)
         print("\t")
      println("Display of CharValue Object: '"+ value + "'")
   }
}

class FloatValue(val value:Float) extends Value {
   var mytype = DefaultType.FLOAT
   def this() = { this(0);undef = false }
   override def floatValue():Float = {
      assert(!undef, "reference to undefined float value")
      return value
   }
   override def toString():String = {
      if (undef) return "undef"
      return "" + value
   }
   def display(j:Int):Unit = {
      for (i <- 0 to j)
         print("\t")
      println("Display of FloatValue Object: " + value)
   }
}

class Binary(val op:Operator, val term1:Expression, val term2:Expression) extends Expression {
   def display(j:Int):Unit = {
      for (i <- 0 to j)
         print("\t")
      println("Display of Binary Object")
      op.display(j+1)
      term1.display(j+1)
      term2.display(j+1)
   }
}

class Unary(val op:Operator, val term:Expression) extends Expression {
   def display(j:Int) = {
      for (i <- 0 to j)
         print("\t")
      println("Display of Unary Object")
      op.display(j+1)
      term.display(j+1)
   }
}

class Operator(val v:String) {

    // Operator = BooleanOp | RelationalOp | ArithmeticOp | UnaryOp
    // BooleanOp = && | ||
    val AND = "&&";
    val OR = "||";
    // RelationalOp = < | <= | == | != | >= | >
    val LT = "<";
    val LE = "<=";
    val EQ = "==";
    val NE = "!=";
    val GT = ">";
    val GE = ">=";
    // ArithmeticOp = + | - | * | /
    val PLUS = "+";
    val MINUS = "-";
    val TIMES = "*";
    val DIV = "/";
    // UnaryOp = !    
    val NOT = "!";
    val NEG = "-";
    // CastOp = int | float | char
    val INT = "int";
    val FLOAT = "float";
    val CHAR = "char";
    // Typed Operators
    // RelationalOp = < | <= | == | != | >= | >
    val INT_LT = "INT<";
    val INT_LE = "INT<=";
    val INT_EQ = "INT==";
    val INT_NE = "INT!=";
    val INT_GT = "INT>";
    val INT_GE = "INT>=";
    // ArithmeticOp = + | - | * | /
    val INT_PLUS = "INT+";
    val INT_MINUS = "INT-";
    val INT_TIMES = "INT*";
    val INT_DIV = "INT/";
    // UnaryOp = !    
    val INT_NEG = "-";
    // RelationalOp = < | <= | == | != | >= | >
    val FLOAT_LT = "FLOAT<";
    val FLOAT_LE = "FLOAT<=";
    val FLOAT_EQ = "FLOAT==";
    val FLOAT_NE = "FLOAT!=";
    val FLOAT_GT = "FLOAT>";
    val FLOAT_GE = "FLOAT>=";
    // ArithmeticOp = + | - | * | /
    val FLOAT_PLUS = "FLOAT+";
    val FLOAT_MINUS = "FLOAT-";
    val FLOAT_TIMES = "FLOAT*";
    val FLOAT_DIV = "FLOAT/";
    // UnaryOp = !    
    val FLOAT_NEG = "-";
    // RelationalOp = < | <= | == | != | >= | >
    val CHAR_LT = "CHAR<";
    val CHAR_LE = "CHAR<=";
    val CHAR_EQ = "CHAR==";
    val CHAR_NE = "CHAR!=";
    val CHAR_GT = "CHAR>";
    val CHAR_GE = "CHAR>=";
    // RelationalOp = < | <= | == | != | >= | >
    val BOOL_LT = "BOOL<";
    val BOOL_LE = "BOOL<=";
    val BOOL_EQ = "BOOL==";
    val BOOL_NE = "BOOL!=";
    val BOOL_GT = "BOOL>";
    val BOOL_GE = "BOOL>=";
    // Type specific cast
    val I2F = "I2F";
    val F2I = "F2I";
    val C2I = "C2I";
    val I2C = "I2C";

   def display(j:Int):Unit = {
      for (i <- 0 to j)
         print("\t")
      println("Display of Operator Object: "+v)
   }

   override def toString():String = { return v }
   override def equals(obj:Any):Boolean = { return v.equals(obj) }

   def BooleanOp():Boolean = { return v.equals(AND) || v.equals(OR) }
   def RelationalOp():Boolean = { return v.equals(LT) || v.equals(LE) || v.equals(EQ) ||
                                         v.equals(NE) || v.equals(GT) || v.equals(GE) }
   def ArithmeticOp():Boolean = { return v.equals(PLUS) || v.equals(MINUS) || v.equals(TIMES) || v.equals(DIV) }
   def NotOp():Boolean = { return v.equals(NOT) }
   def NegateOp():Boolean = { return v.equals(NEG) }
   def intOp():Boolean = { return v.equals(INT) }
   def floatOp():Boolean = { return v.equals(FLOAT) }
   def charOp():Boolean = { return v.equals(CHAR) }

    val intMap:Array[(String, String)] = Array(
        (PLUS, INT_PLUS), (MINUS, INT_MINUS),
        (TIMES, INT_TIMES), (DIV, INT_DIV),
        (EQ, INT_EQ), (NE, INT_NE), (LT, INT_LT),
        (LE, INT_LE), (GT, INT_GT), (GE, INT_GE),
        (NEG, INT_NEG), (FLOAT, I2F), (CHAR, I2C)
    );

    val floatMap:Array[(String, String)] = Array(
        (PLUS, FLOAT_PLUS), (MINUS, FLOAT_MINUS),
        (TIMES, FLOAT_TIMES), (DIV, FLOAT_DIV),
        (EQ, FLOAT_EQ), (NE, FLOAT_NE), (LT, FLOAT_LT),
        (LE, FLOAT_LE), (GT, FLOAT_GT), (GE, FLOAT_GE),
        (NEG, FLOAT_NEG), (INT, F2I)
    );

    val charMap:Array[(String,String)] = Array(
        (EQ, CHAR_EQ), (NE, CHAR_NE), (LT, CHAR_LT),
        (LE, CHAR_LE), (GT, CHAR_GT), (GE, CHAR_GE),
        (INT, C2I)
    );

    val boolMap:Array[(String,String)] = Array(
        (EQ, BOOL_EQ), (NE, BOOL_NE), (LT, BOOL_LT),
        (LE, BOOL_LE), (GT, BOOL_GT), (GE, BOOL_GE), (OR,OR), (AND,AND), (NOT,NOT)
    );

    def map(tmap:Array[(String,String)], op:String):Operator = {
      for (i <- 0 until tmap.length) {
         if (tmap(i)._1.equals(op))
            return new Operator(tmap(i)._2)
      }
      assert(false, "should never reach here")
      return null
   }

    def intMap(op:String):Operator = { return map(intMap, op) }
    def floatMap(op:String):Operator = { return map(floatMap, op) }
    def boolMap(op:String):Operator = { return map(boolMap, op) }
    def charMap(op:String):Operator = { return map(charMap, op) }


}
