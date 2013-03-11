import java.util._
import LexerTest._

object ParserTester {

   class Parser(ts:Lexer) {
      var token:Token = _
      var lexer:Lexer = _
  
      lexer = ts
      token = lexer.next()

   def mymatch(t:TokenType):String = {
      val value = token.value()
      if (token.`type`().equals(t))
         token = lexer.next()
      else
         myerror(t)
      return value
   }

   def myerror(tok:TokenType):Unit = { System.err.println("Syntax error: expecting: " + tok + "; saw: " + token);System.exit(1); }

   def myerror(s:String):Unit = { System.err.println("Syntax error: expecting: " + s + "; saw: " + token);System.exit(1) }
  
   def program():Program = {
      val g:Declarations = declarations() //match globals
      val f:Functions = functions() //match functions
      return new Program(g, f)
   }
  /* 
   def program():Program = {
      val header:Array[TokenType] = Array(TokenType.Int, TokenType.Main, TokenType.LeftParen, TokenType.RightParen)
      for (t <- header)
         mymatch(t)
      mymatch(TokenType.LeftBrace)
      val d:Declarations = declarations()
      val b:Block = statements()
      mymatch(TokenType.RightBrace)
      return new Program(d,b)
   }
   */

   def functions():Functions = {
      val f:Functions = new Functions()
      while (token.`type`().equals(TokenType.Bool) || token.`type`().equals(TokenType.Char) || token.`type`().equals(TokenType.Float) || token.`type`().equals(TokenType.Int) || token.`type`().equals(TokenType.Void)) {
         function(f)
      }
      return f
   }

   def expressions():Expressions = {
      val e:Expressions = new Expressions()
      if (token.`type`().equals(TokenType.RightParen)) {
         mymatch(TokenType.RightParen)
         return e
      }
      e.add(expression())
      while (token.`type`().equals(TokenType.Comma)) {
         mymatch(TokenType.Comma)
         e.add(expression())
      }
      return e
   }

   def declarations():Declarations = {
      val d:Declarations = new Declarations()
      while (token.`type`().equals(TokenType.Bool) || token.`type`().equals(TokenType.Char) || token.`type`().equals(TokenType.Float) || token.`type`(). equals(TokenType.Int)) {
         declaration(d)
      }
      return d
   }

   def declarationsp():Declarations = {
      val d:Declarations = new Declarations()
      if (token.`type`().equals(TokenType.Bool) || token.`type`().equals(TokenType.Char) || token.`type`().equals(TokenType.Float) || token.`type`(). equals(TokenType.Int)) 
         singleD(d)
      while (!(token.`type`().equals(TokenType.RightParen))) {
            mymatch(TokenType.Comma)
            singleD(d)
      }
      return d
   }

   def singleD(ds:Declarations):Unit = {
      val current_type:Type = new Type(token.value())
      token = lexer.next()
      val current_variable:Variable = new Variable(mymatch(TokenType.Identifier))
      ds.add(new Declaration(current_variable, current_type))
   }
         
   def declaration(ds:Declarations):Unit = {
      val current_type:Type = new Type(token.value())
      do {
         token = lexer.next()
         var current_variable:Variable = new Variable(mymatch(TokenType.Identifier))
         ds.add(new Declaration(current_variable, current_type)) }
      while (token.`type`().equals(TokenType.Comma))
         mymatch(TokenType.Semicolon)
   }

   var lastF:Variable = _

   def function(fs:Functions):Unit = {
      val current_type:Type = new Type(token.value())
      var name:Variable = new Variable("")
      //get function maaaaan
      //class Function(val t:Type, val id:Variable, val params:Declarations, val locals:Declarations, val body:Block) 
      token = lexer.next()
      if (token.`type`().equals(TokenType.Main)) {
         name = new Variable(mymatch(TokenType.Main))
      } else {
         name = new Variable(mymatch(TokenType.Identifier))
      }
      lastF = name
      mymatch(TokenType.LeftParen)
      val p:Declarations = declarationsp()
      mymatch(TokenType.RightParen)
      mymatch(TokenType.LeftBrace)
      val l:Declarations = declarations()
      val b:Block = statements()
      mymatch(TokenType.RightBrace)
      println("Totally just matched a function called: "+name)
      fs.add(new Function(current_type, name, p, l, b))
   }
  
   def statement():Statement = {
      val s:Statement = new Skip()
      if (token.`type`().equals(TokenType.Identifier)) {
         val v:Variable = new Variable(mymatch(TokenType.Identifier))
         if (token.`type`().equals(TokenType.LeftParen)) {
            return callS(v) }
         return assignment(v) }
      else if (token.`type`().equals(TokenType.Return)) {
         mymatch(TokenType.Return)
         val result:Expression = expression()
         mymatch(TokenType.Semicolon)
         return new Return(lastF,result)
      }
      else if (token.`type`().equals(TokenType.LeftBrace)) {
         mymatch(TokenType.LeftBrace)
         val b:Statement = statements()
         mymatch(TokenType.RightBrace)
         return b }
      else if (token.`type`().equals(TokenType.If)) {
         return ifStatement() }
      else if (token.`type`().equals(TokenType.While)) {
         return whileStatement() }
      mymatch(TokenType.Semicolon)
      return s
   }

   def statements():Block = {
      val b:Block = new Block
      while (!token.`type`().equals(TokenType.RightBrace)) { b.members.add(statement()) }
      return b
   }

   def callS(name:Variable):SCall = {
      mymatch(TokenType.LeftParen)
      val args:Expressions = expressions()
      mymatch(TokenType.RightParen)
      mymatch(TokenType.Semicolon)
      return new SCall(name, args)
   }  

   def assignment(target:Variable):Assignment = {
      //val target:Variable = new Variable(mymatch(TokenType.Identifier))
      mymatch(TokenType.Assign)
      val source:Expression = expression
      mymatch(TokenType.Semicolon)
      return new Assignment(target, source)
   }

   def ifStatement():Conditional = {
      mymatch(TokenType.If)
      mymatch(TokenType.LeftParen)
      val e:Expression = expression()
      mymatch(TokenType.RightParen)
      val s1:Statement = statement()
      if (token.`type`().equals(TokenType.Else)) {
         mymatch(TokenType.Else)
         val s2:Statement = statement()
         return new Conditional(e, s1, s2) }
      return new Conditional(e, s1)
   }

   def whileStatement():Loop = {
      mymatch(TokenType.While)
      mymatch(TokenType.LeftParen)
      val e:Expression = expression()
      mymatch(TokenType.RightParen)
      val s1:Statement = statement()
      return new Loop(e, s1)
   }

   def expression():Expression = {
      var e:Expression = conjunction()
      while (isOrOp()) {
         val op:Operator = new Operator(mymatch(token.`type`()))
         val term2:Expression = conjunction()
         e = new Binary(op, e, term2)
      }
      return e
   }

   def conjunction():Expression = {
      var e:Expression = equality()
      while (isAndOp()) {
         val op:Operator = new Operator(mymatch(token.`type`()))
         val term2:Expression = equality()
         e = new Binary(op, e, term2)
      }
      return e
   }
  
   def equality():Expression = {
      var e:Expression = relation()
      while (isEqualityOp()) {
         val op:Operator = new Operator(mymatch(token.`type`()))
         val term2:Expression = relation()
         e = new Binary(op, e, term2)
      }
      return e
   }

   def relation():Expression = {
      var e:Expression = addition()
      while (isRelationalOp()) {
         val op:Operator = new Operator(mymatch(token.`type`()))
         val term2:Expression = addition()
         e = new Binary(op, e, term2)
      }
      return e
   }

   def addition():Expression = {
      var e:Expression = term()
      while (isAddOp()) {
         val op:Operator = new Operator(mymatch(token.`type`()))
         val term2:Expression = term()
         e = new Binary(op, e, term2)
      }
      return e
   }
  
   def term():Expression = {
      var e:Expression = factor()
      while (isMultiplyOp()) {
         val op:Operator = new Operator(mymatch(token.`type`()))
         val term2:Expression = factor()
         e = new Binary(op, e, term2)
      }
      return e
   }
  
   def factor():Expression = {
      if (isUnaryOp()) {
         val op:Operator = new Operator(mymatch(token.`type`()))
         val term:Expression = primary()
         return new Unary(op, term)
      }
      else return primary()
   }

   def primary():Expression = {
      var e:Expression = null
      if (token.`type`().equals(TokenType.Identifier)) {
         e = new Variable(mymatch(TokenType.Identifier))
         if (token.`type`().equals(TokenType.LeftParen)) {
            mymatch(TokenType.LeftParen)
            val args:Expressions = expressions()
            mymatch(TokenType.RightParen)
            return new ECall(e.asInstanceOf[Variable], args)
         }
      } else if (isLiteral()) { e = literal() }
        else if (token.`type`().equals(TokenType.LeftParen)) {
            token = lexer.next()
            e = expression()
            mymatch(TokenType.RightParen)
            } else if (isType()) {
                  val op:Operator = new Operator(mymatch(token.`type`()))
                  mymatch(TokenType.LeftParen)
                  val term:Expression = expression()
                  mymatch(TokenType.RightParen)
                  e = new Unary(op, term)
               } else myerror("Call | Identifier | Literal | ( | Type")
               return e
            }


   def literal():Value = {
      val tval:String = token.value();
      if (token.`type`().equals(TokenType.IntLiteral))
      {
         token = lexer.next();
	 return new IntValue(Integer.parseInt(tval));
      }
      else if (token.`type`().equals(TokenType.CharLiteral))
      {
         token = lexer.next();
         return new CharValue(tval.charAt(0));
      }
      else if (token.`type`().equals(TokenType.FloatLiteral))
      {
         token = lexer.next();
	 return new FloatValue(tval.toFloat);
      }
      else if (token.`type`().equals(TokenType.True))
      {
         token = lexer.next();
	 return new BoolValue(true);
      }
      else if (token.`type`().equals(TokenType.False))
      {
         token = lexer.next();
	 return new BoolValue(false);
      }
      else myerror("Unsure what type of literal this token is!");
      return null; //should never reach here
   }

   def isOrOp():Boolean = { return token.`type`().equals(TokenType.Or) }

   def isAndOp():Boolean = { return token.`type`().equals(TokenType.And) }

   def isAddOp():Boolean = { return token.`type`().equals(TokenType.Plus) || token.`type`().equals(TokenType.Minus) }

   def isMultiplyOp():Boolean = { return token.`type`().equals(TokenType.Multiply) || token.`type`().equals(TokenType.Divide) }

   def isUnaryOp():Boolean = { return token.`type`().equals(TokenType.Not) || token.`type`().equals(TokenType.Minus) }

   def isEqualityOp():Boolean = { return token.`type`().equals(TokenType.Equals) || token.`type`().equals(TokenType.NotEqual) }

   def isRelationalOp():Boolean = { return token.`type`().equals(TokenType.Less) ||
                                         token.`type`().equals(TokenType.LessEqual) ||
                                         token.`type`().equals(TokenType.Greater) ||
                                         token.`type`().equals(TokenType.GreaterEqual) }

   def isType():Boolean = { return token.`type`().equals(TokenType.Int) ||
                                   token.`type`().equals(TokenType.Bool) ||
                                   token.`type`().equals(TokenType.Float) ||
                                   token.`type`().equals(TokenType.Char) }

   def isLiteral():Boolean = { return token.`type`().equals(TokenType.IntLiteral) ||
                                      token.`type`().equals(TokenType.FloatLiteral) ||
                                      token.`type`().equals(TokenType.CharLiteral) ||
                                      isBooleanLiteral() }

   def isBooleanLiteral():Boolean = { return token.`type`().equals(TokenType.True) ||
                                             token.`type`().equals(TokenType.False) }

   }

   def main(args: Array[String]):Unit = {
      val parser:Parser = new Parser(new Lexer(args(0)))
      val prog:Program = parser.program()
      prog display
   }
}    
