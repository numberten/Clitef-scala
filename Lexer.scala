//Lexer!!!

import java.io._

object LexerTest {

    class Lexer(val fileName:String) { // source filename
    
    var isEof:Boolean = false;
    var ch:Char = ' '; 
    var input:BufferedReader = _;
    var line:String = "";
    var lineno:Int = 0;
    var col:Int = 1;
    val letters:String = "abcdefghijklmnopqrstuvwxyz" + "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    val digits:String = "0123456789";
    val eolnCh:Char = '\n';
    val eofCh:Char = '\004';

        try {
            input = new BufferedReader (new FileReader(fileName));
        }
      catch {
         case e:FileNotFoundException => println("File not found: " + fileName);System.exit(1);
         case unknown => println("Unknown exception " + unknown);System.exit(-1);
            }

    def nextChar():Char = { // Return next char
        if (ch == eofCh)
            myerror("Attempt to read past end of file");
        col = col + 1
        if (col >= line.length()) {
            try {
               line = input.readLine( );
            }
            catch {
               case e:IOException => System.err.println(e);System.exit(1);
               case _ => println("Unknown exception in nextChar()");System.exit(-1);
            }
            if (line == null) // at end of file
               line = "" + eofCh;
            else {
               println(lineno + ":\t" + line);
               lineno = lineno + 1;
               line += eolnCh;
            }
            col = 0;
        } // if col
        line.charAt(col);
    }
            
   def isSkip(x:Char):Boolean = {
      if (x == ' ' || x == '\t' || x == '\r' || x == eolnCh)
         true
      else
         false
   }

   def foundCharLit():Token = { //comment out, for inferred Unit type, due to Token/Unit type mismatch...   :Token = {
      var ch1:Char = nextChar()
      val ch2:Char = nextChar()
      if (ch2 == '\'') {
      ch = nextChar()
      Token.mkCharLiteral("" + ch1)
      }
      else {
         myerror("Attempt to delcare multiple-character character literal.")
         Token.commaTok //type system hack, will never reach here
      }
   }

   def foundDivLit():Boolean = {
      ch = nextChar()
      if (ch != '/') {
         true //Token.divideTok
      } else {
            do {
               ch = nextChar()
               }
            while (ch != eolnCh)
               ch = nextChar()
               false //found a comment
            }
   }


   def next():Token = { // Return next token
      var myToken:Token = Token.commaTok //temporary
      while (true) {
            if (isLetter(ch)) { // ident or keyword
                val spelling:String = concat(letters + digits);
                return Token.keyword(spelling);
            } else if (isDigit(ch)) { // int or float literal
                var number:String = concat(digits);
                if (ch != '.')  // int Literal
                    return Token.mkIntLiteral(number);
                number += concat(digits);
                return Token.mkFloatLiteral(number);
            } else ch match {
                  case x if (isSkip(x)) => ch = nextChar();
                  case '/' if (foundDivLit == true) => return Token.divideTok
                  case '\'' => return foundCharLit
                  case '\004' => return Token.eofTok
                  case '+' => ch = nextChar(); return Token.plusTok
                  case '-' => ch = nextChar(); return Token.minusTok;
                  case '*' => ch = nextChar(); return Token.multiplyTok;
                  case '(' => ch = nextChar(); return Token.leftParenTok;
                  case ')' => ch = nextChar(); return Token.rightParenTok;
                  case '{' => ch = nextChar(); return Token.leftBraceTok;
                  case '}' => ch = nextChar(); return Token.rightBraceTok;
                  case ';' => ch = nextChar(); return Token.semicolonTok;
                  case ',' => ch = nextChar(); return Token.commaTok;
                  case '&' => check('&'); return Token.andTok;
                  case '|' => check('|'); return Token.orTok;
                  case '=' => return chkOpt('=', Token.assignTok, Token.eqeqTok);
                  case '<' => return chkOpt('=', Token.ltTok, Token.lteqTok);
                  case '>' => return chkOpt('=', Token.gtTok, Token.gteqTok);
                  case '!' => return chkOpt('=', Token.notTok, Token.noteqTok);
                  case _   => myerror("Illegal character " + ch);
                  }
               }
               //should never reach here
               println("If you see this, something bad happened")
               return myToken
   } 

   def isLetter(c:Char):Boolean = { (c>='a' && c<='z' || c>='A' && c<='Z') }

   def isDigit(c:Char):Boolean = { (c>='0' && c<='9') }

   def check(c:Char) = {
      ch = nextChar()
      if (ch != c)
         myerror("Illegal character, expecting " + c)
      ch = nextChar()
   }
   
   def chkOpt(c:Char, one:Token, two:Token):Token = {
      ch = nextChar()
      if (ch != c)
         return one
      ch = nextChar()
      two
   }
  
  def concat(set:String):String = {
   var r:String = ""
   do {
      r += ch
      ch = nextChar()
      } while (set.indexOf(ch) >= 0)
      r
   }

   def myerror(msg:String) = {
      System.err.print(line)
      System.err.println("Error: column " + col + " " + msg)
      System.exit(1)
   }
}

   def main(args: Array[String]) = {
      val lexer = new Lexer(args(0))
      var tok = lexer.next()
      while (tok != Token.eofTok) {
         println(tok.toString())
         tok = lexer.next()
      }
   }
}

