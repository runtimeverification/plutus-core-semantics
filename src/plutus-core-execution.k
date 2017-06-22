require "plutus-core-syntax.k"
require "substitution.k"

module PLUTUS-CORE-EXECUTION
  imports PLUTUS-CORE-SYNTAX
  imports SUBSTITUTION

  configuration
    <T> <k> $PGM:Tm </k> </T>

  /***************************************************************************/
  /***********************  BUILT-IN REDUCTIONS   ****************************/
  /***************************************************************************/

  rule ( builtin addInt:Name       ( I1:Int I2:Int ) ) => I1 +Int I2
  rule ( builtin subtractInt:Name  ( I1:Int I2:Int ) ) => I1 -Int I2
  rule ( builtin multiplyInt:Name  ( I1:Int I2:Int ) ) => I1 *Int I2
  rule ( builtin divideInt:Name    ( I1:Int I2:Int ) ) => I1 /Int I2
    when I2 =/=Int 0
  rule ( builtin remainderInt:Name ( I1:Int I2:Int ) ) => I1 %Int I2
    when I2 =/=Int 0
  rule ( builtin lessThanInt:Name  ( I1:Int I2:Int ) ) => I1 <Int I2
  rule ( builtin equalsInt:Name    ( I1:Int I2:Int ) ) => I1 ==Int I2

  rule ( builtin addFloat:Name      ( F1:Float F2:Float ) ) => F1 +Float F2
  rule ( builtin subtractFloat:Name ( F1:Float F2:Float ) ) => F1 -Float F2
  rule ( builtin multiplyFloat:Name ( F1:Float F2:Float ) ) => F1 *Float F2
  rule ( builtin divideFloat:Name   ( F1:Float F2:Float ) ) => F1 /Float F2
  rule ( builtin lessThanFloat:Name ( F1:Float F2:Float ) ) => F1 <Float F2
  rule ( builtin equalsFloat:Name   ( F1:Float F2:Float ) ) => F1 ==Float F2

  rule ( builtin ceil:Name  ( F:Float ) ) => ceilFloat(F:Float)
  rule ( builtin floor:Name ( F:Float ) ) => floorFloat(F:Float)
  /*rule ( builtin round:Name ( F:Float ) ) => roundFloat(F:Float)*/

  syntax Ctx ::= "EmptyCtx"
               | "(" "let" Ctx Var Tm ")"
               | "[" Ctx Tm  "]"
               | "[" Val Ctx "]"
               | "(" "con" QualC ValList Ctx TmList ")"
               | "(" "case" Ctx ClList ")"
               | "(" "success" Ctx ")"
               | "(" "bind" Ctx Var Tm ")"
               | "(" "builtin" Name ValList Ctx TmList ")"
               | Ctx "{" Tm "}"

  // Context insertion.
  syntax CtxInsrt ::= Ctx "{" Tm "}"

  rule EmptyCtx                             { N:Tm } => N
  rule ( let K:Ctx X:Var M:Tm )  { N:Tm } => ( let K { N } X M )
  rule [ K:Ctx M:Tm   ]          { N:Tm } => [ K { N } M ]
  rule [ M:Val  K:Ctx ]          { N:Tm } => [ M K { N } ]
  rule ( case K:Ctx CS:ClList )  { N:Tm } => ( case ( K { N } ) CS )
  rule ( success K:Ctx )         { N:Tm } => ( success K { N } )
  rule ( bind K:Ctx X:Var M:Tm ) { N:Tm } => ( bind ( K { N } ) X M )
  rule ( builtin NM:Name VS:ValList K:Ctx MS:TmList ) { N }
    => ( builtin NM VS K { N } MS )
  rule ( con Q:QualC VS:ValList K:Ctx L:TmList ) { N }
    => ( con Q VS ( K { N } ) L )

  syntax Instr ::= "(" "success" Val ")"
                 | "(" "failure" ")"
                 | "(" "txhash" ")"
                 | "(" "blocknum" ")"
                 | "(" "blocktime" ")"
                 | "(" "bind" Tm Var Tm ")"

  // Local reductions.
  rule [ ( lam X M ) (N:Tm) ] => M[N / X]

endmodule
