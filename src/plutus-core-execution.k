require "plutus-core-syntax.k"
require "substitution.k"

module PLUTUS-CORE-EXECUTION
  imports PLUTUS-CORE-SYNTAX
  imports SUBSTITUTION

  configuration
    <T>
      <k> $PGM:Tm </k>
      <env> .Map </env>
    </T>

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

  // Local reductions.
  rule [ ( lam X M ) (N:Tm) ]  => M[N / X]
  rule ( let N:Tm X:Var M:Tm ) => M[N / X]
  rule failureE => failureV

  rule
    <k> ( define N:Name M:Tm ) </k>
    <env> E => E[N <- M] </env>

endmodule
