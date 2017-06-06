require "plutus-core-syntax.k"

module PLUTUS-CORE-EXECUTION
  imports PLUTUS-CORE-SYNTAX
  imports INT

  /***************************************************************************/
  /***********************  BUILT-IN REDUCTIONS   ****************************/
  /***************************************************************************/

  rule ( builtin addInt:Name       ( I1:Int I2:Int ) ) => I1 +Int I2
  rule ( builtin subtractInt:Name  ( I1:Int I2:Int ) ) => I1 -Int I2
  rule ( builtin multiplyInt:Name  ( I1:Int I2:Int ) ) => I1 *Int I2
  /*rule ( builtin divideInt:Name    ( I1:Int I2:Int ) ) => I1 /Int I2*/
    /*when I2 =/=K 0*/
  /*rule ( builtin remainderInt:Name ( I1:Int I2:Int ) ) => I1 %Int I2*/
    /*when I2 =/=K 0*/
  /*rule ( builtin lessThanInt:Name  ( I1:Int I2:Int ) ) => I1 <Int I2*/
  /*rule ( builtin equalsInt:Name    ( I1:Int I2:Int ) ) => I1 ==K I2*/

endmodule
