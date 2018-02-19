module IMP-SYNTAX

  syntax Id ::= "main" [token]

  syntax Decl ::= "var" IELEExps ";"
                | "function" Id "(" Ids ")" Block

  syntax IELEExp ::= Int | String | Id // | Bool
               | "(" IELEExp ")"             [bracket]
               | "++" IELEExp
               > IELEExp "[" IELEExps "]"        [strict]
               > IELEExp "(" IELEExps ")"        [strict]
               | "-" IELEExp                 [strict]
               | "sizeOf" "(" IELEExp ")"    [strict]
               | "read" "(" ")"
               > left:
                 IELEExp "*" IELEExp             [strict, left]
               | IELEExp "/" IELEExp             [strict, left]
               | IELEExp "%" IELEExp             [strict, left]
               > left:
                 IELEExp "+" IELEExp             [strict, left]
               | IELEExp "-" IELEExp             [prefer, strict, left]
               > non-assoc:
                 IELEExp "<" IELEExp             [strict, non-assoc]
               | IELEExp "<=" IELEExp            [strict, non-assoc]
               | IELEExp ">" IELEExp             [strict, non-assoc]
               | IELEExp ">=" IELEExp            [strict, non-assoc]
               | IELEExp "==" IELEExp            [strict, non-assoc]
               | IELEExp "!=" IELEExp            [strict, non-assoc]
               > "!" IELEExp                 [strict]
               > left:
                 IELEExp "&&" IELEExp            [strict(1), left]
               | IELEExp "||" IELEExp            [strict(1), left]
               > "spawn" Block
               > IELEExp "=" IELEExp             [strict(2), right]

  syntax Ids  ::= List{Id,","}
  syntax IELEExps ::= List{IELEExp,","}          [strict]  // automatically hybrid now

  syntax Block ::= "{" "}"
                | "{" Stmts "}"

  syntax Stmt ::= Decl | Block
                | IELEExp ";"                               [strict]
                | "if" "(" IELEExp ")" Block "else" Block   [avoid, strict(1)]
                | "if" "(" IELEExp ")" Block
                | "while" "(" IELEExp ")" Block
                | "for" "(" Stmts IELEExp ";" IELEExp ")" Block
                | "return" IELEExp ";"                      [strict]
                | "return" ";"
                | "print" "(" IELEExps ")" ";"              [strict]
                | "try" Block "catch" "(" Id ")" Block
                | "throw" IELEExp ";"                       [strict]
                | "join" IELEExp ";"                        [strict]
                | "acquire" IELEExp ";"                     [strict]
                | "release" IELEExp ";"                     [strict]
                | "rendezvous" IELEExp ";"                  [strict]

  syntax Stmts ::= Stmt
                 | Stmts Stmts                          [right]

endmodule