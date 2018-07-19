```k
module PLUTUS-CORE-SPEC
    imports PLUTUS-CORE

    rule <k> 1 ! 1      </k> => <k> int(1, 1)               </k>                    [specification]
    rule <k> 1 ! 128    </k> => <k> (error (con (integer))) </k>                    [specification]
    rule <k> 1 ! -128   </k> => <k> int(1, -128)            </k>                    [specification]
    rule <k> 1 ! -129   </k> => <k> (error (con (integer))) </k>                    [specification]

    rule <k> 2 !  32768 </k> => <k> (error (con (integer))) </k>                    [specification]
    rule <k> 2 ! -32768 </k> => <k> int(2, -32768)          </k>                    [specification]
    rule <k> 2 ! -32769 </k> => <k> (error (con (integer))) </k>                    [specification]
endmodule
```
