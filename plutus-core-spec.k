module PLUTUS-CORE-SPEC
    imports PLUTUS-CORE

    rule <k> (con 1 ! 1      ) </k> => <k> int(1, 1)      </k>         [specification]
    rule <k> (con 1 ! 128    ) </k> => <k> (error)        </k>         [specification]
    rule <k> (con 1 ! -128   ) </k> => <k> int(1, -128)   </k>         [specification]
    rule <k> (con 1 ! -129   ) </k> => <k> (error)        </k>         [specification]

    rule <k> (con 2 !  32768 ) </k> => <k> (error)        </k>         [specification]
    rule <k> (con 2 ! -32768 ) </k> => <k> int(2, -32768) </k>         [specification]
    rule <k> (con 2 ! -32769 ) </k> => <k> (error)        </k>         [specification]
endmodule
