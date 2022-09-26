;;(require 'generic-x) ;; we need this

(define-generic-mode 
  'uplc-mode                        ;; name of the mode to create
  '("#")                            ;; comments start with '#' 
  '("lam" "builtin" "program"
    "force" "delay" "con"
    "Constr" "Map" "List"
    "Integer" "ByteString")         ;; some keywords
  '(("\\[" . 'font-lock-preprocessor-face)     ;; '[' is an operator
    ("\\]" . 'font-lock-preprocessor-face)     ;; ']' is an operator
    ;; Version constant
    ("[0-9]+.[0-9]+.[0-9]+" . 'font-lock-constant-face)
    ;; Type constants
    ("integer\\|data\\|bytestring\\|string\\|unit\\|bool\\|list\\|pair"
     .
     'font-lock-type-face)
    ;; Builtin for Integers
    ("addInteger\\|subtractInteger\\|multiplyInteger\\|divideInteger\\|quotientInteger\\|remainderInteger\\|modInteger\\|equalsInteger\\|lessThanInteger\\|lessThanEqualsInteger"
     .
     'font-lock-builtin-face)
    ;; Builtin for ByteStrings
    ("appendByteString\\|consByteString\\|sliceByteString\\|lengthOfByteString\\|indexByteString\\|equalsByteString\\|lessThanByteString\\|lessThanEqualsByteString"
     .
     'font-lock-builtin-face)
    ;; Builtin for crypto
    ("sha2_256\\|sha3_256\\|blake2b_256\\|verifySignature"
     .
     'font-lock-builtin-face)
    ;; Builtin for Strings
    ("appendString\\||equalsString\\|encodeUtf8\\|decodeUtf8"
     .
     'font-lock-builtin-face)
    ;; Polymorphic builtins
    ("ifThenElse\\|chooseUnit\\|trace"
     .
     'font-lock-builtin-face)
    ;; Builtin for pairs
    ( "fstPair\\|sndPair"
     .
     'font-lock-builtin-face)
    ;; Builtin for lists
    ("chooseList\\|mkCons\\|headList\\|tailList\\|nullList"
     .
     'font-lock-builtin-face)
    ;; Data builtins
    ("chooseData\\|constrData\\|mapData\\|listData\\|iData\\|bData\\|unConstrData\\|unMapData\\|unListData\\|unIData\\|unBData\\|equalsData\\|mkPairData\\|mkNilData\\|mkNilPairData"
     .
     'font-lock-builtin-face)
    ("[a-z][a-zA-Z0-9]*\\(_\\([0-9]+\\|[a-zA-Z]+\\)\\)*"
     .
     'font-lock-variable-name-face))           
  '("\\.uplc$")                     ;; files for which to activate this mode 
   nil                              ;; other functions to call
  "A mode for UPLC files."          ;; doc string for this mode
)
