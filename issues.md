1. Typo in the `abs` rule for `checks`, lower case `k` should be capital `K`.
2. In `con` rule for `checks`, the condition with `forall` is not syntactically valid (does not parse as a `Type`). The application should be wrapped in `con`?
3. What do the Oxford (semantic) brackets mean?
4. Where is K coming from here: “Θ ⊢ c : (forall α K (fun T [κ α]))”?
5. In `decl-valid-define`, should the synthesis be a checking?
6. Section IV: `leads` -> `leaves`
7. Just to confirm, a program like `(define r [f 3])` is not valid?
