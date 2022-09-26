# UPLC Emacs Major Mode

## Just run it!

1. Install Emacs [package](https://github.com/zenozeng/yafolding.el)
   `yafolding`.
   - [Here](https://www.emacswiki.org/emacs/InstallingPackages)
   are some instructions on how to install a package in Emacs.
2. On emacs, `M-x load-file` and load `uplc-mode.el`.
3. Run `M-x uplc-mode`.

4. Position the cursor near a a `[` or `(`. Then press `C-c RET`. That
   "block" will be replaced by `...`. UPLC major mode uses `yafolding`
   which collapses thunks of text based on indentation. For example,
   ```scheme
   (lam case_BankState_4
        [ [ [ [ case_BankState_4 arg_0_0 ] arg_1_1 ] arg_2_2 ] arg_3_3 ]
   )
   ```
   becomes
   ```scheme
   (lam case_BankState_4 ...
   )
   ```

5. Position the cursor at the first column of a line such as the
   following one.
   ```scheme
   (lam 
     X
     [ ... ]
   )
   ```
   Now press `C-c C-k` and the resulting text should be as follows.
   ```scheme
   (lam X
     [ ... ]
   )
   ```

## Adding UPLC Emacs mode to `.emacs`

Just add the following configurations (or similar) to your `.emacs`
file, 
```scheme
(add-to-list 'load-path "/data/RV/plutus-core-semantics/editor-support/emacs/")
(require 'uplc-mode)
(add-to-list 'auto-mode-alist '("\\.uplc\\'" . uplc-mode))
```
where the path in `load-path` should be absolute (or one that emacs may find).
