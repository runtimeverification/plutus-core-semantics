#!/usr/bin/env python3

from kninja import *
import sys

# Helpers
#
class Plutus(KProject):
    def __init__(self):
        super().__init__(builddir = '.build')
        self.include('lib/build.ninja')
        self.lazy   = self.kdefinition( name    = 'lazy'
                                      , main    = self.tangleddir('plutus-core.k')
                                      , backend = 'java'
                                      , alias   = 'lazy'
                                      )
        self.strict = self.kdefinition( name    = 'strict'
                                      , main    = self.tangleddir('plutus-core.k')
                                      , backend = 'java'
                                      , alias   = 'strict'
                                      )
        self.ocaml = self.kdefinition_no_build( name             = 'ocaml'
                                              , kompiled_dirname = 'plutus-core-kompiled'
                                              , alias            = 'spec-ocaml'
                                              )
        self.testdir = '$builddir/t/'
        self.lazy_tests = []
        self.strict_tests = []
        self.ocaml_tests = []

    def write_aliases(self):
        self.build('t/lazy', 'phony', inputs = self.lazy_tests) 
        self.build('t/strict', 'phony', inputs = self.strict_tests) 
        self.build('t/ocaml', 'phony', inputs = self.ocaml_tests) 

    def test(self, input):
        expected = input + '.expected'
        self.lazy_tests += self.lazy.krun_and_check ('$builddir/t/', input, expected)
        self.strict_tests += self.strict.krun_and_check('$builddir/t/', input, expected)
        self.ocaml_tests += self.ocaml.krun_and_check('$builddir/t/', input, expected, krun_flags = '--interpret')

    def test_ocaml(self, input):
        expected = input + '.ocaml.expected'
        self.ocaml_tests += self.ocaml.krun_and_check('$builddir/t/', input, expected, krun_flags = '--interpret')

    def test_java(self, input):
        expected = input + '.java.expected'
        self.lazy_tests   += self.lazy.krun_and_check('$builddir/t/', input, expected)
        self.strict_tests += self.strict.krun_and_check('$builddir/t/', input, expected)

plutus = Plutus()

# Basic tests
# -----------
#
# Since the OCaml does not support reachability claims (even
# concrete ones) these also function as smoke tests for the OCaml backend)
#
plutus.test('t/builtin-app.plc')

# We need distinct exptected and actual files for these.
plutus.test_ocaml('t/bytestring.plc')
plutus.test_java('t/bytestring.plc')

# Cryptography
# ------------
#
# We do not yet support hashing on the Java backend since the SHA3 hook does
# not exist, and the SHA2 hook is missing an alias into the HASH namespace.
#
plutus.test_ocaml('t/sha2.plc')
# plutus.test_java('t/sha2.plc')
plutus.test_ocaml('t/sha3.plc')
# plutus.test_java('t/sha3.plc')

# Complex tests
# -------------
#
# These are tests involving recursion, and other tests from the Roman and the
# IOHK Plutus team.
#
# plutus.test('t/11-scott-to-int.plc')
plutus.test('t/if-then-else.plc')
plutus.test('t/sum-list.plc')
plutus.test('t/sum.plc')
plutus.test('t/fact.plc')

plutus.write_aliases()
