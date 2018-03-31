#!/usr/bin/env python2

from subprocess import Popen, PIPE

import pytest
import sys
import xml.dom.minidom
import string
import json
import tempfile
import os

_base = os.path.dirname(os.path.dirname(os.path.realpath(__file__)))
def base(*args):
    return os.path.join(_base, *args)
def bin(*args):
    return base('bin', *args)

def generate_tests():
    return [("arith-ops", "Foo", "add",     [19, 23],            42  ),
            ("arith-ops", "Foo", "addFive", [12],                17  ),
            ("arith-ops", "Foo", "sub",     [19, 23],            -4  ),
            ("arith-ops", "Foo", "mult",    [19, 23],            437 ),
            ("arith-ops", "Foo", "mult",    [19, -23],           -437),
            ("arith-ops", "Foo", "div",     [437, 19],           23  ),
            ("arith-ops", "Foo", "div",     [440, 19],           23  ),
            ("arith-ops", "Foo", "div",     [0, 19],             0   ),
            ("arith-ops", "Foo", "mod",     [440, 19],           3   ),
            ("arith-ops", "Foo", "mod",     [-440, 19],          -3  ),
            ("arith-ops", "Foo", "mod",     [0, 19],             0   ),
            ("arith-ops", "Foo", "one",     [],                  1   ),
            ("arith-ops", "Foo", "complex", [5, 4, 7, 11, 2, 3], 7   ),

            ("cmp-ops", "Foo", "lessThan",      [12, 12], "(con Prelude.False .ValList)"),
            ("cmp-ops", "Foo", "lessThan",      [12, 17], "(con Prelude.True .ValList)" ),
            ("cmp-ops", "Foo", "lessThan",      [17, 12], "(con Prelude.False .ValList)"),
            ("cmp-ops", "Foo", "lessThanFive",  [17],     "(con Prelude.False .ValList)"),
            ("cmp-ops", "Foo", "lessThanEq",    [12, 12], "(con Prelude.True .ValList)" ),
            ("cmp-ops", "Foo", "lessThanEq",    [12, 17], "(con Prelude.True .ValList)" ),
            ("cmp-ops", "Foo", "lessThanEq",    [17, 12], "(con Prelude.False .ValList)"),
            ("cmp-ops", "Foo", "greaterThan",   [12, 12], "(con Prelude.False .ValList)"),
            ("cmp-ops", "Foo", "greaterThan",   [12, 17], "(con Prelude.False .ValList)"),
            ("cmp-ops", "Foo", "greaterThan",   [17, 12], "(con Prelude.True .ValList)" ),
            ("cmp-ops", "Foo", "greaterThanEq", [12, 12], "(con Prelude.True .ValList)" ),
            ("cmp-ops", "Foo", "greaterThanEq", [12, 17], "(con Prelude.False .ValList)"),
            ("cmp-ops", "Foo", "greaterThanEq", [17, 12], "(con Prelude.True .ValList)" ),
            ("cmp-ops", "Foo", "equals",        [12, 12], "(con Prelude.True .ValList)" ),
            ("cmp-ops", "Foo", "equals",        [12, 17], "(con Prelude.False .ValList)"),
            ("cmp-ops", "Foo", "myTrue",        [],       "(con Prelude.True .ValList)" ),

            pytest.mark.xfail(reason="exit code unimplemented")
              (("arith-ops", "Foo", "div", [19, 0], None)),
            pytest.mark.xfail(reason="exit code unimplemented")
              (("arith-ops", "Foo", "mod", [19, 0], None))]

@pytest.mark.parametrize("file, mod, fct, args, expected", generate_tests())
def test_plc(file, mod, fct, args, expected):
    krun_args = [bin("kplc"), "run", "execution", base("test/execution/", file +".plc"),
                 "-cMAINMOD=#token(\"" + mod + "\", \"UpperName\")",
                 "-pMAINMOD=printf %s",
                 "-cMAINFCT=#token(\"" + fct + "\", \"LowerName\")",
                 "-pMAINFCT=printf %s",
                 "-cMAINARGS=" + kast_args(args),
                 "-pMAINARGS=printf %s"]
    krun = Popen(krun_args, stdout=PIPE)
    (output, err) = krun.communicate()
    exit_code = krun.wait()

    if expected == None:
        # TODO: add exit code to semantics
        assert exit_code == 1
    else:
        assert exit_code == 0
        assert extract_exec_output(output) == str(expected)

@pytest.mark.parametrize("file, mod, fct, args, expected", generate_tests())
def test_iele(file, mod, fct, args, expected):
    template = json.load(open(base("test/translation/template.iele.json")))
    account = "0x1000000000000000000000000000000000000000"
    template["pre"][account]["code"] = template["postState"][account]["code"] = base("test/translation/", file + ".iele")
    template["blocks"][0]["results"][0]["out"] = [hex(expected)]
    template["blocks"][0]["transactions"][0]["function"] = fct
    template["blocks"][0]["transactions"][0]["arguments"] = map(hex, args)
    iele_test = { mod : template }

    temp_json = tempfile.NamedTemporaryFile()
    json.dump(iele_test, temp_json)
    temp_json.write("\n")
    temp_json.flush()
    blockchain_args = ["../.build/iele/blockchaintest", temp_json.name]

    blockchaintest = Popen(blockchain_args, stdout=PIPE)
    (output, err) = blockchaintest.communicate()
    exit_code = blockchaintest.wait()

    if expected == None:
        assert exit_code == 1
    else:
        assert exit_code == 0

    print iele_test
    assert False

def kast_args(args):
    if args == []:
        return "`.List{\"tmList\"}`(.KList)"
    else:
        return "tmList(#token(\"" + str(args[0]) + "\", \"Int\")," + kast_args(args[1:]) + ")"

def extract_exec_output(config):
    config_xml = xml.dom.minidom.parseString(config)
    output = config_xml.getElementsByTagName('k')[0].firstChild.data
    return output.strip()
