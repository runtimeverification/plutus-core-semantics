#!/usr/bin/env python2

from subprocess import Popen, PIPE

import json
import os
import pytest
import string
import sys
import tempfile
import xml.dom.minidom

_base = os.path.dirname(os.path.dirname(os.path.realpath(__file__)))
def base(*args):
    return os.path.join(_base, *args)
def bin(*args):
    return base('bin', *args)

class ExitCode_NotPublic: pass
class ExitCode_DivByZero: pass

def toIeleExitStatus(expected):
    if   expected == ExitCode_NotPublic: return "0x01"
    elif expected == ExitCode_DivByZero: return "0x04"
    else                               : return ""

def toPlutusExitCode(expected):
    if   expected == ExitCode_NotPublic: return 1
    elif expected == ExitCode_DivByZero: return 1
    else                               : return 0

def toIeleReturn(expected):
    if type(expected) is int: return [hex(expected)]
    elif expected == False  : return ["0x0"]
    elif expected == True   : return ["0x1"]
    else                    : return []

def toPlutusReturn(expected):
    if type(expected) is int: return str(expected)
    elif expected == False  : return "(con Prelude.False .ValList)"
    elif expected == True   : return "(con Prelude.True .ValList)"
    else                    : return []

def generate_tests(type):
    passing = [
            ("arith-ops", "Foo", "notPublic",   [19, 23],            ExitCode_NotPublic),

            ("arith-ops", "Foo", "add",         [19, 23],            42  ),
            ("arith-ops", "Foo", "addFive",     [12],                17  ),
            ("arith-ops", "Foo", "addFiveApp",  [6],                 11  ),
            ("arith-ops", "Foo", "add1923App",  [1234],              42  ),
            ("arith-ops", "Foo", "sub",         [19, 23],           -4   ),
            ("arith-ops", "Foo", "mult",        [19, 23],            437 ),
            ("arith-ops", "Foo", "mult",        [19, -23],          -437 ),
            ("arith-ops", "Foo", "div",         [437, 19],           23  ),
            ("arith-ops", "Foo", "div",         [440, 19],           23  ),
            ("arith-ops", "Foo", "div",         [0,   19],           0   ),
            ("arith-ops", "Foo", "div",         [19, 0],             ExitCode_DivByZero),
            ("arith-ops", "Foo", "mod",         [440, 19],           3   ),
            ("arith-ops", "Foo", "mod",         [-440, 19],          -3  ),
            ("arith-ops", "Foo", "mod",         [0, 19],             0   ),
            ("arith-ops", "Foo", "mod",         [19, 0],             ExitCode_DivByZero),
            ("arith-ops", "Foo", "one",         [],                  1   ),
            ("arith-ops", "Foo", "complex",     [5, 4, 7, 11, 2, 3], 7   ),
            ("arith-ops", "Foo", "complex",     [7, 4, 7, 11, 2, 3], 6   ),

            ("cmp-ops", "Foo", "lessThan",      [12, 12], False),
            ("cmp-ops", "Foo", "lessThan",      [12, 17], True ),
            ("cmp-ops", "Foo", "lessThan",      [17, 12], False),
            ("cmp-ops", "Foo", "lessThanFive",  [17],     False),
            ("cmp-ops", "Foo", "lessThanEq",    [12, 12], True ),
            ("cmp-ops", "Foo", "lessThanEq",    [12, 17], True ),
            ("cmp-ops", "Foo", "lessThanEq",    [17, 12], False),
            ("cmp-ops", "Foo", "greaterThan",   [12, 12], False),
            ("cmp-ops", "Foo", "greaterThan",   [12, 17], False),
            ("cmp-ops", "Foo", "greaterThan",   [17, 12], True ),
            ("cmp-ops", "Foo", "greaterThanEq", [12, 12], True ),
            ("cmp-ops", "Foo", "greaterThanEq", [12, 17], False),
            ("cmp-ops", "Foo", "greaterThanEq", [17, 12], True ),
            ("cmp-ops", "Foo", "equals",        [12, 12], True ),
            ("cmp-ops", "Foo", "equals",        [12, 17], False),
            ## ("cmp-ops", "Foo", "myTrue",        [],       True ),
           ]

    unimplemented = [
            ("ctor-duplicate",               "Duplicate", "one",  [], None),
            ("module-call-private-indirect", "Foo",       "bar", [0],   19),
            ("module-call-private-indirect", "Foo",       "baz", [0],   23),
           ]

    if type == 'translation':
        return (passing                                                                    +
                map(pytest.mark.xfail(reason="unimplemented"), unimplemented)              +
                map(pytest.mark.xfail(reason="no alg data types" ), tr_no_alg_data_types)
               )
    if type == 'execution':
        return (passing                                                                    +
                map(pytest.mark.xfail(reason="unimplemented"), unimplemented)              +
                tr_no_alg_data_types
               )

@pytest.mark.parametrize("file, mod, fct, args, expected", generate_tests('execution'))
def test_execution(file, mod, fct, args, expected):
    krun_args = [bin("kplc"), "run", "execution", base("test/", file +".plc"),
                 "-cMAINMOD=#token(\"" + mod + "\", \"UpperName\")",
                 "-pMAINMOD=printf %s",
                 "-cMAINFCT=#token(\"" + fct + "\", \"LowerName\")",
                 "-pMAINFCT=printf %s",
                 "-cMAINARGS=" + kast_args(args),
                 "-pMAINARGS=printf %s"]
    krun = Popen(krun_args, stdout=PIPE)
    (output, err) = krun.communicate()
    exit_code = krun.wait()

    assert exit_code == toPlutusExitCode(expected)
    if 0 == toPlutusExitCode(expected):
        assert extract_exec_output(output) == toPlutusReturn(expected)

@pytest.mark.parametrize("file, mod, fct, args, expected", generate_tests('translation'))
def test_translation(file, mod, fct, args, expected):
    template = json.load(open(base("test/template.iele.json")))
    account = "0x1000000000000000000000000000000000000000"
    template["pre"][account]["code"] = template["postState"][account]["code"] = base("test/", file + ".iele")
    template["blocks"][0]["transactions"][0]["function"] = fct
    template["blocks"][0]["transactions"][0]["arguments"] = map(hex, args)
    template["blocks"][0]["results"][0]["status"] = toIeleExitStatus(expected)
    template["blocks"][0]["results"][0]["out"] = toIeleReturn(expected)

    iele_test = { mod : template }

    temp_json = tempfile.NamedTemporaryFile(delete=False)
    json.dump(iele_test, temp_json)
    temp_json.write("\n")
    temp_json.flush()

    # Print JSON spec for debugging.
    json.dump(iele_test, sys.stdout, indent=2)

    blockchain_args = ["./blockchaintest", temp_json.name]
    blockchaintest = Popen(blockchain_args, stdout=PIPE, cwd=base(".build/iele/"))
    (output, err) = blockchaintest.communicate()
    exit_code = blockchaintest.wait()
    print output.replace('`<', "\n`<")

    assert exit_code == 0

def kast_args(args):
    if args == []:
        return "`.List{\"tmList\"}`(.KList)"
    else:
        return "tmList(#token(\"" + str(args[0]) + "\", \"Int\")," + kast_args(args[1:]) + ")"

def extract_exec_output(config):
    config_xml = xml.dom.minidom.parseString(config)
    output = config_xml.getElementsByTagName('k')[0].firstChild.data
    return output.strip()
