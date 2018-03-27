#!/usr/bin/env python2

from subprocess import Popen, PIPE

import pytest
import sys
import xml.dom.minidom
import string

@pytest.mark.parametrize("file, mod, fct, args, expected", [
    ("test/execution/arith-ops.plc", "Foo", "add",     [19, 23],            "42"  ),
    ("test/execution/arith-ops.plc", "Foo", "addFive", [12],                "17"  ),
    ("test/execution/arith-ops.plc", "Foo", "sub",     [19, 23],            "-4"  ),
    ("test/execution/arith-ops.plc", "Foo", "mult",    [19, 23],            "437" ),
    ("test/execution/arith-ops.plc", "Foo", "mult",    [19, -23],           "-437"),
    ("test/execution/arith-ops.plc", "Foo", "div",     [437, 19],           "23"  ),
    ("test/execution/arith-ops.plc", "Foo", "div",     [440, 19],           "23"  ),
    ("test/execution/arith-ops.plc", "Foo", "div",     [0, 19],             "0"   ),
    ("test/execution/arith-ops.plc", "Foo", "mod",     [440, 19],           "3"   ),
    ("test/execution/arith-ops.plc", "Foo", "mod",     [-440, 19],          "-3"  ),
    ("test/execution/arith-ops.plc", "Foo", "mod",     [0, 19],             "0"   ),
    ("test/execution/arith-ops.plc", "Foo", "one",     [],                  "1"   ),
    ("test/execution/arith-ops.plc", "Foo", "complex", [5, 4, 7, 11, 2, 3], "7"   ),

    pytest.mark.xfail(reason="exit code unimplemented")
      (("test/execution/arith-ops.plc", "Foo", "div", [19, 0], None)),
    pytest.mark.xfail(reason="exit code unimplemented")
      (("test/execution/arith-ops.plc", "Foo", "mod", [19, 0], None))
])

def test_plc(file, mod, fct, args, expected):
    krun_args = ["../kplc", "run", "execution", file,
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
        pass
    else:
        assert exit_code == 0
        assert extract_exec_output(output) == expected

def kast_args(args):
    if args == []:
        return "`.List{\"tmList\"}`(.KList)"
    else:
        return "tmList(#token(\"" + str(args[0]) + "\", \"Int\")," + kast_args(args[1:]) + ")"

def extract_exec_output(config):
    config_xml = xml.dom.minidom.parseString(config)
    output = config_xml.getElementsByTagName('k')[0].firstChild.data
    return output.strip()
