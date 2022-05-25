some_instances = {
    "integer" : "10",
    "data" : "{ByteString #AFD3}",
    "bytestring" : "#AFD3",
    "string" : "\"Ola\"",
    "unit" : "()",
    "bool" : "True",
    "list(integer)" : "[42, 10, 53]",
    "pair(integer)(integer)" : "(90, 93)"
}

def mk_apply(m, n):
    return "[" + m + "" + n + "]"

def mk_con(type_name, con):
    return "(con "+ type_name + " " + con + ")"

def mk_prg():
    return "program 1.0.0"

def mk_label(type_name):
    return type_name.replace("(","-").replace(")", "-")

def write_program(bn, basic_prg, type_name, file_label):
    prg = "(" + mk_prg() + mk_apply(basic_prg, mk_con(type_name, some_instances[type_name])) + ")"
    fn = bn + "-" + file_label + ".uplc"
    h = open(fn, "w")
    h.write(prg)
    h.close()

def wrong_type_binary(bn, type_name, first_arg, second_arg):
    global some_instances
    prg = mk_prg()
    builtin = "(builtin " + bn + ")"

    # Wrong 2nd argument
    if second_arg:
        basic_prg = mk_apply(builtin, mk_con(type_name, some_instances[type_name]))

        for i in some_instances.keys():
            if i != type_name:
                file_label = mk_label(i) + "-wrong-2nd-argument"
                write_program(bn, basic_prg, i, file_label)
        print("There were " + str(len(some_instances.keys())) + " files generated to test for wrong 2nd argument type for builtin " + bn + ".")

    # Wrong 1st argument
    if first_arg:
        for i in some_instances.keys():
            if i != type_name:
                basic_prg = mk_apply(builtin, mk_con(i, some_instances[i]))
                file_label = mk_label(i) + "-wrong-1st-argument"
                write_program(bn, basic_prg, type_name, file_label)
        print("There were " + str(len(some_instances.keys())) + " files generated to test for wrong 1st argument type for builtin " + bn + ".")

integer_bn = [ "addInteger", "subtractInteger", "multiplyInteger", \
               "divideInteger", "quotientInteger", "remainderInteger", \
               "modInteger", "equalsInteger", "lessThanInteger", "lessThanEqualsInteger"]

for bn in integer_bn:
    wrong_type_binary(bn, "integer", True, False)
