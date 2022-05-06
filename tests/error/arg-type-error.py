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

def wrongTypeBinary(bn, type_name):
    global some_instances
    prg = mk_prg()
    builtin = "(builtin " + bn + ")"

    # Wrong 2nd argument
    basic_prg = mk_apply(builtin, mk_con(type_name, some_instances[type_name]))
    
    for i in some_instances.keys():
        if i != type_name:
            prg = "(" + prg + mk_apply(basic_prg, mk_con(i, some_instances[i])) + ")"
            fn = bn + "-wrong-2nd-argument-" + i.replace("(","-").replace(")", "-") + ".uplc"
            print("// " + fn) 
            print(prg)
            h = open(fn, "w")
            h.write(prg)
            h.close()
            prg = mk_prg()
            print()

    # Wrong 1st argument    
    for i in some_instances.keys():
        if i != type_name:
            basic_prg = mk_apply(builtin, mk_con(i, some_instances[i]))
            prg = "(" + prg + mk_apply(basic_prg, mk_con(type_name, some_instances[type_name])) + ")"
            fn = bn + "-wrong-1st-argument-" + i.replace("(","-").replace(")", "-") + ".uplc"
            print("// " + fn) 
            print(prg)
            h = open(fn, "w")
            h.write(prg)
            h.close()
            prg = mk_prg()
            print()
            
integer_bn = [ "addInteger", "subtractInteger", "multiplyInteger", \
               "divideInteger", "quotientInteger", "remainderInteger", \
               "modInteger", "equalsInteger", "lessThanInteger", "lessThanEqualsInteger"]

for bn in integer_bn:
    wrongTypeBinary(bn, "integer")
