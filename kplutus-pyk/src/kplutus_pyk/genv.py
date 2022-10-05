from typing import Any, List, Tuple

from pyk.kast import (
    KApply,
    KAtt,
    KFlatModule,
    KImport,
    KInner,
    KLabel,
    KProduction,
    KRewrite,
    KRule,
    KTerminal,
    KToken,
    KVariable,
)
from pyk.ktool import KRun

_lam = '(lam__)_UPLC-SYNTAX_Term_UplcId_Term'
_app = '[__]_UPLC-SYNTAX_Term_Term_TermList'


def extract_genv(pgm: KInner, krun: KRun) -> Tuple[KInner, KInner]:
    run_node = pgm.to_dict()
    current = run_node["args"][1]
    nest: List[Any] = []

    # Recurse down the program and find where the global environment
    # definition ends, and the contract body begins
    while True:
        try:
            args = current['args']
            label = current['label']['name']
        except KeyError:
            # Break if the current term is a token
            break
        if label == _lam:
            nest.pop()
            current = args[1]
            continue
        if label == _app:
            nest.append(current)
            current = args[0]
            continue
        # Break if the current term is neither a lambda nor an application
        break

    if len(nest) > 0:
        # The contract body can be a nested application, which will make
        # the above loop break late, so some bookkeeping is required
        current = nest[0]

    contract_body = KInner.from_dict(current)

    # 1. Apply (error) to the body of the contract
    # 2. Execute the program with the application of (error). It will stop at (error)
    # 3. The <env> cell after execution will contain the global environment
    err_term = KApply("(error)_UPLC-SYNTAX_Term")
    err_app = KApply(_app, [err_term, contract_body])
    current.update(err_app.to_dict())
    genv_cfg = krun.run(KInner.from_dict(run_node))
    genv = KInner.from_dict(genv_cfg.kast.to_dict()["args"][1]["args"][0])

    return (genv, contract_body)


def make_genv_module(genv: KInner, name: str) -> KFlatModule:
    genv_module_name = name.upper() + "-GLOBAL-ENVIRONMENT"

    genv_map_name = "GLOBAL_ENV"
    genv_token = KToken(genv_map_name, "UplcId")
    key_var = KVariable("KEY")

    genv_inkeys_lhs = KApply("#inKeysgEnv(_)_UPLC-GENVIRONMENT_Bool_UplcId", [key_var])
    genv_inkeys_rhs = KApply("_in_keys(_)_MAP_Bool_KItem_Map", [key_var, genv_token])
    genv_inkeys_rule = KRule(KRewrite(genv_inkeys_lhs, genv_inkeys_rhs))

    genv_lookup_lhs = KApply("gLookup(_)_UPLC-GENVIRONMENT_Value_UplcId", [key_var])
    genv_lookup_rhs = KApply("project:Value", [KApply(KLabel("Map:lookup"), [genv_token, key_var])])
    genv_lookup_rule = KRule(KRewrite(genv_lookup_lhs, genv_lookup_rhs))

    genv_alias = KProduction("Map", [KTerminal(genv_map_name)], att=KAtt({"alias": ""}))
    genv_lhs = genv_token
    genv_rule = KRule(KRewrite(genv_lhs, genv))

    genv_module = KFlatModule(
        genv_module_name,
        [genv_inkeys_rule, genv_lookup_rule, genv_alias, genv_rule],
        [KImport("UPLC-WITH-LOCAL-GLOBAL-ENV")],
    )
    return genv_module
