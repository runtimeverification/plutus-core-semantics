import json
import sys
from pathlib import Path
from subprocess import CalledProcessError
from typing import Iterable

from pyk.cli_utils import run_process
from pyk.cterm import CTerm, build_claim
from pyk.kast import (
    KApply,
    KDefinition,
    KFlatModule,
    KImport,
    KInner,
    KRequire,
    KSort,
    KToken,
    KVariable,
    bottom_up,
    read_kast_definition,
)
from pyk.kastManip import if_ktype, substitute
from pyk.ktool import KPrint


class KPlutus:
    @staticmethod
    def kompile(main_file: Path, args: Iterable[str] = ()) -> None:
        command = ['kompile', str(main_file)] + list(args)
        try:
            run_process(command)
        except CalledProcessError as err:
            sys.stderr.write(f'\nkompile stdout:\n{err.stdout}\n')
            sys.stderr.write(f'\nkompile stderr:\n{err.stderr}\n')
            sys.stderr.write(f'\nkompile returncode:\n{err.returncode}\n')
            sys.stderr.flush()
            raise

    @staticmethod
    def uplc_to_k(main_file: Path, definition_dir: Path, args: Iterable[str] = ()) -> None:
        command = ['kplc', 'kast', str(main_file), '--output', 'json']
        try:
            kast_out = run_process(command)
        except CalledProcessError as err:
            sys.stderr.write(f'\nkplc stdout:\n{err.stdout}\n')
            sys.stderr.write(f'\nkplc stderr:\n{err.stderr}\n')
            sys.stderr.write(f'\nkplc returncode:\n{err.returncode}\n')
            sys.stderr.flush()
            raise

        d = read_kast_definition(definition_dir / 'compiled.json')
        empty_config = d.empty_config(KSort('GeneratedTopCell'))

        # UplcId tokens without an underbar clash with tokens in kprove,
        # so add a prefix with an underbar to them
        def prefix_uplcid(t: KToken) -> KToken:
            if t.sort.name == "UplcId":
                return t.let(token="v_" + t.token)
            return t

        contract = KInner.from_dict(json.loads(kast_out.stdout)['term'])
        contract = bottom_up(if_ktype(KToken, prefix_uplcid), contract)

        true_val = KApply(
            '<con__>_UPLC-SYNTAX_Value_TypeConstant_Constant',
            [KApply('bool_UPLC-SYNTAX_TypeConstant'), KApply('True_UPLC-SYNTAX_Constant')],
        )

        init_subst = {'K_CELL': contract, 'ENV_CELL': KVariable('RHO')}
        final_subst = {'K_CELL': true_val, 'ENV_CELL': KApply('.Map')}

        init_cterm = CTerm(substitute(empty_config, init_subst))
        final_cterm = CTerm(substitute(empty_config, final_subst))

        module_name = main_file.stem.upper()
        claim, _ = build_claim(module_name.lower(), init_cterm, final_cterm)
        claim_module = KFlatModule(module_name + '-SPEC', [claim], [KImport('VERIFICATION')])

        verification_module = KFlatModule("VERIFICATION", [], [KImport("UPLC-WITH-LOCAL-ENV")])

        spec_definition = KDefinition(module_name + '-SPEC', [verification_module, claim_module], [KRequire('uplc.md')])

        p = KPrint(definition_dir)
        print(p.pretty_print(spec_definition))
