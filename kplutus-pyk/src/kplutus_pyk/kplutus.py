import json
import sys
from pathlib import Path
from subprocess import CalledProcessError
from typing import Iterable

from pyk.cli_utils import run_process
from pyk.cterm import CTerm, build_claim
from pyk.kast.inner import KApply, KInner, KSort, KToken, KVariable, Subst, bottom_up
from pyk.kast.manip import if_ktype
from pyk.kast.outer import KDefinition, KFlatModule, KImport, KRequire, read_kast_definition
from pyk.ktool.kprint import KPrint
from pyk.ktool.krun import KRun

from .genv import extract_genv, make_genv_module


class KPlutus:
    kplc_lib_prefix: Path

    def __init__(self, kplc_lib_prefix: Path):
        self.kplc_lib_prefix = kplc_lib_prefix

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

    def uplc_to_k(self, main_file: Path, definition_dir: Path, args: Iterable[str] = ()) -> None:
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
            if t.sort.name == 'UplcId':
                return t.let(token='v_' + t.token)
            return t

        contract_pgm = KInner.from_dict(json.loads(kast_out.stdout)['term'])
        contract_pgm = bottom_up(if_ktype(KToken, prefix_uplcid), contract_pgm)

        krun_definition = self.kplc_lib_prefix / 'llvm' / 'uplc-kompiled'
        krun = KRun(krun_definition)

        genv, contract = extract_genv(contract_pgm, krun)
        true_val = KApply(
            '<con__>_UPLC-SYNTAX_Value_TypeConstant_Constant',
            [KApply('bool_UPLC-SYNTAX_TypeConstant'), KApply('True_UPLC-SYNTAX_Constant')],
        )

        contract_name = main_file.stem.upper()

        # Global environment module
        genv_module = make_genv_module(genv, contract_name)

        # Spec module
        init_subst = {'K_CELL': contract, 'ENV_CELL': KVariable('RHO')}
        final_subst = {'K_CELL': true_val, 'ENV_CELL': KApply('.Map')}

        init_cterm = CTerm(Subst(init_subst)(empty_config))
        final_cterm = CTerm(Subst(final_subst)(empty_config))

        claim, _ = build_claim(contract_name.lower(), init_cterm, final_cterm)
        claim_module = KFlatModule(contract_name + '-SPEC', [claim], [KImport('VERIFICATION')])

        verification_module = KFlatModule('VERIFICATION', [], [KImport(genv_module.name)])

        spec_definition = KDefinition(
            contract_name + '-SPEC', [genv_module, verification_module, claim_module], [KRequire('uplc.md')]
        )

        p = KPrint(definition_dir)
        p.symbol_table['_Map_'] = lambda m1, m2: m1 + '\n' + m2
        print(p.pretty_print(spec_definition))
