import sys
from pathlib import Path
from subprocess import CalledProcessError
from typing import List

from pyk.cli_utils import run_process

# KPlutus class


class KPlutus:
    @staticmethod
    def kompile(main_file: Path, args: List[str] = None) -> None:
        if args == None:
            args = []

        command = ['kompile', str(main_file)] + args
        try:
            run_process(command)
        except CalledProcessError as err:
            sys.stderr.write(f'\nkompile stdout:\n{err.stdout}\n')
            sys.stderr.write(f'\nkompile stderr:\n{err.stderr}\n')
            sys.stderr.write(f'\nkompile returncode:\n{err.returncode}\n')
            sys.stderr.flush()
            raise
