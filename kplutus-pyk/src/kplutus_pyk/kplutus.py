import sys
from pathlib import Path
from subprocess import CalledProcessError
from typing import Iterable

from pyk.cli_utils import run_process


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
