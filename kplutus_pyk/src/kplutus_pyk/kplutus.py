import sys
from subprocess import CalledProcessError

from pyk.cli_utils import run_process

# KPlutus class

class KPlutus():

    @staticmethod
    def kompile() -> None:
        command = ['kompile']
        try:
            run_process(command)
        except CalledProcessError as err:
            sys.stderr.write(f'\nkompile stdout:\n{err.stdout}\n')
            sys.stderr.write(f'\nkompile stderr:\n{err.stderr}\n')
            sys.stderr.write(f'\nkompile returncode:\n{err.returncode}\n')
            sys.stderr.flush()
            raise
