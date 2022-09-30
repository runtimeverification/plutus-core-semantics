import sys
from argparse import ArgumentParser, ArgumentTypeError
from pathlib import Path

from pyk.cli_utils import dir_path, file_path

from .kplutus import KPlutus


def main() -> None:
    sys.setrecursionlimit(15000000)

    parser = create_parser()
    args, remainder = parser.parse_known_args()

    if args.command == 'kompile':
        KPlutus.kompile(args.main_file, remainder)
    if args.command == 'uplc-to-k':
        if not args.definition:
            args.definition = kompiled_dir(".")
        KPlutus.uplc_to_k(args.main_file, args.definition, remainder)
    else:
        raise AssertionError()


def create_parser() -> ArgumentParser:
    kplc_args = ArgumentParser(add_help=False)
    kplc_args.add_argument('--kplc-lib', type=dir_path, help='Path to the kplutus library')

    k_args = ArgumentParser(add_help=False)
    ex = k_args.add_mutually_exclusive_group()
    ex.add_argument('--definition', type=dir_path, dest='definition', help='Path to exact kompiled directory.')
    ex.add_argument(
        '--directory', type=kompiled_dir, dest='definition', help='Path to where the kompiled directory is.'
    )

    parser = ArgumentParser(parents=[kplc_args])

    command_parser = parser.add_subparsers(dest='command', required=True)

    kompile_args = command_parser.add_parser('kompile', help='Kompile KPlutus specification.')
    kompile_args.add_argument('main_file', type=file_path, help='Path to file with main module.')

    uplc_to_k_args = command_parser.add_parser(
        'uplc-to-k', help='K Spec generation helper for compiled UPLC tests', parents=[k_args]
    )
    uplc_to_k_args.add_argument('main_file', type=file_path, help='Path to compiled UPLC test')

    return parser


def kompiled_dir(s: str) -> Path:
    cwd = dir_path(s)
    paths = list(filter(lambda x: x.is_dir(), cwd.glob("*-kompiled")))
    if len(paths) == 0:
        raise ArgumentTypeError("Could not find a compiled definition in current working directory: " + str(cwd))
    elif len(paths) > 1:
        raise ArgumentTypeError(
            "Multiple compiled definitions found in the current working directory: "
            + "\n".join([str(x) for x in paths])
        )
    return paths.pop()


if __name__ == "__main__":
    main()
