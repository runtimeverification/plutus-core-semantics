from argparse import ArgumentParser

from pyk.cli_utils import dir_path, file_path

from .kplutus import KPlutus


def main() -> None:
    parser = create_parser()
    args, remainder = parser.parse_known_args()

    if args.command == 'kompile':
        KPlutus.kompile(args.main_file, remainder)
    if args.command == 'uplc-to-k':
        KPlutus.uplc_to_k(args.main_file, args.definition, remainder)
    else:
        raise AssertionError()


def create_parser() -> ArgumentParser:
    kplc_args = ArgumentParser(add_help=False)
    kplc_args.add_argument('--kplc-lib', type=dir_path, help='Path to the kplutus library')

    k_args = ArgumentParser(add_help=False)
    k_args.add_argument('--definition', type=dir_path, help='Path to definition to use.')

    parser = ArgumentParser(parents=[kplc_args])

    command_parser = parser.add_subparsers(dest='command', required=True)

    kompile_args = command_parser.add_parser('kompile', help='Kompile KPlutus specification.')
    kompile_args.add_argument('main_file', type=file_path, help='Path to file with main module.')

    uplc_to_k_args = command_parser.add_parser(
        'uplc-to-k', help='K Spec generation helper for compiled UPLC tests', parents=[k_args]
    )
    uplc_to_k_args.add_argument('main_file', type=file_path, help='Path to compiled UPLC test')

    return parser


if __name__ == "__main__":
    main()
