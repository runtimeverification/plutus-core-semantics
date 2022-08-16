import argparse

from pyk.cli_utils import file_path

from .kplutus import KPlutus

def main():
    parser = create_parser()
    args = parser.parse_args()

    if args.command == 'kompile':
        KPlutus.kompile()
    else:
        assert False

def create_parser():
    parser = argparse.ArgumentParser(prog='python3 -m kplutus_pyk')

    command_parser = parser.add_subparsers(dest='command', required=True)

    kompile_args = command_parser.add_parser('kompile', help='Kompile KPlutus specification.')
    kompile_args.add_argument('main_file', type=file_path, help='Path to file with main module.')
    kompile_args.add_argument('--main-module', type=str, help='Name of the main module.')
    kompile_args.add_argument('--syntax-module', type=str, help='Name of the syntax module.')

    return parser

if __name__ == "__main__":
    main()
