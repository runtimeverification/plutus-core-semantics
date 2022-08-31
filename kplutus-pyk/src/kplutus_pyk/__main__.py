import argparse

from pyk.cli_utils import file_path

from .kplutus import KPlutus


def main():
    parser = create_parser()
    args, remainder = parser.parse_known_args()

    if args.command == 'kompile':
        KPlutus.kompile(args.main_file, remainder)
    else:
        raise AssertionError()


def create_parser():
    parser = argparse.ArgumentParser()

    command_parser = parser.add_subparsers(dest='command', required=True)

    kompile_args = command_parser.add_parser('kompile', help='Kompile KPlutus specification.')
    kompile_args.add_argument('main_file', type=file_path, help='Path to file with main module.')

    return parser


if __name__ == "__main__":
    main()
