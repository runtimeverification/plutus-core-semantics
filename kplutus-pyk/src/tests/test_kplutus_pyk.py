from unittest import TestCase

from kplutus_pyk import __version__


class VersionTest(TestCase):
    def test_version(self) -> None:
        self.assertEqual(__version__, '0.1.0')
