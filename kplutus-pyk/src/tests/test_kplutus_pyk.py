from unittest import TestCase

from kplutus_pyk import __version__


class TestVersion(TestCase):
    def test_version(self):
        self.assertEqual(__version__, '0.1.0')
