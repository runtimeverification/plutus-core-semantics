import kninja.ninja.ninja_syntax
import os

def basename_no_ext(path):
    return os.path.splitext(os.path.basename(path))[0]

class KProject(ninja.ninja_syntax.Writer):
    def __init__(self, builddir):
        self._builddir = builddir
        super().__init__(open(self.builddir('generated.ninja'), 'w'))
        self.comment('This is a generated file')
        self.newline()
        self.variable('builddir', builddir)

    def builddir(self, *paths):
        return os.path.join(self._builddir, *paths)
    def tangleddir(self, *paths):
        return self.builddir('tangled', *paths)

    def kdefinition(self, name, main, backend, alias):
        kdef = self.kdefinition_no_build( name
                                        , kompiled_dirname = basename_no_ext(main) + '-kompiled'
                                        , alias = alias
                                        )
        kdef.write_alias(alias)
        return kdef

    def kdefinition_no_build(self, name, kompiled_dirname, alias):
        return KDefinition(self, name, self.builddir(name), kompiled_dirname, alias)

class KDefinition:
    def __init__(self, writer, name, directory, kompiled_dirname, alias):
        self.writer           = writer
        self.name             = name
        self.directory        = directory
        self.kompiled_dirname = kompiled_dirname
        self.alias            = alias

    def get_timestamp_file(self):
        return os.path.join(self.directory, self.kompiled_dirname, 'timestamp')

    def write_alias(self, alias):
        # TODO: This assumes that the timestamp file exists. This is not the case
        # in when using the OCaml interpreter.
        self.writer.build(alias, 'phony', self.get_timestamp_file())

    def krun(self, output, input, krun_flags = None):
        self.writer.build( outputs  = [output]
                         , rule     = 'krun'
                         , inputs   = [input]
                         , implicit = [self.alias]
                         , variables = { 'directory' : self.directory
                                       , 'flags'     : krun_flags
                                       }
                         )

    def check_actual_expected(self, name, actual, expected):
        self.writer.build( outputs   = [name]
                         , rule      = 'check-test-result'
                         , inputs    = [actual]
                         , implicit  = [expected]
                         , variables = { 'expected' : expected }
                         )

    def krun_and_check(self, output_dir, input, expected, krun_flags = None):
        basename  = os.path.basename(input)
        actual    = os.path.join(output_dir, basename + '.' + self.name + '.actual')
        test_name = input + '.' + self.name + '.krun'
        self.writer.comment(input + ' (' + self.name + ')')
        self.krun( output = actual
                 , input  = input
                 , krun_flags = krun_flags
                 )

        self.check_actual_expected( name     = test_name
                                  , actual   = actual
                                  , expected = expected
                                  )
        self.writer.default(test_name)
        self.writer.newline()

