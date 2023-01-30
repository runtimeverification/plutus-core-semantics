# kplutus-pyk


## Installation

Prerequsites: `python 3.8.*`, `pip >= 20.0.2`, `poetry >= 1.3.0`.

```bash
make build
pip install dist/*.whl
```

Alternatively, use a virtual environment.

```bash
$ make venv
```

The final line of output from the above `make` command should look something like:

```
. .build/venv/bin/activate
```

This is the command to enter the environment with the `kplutus-pyk` library. Simply copy and paste it
into your shell and press enter. Verify you're in the environment with `which python` and you should
see a path to `.build/venv/bin`. Running `deactivate` will exit the environment and bring you
back you the one you were in before.


## Invoke kplutus-pyk with kplc

`kplc` has the parameter `--pyk` which will tell it to use `kplutus-pyk` wherever it can.
Currently, only `kompile` is implemented. You can see it in action by building the semantics
with it while in the kplutus-pyk environment:

```bash
$ make build KOMPILE_OPTS=--pyk
```


## For Developers

Use `make` to run common tasks (see the [Makefile](Makefile) for a complete list of available targets).

* `make build`: Build wheel
* `make check`: Check code style
* `make format`: Format code
* `make test-unit`: Run unit tests

For interactive use, spawn a shell with `poetry shell` (after `poetry install`), then run an interpreter.
