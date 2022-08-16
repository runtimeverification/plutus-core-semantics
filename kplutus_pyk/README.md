# Building kplutus_pyk

## Prerequisites

On ubuntu:
```bash
$ sudo apt install python3.8 python3-pip # any python>=3.8 can be installed here
$ python3 -m pip install virtualenv      # make sure `python3 --version` is correct!
```

## Build

```bash
$ make venv
```

# Using kplutus_pyk

## Enter the environment

The final line of output from the above `make` command should look something like:

```
. /path/to/kplutus_pyk/venv/bin/activate
```

This is the command to enter the environment with the kplutus_pyk library. Simply copy and paste it
into your shell and press enter. You will notice `(venv)` prefixed to your prompt. You can leave
the environment by running `deactivate`.

## Invoke kplutus_pyk with kplc

`kplc` has the parameter `--pyk` which will tell it to use `kplutus_pyk` wherever it can.
Currently, only `kompile` is implemented. You can see it in action by building the semantics
with it:

```bash
(venv) $ make build KOMPILE_OPTS=--pyk
```

## Invoke kplutus_pyk from the command line

```bash
(venv) $ python3 -m kplutus_pyk
```
