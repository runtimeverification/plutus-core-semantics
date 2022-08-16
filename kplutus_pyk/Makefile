.PHONY: all clean $(VENV_DIR)

MAKEFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
MAKEFILE_DIR  := $(dir $(MAKEFILE_PATH))

all: venv

# virtualenv

VENV_DIR      := venv
VENV	      := $(VENV_DIR)/pyvenv.cfg
ACTIVATE_VENV := . $(MAKEFILE_DIR)$(VENV_DIR)/bin/activate

$(VENV):
	   virtualenv -p python3 $(VENV_DIR)    \
	&& $(ACTIVATE_VENV)                     \
	&& pip install -r requirements/base.txt

$(VENV_DIR): $(VENV)
	@echo $(ACTIVATE_VENV)

clean:
	rm -rf $(VENV_DIR)
	find -type d -name __pycache__ -prune -exec rm -rf {} \;
