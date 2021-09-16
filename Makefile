
SHELL = /bin/bash
OS := $(shell uname)

COMMON = $(PWD)/dotconfig/common.sh
HOOK = "\nif [ -e $(COMMON) ]; then\n\tsource $(COMMON)\nfi"
ifeq ($(OS), Darwin)
	BASHRC = ~/.bash_profile
else
	BASHRC = ~/.bashrc
endif


.PHONY: init update install uninstall clean

init:
	git submodule update --init --recursive

update:
	git pull --recurse-submodules

install:
	git config --global include.path "$(PWD)/dotconfig/.gitconfig.aliases"
	shopt -s xpg_echo && echo $(HOOK) >> $(BASHRC)

uninstall:
	rm -rf .git dotconfig subrepos
	rm .gitignore .gitmodule LICENSE Makefile README.md

clean:
	sh subrepos/mac-cleanup/mac-cleanup
	sudo rm -rf $(TMPDIR)*


.PHONY: macos brew linux

macos:
	sudo softwareupdate -i -a
	xcode-select --install

brew:
	/bin/bash -c "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

linux:
	apt-get update
	apt-get upgrade -y
	apt-get dist-upgrade -f
