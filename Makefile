
SHELL = /bin/bash
OS := $(shell uname)


.PHONY: init update install uninstall clean

init:
	git submodule update --init --recursive

update:
	git pull --recurse-submodules

install:
	git config --global include.path "$PWD/dotconfig/.gitconfig.aliases"
	ifeq ($(OS), Darwin)
		echo "source $PWD/dotconfig/common.sh" >> ~/.bash_profile
	else
		echo "source $PWD/dotconfig/common.sh" >> ~/.bashrc
	endif

uninstall:
	rm -rf .git dotconfig subrepos
	rm .gitignore .gitmodule LICENSE Makefile README.md

clean:
	sh subrepos/mac-cleanup/mac-cleanup
	rm -rf $TMPDIR


.PHONY: macos linux

macos:
	sudo softwareupdate -i -a
	xcode-select --install

linux:
	apt-get update
	apt-get upgrade -y
	apt-get dist-upgrade -f
