
PATH:=/home/arnold/builds/emacs/src:${PATH}

emacs-test:
	emacs --script emacs-common.el

jenkins:
	PATH=${PATH} which emacs
	PATH=${PATH} $(MAKE) emacs-test
