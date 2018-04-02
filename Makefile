

emacs-test:
	emacs --script emacs-common.el

jenkins:
	PATH=/home/arnold/builds/emacs/src:${PATH} $(MAKE) emacs-test
