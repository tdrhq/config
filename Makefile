
PATH:=/home/arnold/builds/emacs/src:${PATH}

emacs-test:
	mkdir -f ~/.emacs.d
	which emacs
	emacs --no-desktop -q  --no-init-file --kill --batch -l jenkins.el

jenkins:
	PATH=${PATH} timeout 10 $(MAKE) emacs-test
