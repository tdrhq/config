
PATH:=/home/arnold/builds/emacs/src:${PATH}

emacs-test:
	test -d ~/.emacs.d || mkdir ~/.emacs.d
	which emacs
	emacs --no-desktop -q  --no-init-file --kill --batch --eval '(progn (package-initialize))' -l jenkins.el

jenkins:
	PATH=${PATH} timeout 10 $(MAKE) emacs-test
