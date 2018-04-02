
PATH:=/home/arnold/builds/emacs/src:${PATH}

emacs-test:
	which emacs
	emacs --no-desktop -q  --no-init-file --kill -l jenkins.el

jenkins:
	PATH=${PATH} timeout 60 $(MAKE) emacs-test
