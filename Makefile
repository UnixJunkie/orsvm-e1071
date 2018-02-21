.PHONY: install uninstall reinstall

build:
	jbuilder build @install

clean:
	rm -rf _build

edit:
	emacs src/*.ml &

install:
	jbuilder build @install
	jbuilder install

uninstall:
	jubilder uninstall

reinstall: uninstall install
