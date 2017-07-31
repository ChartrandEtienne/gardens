
build:
	stack build

exec:
	stack exec garden -- big_oneframe.gif 9 out.gif

install:
	cp ./.stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/garden/garden ~/scripts/garden
