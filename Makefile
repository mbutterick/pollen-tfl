export POLLEN := BUILD
.PHONY: build go clean clean-helper build-helper quick

go:
	raco pollen render
	raco pollen publish

build: build-helper go

clean-helper:
	rm -rf ~/git/pollen-tfl/*
	cd ~/git/pollen-tfl
	git reset --hard HEAD

clean: clean-helper build

build-helper:
	raco pollen reset
	raco pollen setup

quick: go