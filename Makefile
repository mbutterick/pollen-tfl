export POLLEN := BUILD
.PHONY: build go clean clean-helper build-helper quick test

build: test build-helper go

go:
	raco pollen render -p
	raco pollen publish


clean-helper:
	rm -rf ~/git/pollen-tfl/*
	cd ~/git/pollen-tfl
	git reset --hard HEAD

clean: clean-helper build

build-helper:
	raco pollen reset
	raco pollen setup -p

quick: go

test:
	echo $${POLLEN}