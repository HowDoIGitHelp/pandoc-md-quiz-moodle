MERGEBIN := $(shell cabal list-bin merge-md)
QUESTIONS := exampleQuestions.md
ANSWERS := exampleAnswers.md

all: build merge

merge:
	$(MERGEBIN) -a $(QUESTIONS) -q $(ANSWERS)

build:
	cabal build

clean:
	rm -r dist-newstyle
