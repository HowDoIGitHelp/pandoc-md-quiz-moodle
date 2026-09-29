MERGEBIN := $(shell cabal list-bin merge-md)
FILTERBIN := $(shell cabal list-bin md-quiz-moodle)
QUESTIONS := examples/exampleQuestions.md
ANSWERS := examples/exampleAnswers.md
OUTPUT := examples/output.xml

all: build convert

convert:
	> $(OUTPUT)
	$(MERGEBIN) -q $(QUESTIONS) -a $(ANSWERS) | \
		pandoc -t json | \
		$(FILTERBIN) \ | \
		pandoc -f json \
		-t plain | \
		XMLLINT_INDENT="    " xmllint --format - --output $(OUTPUT)

merge:
	$(MERGEBIN) -q $(QUESTIONS) -a $(ANSWERS)

build:
	cabal build

clean:
	rm -r dist-newstyle
