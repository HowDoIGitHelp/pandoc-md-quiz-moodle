MERGEBIN := $(shell cabal list-bin merge-md)
FILTERBIN := $(shell cabal list-bin md-quiz-moodle)
QUESTIONS1 := examples/exampleQuestions.md
ANSWERS1 := examples/exampleAnswers.md
OUTPUT1 := examples/output1.xml
QUESTIONS2 := examples/exampleSAQuestions.md
ANSWERS2 := examples/exampleSAAnswers.md
OUTPUT2 := examples/output2.xml

all: build convert-multichoice convert-shortanswer

convert-multichoice:
	> $(OUTPUT1)
	$(MERGEBIN) -q $(QUESTIONS1) -a $(ANSWERS1) | \
		pandoc -t json | \
		$(FILTERBIN) \ | \
		pandoc -f json \
		-t plain | \
		XMLLINT_INDENT="    " xmllint --format - --output $(OUTPUT1)

convert-shortanswer:
	> $(OUTPUT2)
	$(MERGEBIN) -q $(QUESTIONS2) -a $(ANSWERS2) | \
		pandoc -t json | \
		$(FILTERBIN) \ | \
		pandoc -f json \
		-t plain | \
		XMLLINT_INDENT="    " xmllint --format - --output $(OUTPUT2)

merge:
	$(MERGEBIN) -q $(QUESTIONS) -a $(ANSWERS)

build:
	cabal build

clean:
	rm -r dist-newstyle
	rm $(OUTPUT1)
	rm $(OUTPUT2)
