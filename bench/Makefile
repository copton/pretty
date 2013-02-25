.PHONY: all clean cases

all: report.html report.csv

clean:
	rm -f Bench Bench.hi Bench.o

report.csv report.html: Bench $(wildcard cases/*.c)
	./Bench -g -o report.html -u report.csv -t templates/report.tpl $(wildcard cases/*.c)

Bench: Bench.hs
	ghc -i../dist/build/autogen --make Bench.hs
