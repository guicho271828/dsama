
all: test.tsv
	./run.sh

test.tsv: test.ros
	./test.ros 1000 5 3 > $@


%.pretty.json: %.json
	python -m json.tool $*.json > $@

%.json: %.pddl


clean:
	-rm *.json *.model
allclean: clean
	-rm test.tsv
