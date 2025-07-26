all:
	mkdir -p bin
	csc -X r7rs -R r7rs -static src/main.scm -o bin/scm-checker

clean:
	rm -rf bin
