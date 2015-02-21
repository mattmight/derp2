INSTALLDIR=/usr/local/bin

all: bin/derp2

bin:
	mkdir bin

bin/derp2: bin
	raco exe -o bin/derp2 src/derp2.rkt 

install: bin/derp2
	cp -v bin/derp2 $(INSTALLDIR)

clean:
	rm -rf bin

