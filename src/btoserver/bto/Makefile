

btoblas:
	cd src; $(MAKE) btoblas
	cp -f src/btoblas bin/btoblas

install:
	cd hardware_profiling; $(MAKE)
	cd src; $(MAKE) btoblas
	cp -f src/btoblas bin/btoblas

clean:
	cd src; $(MAKE) clean
	cd bin; rm -f *
