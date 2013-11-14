all: example.native example.byte

FORCE:
	true

%.native %.byte %.cmo: FORCE
	corebuild \
	    -j 4 \
	    -cflag -g \
	    -cflags -w,-40 \
	    -pkg core \
	    $@

