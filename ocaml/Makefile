all: experiments.native permutation.cmo iet.cmo side_pair.cmo

FORCE:
	true

%.native %.byte %.cmo: FORCE
	corebuild \
	    -j 4 \
	    -cflag -g \
	    -cflags -w,-40 \
	    -pkg core,async \
	    $@

