all: experiments.native permutation.cmo

FORCE:
	true

%.native %.byte %.cmo: FORCE
	corebuild \
	    -cflag -g -j 4 \
	    -pkg core_extended,async \
	    $@

