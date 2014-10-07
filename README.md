Product browser
===============

Some day I'll lay weather radar products that I've been collecting on top of a
map with this thing.


Prerequisites
-------------

- Put World borders dataset from http://www.mappinghacks.com/data/ into
  sub directory `data`
- Get http://shapelib.maptools.org/ and build a shared library out of it.

Something like the following added to shapelib's Makefile should do. Or just
install from your OS.

    libshp.so.1:	$(LIBOBJ)
    	$(CC) -shared -Wl,-soname,libshp.so.1 -o libshp.so.1 $(LIBOBJ) -lc

- Index created from World borders into a file with .idx extension. Run
  `shptreedump -maxdepth 12 -v -o data/TM_WORLD_BORDERS-0.3..idx data/TM_WORLD_BORDERS-0.3.`
  or similar to do that.


To debug FFI stuff
------------------

Launch with `gdb --args racket product-browser.rkt`.

Type in the following in GDB shell:

    handle SIGSEGV nostop
    handle SIGSEGV noprint
    r

Misc
----

- Excellent reference to shapefiles by ESRI: http://shapelib.maptools.org/dl/shapefile.pdf
