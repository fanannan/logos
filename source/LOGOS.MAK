a.out	:			d:\logos.o \
				d:\lgbc.o \
				d:\lexpand.o \
				d:\ltype.o \
				d:\ldeclare.o \
				d:\lsymbol.o \
				d:\lmath.o \
				d:\lchar.o \
				d:\lseq.o \
				d:\llist.o \
				d:\lfile.o \
				d:\lread.o \
				d:\leval.o \
				d:\lpred.o \
				d:\lcont.o \
				d:\larray.o \
				d:\lstruct.o \
				d:\lstring.o \
				d:\lmatch.o \
				d:\letc.o \
				d:\lprint.o \
				d:\lgraphic.o \
				d:\latom.o \
				c:\logos\go32\logos.doc
	del	*.o*
	ld	@c:\logos\go32\linkf.mak -lc -lgr -lgr_p
	strip	+strip-all	a.out
	ren	a.out	logos.org
	del	*.out
	ld	@c:\logos\go32\linkf.mak -lc -lgr98
	strip	+strip-all	a.out
	ren	a.out	logos.out
	bdiff	-i	logos.org logos.out
	del	logos.out
	ren	logos.org	logos.out
	copy	logos.out	c:\logos\logos05\*.*
	copy	logos.bdf	c:\logos\logos05\*.*
#				d:\ltext.o \
#	ld -M	@linkf.mak -lc > c:\logos\go32\map
#				d:\llink.o \

d:\logos.o	:	c:\logos\go32\logos.p \
			c:\logos\go32\logos.inc
	sed	-f logos.cnv c:\logos\go32\logos.p > d:\logos.q
	cpp	d:/logos.q > d:\logos.s
	as	-o  d:/logos.o   d:/logos.s


d:\lgbc.o	:	c:\logos\go32\lgbc.p \
			c:\logos\go32\logos.inc
	sed	-f logos.cnv c:\logos\go32\lgbc.p > d:\lgbc.q
	cpp d:/lgbc.q > d:\lgbc.s
	as -o  d:/lgbc.o d:/lgbc.s

d:\lexpand.o	:	c:\logos\go32\lexpand.p \
			c:\logos\go32\logos.inc
	sed	-f logos.cnv c:\logos\go32\lexpand.p > d:\lexpand.q
	cpp d:/lexpand.q > d:\lexpand.s
	as -o  d:/lexpand.o d:/lexpand.s

d:\lfile.o	:	c:\logos\go32\lfile.p \
			c:\logos\go32\logos.inc
	sed	-f logos.cnv c:\logos\go32\lfile.p > d:\lfile.q
	cpp	d:/lfile.q > d:\lfile.s
	as -o	d:/lfile.o d:/lfile.s

#d:\llink.o	:	c:\logos\go32\llink.p \
#			c:\logos\go32\logos.inc
#	sed	-f logos.cnv c:\logos\go32\llink.p > d:\llink.q
#	cpp	d:/llink.q > d:\llink.s
#	as -o	d:/llink.o d:/llink.s

d:\lread.o	:	c:\logos\go32\lread.p \
			c:\logos\go32\logos.inc
	sed	-f logos9.cnv c:\logos\go32\lread.p > d:\lread.q9
	sed	-f logos.cnv d:\lread.q9 > d:\lread.q
	cpp	d:/lread.q > d:\lread.s
	as -o	d:/lread.o   d:/lread.s

d:\leval.o	:	c:\logos\go32\leval.p \
			c:\logos\go32\logos.inc
	sed	-f logos9.cnv c:\logos\go32\leval.p > d:\leval.q9
	sed	-f logos.cnv d:\leval.q9 > d:\leval.q
	cpp	d:/leval.q > d:\leval.s
	as -o  d:/leval.o   d:/leval.s

d:\ltype.o	:	c:\logos\go32\ltype.p \
			c:\logos\go32\logos.inc
	sed	-f logos.cnv c:\logos\go32\ltype.p > d:\ltype.q
	cpp	d:/ltype.q > d:\ltype.s
	as -o	d:/ltype.o   d:/ltype.s

d:\lpred.o	:	c:\logos\go32\lpred.p \
			c:\logos\go32\logos.inc
	sed	-f logos.cnv c:\logos\go32\lpred.p > d:\lpred.q
	cpp	d:/lpred.q > d:\lpred.s
	as -o	d:/lpred.o   d:/lpred.s

d:\lcont.o	:	c:\logos\go32\lcont.p \
			c:\logos\go32\logos.inc
	sed	-f logos.cnv c:\logos\go32\lcont.p > d:\lcont.q
	cpp	d:/lcont.q > d:\lcont.s
	as -o	d:/lcont.o   d:/lcont.s

d:\ldeclare.o	:	c:\logos\go32\ldeclare.p \
			c:\logos\go32\logos.inc
	sed	-f logos.cnv c:\logos\go32\ldeclare.p > d:\ldeclare.q
	cpp	d:/ldeclare.q > d:\ldeclare.s
	as -o	d:/ldeclare.o   d:/ldeclare.s

d:\lsymbol.o	:	c:\logos\go32\lsymbol.p \
			c:\logos\go32\logos.inc
	sed	-f logos.cnv c:\logos\go32\lsymbol.p > d:\lsymbol.q
	cpp	d:/lsymbol.q > d:\lsymbol.s
	as -o	d:/lsymbol.o   d:/lsymbol.s

d:\lmath.o	:	c:\logos\go32\lmath.p \
			c:\logos\go32\logos.inc
	sed	-f logos.cnv c:\logos\go32\lmath.p > d:\lmath.q
	cpp	d:/lmath.q > d:\lmath.s
	as -o	d:/lmath.o d:/lmath.s

d:\lchar.o	:	c:\logos\go32\lchar.p \
			c:\logos\go32\logos.inc
	sed	-f logos.cnv c:\logos\go32\lchar.p > d:\lchar.q
	cpp	d:/lchar.q > d:\lchar.s
	as -o	d:/lchar.o   d:/lchar.s

d:\lseq.o	:	c:\logos\go32\lseq.p \
			c:\logos\go32\logos.inc
	sed	-f logos.cnv c:\logos\go32\lseq.p > d:\lseq.q
	cpp	d:/lseq.q > d:\lseq.s
	as -o	d:/lseq.o   d:/lseq.s

d:\llist.o	:	c:\logos\go32\llist.p \
			c:\logos\go32\logos.inc
	sed	-f logos.cnv c:\logos\go32\llist.p > d:\llist.q
	cpp	d:/llist.q > d:\llist.s
	as -o	d:/llist.o d:/llist.s

d:\larray.o	:	c:\logos\go32\larray.p \
			c:\logos\go32\logos.inc
	sed	-f logos.cnv c:\logos\go32\larray.p > d:\larray.q
	cpp	d:/larray.q > d:\larray.s
	as -o	d:/larray.o   d:/larray.s

d:\lstruct.o	:	c:\logos\go32\lstruct.p \
			c:\logos\go32\logos.inc
	sed	-f logos.cnv c:\logos\go32\lstruct.p > d:\lstruct.q
	cpp	d:/lstruct.q > d:\lstruct.s
	as -o	d:/lstruct.o   d:/lstruct.s

d:\lstring.o	:	c:\logos\go32\lstring.p \
			c:\logos\go32\logos.inc
	sed	-f logos.cnv c:\logos\go32\lstring.p > d:\lstring.q
	cpp	d:/lstring.q > d:\lstring.s
	as -o	d:/lstring.o   d:/lstring.s

d:\lmatch.o	:	c:\logos\go32\lmatch.p \
			c:\logos\go32\logos.inc
	sed	-f logos.cnv c:\logos\go32\lmatch.p > d:\lmatch.q
	cpp	d:/lmatch.q > d:\lmatch.s
	as -o	d:/lmatch.o d:/lmatch.s

d:\letc.o	:	c:\logos\go32\letc.p \
			c:\logos\go32\logos.inc
	sed	-f logos.cnv c:\logos\go32\letc.p > d:\letc.q
	cpp	d:/letc.q > d:\letc.s
	as -o 	d:/letc.o   d:/letc.s

d:\lprint.o	:	c:\logos\go32\lprint.p \
			c:\logos\go32\logos.inc
	sed	-f logos.cnv c:\logos\go32\lprint.p > d:\lprint.q
	cpp	d:/lprint.q > d:\lprint.s
	as -o	d:/lprint.o   d:/lprint.s

#d:\ltext.o	:	c:\logos\go32\ltext.p \
#			c:\logos\go32\logos.inc
#	sed	-f logos.cnv c:\logos\go32\ltext.p > d:\ltext.q
#	cpp	d:/ltext.q > d:\ltext.s
#	as -o	d:/ltext.o   d:/ltext.s

d:\lgraphic.o	:	c:\logos\go32\lgraphic.p \
			c:\logos\go32\logos.inc
	sed	-f logos.cnv c:\logos\go32\lgraphic.p > d:\lgraphic.q
	cpp	d:/lgraphic.q > d:\lgraphic.s
	as -o	d:/lgraphic.o d:/lgraphic.s

d:\latom.o	:	c:\logos\go32\latom.p \
			c:\logos\go32\logos.inc
	sed	-f logos.cnv c:\logos\go32\latom.p > d:latom.q
	cpp	d:/latom.q > d:\latom.s
	as	-o d:/latom.o   d:/latom.s

c:\logos\go32\logos.doc	:	c:\logos\go32\logos.fpf
	fplf	c:\logos\go32\logos.fpf
	tab -E8	<c:\logos\go32\logos.doc >c:\logos\go32\logos.tmp
	del	c:\logos\go32\logos.doc
	ren	c:\logos\go32\logos.tmp logos.doc
	copy	logos.doc	c:\logos\logos05\*.*

