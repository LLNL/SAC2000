%%DocumentProcessColors: Black
%%DocumentFonts: Helvetica
%%DocumentProcSets: Adobe_packedarray 1.0 0
%%DocumentSuppliedProcSets: Adobe_packedarray 1.0 0
%%DocumentProcSets: Adobe_cmykcolor 1.1 0
%%DocumentSuppliedProcSets: Adobe_cmykcolor 1.1 0
%%DocumentProcSets: Adobe_cshow 1.1 0
%%DocumentSuppliedProcSets: Adobe_cshow 1.1 0
%%DocumentProcSets: Adobe_customcolor 1.0 0
%%DocumentSuppliedProcSets: Adobe_customcolor 1.0 0
%%DocumentProcSets: Adobe_pattern 1.9 0
%%DocumentSuppliedProcSets: Adobe_pattern 1.9 0
%%DocumentProcSets: Adobe_Illustrator88 1.19 0
%%DocumentSuppliedProcSets: Adobe_Illustrator88 1.19 0
%%BoundingBox: 119 325 525 604
%%ColorUsage: Black&White
%AI3_IncludePlacedImages
%%TemplateBox: 288 360 288 360
%%TileBox: 0 0 552 728
%%DocumentPreview: Header
%%EndComments
%%BeginProcSet: Adobe_packedarray 1.0 0
userdict /Adobe_packedarray 5 dict dup begin put
/initialize
{
/packedarray where
	{
	pop
	}
	{
	Adobe_packedarray begin
	Adobe_packedarray
		{
		dup xcheck
			{
			bind
			} if
		userdict 3 1 roll put
		} forall
 end
	} ifelse
} def
/terminate
{
} def
/packedarray
{
array astore readonly
} def
/setpacking
{
pop
} def
/currentpacking
{
false
} def
currentdict readonly pop end
%%EndProcSet
Adobe_packedarray /initialize get exec
%%BeginProcSet: Adobe_cshow 1.1 0
currentpacking true setpacking
userdict /Adobe_cshow 3 dict dup begin put
/initialize
{
/cshow where
	{
	pop
	}
	{
	userdict /Adobe_cshow_vars 1 dict dup begin put
	/_cshow
		{} def
	Adobe_cshow begin
	Adobe_cshow
		{
		dup xcheck
			{
			bind
			} if
		userdict 3 1 roll put
		} forall
 end
 end
	} ifelse
} def
/terminate
{
} def
/cshow
{
exch
Adobe_cshow_vars
	exch /_cshow
	exch put
	{
	0 0 Adobe_cshow_vars /_cshow get exec
	} forall
} def
currentdict readonly pop end
setpacking
%%EndProcSet
%%BeginProcSet: Adobe_customcolor 1.0 0
currentpacking true setpacking
userdict /Adobe_customcolor 5 dict dup begin put
/initialize
{
/setcustomcolor where
	{
	pop
	}
	{
	Adobe_customcolor begin
	Adobe_customcolor
		{
		dup xcheck
			{
			bind
			} if
		pop pop
		} forall
 end
	Adobe_customcolor begin
	} ifelse
} def
/terminate
{
currentdict Adobe_customcolor eq
	{
 end
	} if
} def
/findcmykcustomcolor
{
5 packedarray
}  def
/setcustomcolor
{
exch
aload pop pop
4
	{
	4 index mul 4 1 roll
	} repeat
5 -1 roll pop
setcmykcolor
} def
/setoverprint
{
pop
} def
currentdict readonly pop end
setpacking
%%EndProcSet
%%BeginProcSet: Adobe_pattern 1.9 0
currentpacking true setpacking
userdict /Adobe_pattern 14 dict dup begin put
/initialize
{
/definepattern where
	{
	pop
	}
	{
	Adobe_pattern begin
	Adobe_pattern
		{
		dup xcheck
			{
			bind
			} if
		pop pop
		} forall
	mark
	cachestatus 7 1 roll pop pop pop pop exch pop exch
		{
		{
		10000 add
		dup 2 index gt
			{
			break
			} if
		dup setcachelimit
		} loop
		} stopped
	cleartomark
	} ifelse
} def
/terminate
{
currentdict Adobe_pattern eq
	{
 end
	} if
} def
errordict
/nocurrentpoint
{
pop
stop
} put
errordict
/invalidaccess
{
pop
stop
} put
/patternencoding
256 array def
0 1 255
{
patternencoding exch ( ) 2 copy exch 0 exch put cvn put
} for
/definepattern
{
17 dict begin
/uniform exch def
/cache exch def
/key exch def
/procarray exch def
/mtx exch matrix invertmatrix def
/height exch def
/width exch def
/ctm matrix currentmatrix def
/ptm matrix def
/str 32 string def
/slice 9 dict def
slice /s 1 put
slice /q 256 procarray length div sqrt floor cvi put
slice /b 0 put
/FontBBox [0 0 0 0] def
/FontMatrix mtx matrix copy def
/Encoding patternencoding def
/FontType 3 def
/BuildChar
	{
	exch
 begin
	slice begin
	dup q dup mul mod s idiv /i exch def
	dup q dup mul mod s mod /j exch def
	q dup mul idiv procarray exch get
	/xl j width s div mul def
	/xg j 1 add width s div mul def
	/yl i height s div mul def
	/yg i 1 add height s div mul def
	uniform
		{
		1 1
		}
		{
		width 0 dtransform
		dup mul exch dup mul add sqrt dup 1 add exch div
		0 height dtransform
		dup mul exch dup mul add sqrt dup 1 add exch div
		} ifelse
	width 0 cache
		{
		xl 4 index mul yl 4 index mul xg 6 index mul yg 6 index mul
		setcachedevice
		}
		{
		setcharwidth
		} ifelse
	gsave
	scale
	newpath
	xl yl moveto
	xg yl lineto
	xg yg lineto
	xl yg lineto
	closepath
	clip
	newpath
 end
 end
	exec
	grestore
	} def
key currentdict definefont
end
} def
/patterncachesize
{
gsave
newpath
0 0 moveto
width 0 lineto
width height lineto
0 height lineto
closepath
patternmatrix setmatrix
pathbbox
exch ceiling 4 -1 roll floor sub 3 1 roll
ceiling exch floor sub
mul 1 add
grestore
} def
/patterncachelimit
{
cachestatus 7 1 roll pop pop pop pop pop pop 8 mul
} def
/patternpath
{
exch dup begin setfont
ctm setmatrix
concat
slice exch /b exch slice /q get dup mul mul put
FontMatrix concat
uniform
	{
	width 0 dtransform round width div exch round width div exch
	0 height dtransform round height div exch height div exch
	0 0 transform round exch round exch
	ptm astore setmatrix
	}
	{
	ptm currentmatrix pop
	} ifelse
{currentpoint} stopped not
	{
	pop pop
	pathbbox
	true
	4 index 3 index eq
	4 index 3 index eq
	and
		{
		pop false
			{
			{pop pop}
			{pop pop pop true}
			{pop pop pop pop pop pop pop true}
			{pop true}
			pathforall
			} stopped
			{
			pop pop pop pop pop true
			} if
		} if
		{
		height div ceiling height mul 4 1 roll
		width div ceiling width mul 4 1 roll
		height div floor height mul 4 1 roll
		width div floor width mul 4 1 roll
		2 index sub height div ceiling cvi exch
		3 index sub width div ceiling cvi exch
		4 2 roll moveto
		FontMatrix mtx invertmatrix
		dup dup 4 get exch 5 get rmoveto
		ptm ptm concatmatrix pop
		slice /s
		patterncachesize patterncachelimit div ceiling sqrt ceiling cvi
		dup slice /q get gt
			{
			pop slice /q get
			} if
		put
		0 1 slice /s get dup mul 1 sub
			{
			slice /b get add
			gsave
			0 1 str length 1 sub
				{
				str exch 2 index put
				} for
			pop
			dup
				{
				gsave
				ptm setmatrix
				1 index str length idiv {str show} repeat
				1 index str length mod str exch 0 exch getinterval show
				grestore
				0 height rmoveto
				} repeat
			grestore
			} for
		pop pop
		}
		{
		pop pop pop pop
		} ifelse
	} if
end
} def
/patternclip
{
clip
} def
/patternstrokepath
{
strokepath
} def
/patternmatrix
matrix def
/patternfill
{
dup type /dicttype eq
	{
	Adobe_pattern /patternmatrix get
	} if
gsave
patternclip
Adobe_pattern /patternpath get exec
grestore
newpath
} def
/patternstroke
{
dup type /dicttype eq
	{
	Adobe_pattern /patternmatrix get
	} if
gsave
patternstrokepath
true
	{
		{
			{
			newpath 
			moveto
			}
			{
			lineto
			}
			{
			curveto
			}
			{
			closepath
			3 copy
			Adobe_pattern /patternfill get exec
			} pathforall
		pop pop	pop
		} stopped
			{
			pop pop pop pop pop
			patternclip
			Adobe_pattern /patternfill get exec
			} if
	}
	{
	patternclip
	Adobe_pattern /patternfill get exec
	} ifelse
grestore
newpath
} def
/patternashow
{
3 index type /dicttype eq
	{
	Adobe_pattern /patternmatrix get 4 1 roll
	} if
	{
	pop pop (0) exch
	2 copy 0 exch put pop
	gsave
	false charpath
	currentpoint
	6 index 6 index 6 index
	Adobe_pattern /patternfill get exec
	grestore
	newpath moveto
	2 copy rmoveto
	} exch cshow
pop pop pop pop pop
} def
/patternashowstroke
{
4 index type /dicttype eq
	{
	patternmatrix /patternmatrix get 5 1 roll
	} if
4 1 roll
	{
	pop pop (0) exch
	2 copy 0 exch put pop
	gsave
	false charpath
	currentpoint
	4 index setmatrix
	7 index 7 index 7 index
	Adobe_pattern /patternstroke get exec
	grestore
	newpath moveto
	2 copy rmoveto
	} exch cshow
pop pop pop pop pop pop
} def
currentdict readonly pop end
setpacking
%%EndProcSet
%%BeginProcSet: Adobe_Illustrator88 1.19 0
currentpacking true setpacking
userdict /Adobe_Illustrator88 73 dict dup begin put
/initialize
{
userdict /Adobe_Illustrator88_vars 31 dict dup begin put
/_lp /none def
/_pf {} def
/_ps {} def
/_psf {} def
/_pss {} def
/_a null def
/_as null def
/_tt 2 array def
/_tl 2 array def
/_tm matrix def
/t {} def
/_gf null def
/_cf 4 array def
/_if null def
/_of false def
/_fc {} def
/_gs null def
/_cs 4 array def
/_is null def
/_os false def
/_sc {} def
/_pd 1 dict def
/_ed 15 dict def
/_pm matrix def
/_fm null def
/_fd null def
/_fdd null def
/_sm null def
/_sd null def
/_sdd null def
/_i null def
Adobe_Illustrator88 begin
Adobe_Illustrator88 dup /nc get begin
	{
	dup xcheck
		{
		bind
		} if
	pop pop
	} forall
end
end
end
Adobe_Illustrator88 begin
Adobe_Illustrator88_vars begin
newpath
} def
/terminate
{
end
end
} def
/_
null def
/ddef
{
Adobe_Illustrator88_vars 3 1 roll put
} def
/xput
{
dup load dup length exch maxlength eq
	{
	dup dup load dup
	length 2 mul dict copy def
	} if
load begin def end
} def
/npop
{
	{
	pop
	} repeat
} def
/sw
{
stringwidth
exch 5 -1 roll 3 index 1 sub mul add
4 1 roll 3 1 roll 1 sub mul add
} def
/ss
{
3 -1 roll pop
4 1 roll
	{
	2 npop (0) exch
	2 copy 0 exch put pop
	gsave
	false charpath
	currentpoint
	4 index setmatrix
	stroke
	grestore
	moveto
	2 copy rmoveto
	} exch cshow
3 npop
} def
/sp
{
exch pop
	{
	2 npop (0) exch
	2 copy 0 exch put pop
	false charpath
	2 copy rmoveto
	} exch cshow
2 npop
} def
/pl
{
transform
0.25 sub round 0.25 add exch
0.25 sub round 0.25 add exch
itransform
} def
/setstrokeadjust where
{
pop true setstrokeadjust
/c
{
curveto
} def
/C
/c load def
/v
{
currentpoint 6 2 roll curveto
} def
/V
/v load def
/y
{
2 copy curveto
} def
/Y
/y load def
/l
{
lineto
} def
/L
/l load def
/m
{
moveto
} def
}
{
/c
{
pl curveto
} def
/C
/c load def
/v
{
currentpoint 6 2 roll pl curveto
} def
/V
/v load def
/y
{
pl 2 copy curveto
} def
/Y
/y load def
/l
{
pl lineto
} def
/L
/l load def
/m
{
pl moveto
} def
} ifelse
/d
{
setdash
} def
/cf
currentflat def
/i
{
dup 0 eq
	{
	pop cf
	} if
setflat
} def
/j
{
setlinejoin
} def
/J
{
setlinecap
} def
/M
{
setmiterlimit
} def
/w
{
setlinewidth
} def
/H
{} def
/h
{
closepath
} def
/N
{
newpath
} def
/n
/N load def
/F
{
_pf
} def
/f
{
closepath
F
} def
/S
{
_ps
} def
/s
{
closepath
S
} def
/B
{
gsave F grestore
S
} def
/b
{
closepath
B
} def
/W
{
clip
} def
/ta
{
_as moveto
_tt aload pop 4 -2 roll
} def
/tl
{
_tl aload pop translate
} def
/as
{
{
0 0
}
{
2 copy _tt aload pop 4 -2 roll sw
exch neg 2 div exch neg 2 div
}
{
2 copy _tt aload pop 4 -2 roll sw
exch neg exch neg
}
{
0 0
}
} cvlit def
/z
{
/_a exch ddef
/_as as _a get ddef
_a 2 le
	{
	0 _tt astore pop
	0 exch neg _tl astore pop
	}
	{
	0 exch neg _tt astore pop
	neg 0 _tl astore pop
	} ifelse
exch findfont exch scalefont setfont
} def
/tm
{
_tm currentmatrix pop
concat
} def
/I
{
tm
/t
	{
	ta sp
	tl
	} ddef
} def
/o
{
tm
/t
	{
	ta 4 npop
	tl
	newpath
	} ddef
} def
/e
{
tm
/t
	{
	ta _psf
	tl
	newpath
	} ddef
} def
/r
{
tm
/t
	{
	ta _tm _pss
	tl
	newpath
	} ddef
} def
/a
{
tm
/t
	{
	2 copy
	ta _psf
	newpath
	ta _tm _pss
	tl
	newpath
	} ddef
} def
/T
{
_tm setmatrix
} def
/Z
{
pop
findfont begin
currentdict dup length 1 add dict begin
	{
	1 index /FID ne
		{
		def
		}
		{
		2 npop
		} ifelse
	} forall
/FontName exch def dup length 0 ne
	{
	/Encoding Encoding 256 array copy def
	0 exch
		{
		dup type /nametype eq
			{
			Encoding 2 index 2 index put pop
			1 add
			}
			{
			exch pop
			} ifelse
		} forall
	} if pop
currentdict dup end end
/FontName get exch definefont pop
} def
/u
{} def
/U
{} def
/q
{
gsave
} def
/Q
{
grestore
} def
/`
{
/_i save ddef
6 1 roll 4 npop
concat
userdict begin
/showpage {} def
false setoverprint
pop
} def
/~
{
end
_i restore
} def
/@
{} def
/&
{} def
/O
{
0 ne
/_of exch ddef
/_lp /none ddef
} def
/R
{
0 ne
/_os exch ddef
/_lp /none ddef
} def
/g
{
/_gf exch ddef
/_fc
{
_lp /fill ne
	{
	_of setoverprint
	_gf setgray
	/_lp /fill ddef
	} if
} ddef
/_pf
{
_fc
fill
} ddef
/_psf
{
_fc
exch pop
ashow
} ddef
/_lp /none ddef
} def
/G
{
/_gs exch ddef
/_sc
{
_lp /stroke ne
	{
	_os setoverprint
	_gs setgray
	/_lp /stroke ddef
	} if
} ddef
/_ps
{
_sc
stroke
} ddef
/_pss
{
_sc
ss
} ddef
/_lp /none ddef
} def
/k
{
_cf astore pop
/_fc
{
_lp /fill ne
	{
	_of setoverprint
	_cf aload pop setcmykcolor
	/_lp /fill ddef
	} if
} ddef
/_pf
{
_fc
fill
} ddef
/_psf
{
_fc
exch pop
ashow
} ddef
/_lp /none ddef
} def
/K
{
_cs astore pop
/_sc
{
_lp /stroke ne
	{
	_os setoverprint
	_cs aload pop setcmykcolor
	/_lp /stroke ddef
	} if
} ddef
/_ps
{
_sc
stroke
} ddef
/_pss
{
_sc
ss
} ddef
/_lp /none ddef
} def
/x
{
/_gf exch ddef
findcmykcustomcolor
/_if exch ddef
/_fc
{
_lp /fill ne
	{
	_of setoverprint
	_if _gf 1 exch sub setcustomcolor
	/_lp /fill ddef
	} if
} ddef
/_pf
{
_fc
fill
} ddef
/_psf
{
_fc
exch pop
ashow
} ddef
/_lp /none ddef
} def
/X
{
/_gs exch ddef
findcmykcustomcolor
/_is exch ddef
/_sc
{
_lp /stroke ne
	{
	_os setoverprint
	_is _gs 1 exch sub setcustomcolor
	/_lp /stroke ddef
	} if
} ddef
/_ps
{
_sc
stroke
} ddef
/_pss
{
_sc
ss
} ddef
/_lp /none ddef
} def
/dp
{
dup null eq
{
pop
_dp 0 ne
	{
	0 1 _dp 1 sub _dl mod
		{
		_da exch get 3 get
		} for
	_dp 1 sub _dl mod 1 add packedarray
	_da 0 get aload pop 8 -1 roll 5 -1 roll pop 4 1 roll
	definepattern pop
	} if
}
{
_dp 0 ne _dp _dl mod 0 eq and
	{
	null dp
	} if
7 packedarray _da exch _dp _dl mod exch put
_dp _dl mod _da 0 get 4 get 2 packedarray
/_dp _dp 1 add def
} ifelse
} def
/E
{
_ed begin
dup 0 get type /arraytype ne
	{
	0
		{
		dup 1 add index type /arraytype eq
			{
			1 add
			}
			{
			exit
			} ifelse
		} loop
	array astore
	} if
/_dd exch def
/_ury exch def
/_urx exch def
/_lly exch def
/_llx exch def
/_n exch def
/_y 0 def
/_dl 4 def
/_dp 0 def
/_da _dl array def
0 1 _dd length 1 sub
	{
	/_d exch _dd exch get def
	0 2 _d length 2 sub
		{
		/_x exch def
		/_c _d _x get _ ne def
		/_r _d _x 1 add get cvlit def
		_r _ ne
			{
			_urx _llx sub _ury _lly sub [1 0 0 1 0 0] 
				[
				/save cvx
				_llx neg _lly neg /translate cvx
				_c
					{
					nc /begin cvx
					} if
				_r dup type /stringtype eq
					{
					cvx
					}
					{
					{exec} /forall cvx
					} ifelse
				_c
					{
					/end cvx
					} if
				/restore cvx
				] cvx
			/_fn 12 _n length add string def
			_y _fn cvs pop
			/_y _y 1 add def
			_fn 12 _n putinterval
			_fn _c false dp
			_d exch _x 1 add exch put
			} if
		} for
	} for
null dp
_n _dd /_pd
end xput
} def
/fc
{
_fm dup concatmatrix pop
} def
/p
{
/_fm exch ddef
9 -2 roll _pm translate fc
7 -2 roll _pm scale fc
5 -1 roll _pm rotate fc
4 -2 roll exch 0 ne
	{
	dup _pm rotate fc
	1 -1 _pm scale fc
	neg _pm rotate fc
	}
	{
	pop
	} ifelse
dup _pm rotate fc
exch dup sin exch cos div 1 0 0 1 0 6 2 roll
_pm astore fc
neg _pm rotate fc
_pd exch get /_fdd exch ddef
/_pf
{
save
0 1 _fdd length 1 sub
	{
	/_fd exch _fdd exch get ddef
	_fd
	0 2 _fd length 2 sub
		{
		gsave
		2 copy get dup _ ne
			{
			cvx exec _fc
			}
			{
			pop
			} ifelse
		2 copy 1 add get dup _ ne
			{
			aload pop findfont _fm
			patternfill
			}
			{
			pop
			fill
			} ifelse
		grestore
		pop
		} for
	pop
	} for
restore
newpath
} ddef
/_psf
{
save
0 1 _fdd length 1 sub
	{
	/_fd exch _fdd exch get ddef
	_fd
	0 2 _fd length 2 sub
		{
		gsave
		2 copy get dup _ ne
			{
			cvx exec _fc
			}
			{
			pop
			} ifelse
		2 copy 1 add get dup _ ne
			{
			aload pop findfont _fm
			10 copy 6 npop exch pop patternashow
			}
			{
			pop
			7 copy 3 npop exch pop ashow
			} ifelse
		grestore
		pop
		} for
	pop
	} for
restore
4 npop newpath
} ddef
/_lp /none ddef
} def
/sc
{
_sm dup concatmatrix pop
} def
/P
{
/_sm exch ddef
9 -2 roll _pm translate sc
7 -2 roll _pm scale sc
5 -1 roll _pm rotate sc
4 -2 roll exch 0 ne
	{
	dup _pm rotate sc
	1 -1 _pm scale sc
	neg _pm rotate sc
	}
	{
	pop
	} ifelse
dup _pm rotate sc
exch dup sin exch cos div 1 0 0 1 0 6 2 roll
_pm astore sc
neg _pm rotate sc
_pd exch get /_sdd exch ddef
/_ps
{
save
0 1 _sdd length 1 sub
	{
	/_sd exch _sdd exch get ddef
	_sd
	0 2 _sd length 2 sub
		{
		gsave
		2 copy get dup _ ne
			{
			cvx exec _sc
			}
			{
			pop
			} ifelse
		2 copy 1 add get dup _ ne
			{
			aload pop findfont _sm
			patternstroke
			}
			{
			pop stroke
			} ifelse
		grestore
		pop
		} for
	pop
	} for
restore
newpath
} ddef
/_pss
{
save
0 1 _sdd length 1 sub
	{
	/_sd exch _sdd exch get ddef
	_sd
	0 2 _sd length 2 sub
		{
		gsave
		2 copy get dup _ ne
			{
			cvx exec _sc
			}
			{
			pop
			} ifelse
		2 copy 1 add get dup _ ne
			{
			aload pop findfont _sm
			11 copy 6 npop 3 -1 roll pop patternashowstroke
			}
			{
			pop
			8 copy 3 npop ss
			} ifelse
		grestore
		pop
		} for
	pop
	} for
restore
5 npop newpath
} ddef
/_lp /none ddef
} def
/A
{
pop
} def
/nc 3 dict def
nc begin
/setgray
{
pop
} bind def
/setcmykcolor
{
4 npop
} bind def
/setcustomcolor
{
2 npop
} bind def
currentdict readonly pop end
currentdict readonly pop end
setpacking
%%EndProcSet
%%EndProlog
%%BeginSetup
Adobe_cshow /initialize get exec
Adobe_customcolor /initialize get exec
Adobe_pattern /initialize get exec
Adobe_Illustrator88 /initialize get exec
%%BeginEncoding: _Helvetica Helvetica
[39/quotesingle 96/grave 128/Adieresis/Aring/Ccedilla/Eacute/Ntilde/Odieresis
/Udieresis/aacute/agrave/acircumflex/adieresis/atilde/aring/ccedilla/eacute
/egrave/ecircumflex/edieresis/iacute/igrave/icircumflex/idieresis/ntilde
/oacute/ograve/ocircumflex/odieresis/otilde/uacute/ugrave/ucircumflex
/udieresis/dagger/degree/cent/sterling/section/bullet/paragraph/germandbls
/registered/copyright/trademark/acute/dieresis/.notdef/AE/Oslash
/.notdef/plusminus/.notdef/.notdef/yen/mu/.notdef/.notdef
/.notdef/.notdef/.notdef/ordfeminine/ordmasculine/.notdef/ae/oslash
/questiondown/exclamdown/logicalnot/.notdef/florin/.notdef/.notdef
/guillemotleft/guillemotright/ellipsis/.notdef/Agrave/Atilde/Otilde/OE/oe
/endash/emdash/quotedblleft/quotedblright/quoteleft/quoteright/divide
/.notdef/ydieresis/Ydieresis/fraction/currency/guilsinglleft/guilsinglright
/fi/fl/daggerdbl/periodcentered/quotesinglbase/quotedblbase/perthousand
/Acircumflex/Ecircumflex/Aacute/Edieresis/Egrave/Iacute/Icircumflex
/Idieresis/Igrave/Oacute/Ocircumflex/.notdef/Ograve/Uacute/Ucircumflex
/Ugrave/dotlessi/circumflex/tilde/macron/breve/dotaccent/ring/cedilla
/hungarumlaut/ogonek/caron
]/_Helvetica/Helvetica 0 Z
%%EndEncoding
%%EndSetup
0 A
0 R
0 G
0 i
0 J 0 j 0.3 w 1 M []0 d
%%Note:
