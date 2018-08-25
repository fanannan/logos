/================================
/	λογοσ ver 0.40
/
/	定義ファイル
/================================


#define	public		.globl		/* 読み変え */
#define	db		.byte		/* 読み変え */
#define	dw		.word		/* 読み変え */
#define	dd		.long		/* 読み変え */
#define	str		.ascii		/* 読み変え */
#define	asz		.asciz		/* 読み変え */
#define	ds		.space		/* 読み変え */
#define	ALIGN		.align	2,144	/* 読み変え */

#define	DOS		$0x21		/* int21h */
#define	CON		0		/* 標準入力（コンソール） */
#define	CRT		1		/* 標準出力（画面） */
#define	ERO		2		/* 標準エラー出力（画面） */
#define	AUX		3		/* 標準補助装置（ＲＳ−２３２） */
#define	PRN		4		/* 標準プリンタ */

#define	SCREENMODE	1
#define	MOUSE		$0x33

#define	MINIMUM_AREA	64		/* 各領域の最低確保量（キロバイト）*/
#define	CODESIZE	0x10000		/* コンパイルされたコード領域の大きさ*/
#define	CELLSIZE	0x100000	/* コンス領域の大きさ（バイト） */
#define	ARRAYSIZE	0x10000		/* 配列領域の大きさ（バイト） */
#define	STRUCTSIZE	0x10000		/* 構造体領域の大きさ（バイト） */
#define	SYMBOLSIZE	0x20000		/* シンボル領域の大きさ（バイト）*/
#define	STRINGSIZE	0x10000		/* 文字列領域の大きさ（バイト） */
#define	FLOANUMSIZE	0x10000		/* 浮動小数点数領域の大きさ（バイト）*/
#define	STACKSIZE	0x80000		/* スタック領域の大きさ（バイト）*/

#define	HASHSIZE	255		/* ハッシュテーブルの大きさ */

/*	偶数のタグの付いているものがセルで表現されるもの*/
#define	CELLTAG 0x0000000 /*		セル		*/
#define	NILTAG 0x10000000 /*		nil		*/
#define	SYMBOLTAG 0x30000000 /*		シンボルアトム	*/
#define	CHARACTERTAG 0x50000000 /*	文字		*/
#define	STRUCTTAG 0x60000000 /*		構造体		*/
#define	NUMBERTAG 0x90000000 /*		整数		*/
#define	UNKTAG 0xb0000000 /*		未定義値	*/
#define	FLOATTAG 0xc0000000 /*		実数		*/
#define	EOFTAG 0xd0000000 /*		ファイル終端	*/
#define	ARRAYTAG 0xe0000000 /*		配列		*/
#define	STRINGTAG 0xf0000000 /*		文字列		*/
/#define ARRAYTAG 0x20000000 /*		配列(予備)	*/
/#define VECTORTAG 0x40000000 /*	予備(配列)	*/
/#define RATIONALTAG 0x80000000 /*	予備(有理数)	*/
/#define COMPLEXTAG 0xa0000000 /*	予備(複素数)	*/
/#define STRINGTAG 0x70000000 /*	"7"はスタックとぶつかるため使用不可 */

#define	TOPBIT	27 /*			最大有効ビット	*/
#define MASK 0xf0000000 /*		タグのみを取るマスク */
#define UNMASK 0x0fffffff /*		タグをはずすマスク */
#define	LOW3MASK 0x0fffffff /*		下位３バイトを有効にするマスク */
#define	BINDMARK1 0x00000001 /*		束縛時の目印	*/
#define	BINDMARK2 0x00000002
#define	BINDMARK3 0x00000003
#define	BINDMARK4 0x00000004
#define	BINDMARK5 0x00000005

#define	RESTROOM	0x100		/* GBCを起動する残り領域の大きさ */

#define	BUFSIZE		0x1000		/* 入力バッファの長さ */
#define	NAMELENGTH	0x1000		/* アトムの最大の長さ */
#define	MAXBASE	36			/* 最大の基数 */

#define SPC_CHR 0x20 /*		スペース */
#define HT_CHR 0x09 /*		ハードタブ */
#define PAGE_CHR 0x0c /*	ページ */
#define RET_CHR 0x0d /*		リターン */
#define LF_CHR 0x0a /*		ラインフィード */
#define BACKSPC_CHR 0x08 /*	バックスペース */
#define BELL_CHR 0x07 /*	ベル */
#define YEN_CHR 0x5c /*		\ */
#define ESC_CHR 0x5c /*		\ */
#define	EOF_CHR 0x1a /*		ファイル終端 */
#define DOSESC_CHR 0x1b /*	ＭＳＤＯＳのエスケープ文字 */
#define REM_CHR 0x3b /*		注釈の開始記号 ';' ?? */
#define LPAR_CHR 0x28 /*	( */
#define RPAR_CHR 0x29 /*	) */
#define DOT_CHR 0x2e /*		. */
#define PLUS_CHR 0x2b /*	+ */
#define MINUS_CHR 0x2d /*	- */
#define SHARP_CHR 0x23 /*	# */
#define QUOTE_CHR 0x27 /*	' */
#define BACKQUOTE_CHR 0x60 /*	` */
#define COMMA_CHR 0x2c /*	, */
#define AT_CHR 0x40 /*		@ */
#define STR_SEPARATER 0x22 /*	" */

#define	NEXTATOM	0
#define	VALUE	4
#define	PLIST	8
#define	FUNCTION	12
#define	FUNCTYPE	16
/#define	FUNCARGS	17
/#define	VALUETYPE	18
#define	ATOMNAME	20

#define	CONST	0x01000000
#define	SPECIAL	0x02000000

#define	SUBR	1
#define	SUBR0	0x00000100
#define	SUBR1	0x00000101
#define	SUBR2	0x00000102
#define	SUBR3	0x00000103
#define	SUBR4	0x00000104
#define	SUBRN	0x0000010f

#define	FSUBR	2
#define	FSUBR0	0x00000200

#define	EXPR	3
#define	FEXPR	4
#define	MACRO	5

#define	ARRAYTYPE	0
#define	DISPLACE	4
#define	INDEX_OFFSET	8
#define	DIMENSION	12
#define	ARRAYS		16

#define	ERR_MSG_BUFFER 1008 /* 1000+8 */

#define	TAKE_CHAR	call	__take_char
#define	NEXT_CHAR	incl	bufferpointer
#define	BACK_CHAR	decl	bufferpointer
#define	PUSHBUFFER	call	__pushbuffer
#define	POPBUFFER	call	__popbuffer

#define	NUMEXTEND	call	__numextend
#define	NUMEXTEND2	call	__numextend2
#define	NUMEXTEND3	call	__numextend3
#define	NUMEXTEND4	call	__numextend4
#define	FLOANUMPE	call	__floanump_e
#define	UNKP		call	__unkp
#define	UNKP2		call	__unkp2
#define	EOFP		call	__eofp
#define	EOFP2		call	__eofp2
#define	TP		call	__tp
#define	NILP		cmpl	$NILTAG,%eax
#define	NILP2		cmpl	$NILTAG,%ebx
#define	NILP3		cmpl	$NILTAG,%ecx
#define	NILP4		cmpl	$NILTAG,%edx

#define	UNMASK1		andl	$UNMASK,%eax
#define	UNMASK2		andl	$UNMASK,%ebx
#define	UNMASK3		andl	$UNMASK,%ecx
#define	UNMASK4		andl	$UNMASK,%edx

#define	CAR		call	l_car
#define	CDR		call	l_cdr
#define	M_CAR		movl	(%eax),%eax
#define	M_CDR		movl	4(%eax),%eax

#define	PUSH1		pushl	%eax
#define	PUSH2		pushl	%ebx
#define	PUSH3		pushl	%ecx
#define	PUSH4		pushl	%edx
#define	POP1		popl	%eax
#define	POP2		popl	%ebx
#define	POP3		popl	%ecx
#define	POP4		popl	%edx

#define	SET1(x)		movl	x,%eax
#define	SET2(x)		movl	x,%ebx
#define	SET3(x)		movl	x,%ecx
#define	SET4(x)		movl	x,%edx

#define	SET1NIL		movl	$NILTAG,%eax
#define	SET2NIL		movl	$NILTAG,%ebx
#define	SET3NIL		movl	$NILTAG,%ecx
#define	SET4NIL		movl	$NILTAG,%edx

#define	SET1_2		movl	%ebx,%eax
#define	SET1_3		movl	%ecx,%eax
#define	SET1_4		movl	%edx,%eax
#define	SET2_1		movl	%eax,%ebx
#define	SET2_3		movl	%ecx,%ebx
#define	SET2_4		movl	%edx,%ebx
#define	SET3_1		movl	%eax,%ecx
#define	SET3_2		movl	%ebx,%ecx
#define	SET3_4		movl	%edx,%ecx
#define	SET4_1		movl	%eax,%edx
#define	SET4_2		movl	%ebx,%edx
#define	SET4_3		movl	%ecx,%edx

#define	ERRNO(x)	movl	$x,err_no
#define	JUMP_ERR	jmp	err

#define PREAX		call	__preax
#define PRSTR		call	__prstr
#define PRCHR		call	__prchr
#define	PRLF		call	__lf
#define	PRBELL		call	__bell
#define	PRSPC		call	__spc

