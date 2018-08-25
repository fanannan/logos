;**********************************************************
;
;	λογοσ動的束縛コンパイラ ver0.30
;
;					１９９３年４月
;					澤田高宏／《ぬえ》
;					NIFTY-PFE02147
;**********************************************************
;
;  コンパイラの使い方
;
;	function-nameをコンパイルしたい関数とすると、
;
;	(compile 'function-name)
;
;	とすることで、この関数と、この関数の中に含まれている
;	ユーザー定義関数をコンパイルします。
;
;	もし、この関数の中に含まれているユーザー関数をコンパ
;	イルしたくないときには、
;
;	(comp 'function-name)
;
;	とします。
;
;　注意事項
;
;	&restやドット対を引数とする関数やラムダ式には対応して
;	いません。
;	また、関数Ａから関数Ｂを呼び出している時、関数Ａを
;	(compile 'Ａ)すると関数Ｂもコンパイルされますが、その
;	後で関数Ｂを再定義しても、コンパイルされた関数Ａには
;	影響しないため、関数Ａの動作は変わりません。
;	　例	(defun Ａ (x)(Ｂ x))	Ａの定義
;		(defun Ｂ (X)(* x x))	Ｂの定義
;		(Ａ 10) →　100		Ａの動作
;		(defun Ｂ (X)(+ x x))	Ｂの再定義
;		(Ａ 10) →　20		インタプリタではＡの
;					動作がＢの定義変更で
;					変わる
;		(defun Ｂ (X)(* x x))	Ｂの定義を戻す
;		(compile 'Ａ)		Ａをコンパイル
;		(Ａ 10) →　100		Ａの動作は同じ
;		(defun Ｂ (X)(+ x x))	Ｂの再定義
;		(Ａ 10) →　100		コンパイルするとＢの
;					定義変更が無効
;		(Ｂ 10) →　20		Ｂの変更は有効
;
;	なお、(compile 'compile)とするとコンパイラ自身をコン
;	パイルすることができますが、約５０ＫＢの大きさになる
;	ことから、あまりお勧めしません。この際、大量のスタッ
;	クを使用しますので、スタック領域を広げてから(-k オプ
;	ション)コンパイルするようにして下さい。
;
;	compでコンパイルした結果は次の変数に入ります
;
;	*code-list*	中間コード
;	*asm-list*	アセンブラのコード(gas形式に少々準拠)
;	*object-code*	リンク用のデータ
;
;==========================================================
;
;	Lisp Compiler Program for λογοσ
;		動的束縛中間コードコンパイラ
;
;==========================================================
(defvar *code-list*)

(defun com (name)
	"引数に関数名を取り、コンパイルされた中間コード を返す"
	(prog	(arg exp type bind-list narg start-label exit-label
		*string-print* *print-escape* *uf*)

;;; ----- 開始処理 -----
		(setq type (type-of name))	;関数タイプの取得
		(unless (member type '(expr fexpr macro))
			(return (msg t "Not appropriate to compile!"t)) )
						;exprのみを対象とし、
						;組込関数なら処理中止
		(setq arg (car (symbol-function name))
		    exp (cdr (symbol-function name))
		    narg (length arg))	;引数、関数表現の取得
		(msg t "compiling : " x )
;		(msg t "--- LOGOS Compiler (Phase I) ---"
;		     t "Target Function   : " name
;		     t "Function Type     : " type
;		     t "Argment Variables : " arg
;		)				;関数の概要を表示
						;変数の初期化
		(setq	*code-list*	nil	;コンパイルされたコードのリスト
			*errors* 	0	;エラー発生数
			start-label (new-label)	;処理最初ラベル
			exit-label  (new-label)	;処理最終ラベル
			*uf*		nil	;ブロック形式判別の大域変数(lookで使う)
			*block* (set-difference
					(look #'(lambda (x)
							(or	(user-function-p* x)
								(compiled-function-p x)
							)
						)
						 exp)
					(list name))
		)
		(unless *block*
			(setq	*uf* nil
				*block* (look #'(lambda (x)
							(or	(eq x 'return-from)
								(eq x 'return)) )
						exp)
			)
		)

;;; ----- 関数導入部のコード出力 -----
		(gen-code (list '*entry name type narg))
		(when *block*
			(gen-code '(*rem *** function-block ***))
			(gen-code (list '*call start-label))
			(gen-code (list '*jump exit-label))
			(gen-label start-label)		;処理最初ラベル

			(gen-spush '%esp)		;%esp
			(gen-spush (list 'symbol name))	;ブロックの名前
			(gen-spush '(bindmark 1))	;ブロックの印
							;関数入口での処理コード出力
		)
		(setq bind-list (func-bind arg));ラムダ変数の束縛
;;; ----- 関数の本体のコンパイル -----
		(let ((*block* t))
			(mapc #'comval exp)	;関数本体のコンパイル
		)
;;; ----- 関数脱出部のコード出力 -----
		(unbind bind-list)		;変数の解放
						;関数出口での処理コード出力
		(when *block*
			(gen-skip 3)
			(gen-code '(*return))
		)
		(gen-label exit-label)
		(gen-code (list '*exit))	;終了
;;; ----- 終了処理 -----
		(msg	t "Intermdeiate Code : " (length *code-list*)
;			t
			)
		(return (if (plusp *errors*)
				nil		;エラーがあればnilを返す
				(setq *code-list* (reverse *code-list*)) )) ))
						;中間コードを返す

;==========================================================
;
;	束縛と解放	 &restには、まだ対応していない
;
;==========================================================
(defun func-bind (vars)				;変数の束縛を行う
	(gen-code (list '*rem '*** 'function-binding vars '***))
	(if *block*
		(progn
			(gen-spush '(bindmark 2))	;ブロックの印
			(when vars
				(mapcar #'bind vars '(1 2 3 4))
			)				;引数をそのまま返す
			(gen-spush '(bindmark 3))	;ブロックの印
		)
		(when vars
			(mapcar #'bind vars '(1 2 3 4))
		)					;引数をそのまま返す
	)
	vars)

(defun let-bind (vars)				;変数の束縛を行う
	(gen-code (list '*rem '*** 'let-binding vars '***))
	(gen-spush '(bindmark 2))		;ブロックの印
	(when vars
		(mapcar #'bind vars (make-list (length vars)))
		)				;引数をそのまま返す
	(gen-spush '(bindmark 3))		;ブロックの印
	vars)

(defun bind (x y)
	(if (or (not (symbolp x))(constantp x))	;束縛できない引数の時エラー
		(err-code (list '*error 'illegal 'argment x))
		(progn				;変数値の待避
			(if *block* (gen-spush (list 'symbol x)))
			(gen-spush (list 'symbol-value x))
			(gen-load  (list 'symbol-value x) y) )))

(defun unbind (vars)				;変数束縛の解放を行う
	(gen-code (list '*rem '*** 'unbind vars '***))
	(if *block*
		(progn
			(gen-skip 1)				;ブロックの印
			(when vars
				(mapcar #'(lambda (x)
					(progn
						(gen-spop (list 'symbol-value x))
						(gen-skip 1)	;ダミー
						)
					)
				(reverse vars)) )
			(gen-skip 1)				;ブロックの印
		)
		(when vars
			(mapcar #'(lambda (x)
					(progn
						(gen-spop (list 'symbol-value x))
						)
					)
				(reverse vars)) )
	)
	(gen-code '(*rem *** unbind-end ***))
	)

;==========================================================
;
;	関数評価
;
;==========================================================
(defun comval (exp x func)				;xはダミー
;(msg t "#COMVAL# :" exp)
	(cond
		((constantp exp)		;引数が定数の場合
			(if (symbolp exp)
				(gen-load 1 (list (type-of* (eval exp))(eval exp)))
				(gen-load 1 (list (type-of* exp) exp))
			))
						;(文字列,論理値,配列,構造体)
						;(文字,浮動小数点,リスト!?)
		((atom exp)			;引数がアトムの場合
			(gen-load 1 (list 'symbol-value exp)))
						;レジスタ１にその値をロード
		((or (eq (setq x (car exp)) 'quote)
		 \00\00\00\00(eq x 'function))		;引数のcarをxに取り出す
						;引数がquoteされている場合
			(gen-load 1 (list (type-of* (second exp)) (second exp))))
						;quoteされている内容をロード
		((eq (caar exp) 'lambda)	;ラムダ式のとき，
			(com-lambda exp))
						;ラムダ式の処理をする
		((eq (type-of x) 'subr)		;subr関数の時、
			(call-subr x (cdr exp)));subr関数呼出処理
		((eq (type-of x) 'expr)		;expr関数の時、
			(call-expr x (cdr exp)));expr関数呼出処理
		((eq (type-of x) 'fexpr)	;fexpr関数の時、
			(call-fexpr x (cdr exp)));fexpr関数呼出処理
		((eq (type-of x) 'macro)	;macro関数の時、
			(call-macro exp))	;macro関数呼出処理
						;以下特殊形式
		((eq x 'progn)			;progn構文の時．
			(mapc #'comval (cdr exp) ))
		((setq func (cdr (assoc x '(	;特殊形式とその処理関数名
				(or . com-or)
				(and . com-and)

				(psetq . com-psetq)
				(setq . com-setq)
				(swap . com-swap)

				(prog1 . com-prog1)
				(prog2 . com-prog2)

				(_let . com-let)

				(if . com-if)
				(when . com-when)
				(unless . com-unless)
				(cond . com-cond)
				(case . com-case)

				(block . com-block)
				(loop . com-loop)

				(_prog . com-prog)
				(tagbody . com-tagbody)
				(go . com-go)

				(return-from . com-fsubr2)
				(return . com-fsubr1)

;				(catch . com-fsubr12)
				(throw . com-fsubr12)

				(incf . com-fsubr2)
				(incq . com-fsubr2)
				(decf . com-fsubr2)
				(decq . com-fsubr2)

				(the . com-fsubr2)
			) )) )
			(funcall func (kwote (cdr exp))))
		((eq (type-of (car exp))'fsubr)	;その他の特殊形式
			(gen-load 1 (list (type-of (cdr exp))(cdr exp)))
			(gen-code (list '*link (car exp) 'fsubr 1)) )
		((typep (car exp) 'symbol)
			(gen-load 1 (list (type-of exp) exp))
			(gen-code (list '*link 'eval 'subr 1)) )
		(t	(err-code		;それ以外の時，
				(list '*Error 'Illegal 'expression exp)))
						;エラー
		) )

(defun com-lambda (exp)
	(let ((y (cdr exp)))
		(let-bind (cadar exp))		;引数の束縛
		(dolist (x (cadar exp))
			(comval (car y))
			(gen-load (list 'symbol-value x) 1)
			(setq y (cdr y))
		)
		(mapc #'comval (cddar exp))	;本体の評価
		(unbind (cadar exp))		;引数の復帰
	))

(defun call-subr (fn args nargs sargs)		;subr関数呼び出しの処理
	(gen-code (list '*rem '--- 'call fn 'subr '---))
	(if (setq nargs (length args)) nil (setq nargs 0))
	(setq sargs (symbol-args fn))
	(when (consp sargs)
		(setq sargs (length sargs))
		(if sargs sargs (setq sargs 0)))
						;自己再帰(&restには未対応)
	(comlis args)				;引数の評価
	(if (minusp sargs)
		(make-up-args args)		;多引数関数
		(progn				;通常の引数の関数
		  (load-args args fn)		;引数の設定
		  (if (lessp nargs sargs)	;引数が不足しているとき
			(mapcar #'(lambda (x)	;nilを代入する
					(if (< nargs x (1+ sargs))
						(gen-load x nil) ))
			'(1 2 3 4) ))		;subrの引数は４つまで
		)
	)
	(gen-code (list '*link fn 'subr sargs)) )

(defun call-expr (fn args nargs)		;expr関数呼び出しの処理
;(msg t fn 2 name)
	(if (eq fn name)			;自己呼出！
		(return-from call-expr (call-subr fn args)))
	(gen-code (list '*rem '--- 'call fn 'expr '---))
	(if args
		(progn
			(comlis args)		;引数の評価
			(make-up-args args 'quote)
			(gen-load 2 1)		;引数の設定 → 1
		)
		(gen-load 2 nil) )
	(gen-load 1 fn)				;関数名
	(gen-code (list '*link 'cons 'subr 2))	;関数形式生成
	(gen-code (list '*link 'eval 'subr 1)) );評価

(defun call-fexpr (fn args nargs)		;fexpr関数呼び出しの処理
						;自己呼出！
	(gen-code (list '*rem '--- 'call fn 'fexpr '---))
	(gen-load 2 (list (type-of args) args))
	(gen-load 1 fn)				;関数名
	(gen-code (list '*link 'cons 'subr 2));関数形式生成
	(gen-code (list '*link 'eval 'subr 1)) );評価

(defun call-macro (exp)				;macro関数呼び出しの処理
	(gen-code (list '*rem '--- 'call (car exp) 'macro '---))
	(comval (eval (list 'macroexpand-1 exp))) )

(defun comlis (exp)
	(dolist (x exp)
		(comval	x)
		(gen-spush 1) ))

(defun load-args (exp)					;subr関数の引数設定
	(if exp						;引数があれば、
	   (dotimes (n (length exp))			;引数の数だけ、
		   (gen-spop (- (length exp) n))	;spop節を生成する
	    )))

(defun make-up-args (exp q)				;expr,多引数subrの引数設定
	(gen-load 2 nil)
	(dotimes (n (length exp))			;引数の数だけ、
		(gen-spop 1)				;引数をリストにして
		(if q (gen-code '(*link kwote subr 1)))
		(gen-code '(*link cons subr 2))	;返す
		(gen-load 2 1) ))

;----------------------------------------------------------
;	特殊形式
;----------------------------------------------------------
(defun com-or (exp)				;or の処理
	(gen-code '(*rem *** or ***))
	(let ((lab (new-label)))
		(mapc #'(lambda (x)(com-or* x t lab)) exp)
		(gen-load 1 nil)
		(gen-label lab) ))

(defun com-and (exp)				;andの処理
	(gen-code '(*rem *** and ***))
	(let ((lab (new-label)))
		(mapc #'(lambda (x)(com-or* x nil lab)) exp)
		(gen-label lab) ))

(defun com-or* (exp f lab)
	(comval exp)
	(if f (gen-jumpt lab f)(gen-jumpnil lab f)))

(defun com-psetq (exp reg)			;psetqの処理
	(unless exp (return-from com-psetq))
	(gen-code '(*rem *** psetq ***))
	(setq reg 1)
	(do ((x exp (cddr x)))
		((endp x))			;psetqの終了判定
		(unless (symbolp (car x))
			(err-code '(*rem Error! psetq needs symbols)))
		(comval (cadr x))		;内容の展開（結果はレジスタ１）
		(gen-load reg 1)
		(gen-spush reg) 
		(setq reg (less 4 (incf reg))) );使用レジスタを増やして最適化の効率を上げる
	(do ((x (if (evenp (length exp))(reverse exp)(cons nil (reverse exp)))
		(cddr x)))
		((endp x))			;psetqの終了判定
		(gen-spop reg)
		(gen-load (list 'symbol-value (cadr x)) reg)
		(setq reg (greater 1 (decf reg))) )
	(gen-load 1 nil) )			;nilを返す

(defun com-setq (exp)				;setqの処理
	(unless exp (return-from setq))
	(gen-code '(*rem *** setq ***))
	(do ((x exp (cddr x)))
		((endp x))			;setqの終了判定
		(unless (symbolp (car x))
			(err-code '(*rem Error! setq needs symbols)))
		(comval (cadr x))		;内容の展開（結果はレジスタ１）
		(gen-load (list 'symbol-value (car x)) 1) ))

(defun com-swap (exp)				;swapの処理
	(gen-code '(*rem *** swap ***))
	(gen-load 1 (list 'symbol-value (car exp)))
	(gen-load 2 (list 'symbol-value (cadr exp)))
	(gen-load (list 'symbol-value (cadr exp)) 1)
	(gen-load (list 'symbol-value (car exp)) 2) )

(defun com-prog1 (exp f)			;prog1の処理
	(if f nil (gen-code '(*rem *** prog1 ***)))
	(comval (car exp))
	(gen-spush 1)
	(mapc #'comval (cdr exp))
	(gen-spop 1) )

(defun com-prog2 (exp)				;prog2の処理
	(gen-code '(*rem *** prog2 ***))
	(comval (car exp))
	(com-prog1 (cdr exp) t) )

(defun com-let (exp)				;letの処理
	(gen-code '(*rem *** let ***))
	(let-bind (car exp))
	(mapc #'comval (cdr exp))
	(unbind (car exp)) )

(defun com-if (exp)				;if構文の処理
	(let ((lab1 (new-label))(lab2 (new-label)))
		(gen-code '(*rem *** if ***))
		(comval (first exp))		;条件部処理
		(gen-jumpnil lab2)		;条件判断
		(gen-code '(*rem *** if true ***))
		(comval (second exp))		;条件成立時処理
		(gen-code (list '*jump lab1))
		(gen-code '(*rem *** if false ***))
		(gen-label lab2)
		(if (third exp)			;条件不成立時処理
			(comval (third exp))
			(gen-load 1 '(nil)))
		(gen-label lab1)
		(gen-code '(*rem *** if end ***)) ))

(defun com-when (exp)				;when構文の処理
	(com-when* exp 'when))

(defun com-unless (exp)				;unless構文の処理
	(com-when* exp 'unless))

(defun com-when* (exp f)
	(let ((lab1 (new-label))(lab2 (new-label)))
		(gen-code (list '*rem '*** f '***))
		(comval (car exp))			;条件部処理
		(if (eq f 'when)(gen-jumpnil lab2)(gen-jumpt lab2));条件判断
		(gen-code (list '*rem '*** f 'true '***))
		(mapc #'comval (cdr exp))		;条件成立時処理
		(gen-code (list '*jump lab1))		;条件成立後出口へ
		(gen-label lab2)			;条件不成立時nilを返す
		(gen-load 1 nil)
		(gen-label lab1)
		(gen-code (list '*rem '*** f 'end '***)) ))

(defun com-cond (exp)				;cond構文の処理
	(prog (lab1 lab2)
		(gen-code '(*rem *** cond ***))
		(setq lab1 (new-label))		;condの出口設定
	condloop:				;cond本体
		(unless exp 			;condの終わり
			(gen-load 1 nil)	;［該当無し］
			(gen-label lab1)
			(gen-code '(*rem *** cond end ***))
						;［condの出口］
			(return))
		(if (null (caar exp))	(go condloop:))
						;［条件節なし］
		(when (or (constantp (caar exp)) (eq (caaar exp) 'quote))
			(gen-code '(*rem *** cond last clause ***))
			(mapc #'comval (cdar exp))
			(gen-label lab1)
			(gen-code '(*rem *** cond end ***))
						;［condの出口］
			(return))
		(setq lab2 (new-label))
		(gen-code '(*rem *** cond condition ***))
		(comval (caar exp))
		(gen-jumpnil lab2)
		(gen-code '(*rem *** cond condition sucsess ***))
		(if (cdar exp)			;条件成立時処理
			(mapc #'comval (cdar exp))
			(gen-load 1 '(nil)))
		(gen-code (list '*jump lab1))
		(gen-label lab2)
		(setq exp (cdr exp))
		(go condloop:)))

(defun com-case (exp)				;case構文の処理
	(prog (lab1 lab2)
		(gen-code '(*rem *** case ***))
		(setq	lab1 (new-label))	;caseの出口設定
		(comval (car exp))		;key
	loop
		(setq exp (cdr exp))		;case本体
		(unless exp 			;caseの終わり
			(gen-load 1 nil)	;［該当無し］
			(gen-label lab1)
			(gen-code '(*rem *** case end ***))
			(return))		;［caseの出口］
		(setq lab2 (new-label))
		(unless (member (caar exp) '(t else otherwise))
						;これは最適化してはならない
			(gen-code (list '*jumpneq 
					1
					(list (type-of* (caar exp))(caar exp))
					lab2))
		)				;キーと一致したか？
		(if (cdar exp)			;条件成立時処理
			(mapc #'comval (cdar exp))
			(gen-load 1 '(nil)))
		(gen-code (list '*jump lab1))
		(gen-label lab2)
		(go loop)))

(defun com-block (exp)				;blockの処理
	(let ((lab1 (new-label))(lab2 (new-label)))
		(gen-code '(*rem *** block ***))
		(gen-code (list '*call lab1))		;実行先記録のために呼び出す
		(gen-code (list '*jump lab2))
		(gen-label lab1)
		(gen-spush '%esp)			;%esp
		(gen-spush (list (type-of* (car exp))(car exp)))	;ブロックの名前
		(gen-spush '(bindmark 1))		;ブロックの印
		(mapc #'comval (cdr exp))		;本体の評価
		(gen-skip 3)
		(gen-code '(*return))
		(gen-label lab2) )
	)

(defun com-loop (exp)				;loopの処理
	(gen-code '(*rem *** loop ***))
	(let* ((lab1 (new-label))(lab2 (new-label))(lab3 (new-label)))
		(gen-code (list '*call lab1))	;実行先記録のために呼び出す
		(gen-code (list '*jump lab3))
		(gen-label lab1)
		(gen-spush '%esp)		;%esp
		(gen-spush nil)			;ブロックの名前
		(gen-spush '(bindmark 1))	;ブロックの印
		(gen-label lab2)		;ラベル
		(mapc #'comval exp)		;本体の評価
		(gen-code (list '*jump lab2))	;くり返し
		(gen-label lab3)		;ラベル
		 ))

(defun com-fsubr1 (exp)				;セル無消費に変更するべき
	(gen-code (list '*rem '*** x '***))
	(if exp	(progn	(comval (car exp))
			(gen-code '(*link kwote subr 1))
			(gen-code '(*load 2 nil))
			(gen-code '(*link cons subr 2)))
		(gen-code '(*load 1 nil))
		)
	(gen-code (list '*link x 'fsubr 1))
	)

(defun com-fsubr2 (exp)
	(gen-code (list '*rem '*** x (car exp) '***))
	(gen-spush (list (type-of* (car exp)) (car exp)))
	(com-fsubr2* exp) )

(defun com-fsubr12 (exp)
	(gen-code (list '*rem '*** x (car exp) '***))
	(comval (car exp))
	(gen-code '(*link kwote subr 1))
	(gen-spush 1)
	(com-fsubr2* exp) )

(defun com-fsubr2* (exp)			;セル無消費に変更するべき
	(if (cadr exp)
		(progn
			(comval (cadr exp))
			(gen-code '(*link kwote subr 1))
			(gen-code '(*load 2 nil))
			(gen-code '(*link cons subr 2))
			(gen-load 2 1)
		)
		(gen-code '(*load 2 nil))
	)
	(gen-spop 1)
	(gen-code '(*link cons subr 2))
	(gen-code (list '*link x 'fsubr 1))
	)

(defun com-prog (exp)				;progの処理
	(let (	binding-list 
		*prog-exit*
		(prog-list (car exp))
		(lab1 (new-label))
		(lab2 (new-label))	)

		(setq exp (cdr exp))
		(gen-code '(*rem *** prog ***))

		(gen-code (list '*call lab1))		;実行先記録のために呼び出す
		(gen-code (list '*jump lab2))
		(gen-label lab1)
		(gen-spush '%esp)			;%esp
		(gen-spush '(nil))			;ブロックの名前
		(gen-spush '(bindmark 1))		;ブロックの印

		(setq binding-list (let-bind prog-list))
							;束縛と束縛リストの保存
		(com-tagbody exp)			;本体の処理(tagbody呼び出し方法に注意)
		(gen-load 1 nil)			;progの返り値は常にnil
		(unbind binding-list)			;束縛の解放
							;ただしreturnでは値を返す
		(gen-skip 3)
		(gen-code '(*return))

		(gen-label lab2)		;progの終了
		(gen-code '(*rem *** prog end ***)) ))


(defun com-tagbody (exp)			;tagbodyの処理
	(let (*go-list*)
		(gen-code '(*rem *** tagbody ***))
		(dolist (s exp)
			(when	(atom s)
					(setq *go-list*
						(cons	(list s (new-label))
							*go-list*))
						))
		(dolist (s exp)
			(cond	((atom s)
					(gen-code (list '*rem '--- 'label s '---))
					(gen-label (get-label s)) )
				((listp s)
					(comval s) )) )
	)))

(defun com-go (exp)				;goの処理
	(gen-code (list '*rem '*** 'go (car exp) '***))
	(gen-code (list '*jump (get-label (car exp)))) )

;==========================================================
;
;	補助関数
;
;==========================================================
(defun gen-spush (x)
	(gen-code (list '*spush x)))

(defun gen-spop (x)
;(msg t "gen-spop " x)
    (prog ((codes *code-list*) other-codes dir load-list data-list w)
	loop
	(unless (setq dir (caar codes))
		(err-code "対応するspushのないspopが発行されました")
		(return-from gen-spop))
;(msg t "-------  " (car codes))
	(when (eq dir '*spush)
		(setq w (car codes))
		(when (neq  (cadr w) x)		;もし引数が同じでなく、
			(unless (member (cadr w) load-list)
						;その引数が変更されてなければ、
				(setq *code-list* codes)
				(del-code)	;spush節を消し、spopを作らない
				(when
				 other-codes
				   (setq *code-list*
				     (append
				     (reverse other-codes)
				      *code-list*)))
				(gen-load x (cadr w))
				(return-from gen-spop) )
			(gen-code (list '*spop x))
			(return-from gen-spop) );最適化できなければspopを出力
		(setq *code-list* codes)	;引数が同じであれば、
		(del-code)			;spush節を消し、spopを作らない
		(when other-codes		;（引数変更がないことは確認済）
		   (setq *code-list* (append (reverse other-codes) *code-list*)))
		(return-from gen-spop) )
	(unless (or (eq dir '*link)		;引数の内容が変更されている
		    (eq dir '*spop)		;可能性のあるものがあれば
		    (and (eq dir '*load)	;spush-spopの最適化中止
		        (equal (cadar codes) x)
			(setq load-list (adjoin x load-list)) )
		    (and(eq dir '*load)
		    	(member (cadar codes) (setq data-list (adjoin (caddar codes) data-list)))
		    	)
		)
		(setq other-codes (cons (car codes) other-codes))
		(setq codes (cdr codes))	;そうでなければ、spushを探す
		(go loop) )
	(gen-code (list '*spop x)) ))		;最適化できなければspopを出力

(defun gen-load (x y w)				;前がspopの際の最適化は？
;(msg t "gen-sload " x 1 y)
	(if (equal x y)(return-from gen-load))
	(prog (z (c *code-list*))
	loop
		(setq	z (car c)
			c (cdr c))
		(if (eq (car z) '*rem) (go loop))
		(and	(eq (car z) '*load)
			(equal (cadr z) y)
			(equal (caddr z) x)
			(return-from gen-load)
		)
	)

	(when (and (eq (caar *code-list*) '*load);前の引数が同じload節ならば、
		 (equal (cadar *code-list*) x))
;(msg t "gen-sload delete (!1) ")
		(del-code)			;そのload節を消し、
		(gen-load x y)
		(return-from gen-load)
	)
	(if (and (eq (caar *code-list*) '*load)	;前のが引数が同じload節で、
		 (equal (cadar *code-list*) y)	;間に入る引数が一致していて
		 (not (and (eq (car (caddar *code-list*)) 'symbol-value)
		 	(eq (car x) 'symbol-value) ))
		 )				;前の引数と今回の引数がシンボルの値でなければ
	    (progn
		(setq w (third (car *code-list*)))
						;前の節で代入する要素を待避し、
;(msg t "gen-sload delete (!2) " x 1 w)
		(del-code)			;load節を消し、
						;無駄なloadを作らない
		(if (nequal x w)		;もし引数が同じでなければ、
			(gen-load x w) )	;引数をloadすることで
	    )					;済ませる
	    (gen-code (list '*load x y))) ))	;ここ一ヶ所で本物の生成

(defun gen-skip (s)
	(let (x y z)
		(if	(and	(eq (caar *code-list*) '*rem)
				(eq (caadr *code-list*) '*skip))
			(del-code)
			))

		(when (eq (caar *code-list*) '*skip)
			(incf s (cadar *code-list*))
			(del-code))
		(gen-code (list '*skip s))
	))


(defun gen-jumpt (label f)
;(msg t "jumpt : " label)
		(if	(eq (caar *code-list*) '*link)
			(case (cadar *code-list*) 
;				(null	(if f	(gen-code (list '*jumpt label))
;						(progn
;							(del-code)
;							(gen-jumpnil label))))
;				(not	(if f	(gen-code (list '*jumpt label))
;						(progn
;							(del-code)
;							(gen-jumpnil label))))
;				(eq
;					(del-code)
;					(gen-code (list '*jumpeq 1 2 label)))
				(zerop
					(del-code)
					(gen-code (list '*jumpeq 1 '(fixnum 0) label)))
				(onep
					(del-code)
					(gen-code (list '*jumpeq 1 '(fixnum 1) label)))
				(t	(gen-code (list '*jumpt label)))
			)
			(gen-code (list '*jumpt label))
				))

(defun gen-jumpnil (label f)
;(msg t "jumpnil : " label)
		(if	(eq (caar *code-list*) '*link)
			(case (cadar *code-list*) 
;				(null	(if f	(gen-code (list '*jumpnil label))
;						(progn
;							(del-code)
;							(gen-jumpt label))))
;				(not	(if f	(gen-code (list '*jumpnil label))
;						(progn
;							(del-code)
;							(gen-jumpt label))))
;				(eq
;					(del-code)
;					(gen-code (list '*jumpneq 1 2 label)))
				(zerop
					(del-code)
					(gen-code (list '*jumpneq 1 '(fixnum 0) label)))
				(onep
					(del-code)
					(gen-code (list '*jumpneq 1 '(fixnum 1) label)))
				(t	(gen-code (list '*jumpnil label)))
			)
			(gen-code (list '*jumpnil label))
				))

(defun gen-label (label)
;	(if (eq (cadar *code-list*) label)
;		(del-code))			;意味のないジャンプを消す
	(gen-code (list '*label label)))

(defun gen-code (code)				;コードの生成
	(setq *code-list* (cons	code
				(progn
;					(msg t "=== new code : " code)
					 *code-list*)
			)))

(defun del-code ()
;	(msg t "xxx del code : " (car *code-list*))
	(setq *code-list* (cdr *code-list*)) )

(defun err-code (u)				;エラーの生成
	(incf *errors*)				;エラー発生数の増加
	(msg t u)				;エラーの表示
	(gen-code u) )				;エラーの埋め込み

(defun new-label ()(gensym "label-"))		;ラベル（数字）の作成

(defun get-label (label)			;ラベルの探索
	(cadr (assoc label *go-list*)))

(defun type-of* (x)
	(case (type-of x)
		(subr	'symbol)
		(fsubr	'symbol)
		(expr	'symbol)
		(fexpr	'symbol)
		(macro	'symbol)
		(t	(type-of x))))

;==========================================================
;
;		中間コードコンパイラ
;
;==========================================================
(defvar *asm-list*)

(defun lap (codes)				;code-list->asm-list
	(prog (name type narg)
;		(msg t "--- LOGOS Compiler (Phase II) ---")
		(setq code (first codes))	;一つ目の中間コードを取出す
		(if (neq (first code) '*entry)	;正常なコードでないとき
			(return (msg t "***ERROR*** not compiled code!" t)))
						;異常終了する
		(setq name (second code)	;関数名
		      type (third code)		;関数のタイプ
		      narg (fourth code)	;引数の数
		      *asm-list* nil)		;機械語リストの初期化
		(setq *print-escape* nil)
		(mapc #'expand-code codes)
		(msg	t "Instruction  Size : " (length *asm-list*)
;			t
			)
		(return	(setq *asm-list* (reverse *asm-list*)))
						;アセンブラのコードを反転し
						;関数の値として返す
	)
)

(defun expand-code (order)				;中間コードの展開を行う
	(case	(first order)
		(*entry		(gen-asm (list '/ name type narg))
				(gen-asm (list '/ #\tab '\.globl name))
				(gen-asm (list '/ (make-symbol
						   (strcat
						    (symbol-name name)":")))))
		(*link
;;;				(gen-asm (list #\tab 'push '%ebx))
;;;				(gen-asm (list #\tab 'movl nil '\, '%ebx))
;;;				(gen-asm (list #\tab 'call 'print))
;;;				(gen-asm (list #\tab 'pop '%ebx))
				(gen-asm (list #\tab 'call (cadr order)))
				
		)
		
;;;;		(*linke		(gen-asm (list 'linke?)) )

		(*load		(expand-load (rest order)) )
		(*spush		(expand-spush (second order)) )
		(*spop		(expand-spop (second order)) )
		(*skip		(expand-skip (second order)) )

		(*label		(gen-asm (list 'LABEL (second order))))
		(*jump		(gen-asm (list #\tab 'jmp (second order))))
		(*jumpnil	(gen-asm (list #\tab 'cmpl '%eax '\, '(nil)))
				(gen-asm (list #\tab 'je  (second order))))
		(*jumpt		(gen-asm (list #\tab 'cmpl '%eax '\, '(nil)))
				(gen-asm (list #\tab 'jne (second order))))
		(*jumpeq	(gen-asm (list #\tab 'cmpl (setreg (second order))
						 '\, (setreg (third order))))
				(gen-asm (list #\tab 'je (fourth order))))
		(*jumpneq	(gen-asm (list #\tab 'cmpl (setreg (second order))
						 '\, (setreg (third order))))
				(gen-asm (list #\tab 'jne (fourth order))))

		(*call		(gen-asm (list #\tab 'call (cadr order))))
		(*return	(expand-ret))
		(*exit		(expand-ret)
				(gen-asm (list '/ name '---exit---)))
		(*rem		(gen-asm (list '/ (rest order))))
		(t		(gen-asm (list '***ERROR*** order))) )))

(defun expand-load (exp)
  (gen-asm (list #\tab 'movl (setreg (second exp)) '\, (setreg (first exp))) ))

(defun expand-spush (exp)
	(let ((n 4))
		(when	(and	(eq (cadar *asm-list*) 'addl)
				(eq (fifth (car *asm-list*)) '%ebp)
				(eq (cadadr *asm-list*) 'movl)
				(eq (car (fifth (cadr *asm-list*))) '%ebp)
				)
			(incf n (third (car *asm-list*)))
			(del-asm))
		(if (or (integerp exp)(registerp exp)(not (valuep exp)))
			(gen-asm (list #\tab 'movl (setreg exp) '\, (list '%ebp (- n 4))))
			(progn
				(gen-asm (list #\tab 'movl exp '\, '%esi))
				(gen-asm (list #\tab 'movl '%esi '\, (list '%ebp (- n 4))))
		)	)
	(gen-asm (list #\tab 'addl n '\, '%ebp))
	))

(defun expand-spop (exp)
	(let ((n 4))
		(when	(and	(eq (cadar *asm-list*) 'subl)
				(eq (fifth (car *asm-list*)) '%ebp))
			(incf n (third (car *asm-list*)))
			(del-asm))
		(if (or (integerp exp)(registerp exp)(not (valuep exp)))
			(gen-asm (list #\tab 'movl (list '%ebp (- n)) '\, (setreg exp) ))
			(progn
				(gen-asm (list #\tab 'movl (list '%ebp (- n)) '\, '%esi))
				(gen-asm (list #\tab 'movl '%esi '\, exp)) 
		)	)
		(gen-asm (list #\tab 'subl n '\, '%ebp)) 
	))

(defun expand-skip (exp)
	(let ((n (* exp 4)))
		(when	(and	(eq (cadar *asm-list*) 'subl)
				(eq (fifth (car *asm-list*)) '%ebp))
			(incf n (third (car *asm-list*)))
			(del-asm))
		(gen-asm (list #\tab 'subl n '\, '%ebp)) 
	) )))

(defun expand-ret ()
	(if	(and	(or	(eq (caar *asm-list*) '/)
				(eq (caar *asm-list*) 'label))
			(eq (cadadr *asm-list*) 'ret))
		(swap-asm)
		(gen-asm (list #\tab 'ret))
	))

(defun setreg (x)
	(case x
		(1 '%eax)
		(2 '%ebx)
		(3 '%ecx)
		(4 '%edx)
		(%esp '%esp)
		(t (if (and x (symbolp x))	;nilでないシンボルなら
			(list (type-of* x) x)	;(addres シンボル)
			x)) ))			;それ以外ならそのまま返す

(defun gen-asm (code)
;	(print code)
	(setq *asm-list* (cons code *asm-list*)))

(defun del-asm ()
;	(msg t "xxx del asm  : " (car *asm-list*))
	(setq *asm-list* (cdr *asm-list*)) )

(defun swap-asm ()
	(let ((x (car *asm-list*))(y (cadr *asm-list*)))
		(del-asm)(del-asm)
		(gen-asm x)(gen-asm y) ))


;==========================================================
;
;		ネイティブコードアセンブラ
;
;==========================================================
(defvar *object-code*)				;機械語が返される変数

(defun asm (codes)				;ニーモニックを機械語のリストにする
	(prog (func name)
		(setq	func (cdar codes)	;関数名、タイプ、引数数
			name (car func)		;関数名
			*label-table* '((0 . 0));ラベル表
			*size* 0		;機械語コードのサイズ
			*object-code* '(0)	;機械語出力初期化
		)
;		(msg	t "--- LOGOS Compiler (Phase III) ---")
		(mapc #'asemble codes)
		(asmpass2 (reverse *object-code*))
		(msg	t "Machine Code Size : " *size*
;			t
			)			;コードを反転し関数の値として返す
		(return	(setq *object-code* (cons func (reverse *object-code*)))) ))

(defun asemble (code)
;(msg t code)
	(if (eq (first code) '/)
		(return-from asemble))		;注釈行のとき
	(if (eq (first code) 'LABEL)		;ラベルのとき
		(return-from asemble (asm-label code)))
	(case	(second code)			;それ以外のとき
		(movl	(asm-mov code))
		(addl	(asm-add code))
		(subl	(asm-sub code))
		(cmpl	(asm-cmp code))
		(jmp	(asm-jmp code))
		(je	(asm-je code))
		(jne	(asm-jne code))
		(call	(asm-call code))
		(push	(asm-push code))
		(pop	(asm-pop code))
		(ret	(gen-object (list #xc3)))
		(t	(asm-error code)) ))

(defun asmpass2 (codes)
	(prog (pos dir pass2)
	loop	(if (null codes)(return (setq *object-code* pass2)))
		(setq	pos (car codes)
			dir (cadr codes))
;(msg t "asm 2 : " dir 2 (car dir))
		(case (car dir)
			(#xe9			;jmp
				(setq dir (append 
					'(#xe9)
					(32bits-data
						(- (cdr (assoc (second dir)  *label-table*)) pos 5)
						'disp ))) )
			(#x0f			;jmp if
				(setq dir (append 
					(list #x0f (second dir))
					(32bits-data
						(- (cdr (assoc (third dir)  *label-table*)) pos 6)
					 	'disp ))) )
			(#xe8			;call
				(if (string-search 'label- (cadr dir))
				(setq dir (append 
					'(#xe8)
					(32bits-data
						(- (cdr (assoc (second dir)  *label-table*)) pos 5)
						'disp ))) )))
		(setq	pass2 (cons dir pass2)
			codes (cddr codes))
		(go loop) ))

;----------------------------------------------------------
;	代入操作を変換
;----------------------------------------------------------
(defun asm-mov (code)
	(let ((source (third code))(dist (fifth code)))
		(cond
			((registerp source)		;レジスタ
				(asm-mov-reg source dist))
			((stackp source)		;スタック
				(asm-mov-stack source dist))
			((symbolp source)		;論理値
				(asm-mov-log source dist))
			((valuep source)		;シンボルアトムの値
				(asm-mov-value source dist))
			((listp source)			;その他のデータ
				(asm-mov-data source dist))
			(t	(asm-error code)) )))

(defun asm-mov-reg (source dist)
	(gen-object
	 (append
	  (if (and (eq source '%eax)(valuep dist))'(#xa3);acc->value
	   (list	#x89
	    (logior 
		(ash	(cond
				((registerp dist) #b11)	;reg->reg
				((stackp dist)		;reg->stack
					(if (eq (stack-type dist) 8) #b01 #b10))
				(t #b00)		;reg->value,(reg)
			) 6)				;bit 7-6
		(ash	(register-number source) 3)	;bit 5-3
		(if (or	(stackp dist)			;reg->stack
			(valuep dist))			;reg->value
			#b101				;bit 2-0
			(register-number (if (listp dist) (car dist) dist)))
	   )) )
	  (cond
		((stackp dist)
			(if (eq (stack-type dist) 8)	;displacement
				(list (if (cadr dist) (cadr dist) 0))
				(32bits-data (cadr dist) 'disp)))
		((valuep dist)	(list dist)) ))))

(defun asm-mov-stack (source dist)
	(gen-object
	 (append
	  (list	#x8b
	   (logior 
		(ash	(if (eq (stack-type source) 8) #b01 #b10)
			6)				;bit 7-6
		(ash	(register-number dist) 3)	;bit 5-3
		#b101					;bit 2-0
	   ))
	  (if (eq (stack-type source) 8)			;displacement
		(list (if (cadr source) (cadr source) 0))
		(32bits-data (cadr source) 'disp)) )))

(defun asm-mov-value (source dist)
	(gen-object
	 (append
	  (if (eq dist '%eax)'(#xa1)			;acc->value
	   (list	#x8b
	    (logior 
		#b00					;bit 7-6
		(ash	(register-number dist) 3)	;bit 5-3
		#b101					;bit 2-0
	    )))
	  (list source) )))

(defun asm-mov-data (source dist)
	(gen-object
	 (append
	  (if (registerp dist)(list (logior #xb8 (register-number dist)) )	;data->reg
	   (list	#xc7
	    (logior 
		(ash	(cond
				((stackp dist)		;data->stack
					(if (eq (stack-type dist) 8) #b01 #b10))
				((registerp (car dist))
					#b01)		;data->(reg)
				(t #b00)		;data->value
			) 6)				;bit 7-6
		#b000000				;bit 5-3
		(if (or	(stackp dist)			;reg->stack
			(valuep dist))			;reg->value
			#b101				;bit 2-0
			(register-number (if (listp dist) (car dist) dist)))
	   )) )
	  (cond
		((registerp dist)	(list source))	;data->reg
		((stackp dist)				;data->stack
			(append
			(if (eq (stack-type dist) 8)	;displacement
				(list (if (cadr dist) (cadr dist) 0))
				(32bits-data (cadr dist) 'disp)
			)
			(list source)			;data
			)
		)
		((valuep dist)		(list dist source));data->value
		((registerp (car dist))	(list source))	;data->(reg)
		 ))))

(defun asm-mov-log (source dist)			;論理値
	(asm-mov-data (list source) dist))

;----------------------------------------------------------
;	スタック操作を変換
;----------------------------------------------------------
(defun asm-push (code)
	(gen-object (list (+ #x50 (register-number (third code)) ))))

(defun asm-pop (code)
	(gen-object (list (+ #x58 (register-number (third code)) ))))

;----------------------------------------------------------
;	算術演算を変換(スタック操作専用)
;----------------------------------------------------------
(defun asm-add (code)
	(let ((op3 (third code))(op5 (fifth code)))
		(cond	((and (eq op5 '%ebp) op3)
				(gen-object (list #x83 #xc5 op3)))
			(t	(asm-error code)) )))

(defun asm-sub (code)
	(let ((op3 (third code))(op5 (fifth code)))
		(cond	((and (eq op5 '%ebp) op3)
				(gen-object (list #x83 #xed op3)))
			(t	(asm-error code)) )))

;----------------------------------------------------------
;	比較操作を変換(nil専用)
;----------------------------------------------------------
(defun asm-cmp (code)
	(let ((op3 (third code))(op5 (fifth code)))
		(cond	((and (eq op3 '%eax)(listp op5))
				(gen-object (list #x3d op5)) )	;data<>%eax
			((and (symbolp op3)(symbolp op5))
				(gen-object (list #x39 
						(logior #xc0 
							(ash (register-number op5) 3)
							(register-number op3)
							)) )	;reg<>reg
			)
			(t	(asm-error code)) )))

;----------------------------------------------------------
;	実行位置制御を変換
;----------------------------------------------------------
(defun asm-label (code)
	(setq	*label-table*
		(cons	(cons	(second code) *size*)
			*label-table*)))

(defun asm-jmp (code)
	(gen-object (list #xe9 (third code) ) ))

(defun asm-je (code)
	(gen-object (list #x0f #x84 (third code) ) ))

(defun asm-jne (code)
	(gen-object (list #x0f #x85 (third code) ) ))

(defun asm-call (code)
		(gen-object (list #xe8 (third code))) )

;----------------------------------------------------------
;	補助関数
;----------------------------------------------------------
(defun 32bits-data (n type)			;４バイトの数値列を作る
	(if (eq type 'symbol-value)(incf n 4))	;値の位置
	(list	(logand n #xff)
		(div (logand n #xff00) #x100)
		(div (logand n #xff0000) #x10000)
		(logior
			(case type
				(disp		#x00)
				(bindmark	#x00)
				(symbol-value	#x00)
				(cons		#x00)
				(cons-data	#x00)
				(nil		#x10)
				(subr		#x30)
				(fsubr		#x30)
				(expr		#x30)
				(fexpr		#x30)
				(macro		#x30)
				(symbol		#x30)
				(string		#xf0)
				(character	#x50)
				(struct		#x60)
				(array		#xe0)
				(fixnum		#x90)
				(floanum	#xc0)
				(unk		#xb0)
				(eof		#xd0)
			)
			(logand (div (logand n #xff000000) #x1000000)
				(if (eq type 'disp) #xff #xf) )
		))))

(defun registerp (x)				;レジスタか？
	(member x '(%eax %ebx %ecx %edx %ebp %esp %esi %edi)))	;レジスタでなければnil

(defun register-number (x)			;レジスタの番号を求める
	(cdr (assoc x '(
		(%eax . 0)(%ecx . 1)(%edx . 2)(%ebx . 3)
		(%esp . 4)(%ebp . 5)(%esi . 6)(%edi . 7) ))))

(defun stackp (x)				;スタックか？
	(eq (car x) '%ebp))			;スタックでなければnil

(defun stack-type (x)				;スタックのディスプレースメントタイプは？
	(if (or (null (cadr x)) 		;ディスプレースメントなしなら8
		(lessp (abs (cadr x)) #x80))	;１バイトのディスプレースメントなら32
		8 32) )

(defun valuep (x)				;シンボルアトムの値を参照しているか？
	(or	(eq (car x) 'symbol-value)
		(eq (car x) 'cons-data)
	))

(defun gen-object (code)
	(let (*print-escape* (*print-base* 16))
;		(msg #\tab #\tab code)
		(setq	*size* (+ *size* (count code))
			*object-code* (cons *size* (cons code *object-code*)))) )

(defun asm-error (code)
	(msg t "Error!\a" 2 code)
	(return-from asm)			;大域脱出
	)

(defun count (code)				;コードの長さを数える
	(let ((n 0))
		(mapc #'(lambda (x)
				(if (integerp x);数値ならコード変換済み
					(incf n);データは４バイト増加
					(incf n 4)) )
			code)
		n))

;==========================================================
;
;		リンカ
;
;==========================================================
(defun link (codes)
	(let (name type nargs x *f* (object-file "logos.obj"))
;		(msg	t "--- LOGOS Compiler (Phase IV) ---")
		(setq	name	(caar codes)		;関数名
			type	(cadar codes)		;関数のタイプ
			nargs	(caddar codes)		;引数の数
			codes	(rest codes)		;コード本体
			*first-position* (second (codearea)) )
;		(with-open-file (*f* object-file :byte :output)
			(mapcar #'linkone codes) 
;			)
		(setq x (address-of name))
		(poke (+ x 12)(rem *first-position* #x100))
		(poke (+ x 13)(rem (div *first-position* #x100) #x100))
		(poke (+ x 14)(rem (div *first-position* #x10000) #x100))
		(poke (+ x 15)(rem (div *first-position* #x1000000) #x100))
		(poke (+ x 16)(if nargs nargs 0))	;args
		(poke (+ x 17)(if (eq type 'expr) 1 2))	;expr->subr or not->2 ?
		(let ((*print-base* 16)(*print-radix* t))
			(msg	t "Code Address from : " *first-position*
				t "             to   : " (sub1 (second (codearea)))
				t)
		)
	))

(defun linkone (code)
;(msg t "code >>> " code #\tab)
	(and	(eq (car code) #xe8)			;外部関数の呼出(call)
		(symbolp (cadr code))
		(setq code (append '(#xe8)
				(32bits-data
					(sub	(if (eq name (cadr code))
							*first-position*
							(symbol-function (cadr code)) 
							)
						(add (second (codearea)) 5) )
					'disp)) ))
	(mapc #'set-code code)				;実アドレスの算定と書き込み
	)

(defun set-code (x)
;(msg t (second (codearea)) 1 x)
	(if (integerp x)
		(progn (poke-code* x)			;(write-byte x *f*)
		)
		(let (y (data (car x)))			;ＧＢＣのためにPLISTにデータを保持
			(unless (member data '(symbol symbol-value fixnum character
						eof unk bindmark nil))
				(setq y (getprop name data))
				(remprop name data)
				(put name data (cons (second (codearea)) y))

;				(let ((*print-base* 16))
;					(msg t "Data : " data 2 (getprop name data))
;				)
			)
			(mapc #'poke-code* (32bits-data (address-of (cadr x)) data))
		) ))

(defun poke-code* (n)
	(if (minusp n)(poke-code (add n 256))(poke-code n)))

;(defun set-code ()(return-from set-code))
;(defun poke ()(return-from poke))

;==========================================================
;
;	コンパイラ呼出関数
;
;==========================================================
(defun look (pred f)				;引数x中にあるpredを満たす要素を求める
		(if (atom f) 
			(if (pred f) (setq *uf* (adjoin f *uf*)))
			(progn	(look pred (car f))
				(look pred (cdr f))
				*uf*
			)))

(setq user-function-p* #'(lambda (x)(or (typep x 'expr)(typep x 'fexpr))))

(defun comp (x)
	(and	(user-function-p* x)
		(com x)
		(lap *code-list*)
		(asm *asm-list*)
		(link *object-code*)))

(defun compile (x *w*)
	(if (functionp x)(compile* x)))

(defun compile* (x *print-escape*)
	(let (*uf*)
		(setq *w* (adjoin x *w*))
		(look user-function-p* (fdefinition x))
;		(progn	(msg t "compile   : " x 2) t)
		(if *uf* (mapc #'compile* (set-difference *uf* *w*)))
		(comp x) ))

;==========================================================
;
;	 コンパイラサンプル
;
;==========================================================
(defun ap (u v)(cond ((null u) v)(t (cons (car u)(ap (cdr u) v)))))

(defun tak (x y z)
	(if (not (< y x))
		z
		(tak 	(tak (1- x) y z)
			(tak (1- y) z x)
			(tak (1- z) x y))))
(defun fib (n)
;	(print n)
	(if	(lessp n 2)
		1
		(add	(fib (1- n))
			(fib (sub n 2)) ) ))

