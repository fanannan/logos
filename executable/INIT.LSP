;=======================================
;	初期設定
;=======================================
(setq *car-of-nil*	t)		;nilのcarを許すか(t:許可 nil:エラー)
(setq *read-eval*	t)		;#.の動作(t:許可 nil:エラー)
(setq *trace-function-message* '関数\s:\s);トレース表示(評価するＳ式)
(setq *trace-argments-message* '引数\s:\s);トレース表示(引数)
(setq *trace-result-message* '結果\s:\s);トレース表示(返り値)
(setq *prompt*		'》\s)		;プロンプトの設定
(setq *paren-counter*	t)		;括弧数表示（t:行う nil:行わない）
;(setq *console-mode*	nil)		;入力モードの設定(t:バッファードキー入力
(setq *console-mode* (or(pc9801p)(pcatp))); nil:ハンドルからの入力 )
;(バッファードキー入力に対応しているgo32でないとバッファードキー入力は使用できない)
;(また、バッファードキー入力に対応しても、ヒストリ機能を持つＴＳＲと相性が悪い場合もある)
(setq *print-paren*	t)		;Ｓ式の括弧の表示（nil:表示しない）
(setq *print-level*	100)		;表示時の括弧の深さ制限（nil:制限しない）
(setq *print-length*	100)		;表示時のＳ式の要素の個数制限
					;（nil:制限しない）
(setq *print-pretty*	t)		;quoteの略記表示を行う（nil:行わない）
(setq *print-array*	t)		;配列要素の表示（nil:表示しない）
(setq *print-escape*	t)		;文字、文字列の接頭接尾詞の表示
(setq *print-case*	:downcase)	;シンボルアトムの表示形式
					;(:upcase : 大文字 :downcase : 小文字
					; :capitalize : １文字目のみ大文字)
(setq *print-fixnum-format*	nil)	;整数の表示形式
(setq *print-floanum-format*	"%#.8g");浮動小数点数の表示形式
(setq *screen-size*	'(600 800))	:グラフィック画面のサイズ(SVGA用)
(setq *gc-flag*		t)		;ガーベジコレクタ起動表示フラグ
;(setq *gc-message* '\n\a\sGBC\sWakes\n);ガーベジコレクタ起動表示（nilなら表示せず）
(setq *gc-message* nil)
(setq *auto-memory-expansion*	t)	;メモリの自動拡張フラグ（t:拡張する）
(setq *input-echo*	nil)		;ファイル入力の画面表示
(setq *load-verbose*	nil)		;load時のファイル名の表示
(setq *load-print*	nil) ;'\s)	;load時のファイル入力結果の画面表示
					;（*load-print*の値が区切りになる）
(setq *manual-file*	"logos.doc")	;マニュアルの指定
(setq *error-header*	'\a※\s)	;エラー表示の接頭詞
(setq *error-message-file* "logos.err")	;エラーメッセージファイルの指定
(setq *editor*		"vz ")		;エディタの名前(名前の後に空白必要)
(setq *debug*		nil)		;エラー時のレジスタ内容表示フラグ

;=======================================
;	マクロ定義
;=======================================

;第７章　　制御構造

(defmacro setf (&rest l)
	(cond	((null l))
		((null (cadr l)))
		((symbolp (car l))
			(cons 'setq l))
		(t	(make-set-form (car l)(cadr l))) )
	(cons 'progn (setf-aux l)) )

(defun setf_aux	(l)
	(cond	((null l))
		((symbol (car l))
			(cons	(list 'setq (car l)(cadr l))
				(setf-aux (cddr l))) )
		(t	(cons	(make-set-form 'setq (car l)(cadr l))
				(setf-aux (cddr l))) ) ))

(defun make-set-form (p v)
	(cond	((eq (car p) 'car)
			(list 'car (list 'rplaca (cadr p) v)) )
		((eq (car p) 'cdr)
			(list 'cdr (list 'rplacd (cadr p) v)) )
		((eq (car p) 'nth)
			(list 'car (list 'rplaca (cons 'nthcdr (cdr p)) v)) )
		(t	(msg t "未定義の用法です。(Error at Setf) " (car p) t)) ))

(if (special-form-p 'let)
	(movd 'let '_let))			;特殊形式letの定義を_letに移す

(defmacro let (vars &rest body)			;マクロletの定義
	(expand-let '_let 'psetq vars body))

(defmacro let* (vars &rest body)		;マクロlet*の定義
	(expand-let '_let 'setq vars body))

(if (special-form-p 'prog)
	(movd 'prog '_prog))			;特殊形式progの定義を_progに移す

(defmacro prog (vars &rest body)		;マクロprogの定義
	(expand-let '_prog 'psetq vars body))

(defmacro prog* (vars &rest body)		;マクロprog*の定義
	(expand-let '_prog 'setq vars body))

(defun expand-let (special-form bind-type vars body)
						;let,let*,prog,prog*用展開ルーチン
	(append2
		(list	special-form		;特殊形式の種類
			(mapcar #'(lambda (x)(cond	;束縛変数
						((symbolp x) x)
						(t (car x)))) vars)
			(cons bind-type		;束縛(setq/psetq)
				(mapcan #'(lambda (x)
						(cond
							((consp x) (list (car x)(cadr x)))
							(t (list x nil))))
					vars) ))
		body ))

(defmacro do (vars end-test &rest body)		;マクロdoの定義
	(expand-do 'psetq vars end-test body))

(defmacro do* (vars end-test &rest body)	;マクロdo*の定義
	(expand-do 'setq vars end-test body))

(defun expand-do (do-type vars end-test body)
						;do,do*用展開ルーチン
 (list 'block nil
  (list	'_let
   (mapcar #'(lambda (x)(cond			;束縛変数リスト
			((symbolp x) x)
			(t (car x)))) vars)
   (cons do-type				;束縛変数の初期化
	(mapcan #'(lambda (x)(cond
			((consp x)
				(list (car x)(cadr x)))
			(t (list x nil)))) vars) )
   (cons 'loop
	(list
		(list 'if (car end-test) (cons 'return (cdr end-test)))
						;終了検査と返り値
		(cons 'tagbody 	body)		;ループ本体
		(cons 'psetq			;束縛変数の更新
			(mapcan #'(lambda (x)(cond
				((consp x)
					(list (car x) (caddr x)))
				(t nil))) vars) )) )))) 

(defmacro dolist (vars &rest body)		;マクロdolistの定義
	(let (do-var)
		(setq do-var (gensym))
		(list	'let	(list do-var)
			(list	'setq do-var (second vars))
		(list	'loop
			(list	'if
				(list 'endp do-var)
				(list 'return (third vars)) )
			(list	'setq
				(first vars)
				(list 'car do-var) )
			(cons	'tagbody body)
			(list	'setq
				do-var
				(list 'cdr do-var) ) )))))

(defmacro dotimes (vars &rest body)		;マクロdotimesの定義
		(list	'let	(list (first vars))
			(list	'setq (first vars) 0)
		(list	'loop
			(list	'if
				(list 'eq (first vars) (second vars))
				(list 'return (third vars)) )
			(cons	'tagbody body)
			(list	'incf
				(first vars) ) )))

(defmacro for (vars . body)			;Cライクなforの定義
  (list	'_let
	(list (caar vars))			;束縛変数
	(cons 'setq (first vars))		;束縛変数の初期化
	(list 'loop				;終了チェック
		(list 'unless (second vars) '(return))
		(cons 'progn 	body)		;ループ本体
		(third vars)
	) ))

(defmacro while (cond . body)			;Cライクなwhileの定義
	(list 'loop				;終了チェック
		(list 'unless cond '(return))
		(cons 'progn 	body)		;ループ本体
	))

(defmacro typecase (key &rest body)
	(cons 'case (cons (list 'type-of key) body)))

;第８章　マクロ

(dm macroexpand-1 (x)
	(cons	(cons	'lambda
			(fdefinition (car x)))
		(mapcar 'quote (cdr x)) ))

(defun macro-form-p (x)(typep x 'macro))


;第１１章　パッケージ

(defmacro do-symbols (vars &rest body)		;マクロdo-symbolsの定義
	(let (do-var)
		(setq do-var (gensym))
		(list	'let	(list do-var)
			(list	'setq do-var '(oblist))
		(list	'loop
			(list	'if
				(list 'endp do-var)
				(list 'return (second vars)) )
			(list	'setq
				(first vars)
				(list 'car do-var) )
			(cons	'tagbody body)
			(list	'setq
				do-var
				(list 'cdr do-var) ) )))))
(movd 'do-symbols 'do-all-symbols)		;マクロdo-all-symbolsの定義

;第１５章　リスト

(defun list-length (x)				;リストの長さを求める
	(do (	(n 0 (add n 2))			;(循環リストならnilを返す)
		(fast x (cddr fast))
		(slow x (cdr slow)))
		(nil)
		(when (endp fast)(return n))
		(when (endp (cdr fast))(return (add n 1)))
		(when (and (eq fast slow)(> n 0))(return))))


(defmacro push (item stack)			;プッシュ (push item stack)
 (if (eq (eval item) unk)
     (list 'setq item (list 'list stack))
     (list 'car
           (list 'setq item (list 'cons stack item))) ))

(defmacro pop (stack)				;ポップ (pop stack)
 (list  'prog1
        (list 'car stack)
        (list 'setq stack (list 'cdr stack))) )

;第１９章　構造体

(if (special-form-p 'defstruct)
	(movd 'defstruct '_defstruct))
(defmacro defstruct (&rest body)
  (prog (name)
     (setq name (car body))
       (return
	 (list	'progn
		(list 'defun			;構造体コピー関数定義
			(pack* 'copy '- name) '(n) '(copy-struct n) )
		(list 'defmacro			;構造体コンストラクタ関数定義
			(pack* 'make '- name) '(&rest slots)
			(list 'cons ''make-struct
				(list 'cons
					(eval (cons '_defstruct body))	;ここで構造体を生成
					'(prog (n)
					    (setq n 0)
					    (return
						;スロット名はquoteする
						(mapcar #'(lambda(x)
							(if (oddp (incf n))
								(kwote x)
								x)
							)
							slots
						)
					    )
					)
				)
			)
		)
		(cons 'progn
		  (append
		     (mapcar #'(lambda (x)	;構造体アクセス関数定義
				(list 'defun
					(pack* name '- (if (listp x)(car x) x))
					'(struct)
					(list 'stref 'struct 
					(kwote (if (listp x)(car x) x))
					)
				))
				(cdr body)
		     )
		     (mapcar #'(lambda (x)	;構造体スロット内容変更関数定義
				(list 'defun
					(pack* 'set '- name '- (if (listp x)(car x) x))
					'(struct content)
					(list 'set-struct 'struct 
					(kwote (if (listp x)(car x) x))
					'content)
				))
				(cdr body)
		     )
		   )
		)
	(cons '_defstruct body)			;やむを得ず２回生成する
	)
      )
   )
)


;(defstruct registers ax bx cx dx si di bp f)	;レジスタ設定の構造体定義

;第２２章　入出力

(defun pprint (l s)(prityprint l 2 s))		;プリティプリンター
(defun prityprint (l pos s)
	(cond	((atom l)(prin1 l))
		((eq (car l) 'quote)
			(princ #\' s)
			(prityprint (cadr l) (add1 pos) s)
			)
		((atom (car l))
			(princ #\( s)
			(prin1 (car l) s)
			(ppcdr (cdr l) (+ (add1 pos) (print-length (car l))) )
			(princ #\) s) )
		(t	(princ #\( s)
			(prityprint (car l) (add1 pos) s)
			(cond	((cdr l)
					(terpri s)
					(spaces pos s)
					(ppcdr (cdr l) pos s) ))
			(princ #\) s) ) ))

(defun print-length (x)
	(typecase x
		(symbol		(length (symbol-name x)))
		(string		(+ (length x) (if *print-escape* 2 0)))
		(character	(+ 3 (if *print-escape* 2 0)))
		(t 0) ))

(defun ppcdr (l pos s)
	(cond	((null l))
		((null (cdr l))
			(space s)
			(prityprint (car l) (add1 pos) s) )
		(t
			(space s)
			(prityprint (car l) (add1 pos) s)
			(terpri s)
			(spaces pos s)
			(ppcdr (cdr l) pos s) ) ))

;第２３章　ファイルシステム・インターフェース

(defmacro with-open-file (file &rest body)
	(list 'prog2 (list 'setq (first file) (cons 'open (cdr file)))
		(cons 'progn body)
		(list 'close (first file)) ) )

;第２５章　さまざまな機能

(defun ed (x)					;エディタ呼出
	(if x	(system (strcat *editor* (if (symbolp x)(symbol-name x) x)))
		(if (equalp *editor* "vz ")
			(system "vz @")
			(system *editor*))
		))

(defun deferror (number message)
	(with-open-file (file *error-message-file* :append :io :byte)
				(seek (dec (file-length *error-message-file*)) file)
				(print number file)
				(tab file)
				(prog1 (prin1 message file)(space file)) ))

(defun lisp-implementation-type () "LOGOS")
(defun machine-type () "Intel 80386 or upper CPU Machine with GO32")

;=======================================
;	汎用関数定義
;=======================================

(df msg (l)					;汎用表示関数
	"この関数は引数を評価して順番に表示します。ただし、引数が tの場合には改行し、数字の場合にはその数字分の空白を空けます。"
	(prog (l1)				;例 (msg t 'data: 1 x t)
	msgloop:
		(if(null l)(return))
		(setq l1 (car l))
		(cond
			((eq 't l1)		;t で改行
				(terpri))
			((numberp l1)		;数字分空白を空ける
				(spaces l1)
				)
			(t			;それ以降は評価して表示
				(prin1 (eval l1))))
		(setq l (cdr l))
	(go msgloop:) ))

(de que (l)					;縦に羅列して表示する
	(mapc 'print l)				;(que '(1 2 3 4 5))
	nil)

(defun locate (x y z)				;画面指定と表示
    (cond ((and (numberp x)(lessp 1 x)(lessp x 80)
                (numberp y)(lessp 1 y)(lessp y 24))
	           (princ "\e[")		;(x,y):座標 z:表示内容;
	           (princ y)			;例 (locate 20 10 'やほ！)
	           (princ "\;")
	           (princ x)
	           (princ "H")
		   (if z (princ z))) ))

(defun color (color)				;表示色設定
    (cond ((and (numberp color)(lessp 0 color)(lessp color 7))
           	(princ "\e[3")			;引数は 0-7
           	(princ color)
           	(princ "m")
           	color)))

(defun cls () (princ "\e[2J") nil)		;画面消去 (cls)

(defun trace-all ()				;全ての関数のトレース
	(eval (cons 'trace (oblist))))

(defun trace-expr ()				;全てのユーザー関数のトレース
	(eval (append '(trace)
		(mapcar (function (lambda (x) (if (typep x 'expr) x))) (oblist)))))

;=======================================
;	初期化終了表示
;=======================================

(print 'Ok)

