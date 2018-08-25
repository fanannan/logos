;=======================================
;	サンプル
;=======================================
;	(fib n)		フィボナッチ関数
;	(acker x y)	アッカーマン関数
;	(tree n)	樹木曲線
;	(tree2 n)	樹木曲線
;	(gm n)		グウモスキーミラの写像
;	(lorenz n)	ローレンツアトラクター
;	(c n)		Ｃ曲線
;	(dragon n)	ドラゴン曲線
;	(day)		日付を求める
;	(sieve n)	n迄の素数を求める
;	(nqueen n)	Ｎクイーン
;	(dotak)		たらい回し関数

;	(graphic-on)	グラフィックオン(IBM互換機では別途設定が必要です)

(defun fib (n)					;フィボナッチ関数
	(if	(lessp n 2)
		1
		(add	(fib (1- n))
			(fib (sub n 2)) ) ))

(defun acker (x y)				;アッカーマン関数
	(cond	((zerop x) (add1 y))
		((zerop y) (acker (sub1 x) 1))
		(else	(acker (sub1 x) (acker x (sub1 y))) )))

(defun graphic-on ()				;グラフィック初期化
	(cond	((pc9801p)(screen))		;機種やグラフィックボードで異なる
		((and (pcatp)(null **gif**))
			(screen 7)		;機種によって変更必要
			(setq **gif** t)
			)))

(setq **gif**)					;初期化フラグ
(setq *color* (if (pc9801p) 1 6))
(setq *erase* (if (pc9801p) 0 0))

(defun draw (dx dy)				;相対位置で描画
	(line *x* *y* (incf *x* dx)(incf *y* dy) *color*))

(defun move (dx dy)				;描画位置の相対移動
	(incf *x* dx)
	(incf *y* dy))

(unless (constantp '*pi*)(defconstant *pi* 3.141592653589793238)) ;円周率

(defun tree (n)					;樹木曲線 n:折り返し回数
	(prog (	(*factor1* 0.7)(*factor2* 0.7)
		(*turn1* 0.5)(*turn2* -0.5)
		(*x* 200) (*y* 300))
		(graphic-on)			;*factor* : 枝の倍率
		(gclr *erase*)			;*turn*	  : 角度の変化分
		(cls)				;*x*,*y*  : 画面の描写位置
		(branch n 50 *pi*)))		;初めの枝の長さ:50

(defun tree2 (n)				;樹木曲線(左右非対象) n:折り返し回数
	(prog (	(*factor1* 0.7)(*factor2* 0.9)
		(*turn1* 0.5)(*turn2* -0.2)
		(*x* 200) (*y* 300))
		(graphic-on)			;*factor* : 枝の倍率
		(gclr *erase*)			;*turn*	  : 角度の変化分
		(cls)				;*x*,*y*  : 画面の描写位置
		(branch n 50 *pi*)))		;初めの枝の長さ:50

(defun branch (n length angle)			;樹木曲線の枝を描画
	(let ((dx (* length (sin angle)))(dy (* length (cos angle))))
		(draw dx dy)
		(when (plusp n)
			(branch (sub1 n)(* length *factor1*)(+ angle *turn1*))
			(branch (sub1 n)(* length *factor2*)(+ angle *turn2*))
		)
		(move (- dx)(- dy))))

(defun gm (n)					;ＦＡＩ名物「鳥の羽」（グモウスキー・ミラの写像）
	(let (sx sy q myu x y)
		(graphic-on)(cls)(gclr)
		(setq	sx	200
			sy	200
			q	20
			myu	-0.802
			x	1.0
			y	1.0)
		(dotimes (i n)
			(setq	x1	(+ y (* 0.008 (- 1 (* 0.5 y y)) y)
						(fnf x))
				y	(- (fnf x1) x)
				x	x1
				x2	(+ (* x q) sx)
				y2	(+ (* y q) sy)
				col	(pixel x2 y2))
			(if(> (incf col) 7) (setq col 1))
			(plot x2 y2 col)
		)))

(defun fnf (x)
	(+ (* myu x)
	   (/	(* 2 (- 1 myu) x x)
		(+ 1 (* x x)) )) )

(defun lorenz (n)				;ローレンツ・アトラクター
	(graphic-on)(gclr *erase*)(cls)
	(let ((k 0)(x 1)(y 1)(z 1) dx dy dz
		(*a* 10.0)(*b* 28.0)(*c* (/ 8 3))(*d* 0.01))
		(loop
			(if (> k n)(return)(incf k))
			(setq	dx (* *a* (- y x))
				dy (- (* x (- *b* z)) y)
				dz (- (* x y)(* *c* z))
				x (+ x (* *d* dx))
				y (+ y (* *d* dy))
				z (+ z (* *d* dz)) )
			(plot (- 200 (* x 4))(- 300 (* z 4)) *color*) )))

(defun c (n)					;Ｃ曲線
	(prog ((*x* 200) (*y* 200))
		(graphic-on)(gclr *erase*)(cls)
		(c2 n 2 0) ))

(defun c2 (i dx dy sign)
	(if (zerop i)
		(draw (* dx 80)(* dy 80))
		(let ((x (/ (+ dx dy) 2))(y (/ (- dy dx)2)))
			(c2	(sub1 i) x y)
			(c2	(sub1 i)(- y) x) )))

(defun dragon (n)				;ドラゴン曲線
	(prog ((*x* 200) (*y* 200))
		(graphic-on)(gclr *erase*)(cls)
		(dragon2 n 2 0 1) ))

(defun dragon2 (i dx dy sign)
	(if (zerop i)
		(draw (* dx 80)(* dy 80))
		(let (	(sx (* sign dx))
			(sy (* sign dy)))
			(dragon2 (sub1 i)(/ (- dx sy)2)(/ (+ dy sx)2)1)
			(dragon2 (sub1 i)(/ (+ dx sy)2)(/ (- dy sx)2)-1) )))

(defun day ()					;任意の日付の曜日を求める
	(let (y m d *prompt* *paren-counter*
		(n (vector  '日 '月 '火 '水 '木 '金 '土)))
		(msg t '曜日を求める日付を西暦で入力して下さい t)
		(princ "年 : ")(setq y (read))
		(princ "月 : ")(setq m (read))
		(princ "日 : ")(setq d (read))
		(when (< m 3)(decf y)(incf m 12))
		(msg t 'その日の曜日は\s:\s)
		(svref n
			(rem 
		 	    (integer
				  (+ y (/ y 4)(- (/ y 100))(/ y 400)
					(/ (+ (* 13 m) 8) 5) d) )
			7) )))

(defun sieve (j)				;エラトステネスの篩い
	(let* ((n (integer (/ (- j 3) 2)))	;3 から引数までの素数を求める
		(flag (list-to-vector (make-list (add1 n) 't)))
		(count 1) i p k)
		(for ((i 0)(<= i n)(incf i))
			(when (svref flag i)
				(setq p (+ i i 3))
				(msg 1 p)
				(for ((k (+ i p))(<= k n)(incf k p))
					(set-vector flag k nil) )
				(incf count) ))
		(msg t 'primes\s:\scount t) ))

(defun nqueen (m)(queen m nil m))		;８クイーン
(defun queen (n b l)
	(cond	((zerop n))
		((or (member n b)(qp 1 b))
			(queen (sub1 n) b l))
		(t	(nconc (cond	((eq (length b)(sub1 l))
						(list (cons n b)) )
					(t	(queen l (cons n b) l)) )
				(queen (sub1 n) b l) )) ) )

(defun qp (k m)
	(cond	((null m))
		((eq (abs (sub n (car m))) k) t)
		(t	(qp (add1 k)(cdr m))) ))

(defun tak(x y z)				;たらい回し関数
	(if (greaterp y x)
		z
		(tak	(tak (1- x) y z)
			(tak (1- y) z x)
			(tak (1- z) x y))))

(defun dotak ()(tak 18 12 6))			;ベンチマーク用

(defun sum (term a next b)			;高階関数のサンプル
	(if (> a b)
		0
		(+ (term a) (sum term (next a) next b))))

(defun sum-integers (a b)			;和を求める
	(sum (function +) a (function 1+) b))

(defun sum-squares (a b)			;平方の和を求める
	(sum #'(lambda (x) (* x x)) a #'1+ b))

(defun pai (n)					;πを求める
	(* (pai-sum 1 n) 8))
(defun pai-sum (a b)
	(sum #'(lambda (x) (/ 1 (* x (+ x 2)))) a #'(lambda (x)(+ x 4)) b))


;=======================================
;	サンプル
;=======================================
(unless (fboundp 'compile)			;コンパイラが未定義ならば、
	(msg t t 'コンパイラを読み込みます t)
	(load 'comp.lsp))			;コンパイラを読み込む
;'(

(defun spacing (s x)
	(integer (sub s (1+ (if (zerop x) 0 (integer (log x 10)))))))

(defun fib-test (n)
	(let (f r)
		(msg t 3 '式 4 '→ 4 '値 8 '所要時間)
		(dotimes (x (add1 n))
			(setq r (bench-mark-test '(setq f (fib x))))
			(eval	(list
					'msg
					t 
					(spacing 2 x)
					'(list 'fib x)
					(spacing 9 f)
					'f
					7
					'r)))))

(defun pause ()
	(msg	t '何かキーを押して下さい t)
	(loop (if (kbhit)(return))))

(msg	t '＊フィボナッチ関数のサンプル＊
	t t 'まず、インタプリタで実行します。)
(fib-test 20)
(pause)
(msg	t '今度はコンパイラを使います。
	t 'ここで、フィボナッチ関数をコンパイルします。)
(compile 'fib)
(fib-test 20)
(pause)

(defun acker-test (n m)
	(let (f r)
		(msg t 3 '式 8 '→ 4 '値 8 '所要時間)
		(dotimes (x (add1 n))
		(dotimes (y (add1 m))
			(setq r (bench-mark-test '(setq f (acker x y))))
			(eval	(list
					'msg
					t 
					(spacing 2 x)
					'(list 'acker x y)
					(spacing 9 f)
					'f
					7
					'r))))))

(msg	t '＊アッカーマン関数のサンプル＊
	t t 'まず、インタプリタで実行します。)
(acker-test 3 3)
(pause)
(msg	t '今度はコンパイラを使います。
	t 'ここで、アッカーマン関数をコンパイルします。)
(compile 'acker)
(acker-test 3 3)
(pause)

(defun nqueen-test (x)
	(let (f r)
		(gbc)
		(msg t 3 '式 4 '→ 8 '値)
		(setq r (bench-mark-test '(setq f (nqueen x))))
		(msg t 2 (list 'nqueen x) 9 f)
		(msg t 3 '所要時間 3 r) ))

(msg	t '＊Ｎクイーンのサンプル＊
	t t 'まず、インタプリタで実行します。)
(nqueen-test 8)
(pause)
(msg	t '今度はコンパイラを使います。
	t 'ここで、Ｎクイーンをコンパイルします。)
(compile 'nqueen)
(nqueen-test 8)
(pause)

(defun sieve-test (x)
	(let (f r)
		(gbc)
		(msg t 3 '式 4 '→ 8 '値)
		(msg t 2 (list 'sieve x))
		(setq r (bench-mark-test '(setq f (sieve x))))
		(msg 9 f t 3 '所要時間 3 r) ))

(msg	t '＊素数を求めるサンプル＊
	t t 'まず、インタプリタで5000までの素数を求めます。)
(sieve-test 5000)
(pause)
(msg	t '今度はコンパイラを使います。
	t 'ここで、コンパイルします。)
(compile 'sieve)
(sieve-test 5000)
(pause)

(defun dotak-test ()
	(let (f r)
		(gbc)
		(msg t 3 '式 4 '→ 7 '値)
		(setq r (bench-mark-test '(setq f (dotak))))
		(msg t 2 (list 'dotak) 9 f)
		(msg t 3 '所要時間 3 r) ))

;(msg	t '＊たらい回し関数のサンプル＊
;	t t 'まず、インタプリタで実行します
;	t '\(機種によっては2ー3分かかります\))
;(dotak-test)
;(pause)
;(msg	t '今度はコンパイラを使います。
;	t 'ここで、たらい回し関数をコンパイルします。)
;(compile 'dotak)
;(dotak-test)

;)

(msg t 'グラフィックのサンプルを実行しますか？\(y/n\)\s)
(let (*prompt* *paren-counter*)(if (eq (setq a (read)) 'n) eof (eval a)))

(msg t t 'グラフィックサンプルのコンパイルをします)
(mapc 'compile '(tree tree2 gm lorenz c dragon))

(msg t t '樹木曲線)
(tree 8)
(pause)
(tree2 8)
(pause)
(msg t t 'Ｃ曲線)
(c 9)
(pause)
(msg t t 'ドラゴン曲線)
(dragon 9)
(pause)
(msg t t 'ローレンツアトラクター)
(lorenz 3000)
(pause)
(msg t t 'グモウスキーミラの写像ことＦＡＩカオスの会議室名物「鳥の羽」)
(gm 3000)
(msg t t 'おしまい\(^_^\) 5 (get-decoded-time))

