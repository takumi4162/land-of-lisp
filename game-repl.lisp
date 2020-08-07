;; ----- game-repl関数の定義 -----
;; 仕様:ゲームをプレイするためのCLI用の関数の定義
(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))


;; ----- game-read 関数の定義 -----
;; 仕様: ゲームのコマンドをユーザーように打てるようにする関数
;; ユーザが入力したコマンドに，括弧とクォートをつける
;; concatenate で，read-line してきた文字列に括弧を付ける．
;; read-from-string で，concatenateしたものの"を外す.
;;;; ----- quote-it 関数の定義 -----
;;;; 仕様: 引数にシングルクォートを付ける
;;;; 'x と，(quote x)というリストは同値
;;;; ゆえにlist コマンドで'quoteをxに付け加えてリスト化
;; cmd のcdr にquote-itしていく．
;; その後cmd のcar をcons して返す．
;; 
(defun game-read ()
  (let ((cmd (read-from-string
	      (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
		     (list 'quote x)))
	  (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))


;; ----- game-eval 関数の定義 -----
;; まずは許されるコマンドのリストを定義
;; 
(defparameter *allowed-commands* '(look walk pickup inventory))
;;
;; 仕様: *allowed-commands* 以外のコマンドは実行しない
;; sexp: 評価するS式
;;
(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
    '(i do not know that command.)))


;; ----- game-print 関数の定義 -----
;; ;; ----- tweak-text 関数の定義 -----
;; ;; 一文字をitemに，残りをrestに格納する．(car, cdr)
;; ;; !, ?, .　は終端文字なので，認識したらcaps(大文字フラグ)をオン
;; ;; "がきた場合も例外処理(どうなってる？)
;;
;; prin1-to-stringで一行読み込んで，string-trimで括弧をはがす
;; coerceでlispが得意なリスト処理に落とし込む
;; tweak-textで編集
;; coerceで文字列化してprincで出力
;; 最後にfresh-line
;; 
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
	    ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
	    ((eql item #\") (tweak-text rest caps (not lit)))
	    (lit (cons item (tweak-text rest nil lit)))
	    (caps (cons (char-upcase item) (tweak-text rest nil lit)))
	    (t (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "()"
						  (prin1-to-string lst))
				     'list)
			     t
			     nil)
		 'string))
  (fresh-line))
