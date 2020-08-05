;; アドベンチャーゲームの世界の描写
;; 連想リストを使う．
;; まずはliving-room, garden, atticを作成．
(defparameter *nodes* '((living-room (you are in the living-room.
			       	  a wizard is snorign loudly on the couch.))
			(garden (you are in a beautiful garden.
				     there is a well in front of you.))
			(attic (you are in the attic.
				    there is a giant welding torch in the corner.))))

;; 各スポットに行く通り道の描写
;; これも連想リストを使う．
(defparameter *edges* '((living-room (garden west door)
				     (attic upstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs ladder))))

;; 世界に存在するオブジェクトのリスト
;; 単純なリストを使う．

(defparameter *objects* '(whiskey bucket frog chain))


;; オブジェクトの場所の管理
;; alist(連想リスト)を使う．

(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))

;; 位置の管理
;; living-roomで初期化

(defparameter *location* 'living-room)


;; ----- describe-location関数の定義 -----
;; 仕様: 情景を描写する関数
;; location: シンボル
;; nodes: リスト
;; assocを用いて，locationを含むリストをnodesからパクる
;; (注)関数型プログラミングのために，大域変数*nodes*をそのまま用いたりはしない

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))


;; ----- describe-path関数の定義 -----
;; 仕様: エッジを描写する関数
;; edge: リスト
;; 準クォートで途中にコードを挿入

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))


;; ----- describe-paths関数の定義 -----
;; 仕様: 全てのエッジを描写する関数
;; location: シンボル
;; edges: リスト
;; assocでlocationを含むリストをedgesから抽出
;; cdrでlocationがもつパスのみを抽出
;; mapcar + describe-pathですべてのパスに describe-pathを適応
;; apply + append で，d-path適応後のリストを結合して返す．

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))


;; ----- object-at関数の定義 -----
;; 仕様: 与えられた場所から見えるオブジェクトのリストを返す関数
;; loc: シンボル
;; objs: オブジェクトのリスト
;; obj-locs: オブジェクトの場所のリスト
;;;; ----- ローカル関数at-loc-pの定義 -----
;;;; 仕様: 与えられた場所にあるオブジェクトのリストを返す関数の定義
;;;; obj: オブジェクトのシンボル
;;;; assocでobjを含むリストをobj-locsから抽出
;;;; cadrで場所のみを抽出
;;;; eqでlocとassoc+cadrの返り値を比較して真偽値を返す
;; remove-if-notでat-loc-pの返り値が偽だったものをはじく
;; (remove-if-not は，引数の各要素に対しての真偽値である．)

(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)		; labels の書式に注意．
		     (eq (cadr (assoc obj obj-locs)) loc)))
	  (remove-if-not #'at-loc-p objs)))


;; ----- describe-objects関数の定義 -----
;; 仕様: ある場所に見えるオブジェクトを描写する(文章でゲームっぽくする)関数
;; loc: 場所のシンボル
;; objs: オブジェクトのリスト
;; obj-loc: オブジェクトと場所のalist
;;;; ----- describe-obj関数の定義 -----
;;;; obj: オブジェクトのシンボル
;;;; リストとしてdescribeしていく．
;; 
(defun describe-objects (loc objs obj-locs)
  (labels ((describe-obj (obj)
			 `(you see a ,obj on the floor.)))
	  (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-locs)))))


;; ----- look関数の定義 -----
;; 仕様: 現在地を見渡す関数
;; 引数無し
;; 
(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))


;; ----- walk関数の定義 -----
;; 仕様: ゲーム世界を移動する関数
;; direction: 行き先を表すシンボル
;;
(defun walk (direction)
  (let ((next (find direction
		    (cdr (assoc *location* *edges*))
		    :key #'cadr)))
    (if next
	(progn (setf *location* (car next)) ; find は，発見した要素以下のリストを返すからcar
	       (look))
       '(you cannot go that way.))))


;; ----- pickup関数の定義 -----
;; 仕様: 現在地にいるオブジェクトを取得する関数
;; object: 拾いたいもののシンボル
;; objects-at によって，現在地 *location* にあるオブジェクトのリストを取得
;; member によって，objectがそのリストにあるかを確かめる．(cond によって分岐)
;; objectが存在すれば，pushする．
;; 存在しなければメッセージを返す．
;;
(defun pickup (object)
  (cond ((member object
		 (objects-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(you are now carrying the ,object))
	(t '(you cannot get that.))))


;; ----- inventory関数の定義 -----
;; 仕様: 自分が持っているアイテムの表示
;; 引数無し
;; 'body がプレイヤーを表すlocationなので，それを利用する．
;; つまり，object-at で'bodyを見ればよい
;;
(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))
