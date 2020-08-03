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


;; 情景を描写する関数の定義
;; 関数型プログラミングのために，大域変数*nodes*をそのまま用いたりはしない
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

;; エッジを描写する関数の定義
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; 全てのエッジを描写する関数
;; assocでlocationとそれがもつパスを抽出
;; cdrでlocationがもつパスのみを抽出
;; mapcar + describe-pathですべてのパスに describe-pathを適応
;; apply + append で，d-path適応後のリストを結合して返す．
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))



