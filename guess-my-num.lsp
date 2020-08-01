(defparameter *small* 1)
(defparameter *big* 100)
(defun guess-my-number ()
  (ash (+ *small* *big*) -1))

; 小さかった場合の処理を書き込む．
; setfは，変数の値を更新する．
; smallerなら，さっきの推測値より1は小さいはず．
; それをさらにguessする．
(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun bigger ()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))

(defun start-over ()
  (defparameter *small* 1)
  (defparameter *big* 100)
  (guess-my-number))
