(defparameter *small* 1)
(defparameter *big* 100)
(defun guess-my-number ()
  (ash (+ *small* *big*) -1))

; �����������ꍇ�̏������������ށD
; setf�́C�ϐ��̒l���X�V����D
; smaller�Ȃ�C�������̐����l���1�͏������͂��D
; ����������guess����D
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
