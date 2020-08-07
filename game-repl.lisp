;; ----- game-repl�֐��̒�` -----
;; �d�l:�Q�[�����v���C���邽�߂�CLI�p�̊֐��̒�`
(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))


;; ----- game-read �֐��̒�` -----
;; �d�l: �Q�[���̃R�}���h�����[�U�[�悤�ɑłĂ�悤�ɂ���֐�
;; ���[�U�����͂����R�}���h�ɁC���ʂƃN�H�[�g������
;; concatenate �ŁCread-line ���Ă���������Ɋ��ʂ�t����D
;; read-from-string �ŁCconcatenate�������̂�"���O��.
;;;; ----- quote-it �֐��̒�` -----
;;;; �d�l: �����ɃV���O���N�H�[�g��t����
;;;; 'x �ƁC(quote x)�Ƃ������X�g�͓��l
;;;; �䂦��list �R�}���h��'quote��x�ɕt�������ă��X�g��
;; cmd ��cdr ��quote-it���Ă����D
;; ���̌�cmd ��car ��cons ���ĕԂ��D
;; 
(defun game-read ()
  (let ((cmd (read-from-string
	      (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
		     (list 'quote x)))
	  (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))


;; ----- game-eval �֐��̒�` -----
;; �܂��͋������R�}���h�̃��X�g���`
;; 
(defparameter *allowed-commands* '(look walk pickup inventory))
;;
;; �d�l: *allowed-commands* �ȊO�̃R�}���h�͎��s���Ȃ�
;; sexp: �]������S��
;;
(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
    '(i do not know that command.)))


;; ----- game-print �֐��̒�` -----
;; ;; ----- tweak-text �֐��̒�` -----
;; ;; �ꕶ����item�ɁC�c���rest�Ɋi�[����D(car, cdr)
;; ;; !, ?, .�@�͏I�[�����Ȃ̂ŁC�F��������caps(�啶���t���O)���I��
;; ;; "�������ꍇ����O����(�ǂ��Ȃ��Ă�H)
;;
;; prin1-to-string�ň�s�ǂݍ���ŁCstring-trim�Ŋ��ʂ��͂���
;; coerce��lisp�����ӂȃ��X�g�����ɗ��Ƃ�����
;; tweak-text�ŕҏW
;; coerce�ŕ����񉻂���princ�ŏo��
;; �Ō��fresh-line
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
