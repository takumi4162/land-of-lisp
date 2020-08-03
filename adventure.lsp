;; �A�h�x���`���[�Q�[���̐��E�̕`��
;; �A�z���X�g���g���D
;; �܂���living-room, garden, attic���쐬�D
(defparameter *nodes* '((living-room (you are in the living-room.
			       	  a wizard is snorign loudly on the couch.))
			(garden (you are in a beautiful garden.
				     there is a well in front of you.))
			(attic (you are in the attic.
				    there is a giant welding torch in the corner.))))

;; �e�X�|�b�g�ɍs���ʂ蓹�̕`��
;; ������A�z���X�g���g���D
(defparameter *edges* '((living-room (garden west door)
				     (attic upstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs ladder))))


;; ��i��`�ʂ���֐��̒�`
;; �֐��^�v���O���~���O�̂��߂ɁC���ϐ�*nodes*�����̂܂ܗp������͂��Ȃ�
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

;; �G�b�W��`�ʂ���֐��̒�`
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; �S�ẴG�b�W��`�ʂ���֐�
;; assoc��location�Ƃ��ꂪ���p�X�𒊏o
;; cdr��location�����p�X�݂̂𒊏o
;; mapcar + describe-path�ł��ׂẴp�X�� describe-path��K��
;; apply + append �ŁCd-path�K����̃��X�g���������ĕԂ��D
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))



