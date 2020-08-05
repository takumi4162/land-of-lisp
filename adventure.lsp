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

;; ���E�ɑ��݂���I�u�W�F�N�g�̃��X�g
;; �P���ȃ��X�g���g���D

(defparameter *objects* '(whiskey bucket frog chain))


;; �I�u�W�F�N�g�̏ꏊ�̊Ǘ�
;; alist(�A�z���X�g)���g���D

(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))

;; �ʒu�̊Ǘ�
;; living-room�ŏ�����

(defparameter *location* 'living-room)


;; ----- describe-location�֐��̒�` -----
;; �d�l: ��i��`�ʂ���֐�
;; location: �V���{��
;; nodes: ���X�g
;; assoc��p���āClocation���܂ރ��X�g��nodes����p�N��
;; (��)�֐��^�v���O���~���O�̂��߂ɁC���ϐ�*nodes*�����̂܂ܗp������͂��Ȃ�

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))


;; ----- describe-path�֐��̒�` -----
;; �d�l: �G�b�W��`�ʂ���֐�
;; edge: ���X�g
;; ���N�H�[�g�œr���ɃR�[�h��}��

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))


;; ----- describe-paths�֐��̒�` -----
;; �d�l: �S�ẴG�b�W��`�ʂ���֐�
;; location: �V���{��
;; edges: ���X�g
;; assoc��location���܂ރ��X�g��edges���璊�o
;; cdr��location�����p�X�݂̂𒊏o
;; mapcar + describe-path�ł��ׂẴp�X�� describe-path��K��
;; apply + append �ŁCd-path�K����̃��X�g���������ĕԂ��D

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))


;; ----- object-at�֐��̒�` -----
;; �d�l: �^����ꂽ�ꏊ���猩����I�u�W�F�N�g�̃��X�g��Ԃ��֐�
;; loc: �V���{��
;; objs: �I�u�W�F�N�g�̃��X�g
;; obj-locs: �I�u�W�F�N�g�̏ꏊ�̃��X�g
;;;; ----- ���[�J���֐�at-loc-p�̒�` -----
;;;; �d�l: �^����ꂽ�ꏊ�ɂ���I�u�W�F�N�g�̃��X�g��Ԃ��֐��̒�`
;;;; obj: �I�u�W�F�N�g�̃V���{��
;;;; assoc��obj���܂ރ��X�g��obj-locs���璊�o
;;;; cadr�ŏꏊ�݂̂𒊏o
;;;; eq��loc��assoc+cadr�̕Ԃ�l���r���Đ^�U�l��Ԃ�
;; remove-if-not��at-loc-p�̕Ԃ�l���U���������̂��͂���
;; (remove-if-not �́C�����̊e�v�f�ɑ΂��Ă̐^�U�l�ł���D)

(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)		; labels �̏����ɒ��ӁD
		     (eq (cadr (assoc obj obj-locs)) loc)))
	  (remove-if-not #'at-loc-p objs)))


;; ----- describe-objects�֐��̒�` -----
;; �d�l: ����ꏊ�Ɍ�����I�u�W�F�N�g��`�ʂ���(���͂ŃQ�[�����ۂ�����)�֐�
;; loc: �ꏊ�̃V���{��
;; objs: �I�u�W�F�N�g�̃��X�g
;; obj-loc: �I�u�W�F�N�g�Əꏊ��alist
;;;; ----- describe-obj�֐��̒�` -----
;;;; obj: �I�u�W�F�N�g�̃V���{��
;;;; ���X�g�Ƃ���describe���Ă����D
;; 
(defun describe-objects (loc objs obj-locs)
  (labels ((describe-obj (obj)
			 `(you see a ,obj on the floor.)))
	  (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-locs)))))


;; ----- look�֐��̒�` -----
;; �d�l: ���ݒn�����n���֐�
;; ��������
;; 
(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))


;; ----- walk�֐��̒�` -----
;; �d�l: �Q�[�����E���ړ�����֐�
;; direction: �s�����\���V���{��
;;
(defun walk (direction)
  (let ((next (find direction
		    (cdr (assoc *location* *edges*))
		    :key #'cadr)))
    (if next
	(progn (setf *location* (car next)) ; find �́C���������v�f�ȉ��̃��X�g��Ԃ�����car
	       (look))
       '(you cannot go that way.))))


;; ----- pickup�֐��̒�` -----
;; �d�l: ���ݒn�ɂ���I�u�W�F�N�g���擾����֐�
;; object: �E���������̂̃V���{��
;; objects-at �ɂ���āC���ݒn *location* �ɂ���I�u�W�F�N�g�̃��X�g���擾
;; member �ɂ���āCobject�����̃��X�g�ɂ��邩���m���߂�D(cond �ɂ���ĕ���)
;; object�����݂���΁Cpush����D
;; ���݂��Ȃ���΃��b�Z�[�W��Ԃ��D
;;
(defun pickup (object)
  (cond ((member object
		 (objects-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(you are now carrying the ,object))
	(t '(you cannot get that.))))


;; ----- inventory�֐��̒�` -----
;; �d�l: �����������Ă���A�C�e���̕\��
;; ��������
;; 'body ���v���C���[��\��location�Ȃ̂ŁC����𗘗p����D
;; �܂�Cobject-at ��'body������΂悢
;;
(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))
