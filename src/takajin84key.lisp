(in-package :cl-user)
(defpackage takajin84key
  (:use :cl)
  (:nicknames :takajin)
  (:export #:*default-roule*
           #:password
           #:make-password
           #:validation
           #:change-password
           #:gen-spell))

(in-package :takajin84key)

;;;;;
;;;;; Contents
;;;;;   1. Generic function
;;;;;   2. Utility
;;;;;   3. Class
;;;;;   4. Operator
;;;;;   5. Validation spell
;;;;;   6. Change Password
;;;;;   7. Validation
;;;;;

;;;;;
;;;;; 1. Generic function
;;;;;
(defgeneric validation-spell (password-or-roule spell)
  (:documentation "パスワード(spell)が正しいかチェックします。"))


(defgeneric validation (password spell)
  (:documentation "spell が現在のパスワードと適合するかチェックします。"))


(defgeneric change-password (password old-spell new-spell)
  (:documentation "パスワードを変更します。"))



;;;;;
;;;;; 2. Utility
;;;;;
(defvar *default-roule*
  #'(lambda (spell &optional password)
      (let ((min 3) (max 20)) ;; ここはベタ書き。 min,max は Class:password ではサポートしていない。
        (cond ((null spell) (error "Password が空でちゅよ〜"))
              ((not (stringp spell)) (error "Password が文字列じゃないでちゅよ〜 spell=~a" spell))
              ((< (length spell) min) (error "~a文字以下はダメでちゅよ〜 spell=~a" min spell))
              ((> (length spell) max) (error "~a文字以上はダメでちゅよ〜 spell=~a" max spell))
              ((and (not (null password))
                    (remove nil
                            (mapcar #'(lambda (x) (cl-pass:check-password spell x))
                                    (history password))))
               (error "履歴のチェックでダメでちたよ〜")))
        t))
  "デフォルトのパスワードルール")


(defun get-list-head (lst len)
  (let ((lst-len (length lst)))
    (if (< len lst-len)
        (subseq lst 0 len)
        lst)))


;; この二つは未使用
(defvar *password-characters* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890-=!@#$%^&*()_+|[]{};:,./<>?")
(defun password-charp (string &key (charcters *password-characters*))
  (search string charcters))
(defun gen-spell (&key (length 8) (use-chars *password-characters*))
  (let ((out ""))
    (dotimes (i length)
      (let ((col (random (length use-chars))))
        (setf out
              (concatenate 'string out (subseq use-chars col (+ 1 col))))))
    out))


;;;;;
;;;;; 3. Class
;;;;;
(defclass password ()
  ((spell :documentation ""
          :accessor spell
          :initarg :spell
          :initform nil)
   (history :documentation ""
            :accessor history
            :initarg :history
            :initform nil)
   (history-length :documentation ""
                   :accessor history-length
                   :initarg :history-length
                   :initform 5)
   (roule :documentation ""
          :accessor roule
          :initarg :roule
          :initform *default-roule*))
  (:documentation ""))




;;;;;
;;;;; 4. Operator
;;;;;
(defun make-password (spell &key (roule *default-roule*))
  (assert roule)
  (unless (validation-spell roule spell)
    (error ""))
  (let ((spell-hash (cl-pass:hash spell)))
    (make-instance 'password
                   :spell   spell-hash
                   :history (list spell-hash)
                   :roule   roule)))


;;;;;
;;;;; 5. Validation spell
;;;;;
(defmethod validation-spell ((password password) spell)
  (funcall (roule password) spell password))

(defmethod validation-spell ((roule symbol) spell)
  (funcall roule spell nil))

(defmethod validation-spell ((roule function) spell)
  (funcall roule spell nil))


(defmethod validation-spell (unknown spell)
  (error "これだれかいね。~a" unknown))



;;;;;
;;;;; 6. Change Password
;;;;;
(defmethod change-password ((password password) old-spell new-spell)
  ;; 1. new-spell の validation チェック。
  (validation-spell password new-spell)
  ;; 2. 削除履歴を保管サイクルで削除。
  (setf (history password)
        (get-list-head (history password) (- (history-length password) 1)))
  ;; 3. 古いパスワードの保管
  (setf (history password)
        (cons (spell password) (history password)))
  ;; 4. 新しいパスワードの保管
  (setf (spell password) (cl-pass:hash new-spell))
  password)



;;;;;
;;;;; 7. Validation
;;;;;
(defmethod validation ((password password) spell)
  (cl-pass:check-password spell (spell password)))



