#|
  This file is a part of Envy project.
  Copyright (c) 2013 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage envy
  (:use :cl)
  (:export :config-env-var
           :defconfig
           :config))
(in-package :envy)
;; 定义全局性的hash表
(defvar *config-env-map* (make-hash-table :test 'equal)) ;; 保存需要从环境中获取的环境变量名称
(defvar *package-common-configurations* (make-hash-table :test 'equal)) ;; 默认配置表

;;默认参数是当前包名字
(defun config-env-var (&optional (package-name (package-name *package*)))
  (gethash (package-name (find-package package-name)) *config-env-map*))

(defun (setf config-env-var) (val &optional (package-name (package-name *package*)))
  (setf (gethash (package-name (find-package package-name)) *config-env-map*) val))

(defmacro defconfig (name configurations)
  (if (eq name :common)
      (let ((package-name (package-name *package*))) ;; 如果是common，就在默认表中添加一项
        `(setf (gethash ,package-name *package-common-configurations*)
               ,configurations))
      `(progn
         (defparameter ,name ,configurations)
         (setf (get ',name 'configurationp) t)))) ;; 代码在目标包内执行，为目标包增加一个配置，并标记这个配置存在

(defun package-config (package-name)
  (let* ((package (find-package package-name))
         (package-name (package-name package))
         (env-var (config-env-var package-name)))
    (unless env-var
      (error "Package \"~A\" is not configured. Set which environment variable to determine a configuration by using ~S."
             package-name
             'config-env-var))
    (let ((env (asdf::getenv env-var)))
      (if env ;; 环境变量存在
          (let ((symbol (find-symbol env package))) ;; 得到环境变量所对应的symbol
            (append (if (and symbol
                             (get symbol 'configurationp)
                             (boundp symbol))
                        (symbol-value symbol)
                        nil)
                    (gethash package-name *package-common-configurations* nil)))
          (gethash package-name *package-common-configurations* nil)))))

(defun config (package-name &optional key)
  (if key
      (let ((c (getf (package-config package-name) key))) ;; 得到配置
        (if (functionp c) ;;如果是函数，那么就执行下
            (funcall c)
            c));; 非函数直接返回值
      (package-config package-name)))

(defun config* (&optional key)
  (config (package-name *package*) key))
