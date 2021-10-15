(in-package #:keystone)

(cffi:define-foreign-library keystone
  (:unix (:or "libkeystone.so.1.0" "libkeystone.so"))
  (t (:default "libkeystone")))

(cffi:use-foreign-library keystone)

(cffi:defctype ks-err :int)

(cffi:define-foreign-type ks-err-type ()
  ()
  (:actual-type :ks-err)
  (:simple-parser ks-err))

(cffi:defcfun ("ks_strerror" error-string) :string
  (code :ks-err))

(define-condition keystone-error (error)
  ((error-code
    :initarg :error-code
    :reader keystone-error-code))
  (:report (lambda (c stream)
             (format stream "~a~%" (error-string (keystone-error-code c)))))
  (:documentation "Signalled when a Keystone function returns a code other than KS_ERR_OK"))

(defmethod cffi:translate-from-foreign (value (type ks-err-type))
  (if (zerop value)
      :err-ok
      (error 'keystone-error :error-code value)))

(cffi:defcfun "ks_version" :uint
  (major* :pointer)
  (minor* :pointer))

(defun api-version ()
  (cffi:with-foreign-objects ((major :uint)
                              (minor :uint))
    (let ((combined (ks-version major minor)))
      (values combined
              (cffi:mem-ref major :uint)
              (cffi:mem-ref minor :uint)))))

(cffi:defcfun "ks_arch_supported" :bool
  (arch :ks-arch))

(cffi:defcfun "ks_open" ks-err
  (arch :ks-arch)
  (mode :int)
  (engine** :pointer))

(cffi:defcfun "ks_close" ks-err
  (engine* :pointer))

(cffI:defcfun "ks_errno" ks-err
  (engine* :pointer))

(cffi:defcfun "ks_option" ks-err
  (engine* :pointer)
  (type :ks-opt-type)
  (value :ulong))

(cffi:defcfun "ks_asm" :int
  (engine* :pointer)
  (string :string)
  (address :uint64)
  (encoding** :pointer)
  (encoding-size* :pointer)
  (stat-count* :pointer))

(cffi:defcfun ("ks_free" ks-free) :void (obj :pointer))

(defparameter *non-options* (list :mode :architecture :address))

(defun parse-options (options)
  (remove-if (lambda (opt) (member opt *non-options*))
             (alexandria:plist-alist options)
             :key #'car))

(defun set-options (engine options)
  (dolist (opt-and-value options)
    (destructuring-bind (option . value) opt-and-value
      (let ((opt (cffi:foreign-enum-value 'opt-type option))
            (val (cffi:foreign-enum-value 'opt-value value)))
        (ks-option engine opt val)))))

(defun open-engine (architecture mode)
  (cffi:with-foreign-object (engine** :pointer)
    (ks-open (cffi:foreign-enum-value 'arch architecture)
             (cffi:foreign-enum-value 'mode mode)
             engine**)
    (cffi:mem-ref engine** :pointer)))

(defun close-engine (engine)
  (ks-close engine))

(defmacro with-open-engine ((var architecture mode) &body body)
  `(let ((,var (open-engine ,architecture ,mode)))
     (unwind-protect (progn ,@body)
       (close-engine ,var))))

(defun assemble (code &rest options
                 &key (architecture :x86) (mode :64-bit) (address 0)
                 &allow-other-keys)
  "Assemble CODE string for a given ARCHITECURE operating in MODE, starting at offset
ADDRESS. Returns the assembled bytes, the length of the assembled array, and the number
of assembled instructions.

KEYSTONE> (assemble \"xor rax, rax\" :arch :x86 :mode :64-bit)
#(72 49 92)
3
1

Supported architectures: :ARM, :ARM64, :X86(-64), :PPC, :SPARC, :SYSTEMZ, and :HEXAGON.

The only currently supported additional option is :SYNTAX. Supported values for the
:SYNTAX option are :INTEL,:ATT, :NASM, :MASM, and:GAS.

The default assembly syntax for CODE is :INTEL."
  (cffi:with-foreign-objects ((encoding :pointer)
                              (encoding-size :ulong)
                              (stat-count :ulong))
    (with-open-engine (engine architecture mode)
      (set-options engine (parse-options options))
      (cffi:with-foreign-string (buf code)
        (let ((ret (ks-asm engine buf address encoding encoding-size stat-count)))
          (unless (= ret 0)
            (error 'keystone-error :error-code (ks-errno engine)))
          (let* ((array-size (cffi:mem-ref encoding-size :ulong))
                 (bytes (cffi:foreign-array-to-lisp
                         (cffi:mem-ref encoding :pointer)
                         `(:array :uint8 ,array-size)
                         :element-type '(unsigned-byte 8)))
                 (stats (cffi:mem-ref stat-count :ulong)))
            (ks-free (cffi:mem-ref encoding :pointer))
            (values bytes array-size stats)))))))
