(in-package :cl-user)

(defpackage #:cl-etherscan-client
  (:use #:cl)
  (:nicknames #:etherscan-client #:etherscan)
  #+:sbcl (:shadow :defconstant)
  #+:sb-package-locks (:lock t)
  (:export #:+default-scheme+ ;; constant
           #:+default-host+   ;; constant
           #:+default-port+   ;; constant
           #:etherscan-client ;; class
           #:make-etherscan-client
           #:api-key
           #:native-currency
           #:scheme
           #:host
           #:port
           #:debug-p
           #:proxy
           #:proxy-basic-authorization
           #:request
           #:account-balance
           #:account-balance-multi
           #:account-transaction-list
           #:account-transaction-list-internal
           #:account-token-transfer-list
           #:transaction-transaction-list-internal
           #:contract-get-source-code
           #:gas-oracle
           #:last-price
           #:last-price-in-usd
           #:last-price-in-btc))

(in-package :cl-etherscan-client)

#+sbcl
(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))
