(asdf:defsystem "cl-etherscan-client"
  :name "cl-etherscan-client"
  :description "Etherscan API client for Common Lisp"
  :version "0.0.1"
  :author "Muyinliu Xing <muyinliu@gmail.com>"
  :license "ISC"
  :depends-on ("drakma"
               "jsown"
               "babel"
               "wu-decimal")
  :serial t
  :components ((:file "packages")
               (:file "cl-etherscan-client")))
