(in-package :cl-etherscan-client)

(defconstant +default-scheme+          "https")
(defconstant +default-host+            "api.etherscan.io")
(defconstant +default-port+            443)
(defconstant +default-native-currency+ "eth")

(defclass etherscan-client ()
  ((scheme
    :initarg :scheme
    :initform +default-scheme+
    :reader scheme
    :documentation "https or http")
   (host
    :initarg :host
    :initform +default-host+
    :reader host
    :documentation "API host, default: api.etherscan.io")
   (port
    :initarg :port
    :initform +default-port+
    :reader port
    :documentation "API port, default: 443")
   (api-key
    :initarg :api-key
    :initform (error "parameter api-key is required")
    :reader api-key
    :documentation "Please register and fetch API key on page https://etherscan.io/myapikey")
   (native-currency
    :initarg :native-currency
    :initform (error "parameter native-currency is required")
    :reader native-currency
    :documentation "e.g. eth for Ethereum, bnb for BSC, ht for HECO")
   (proxy
    :initarg :proxy
    :initform nil
    :accessor proxy
    :documentation "e.g. '\(\"127.0.0.1\" 8080\)")
   (proxy-basic-authorization
    :initarg :proxy-basic-authorization
    :initform nil
    :accessor proxy-basic-authorization
    :documentation "e.g. '\(\"username\" \"password\"\)")
   (debug-p
    :initarg :debug-p
    :initform nil
    :accessor debug-p
    :documentation "Enable debug message of etherscan-client")))

(defmethod print-object ((etherscan-client etherscan-client) stream)
  (print-unreadable-object (etherscan-client stream :type t :identity t)
    (format stream ":scheme ~S :host ~S :port ~S"
            (slot-value etherscan-client 'scheme)
            (slot-value etherscan-client 'host)
            (slot-value etherscan-client 'port))))

(defun make-etherscan-client (api-key
                              &key
                                (scheme +default-scheme+)
                                (host +default-host+)
                                (port +default-port+)
                                (native-currency +default-native-currency+)
                                (proxy nil)
                                (proxy-basic-authorization nil)
                                (debug-p nil))
  (assert (stringp api-key))
  (make-instance 'etherscan-client
                 :scheme scheme
                 :host host
                 :port port
                 :api-key api-key
                 :native-currency native-currency
                 :proxy proxy
                 :proxy-basic-authorization proxy-basic-authorization
                 :debug-p debug-p))

(defmethod request ((etherscan-client etherscan-client)
                    (module string)
                    (action string)
                    &key
                      (method :get)
                      (more-parameters nil))
  "Note:
     Rate Limit
       - Free         user:  5 calls/second, up to  100,000 calls/day
       - Standard     user: 10 calls/second, up to  200,000 calls/day
       - Advanced     user: 20 calls/second, up to  500,000 calls/day
       - Professional user: 30 calls/second, up to 1000,000 calls/day"
  (let ((url (format nil "~A://~A:~A/api"
                     (scheme etherscan-client)
                     (host etherscan-client)
                     (port etherscan-client)))
        (parameters (append (list (cons "module" module)
                                  (cons "action" action)
                                  (cons "apikey" (api-key etherscan-client)))
                            more-parameters)))
    (multiple-value-bind (data status)
        (drakma:http-request url
                             :method method
                             :parameters parameters
                             :proxy (proxy etherscan-client)
                             :proxy-basic-authorization (proxy-basic-authorization
                                                         etherscan-client)
                             :connection-timeout 3)
      (let ((response (babel:octets-to-string data :encoding :utf-8)))
        (when (debug-p etherscan-client)
          (format *terminal-io* "request URL: ~S
request method: ~S
request parameters: ~S
response status: ~S
response body: ~S~%"
                  url
                  method
                  parameters
                  status
                  response))
        (if (equal 200 status)
            (values (jsown:val (jsown:parse response) "result") status)
            (values response status))))))

(defmethod account-balance ((etherscan-client etherscan-client)
                            (address string)
                            &key (tag "latest"))
  "Return integer
  Note: balance of native currency of address, require divide by \(10^decimals\)"
  (assert (member tag '("latest" "earliest" "pending")
                  :test #'equal))
  (let ((response (request etherscan-client
                           "account"
                           "balance"
                           :more-parameters (list (cons "address" address)
                                                  (cons "tag" tag)))))
    (when response
      (values (parse-integer response)))))

(defmethod account-balance-multi ((etherscan-client etherscan-client)
                                  (address-list list)
                                  &key (tag "latest"))
  "Return alist of account & balance"
  (assert (member tag '("latest" "earliest" "pending")
                  :test #'equal))
  (let ((response (request etherscan-client
                           "account"
                           "balancemulti"
                           :more-parameters (list (cons "address" (format nil "~{~A~^,~}"
                                                                          address-list))
                                                  (cons "tag" tag)))))
    (when response
      (mapcar #'(lambda (result)
                  (cons (jsown:val result "account")
                        (parse-integer (jsown:val result "balance"))))
              response))))

(defmethod account-transaction-list ((etherscan-client etherscan-client)
                                     (address string)
                                     &key
                                       (start-block 0)
                                       (end-block 99999999)
                                       (page 1)
                                       (offset 0)
                                       (sort "asc"))
  "Return NIL if there is NO transaction of the address,
or return jsown in format if transactions exists:
   \(\(:OBJ
      \(\"blockNumber\" . \"9786439\"\)
      \(\"timeStamp\" . \"1628220184\"\)
      \(\"hash\" . \"0xXXX\"\)
      \(\"nonce\" . \"0\"\)
      \(\"blockHash\" . \"0xXXX\"\)
      \(\"transactionIndex\" . \"27\"\)
      \(\"from\" . \"0xXXX\"\)
      \(\"to\" . \"0xXXX\"\)
      \(\"value\" . \"500000000000000000\"\)
      \(\"gas\" . \"50000\"\)
      \(\"gasPrice\" . \"6000000000\"\)
      \(\"isError\" . \"0\"\)
      \(\"txreceipt_status\" . \"1\"\)
      \(\"input\" . \"0x\"\)
      \(\"contractAddress\" . \"\"\)
      \(\"cumulativeGasUsed\" . \"1269686\"\)
      \(\"gasUsed\" . \"21000\"\)
      \(\"confirmations\" . \"1289817\"\)\)\)
Note : This API endpoint returns a maximum of 10000 records only."
  (assert (integerp start-block))
  (assert (integerp end-block))
  (assert (integerp page))
  (assert (integerp offset))
  (assert (member sort '("asc" "desc")
                  :test #'equal))
  (let ((response (request etherscan-client
                           "account"
                           "txlist"
                           :more-parameters (list (cons "address" address)
                                                  (cons "startblock"
                                                        (write-to-string start-block))
                                                  (cons "endblock"
                                                        (write-to-string end-block))
                                                  (cons "page" (write-to-string page))
                                                  (cons "offset" (write-to-string offset))
                                                  (cons "sort" sort)))))
    (values response)))

(defmethod account-transaction-list-internal ((etherscan-client etherscan-client)
                                              (address string)
                                              &key
                                                (start-block 0)
                                                (end-block 99999999)
                                                (page 1)
                                                (offset 0)
                                                (sort "asc"))
  "Return NIL if there is NO transaction of the address,
or return jsown in format if transactions exists:
   \(\(:OBJ
      \(\"blockNumber\" . \"9786439\"\)
      \(\"timeStamp\" . \"1628220184\"\)
      \(\"hash\" . \"0xXXX\"\)
      \(\"from\" . \"0xXXX\"\)
      \(\"to\" . \"0xXXX\"\)
      \(\"value\" . \"500000000000000000\"\)
      \(\"contractAddress\" . \"\"\)
      \(\"input\" . \"0x\"\)
      \(\"type\" . \"call\"\)
      \(\"gas\" . \"50000\"\)
      \(\"gasUsed\" . \"21000\"\)
      \(\"traceId\" . \"\"\)
      \(\"isError\" . \"0\"\)
      \(\"errCode\" . \"\"\)\)
Note : This API endpoint returns a maximum of 10000 records only."
  (assert (integerp start-block))
  (assert (integerp end-block))
  (assert (integerp page))
  (assert (integerp offset))
  (assert (member sort '("asc" "desc")
                  :test #'equal))
  (let ((response (request etherscan-client
                           "account"
                           "txlistinternal"
                           :more-parameters (list (cons "address" address)
                                                  (cons "startblock"
                                                        (write-to-string start-block))
                                                  (cons "endblock"
                                                        (write-to-string end-block))
                                                  (cons "page" (write-to-string page))
                                                  (cons "offset" (write-to-string offset))
                                                  (cons "sort" sort)))))
    (values response)))

(defmethod account-token-transfer-list ((etherscan-client etherscan-client)
                                        &key
                                          (contract-address nil)
                                          (address nil)
                                          (start-block 0)
                                          (end-block 99999999)
                                          (page 1)
                                          (offset 0)
                                          (sort "asc"))
  "Return NIL if there is NO transaction of the address,
or return jsown in format if transactions exists:
   \(\(:OBJ
      \(\"blockNumber\" . \"9786439\"\)
      \(\"timeStamp\" . \"1628220184\"\)
      \(\"hash\" . \"0xXXX\"\)
      \(\"from\" . \"0xXXX\"\)
      \(\"to\" . \"0xXXX\"\)
      \(\"value\" . \"500000000000000000\"\)
      \(\"contractAddress\" . \"\"\)
      \(\"input\" . \"0x\"\)
      \(\"type\" . \"call\"\)
      \(\"gas\" . \"50000\"\)
      \(\"gasUsed\" . \"21000\"\)
      \(\"traceId\" . \"\"\)
      \(\"isError\" . \"0\"\)
      \(\"errCode\" . \"\"\)\)
Note: This API endpoint returns a maximum of 10000 records only.
Note: offset 0 means no paging."
  (assert (not (and (null contract-address)
                    (null address))))
  (assert (integerp start-block))
  (assert (integerp end-block))
  (assert (integerp page))
  (assert (integerp offset))
  (assert (member sort '("asc" "desc")
                  :test #'equal))
  (let ((response (request etherscan-client
                           "account"
                           "tokentx"
                           :more-parameters (list (cons "contractaddress" contract-address)
                                                  (cons "address" address)
                                                  (cons "startblock"
                                                        (write-to-string start-block))
                                                  (cons "endblock"
                                                        (write-to-string end-block))
                                                  (cons "page" (write-to-string page))
                                                  (cons "offset" (write-to-string offset))
                                                  (cons "sort" sort)))))
    (values response)))

(defmethod contract-get-source-code ((etherscan-client etherscan-client)
                                     (address string))
  "Return NIL if contract source code NOT verified,
or return jsown in format if contract source code verified:
  \(:OBJ
   \(\"SourceCode\" . \"<solidity-contract-source-code>\"\)
   \(\"ABI\" . \"<abi>\"\)
   \(\"ContractName\" . \"<contract-name>\"\)
   \(\"CompilerVersion\" . \"v0.5.16+commit.9c3226ce\"\)
   \(\"OptimizationUsed\" . \"1\"\)
   \(\"Runs\" . \"999999\"\)
   \(\"ConstructorArguments\" . \"000000000000000000000000c626128f9463E25d220ed0AaFbfA0c0D634bcDE1\"\)
   \(\"EVMVersion\" . \"istanbul\"\)
   \(\"Library\" . \"\"\)
   \(\"LicenseType\" . \"None\"\)
   \(\"Proxy\" . \"0\"\)
   \(\"Implementation\" . \"\"\)
   \(\"SwarmSource\" . \"bzzr://4bf8a1fbe4b6bd781ddf75534b3631c356dad6512b210f1494170ef5c33dd6be\"\)\)"
  (let ((response (request etherscan-client
                           "contract"
                           "getsourcecode"
                           :more-parameters (list (cons "address" address)))))
    (when (> (length (jsown:val (first response)
                                "SourceCode"))
             0)
      (first response))))

(defmethod gas-oracle ((etherscan-client etherscan-client))
  "Return example:
  \(:OBJ
   \(\"LastBlock\" . \"12724970\"\)
   \(\"SafeGasPrice\" . \"5\"\)
   \(\"ProposeGasPrice\" . \"5\"\)
   \(\"FastGasPrice\" . \"6\"\)
   \(\"UsdPrice\" . \"580.92\"\)\)"
  (let ((response (request etherscan-client
                           "gastracker"
                           "gasoracle")))
    (values response)))

(defmethod last-price ((etherscan-client etherscan-client))
  "Return example:
  \(:OBJ
   \(\"ethbtc\" . \"0.07013\"\)
   \(\"ethbtc_timestamp\" . \"1637160148\"\)
   \(\"ethusd\" . \"4205.78\"\)
   \(\"ethusd_timestamp\" . \"1637160151\"\)\)"
  (let ((response (request etherscan-client
                           "stats"
                           (format nil "~Aprice" (native-currency etherscan-client)))))
    (values response)))

(defmethod last-price-in-usd ((etherscan-client etherscan-client))
  (wu-decimal:parse-decimal
   (jsown:val (last-price etherscan-client) "ethusd")))

(defmethod last-price-in-btc ((etherscan-client etherscan-client))
  (wu-decimal:parse-decimal
   (jsown:val (last-price etherscan-client) "ethbtc")))
