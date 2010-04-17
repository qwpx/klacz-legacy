(in-package :klacz)


(defbotf say (message &rest text)
  (with-irc (reply-to message text)))

(defbotf + (message a b)
  (bind ((a (parse-integer a))
        (b (parse-integer b)))
    (with-irc (reply-to message (format nil "~D" (+ a b))))))

(defbotf more (message)
  (with-irc (reply-to message *message-pool*)))

(defbotf debug (message)
  (with-irc (reply-to message "Invoked debugger"))
  (break))



(defparameter *m8b-answers*
  '("As I see it, yes"
    "It is certain"
    "It is decidedly so"
    "Most likely"
    "Outlook good"
    "Signs point to yes"
    "Without a doubt"
    "Yes"
    "Yes - definitely"
    "You may rely on it"
    "Reply hazy, try again"
    "Ask again later"
    "Better not tell you now"
    "Cannot predict now"
    "Concentrate and ask again"
    "Don't count on it"
    "My reply is no"
    "My sources say no"
    "Outlook not so good"
    "Very doubtful"
    "You have AIDS anyway"))

(defbotf m8b (message &rest question)
  (declare (ignore question))
  (with-irc (reply-to message (nth (random (length *m8b-answers*)) 
                                   *m8b-answers*))))

(defbotf ping (message)
  (with-irc 
    (reply-to message 
              (format nil "~A: pong" (source message)))))

(defbotf pick (message &rest arguments)
  (flet ((sum-vector (vector)
           (loop for n across vector sum n)))
    (bind ((words (ppcre:split "\\s+" arguments))
           (hashes (mapcar (lambda (w)
                             (list w (sum-vector 
                                      (ironclad:digest-sequence 
                                       :md5 (babel:string-to-octets w)))))
                           words)))
      (with-irc (reply-to message 
                          (format nil "~{~A~^ > ~}"
                                  (mapcar #'first (sort hashes #'> :key #'second))))))))

(defbotf seen (message nick)
  (with-transaction
    (bind ((last-entry (first (select-instances (l log-entry)
                                (where (and (eq (channel-of l) (first (arguments message)))
                                            (eq (nick-of l) nick)))
                                (order-by :descending (date-of l))
                                (limit 1))))
           (reply (if last-entry
                      (with-slots (nick date kind message) last-entry
                        (format nil "Last seen ~A: ~A ~A ~A" 
                                nick 
                                (format-timestring nil date :format *date-format*)
                                kind message))
                      (format nil "Never seen ~A." nick))))
      (with-irc 
        (reply-to message reply)))))
                                     

(defbotf memo (message nick &rest text)
  (with-transaction 
    (make-instance 'memo 
                   :channel (first (arguments message))
                   :from (source message)
                   :to nick
                   :message text))
  (with-irc 
    (reply-to message (format nil "Added memo for \"~A\"." nick))))

(defbotf time (message)
  (with-irc
    (reply-to message (format-timestring nil (now) :format *date-format*))))
