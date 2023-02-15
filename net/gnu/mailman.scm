;;;
;;;   Copyright (c) 2005-2010 Shiro Kawai <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;    1. Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;
;;;    2. Redistributions in binary form must reproduce the above copyright
;;;       notice, this list of conditions and the following disclaimer in the
;;;       documentation and/or other materials provided with the distribution.
;;;
;;;    3. Neither the name of the authors nor the names of its contributors
;;;       may be used to endorse or promote products derived from this
;;;       software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(define-module net.gnu.mailman
  (use rfc.http)
  (use rfc.uri)
  (use rfc.cookie)
  (use gauche.logger)
  (use file.util)
  (export <mailman> mailman-login mailman-logged-in?
          mailman-subscribe mailman-unsubscribe mailman-list)
  )
(select-module net.gnu.mailman)

(define-class <mailman> ()
  ((server   :init-keyword :server)     ; listserver name
   (name     :init-keyword :name)       ; list name
   (password :init-keyword :password)   ; admin password
   (admin-path :init-keyword :admin-path :init-value "/admin.cgi")
   (secure   :init-keyword :secure :init-value #f)
   (cookie   :init-value #f)))

;; API
(define-method mailman-login ((mailman <mailman>))
  (receive (status headers body)
      (http-post (~ mailman'server)
                 #"~(~ mailman'admin-path)/~(~ mailman'name)"
                 `(("adminpw" ,(~ mailman'password))
                   ("admlogin" "Let me in..."))
                 :secure (~ mailman'secure))
    (unless (equal? status "200")
      (log-format "mailman-login status: ~a" status)
      (log-format "mailman-login body: ~a" body))
    (and-let* ([p (assoc "set-cookie" headers)]
               [rx (string->regexp #"~(regexp-quote (~ mailman'name))\\+admin=([^;]+)")]
               [m  (rx (cadr p))])
      (set! (~ mailman'cookie) (m 1))
      #t)))

;; API
(define-method mailman-logged-in? ((mailman <mailman>))
  (boolean (~ mailman'cookie)))

(define-method get-csrf-token ((mailman <mailman>) path)
  (receive (status headers body)
      (http-get (~ mailman'server)
                #"~(~ mailman'admin-path)/~(~ mailman'name)~path"
                :secure (~ mailman'secure)
                :cookie (session-cookie mailman))
    (cond [(equal? status "200")
           (and-let1 line (find #/"csrf_token"/ (string-split body "\n"))
             (rxmatch->string #/value="(\w*)"/ line 1))]
          [else
           (log-format "mailman get-csrf-token status: ~a" status)
           (log-format "mailman get-csrf-token body: ~a" body)
           #f])))

;; API
;;  Addresses can be a single email address, or a list of email addresses
;;  (ugly, but for the compatibility)
(define-method mailman-subscribe ((mailman <mailman>) addresses)
  (and-let1 token (get-csrf-token mailman "/members/add")
    (receive (status headers body)
        (http-post (~ mailman'server)
                   #"~(~ mailman'admin-path)/~(~ mailman'name)/members/add"
                   `(("csrf_token" ,token)
                     ("subscribe_or_invite" "0")
                     ("send_welcome_msg_to_this_batch" "0")
                     ("send_notifications_to_list_owner" "1")
                     ("subscribees" ,(concat-addresses addresses))
                     ("invitation" "")
                     ("setmemberopts_btn" "Submit Your Changes"))
                   :mime-version "1.0"
                   :secure (~ mailman'secure)
                   :cookie (session-cookie mailman))
      (cond [(equal? status "200")
             (log-format "mailman-subscribe OK: ~a" addresses)
             #t]
            [else
             (log-format "mailman-subscribe status: ~a" status)
             (log-format "mailman-subscribe body: ~a" body)
             #f]))))

;; API
;;  Addresses can be a single email address, or a list of email addresses
;;  (ugly, but for the compatibility)
(define-method mailman-unsubscribe ((mailman <mailman>) addresses)
  (and-let1 token (get-csrf-token mailman "/members/remove")
    (receive (status headers body)
        (http-post (~ mailman 'server)
                   #"~(~ mailman'admin-path)/~(~ mailman'name)/members/remove"
                   `(("csrf_token" ,token)
                     ("send_unsub_ack_to_this_batch" "0")
                     ("send_unsub_notifications_to_list_owner" "0")
                     ("unsubscribees" ,(concat-addresses addresses))
                     ("setmemberopts_btn" "Submit Your Changes"))
                   :mime-version "1.0"
                   :secure (~ mailman'secure)
                   :cookie (session-cookie mailman))
      (cond [(equal? status "200")
             (log-format "mailman-unsubscribe OK: ~a" addresses)
             #t]
            [else
             (log-format "mailman-unsubscribe status: ~a" status)
             (log-format "mailman-unsubscribe body: ~a" body)
             #f]))))

;; API
(define-method mailman-list ((mailman <mailman>))
  (define (fetch letter)
    (receive (status headers body)
        (http-get (~ mailman'server)
                  `(,(build-path (~ mailman'admin-path)
                                 (~ mailman'name)
                                 "members" "list")
                    ("letter" ,letter))
                  :secure (~ mailman'secure)
                  :cookie (session-cookie mailman))
      (cond [(equal? status "200")
             (filter-map (^[line]
                           (rxmatch->string #/options\.cgi.*>(.*?)<\/a/ line 1))
                         (string-split body "\n"))]
            [else
             (log-format "mailman-list status: ~a" status)
             (log-format "mailman-list body: ~a" body)
             (raise 'fetch-failed)])))
  (guard (e [(eq? e 'fetch-failed) #f])
    (append-map fetch (string->list "abcdefghijklmnopqrtsuvwxyz"))))

(define (concat-addresses addresses)
  (string-join (if (string? addresses)
                 (list addresses)
                 addresses)
               "\r\n" 'suffix))

(define-method session-cookie ((mailman <mailman>))
  (unless (~ mailman'cookie)
    (error "mailman session hasn't logged in"))
  #"$Version=1;~(~ mailman'name)+admin=~(~ mailman'cookie);$Path=/")
