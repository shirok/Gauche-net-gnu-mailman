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
  (use gauche.logger)
  (export <mailman> mailman-login mailman-subscribe mailman-unsubscribe)
  )
(select-module net.gnu.mailman)

(define-class <mailman> ()
  ((server   :init-keyword :server)     ; listserver name
   (name     :init-keyword :name)       ; list name
   (password :init-keyword :password)   ; admin password
   (cookie   :init-value #f)))

(define-method mailman-login ((mailman <mailman>))
  (receive (status headers body)
      (http-post (ref mailman 'server)
                 #`"/admin.cgi/,(ref mailman 'name)"
                 #`"adminpw=,(ref mailman 'password);admlogin=Let me in...")
    (unless (equal? status "200")
      (log-format "mailman-login status: ~a" status)
      (log-format "mailman-login body: ~a" body))
    (and-let* ((p (assoc "set-cookie" headers))
               (rx (string->regexp #`",(ref mailman 'name)\\+admin=([^;]+)"))
               (m  (rx (cadr p))))
      (set! (ref mailman 'cookie) (m 1))
      #t)))

(define-method mailman-subscribe ((mailman <mailman>) . addresses)
  (let ((uri #`"/admin.cgi/,(ref mailman 'name)/members/add")
        (data `(""
                "--boundary"
                "Content-disposition: form-data; name=\"subscribe_or_invite\""
                ""
                "0"
                "--boundary"
                "Content-disposition: form-data; name=\"send_welcome_msg_to_this_batch\""
                ""
                "0"
                "--boundary"
                "Content-disposition: form-data; name=\"send_notifications_to_list_owner\""
                ""
                "1"
                "--boundary"
                "Content-disposition: form-data; name=\"subscribees\""
                ""
                ,@addresses
                ""
                "--boundary"
                "Content-disposition: form-data; name=\"invitation\""
                ""
                ""
                "--boundary"
                "Content-disposition: form-data; name=\"setmemberopts_btn\""
                ""
                "Submit Your Changes"
                "--boundary--"))
        (cookie #`"$Version=1;,(ref mailman 'name)+admin=,(ref mailman 'cookie);$Path=/"))
    (receive (status headers body)
        (http-post (ref mailman 'server) uri
                   (string-join data "\r\n" 'suffix)
                   :mime-version "1.0"
                   :content-type "multipart/form-data; boundary=boundary"
                   :cookie cookie)
      (cond ((equal? status "200")
             (log-format "mailman-subscribe OK: ~a" addresses)
             #t)
            (else
             (log-format "mailman-subscribe status: ~a" status)
             (log-format "mailman-subscribe body: ~a" body)
             #f)))))
                 
(define-method mailman-unsubscribe ((mailman <mailman>) . addresses)
  (let ((uri #`"/admin.cgi/,(ref mailman 'name)/members/remove")
        (data `(""
                "--boundary"
                "Content-disposition: form-data; name=\"send_unsub_ack_to_this_batch\""
                ""
                "0"
                "--boundary"
                "Content-disposition: form-data; name=\"send_unsub_notifications_to_list_owner\""
                ""
                "0"
                "--boundary"
                "Content-disposition: form-data; name=\"unsubscribees\""
                ""
                ,@addresses
                ""
                "--boundary"
                "Content-disposition: form-data; name=\"setmemberopts_btn\""
                ""
                "Submit Your Changes"
                "--boundary--"))
        (cookie #`"$Version=1;,(ref mailman 'name)+admin=,(ref mailman 'cookie);$Path=/"))
    (receive (status headers body)
        (http-post (ref mailman 'server) uri
                   (string-join data "\r\n" 'suffix)
                   :mime-version "1.0"
                   :content-type "multipart/form-data; boundary=boundary"
                   :cookie cookie)
      (cond ((equal? status "200")
             (log-format "mailman-unsubscribe OK: ~a" addresses)
             #t)
            (else
             (log-format "mailman-unsubscribe status: ~a" status)
             (log-format "mailman-unsubscribe body: ~a" body)
             #f)))))
