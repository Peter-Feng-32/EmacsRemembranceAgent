;;; remembrance.el --- implements emacs front-end for the remembrance agent  -*- lexical-binding:t -*-

;; Copyright (C) 2024 Peter Feng


;; Author: Peter Feng <pfeng32@gatech.edu>
;; Maintainer: Peter Feng <pfeng32@gatech.edu>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Homepage: https://github.com/Peter-Feng-32
;; Keywords: convenience, strings

;;; Commentary:

;;; Code:
(defcustom num-suggestions-voice 1
    "Number of suggestions for the remembrance agent to show based on voice"
    :type '(integer)
    :options '(1 2 3)
    :group 'remembrance-agent
)
;; Todo: refresh remembrance agent buffer here when set.
(defcustom num-suggestions-text 2
    "Number of suggestions for the remembrance agent to show based on buffer text"
    :type '(integer)
    :options '(1 2 3)
    :group 'remembrance-agent
)

(defcustom remem-display-buffer-height (+ num-suggestions-voice num-suggestions-text 1)
    "Height of the remembrance agent buffer"
    :type '(integer)
    :group 'remembrance-agent
)

(defvar remem-buffer-name "*remem-display*"
    "The name of the remembrance display buffer.")

(defvar remem-display-running nil
    "is the remem-display already running?")

(defvar remem-global-timer nil
    "Global timer for running remembrance agent checkup function to make sure window is still alive"
)

(defvar remem-text-type "text")
(defvar remem-voice-type "voice")

;; Remembrance agent scope configs (type,  name, period)
;; period - number of seconds in between queries

(defvar remem-scopes-configs-list (list 
    (list remem-text-type "text 1" 2)
    (list remem-text-type "text 2" 3)
    (list remem-voice-type "voice 1" 1) 
))

(setq current-voice-input "Test Voice")
(setq voice-input-wordcount 5)

(defvar remem-scopes nil)
;; (make-variable-buffer-local 'remem-scopes) ; each buffer has its own set of scopes

(defun format-result (result)
    (format "%d | %s | %s" (car result) (car (cdr result)) (car (cdr (cdr result))))
)

(defun remem-start-scope (scope-type name period index)
    (let (
        (new-scope (vector
                    scope-type ;Text or voice
                    name
                    period
                    nil ;timer
                    nil ;query
        ))
        )

        (if (> period 0)
            (cond
                ((string= scope-type remem-text-type)
                    (remem-set-scope-timer new-scope
                    (run-at-time 1 period 
                        (lambda ()
                            (remem-query index name (remem-last-several-words 5))
                            )
                        )                
                    ) ;Todo: customize the amount of words to look back
                    )
                
                ((string= scope-type remem-voice-type)
                    (remem-set-scope-timer new-scope
                    (run-at-time 1 period 
                        (lambda ()
                            (remem-query index name (last-several-words current-voice-input voice-input-wordcount))     
                    ) ;Todo: customize the amount of words to look back
                    )
                ))
            )
        )
        (setq remem-scopes (cons new-scope remem-scopes))

    )
    "Start a specific scope for remembrance agent"
)

(defun remem-scope-type (scope)
    (aref scope 0)) ; Text or voice

(defun remem-set-scope-type (scope value)
    (aset scope 0 value))

(defun remem-scope-name (scope)
    (aref scope 1)) ; Scope name

(defun remem-set-scope-name (scope value)
    (aset scope 1 value))

(defun remem-scope-period (scope)
    (aref scope 2)) ; Seconds between updating query

(defun remem-set-scope-period (scope value)
    (aset scope 2 value))

(defun remem-scope-timer (scope)
    (aref scope 3)) ; Timer function to run queries.

(defun remem-set-scope-timer (scope value)
    (aset scope 3 value))

(defun remem-scope-query (scope)
    (aref scope 4)) ; Most recent query searched

(defun remem-set-scope-query (scope value)
    (aset scope 4 value))

(defun remem-start-scopes (scopes-configs-list index)
    "Start scopes for remembrance agent based on remem-scopes-config-list"
    (let (
        (current-scope-config (car scopes-configs-list))
        )
        (cond 
        ((null current-scope-config) nil)
        (t
            (remem-start-scope 
                (car current-scope-config)
                (car (cdr current-scope-config))
                (car (cdr (cdr current-scope-config)))
                index
            )
            (remem-start-scopes (cdr scopes-configs-list) (+ index 1))
        )
        )

    )
)

(defun window-displayed-height (&optional window)
    (- (window-height window) 1))

(defun remem-display-buffer (buffer-name)
  (let ((orig-buffer (current-buffer))
        (orig-window (get-buffer-window (current-buffer))))

    (save-excursion
        (let (
            (w (window-at 1 (- (frame-height) 3)))
            (new-buffer (get-buffer-create buffer-name))
            )

            (with-current-buffer new-buffer
                (read-only-mode 1)
            )

            (setq w (split-window w))
            (set-window-buffer w new-buffer)
            (select-window w)
            (enlarge-window (- remem-display-buffer-height (window-displayed-height) 1))
            (set-window-dedicated-p w t))
        )
        (select-window orig-window)
    )
)


(defun remem-fix-window-loss ()
  "Makes sure that Remem dies when the buffer is gone, and respawns when window is no longer visible.  This is because of that stupid resize-windows bug"
  
  (unless (window-live-p (get-buffer-window remem-buffer-name t))
    (cond ((get-buffer remem-buffer-name)    ;; if the buffer exists, respawn the window
           (remem-display-buffer remem-buffer-name))
          ()
        )
    )
)

(defun start-remem ()
    (save-excursion
        (cond (remem-display-running
            (message "Remembrance Agent already running")
            )
            (t
                (remem-display-buffer remem-buffer-name)
                (setq remem-display-running t)
                (setq remem-global-timer
                    (run-at-time 5 3 'remem-fix-window-loss))
            )
        )
        (remem-start-scopes remem-scopes-configs-list 0)
    )
)

(defun remem-query (index name query-string)
    "Make a query on a particular string
    TODO: Actually make the query
    "
    (message "Query %d" index)
    (process-send-string remembrance-agent-query-client (concat (number-to-string index) "|" name "|" query-string))
    (list 1.0 query-string "test result")
)


(defun remem-last-several-words (count)
  "String from count words back to current point.  (word = 5 chars)"
  (let ((orig-window (get-buffer-window (current-buffer)))
        (end) (beg) (retval) (realbufname))
    (save-excursion
        ; place start of sample count words before current position
        ; or as far back as possible
      (setq beg (- (point) (* 5 count)))
      (if (< beg (point-min)) 
          (setq beg (point-min)))
      ; and place end of sample count words ahead of beginning
      (setq end (+ beg (* 5 count)))
      (if (> end (point-max)) 
	  (setq end (point-max)))
      (setq retval (concat " " (buffer-substring beg end)))
;      (while (string-match "\n\\.\n" retval)
;        (setq retval (replace-match "\n .\n" t t retval)))
      )
    (select-window orig-window)
    (setq remem-debug-retval retval)
    (replace-regexp-in-string "\n" " " retval)
    ))

(defun last-several-words (str count)
  (let* ((words (split-string str "\\W+"))   ;; Split string into words
         (last-words (last words count)))       ;; Get the last 5 words
    (string-join last-words " ")))          ;; Join them back into a string

(setq voice-receiver-server
      (make-network-process :name "voice-receiver-server"
                            :buffer "*voice-receiver-server*"
                            :family 'ipv4
                            :service 9999       ;; Port to listen on
                            :server t
                            :noquery t
                            :filter 'voice-receiver-filter))

(defun voice-receiver-filter (proc string)
  "Handle incoming data from PROC with STRING."
  (setq current-voice-input string))

(defun voice-receiver-stop ()
  "Stop the server."
  (when (process-live-p voice-receiver-server)
    (delete-process voice-receiver-server)))

(setq remembrance-agent-query-client
      (make-network-process :name "remembrance-agent-query-client"
                            :buffer "*remembrance-agent-query-client*"
                            :family 'ipv4
                            :host "127.0.0.1"
                            :service 9998       ;; Port to listen on
                            :filter 'remembrance-agent-query-results-filter
                            :sentinel 'client-connection-sentinel
                            ))

(defun remembrance-agent-query-results-filter (proc string)
  "Handle incoming data from PROC with STRING."
    (dolist (line (split-string string "\n"))
        (unless (string-empty-p line)  ; Skip if the line is empty
        (let* (
            (parsed-results (parse-result line))
            (index (car parsed-results))
            (name (car (cdr parsed-results)))
            (result (car (cdr (cdr parsed-results))) )
            )
        (set-text-properties 0 (length result) nil result)

        (with-current-buffer remem-buffer-name   ;; Switch to the buffer
            (read-only-mode 0)
            (goto-char (point-min))
            (forward-line index)         
            (let ((end (line-end-position)))
                (if (and (not (eobp)) (char-equal (char-after end) ?\n))
                    (delete-region (line-beginning-position) (1+ end))  ;; Include newline
                (delete-region (line-beginning-position) end))) 
            (insert (number-to-string index))
            (insert " | ")
            (insert result)  ;; Insert the string
            (insert " | ")
            (insert name)
            (insert " \n")
            (read-only-mode 1)
        ))
    ))
)

(defun parse-result (result)
    (let 
        (
            (results (split-string result "|"))
        )
    (list (string-to-number (car results)) (car (cdr results)) (car (cdr (cdr results))))
    )

)

(defun remembrance-agent-query-client ()
  "Stop the client."
  (when (process-live-p remembrance-agent-query-client)
    (delete-process remembrance-agent-query-client)))

(defun client-connection-sentinel (proc event)
  "Handle connection events for PROC."
  (message "Connection event: %s" event))


(start-remem)
(provide 'remembrance)

