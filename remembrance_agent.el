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

(require 'json)

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

(defvar remem-text-type "T")
(defvar remem-voice-type "V")

;; Remembrance agent scope configs (type,  name, period)
;; period - number of seconds in between queries

(defvar remem-scopes-configs-list (list 
    (list remem-text-type "text 1" 3 5 )
    (list remem-text-type "text 2" 4 10)
    (list remem-voice-type "voice 1" 5 nil) 
))

(setq current-voice-input "")

(defvar remem-scopes nil)
;; (make-variable-buffer-local 'remem-scopes) ; each buffer has its own set of scopes

(defun normalize-spaces-in-string (input-string)
  "Replace all whitespace in INPUT-STRING with single spaces."
  (replace-regexp-in-string "[ \t\n\r]+" " " input-string))

(defun remem-start-scope (scope-type name period num-last-words index)
    "Start a specific scope for remembrance agent"

    (let* (
        (new-scope (vector
            scope-type ;Text or voice
            name
            period
            nil ;timer
            nil ;query
            nil ;client
            num-last-words ;num-last-words
        ))
        (results-filter-local (lambda (proc string) (remembrance-agent-query-results-filter proc string index new-scope) ))
        (client (make-network-process :name "remembrance-agent-query-client"
                            :buffer "*remembrance-agent-query-client*"
                            :family 'ipv4
                            :host "127.0.0.1"
                            :service 9998       ;; Port to listen on
                            :filter results-filter-local
                            :sentinel 'client-connection-sentinel
        ))
        )
        (remem-set-scope-client new-scope client)
        (if (> period 0)
            (cond
                ((string= scope-type remem-text-type)
                    (remem-set-scope-timer new-scope
                    (run-at-time 1 period 
                        (lambda ()
                            (setq query (normalize-spaces-in-string (substring-no-properties (remem-last-several-words (remem-scope-num-last-words new-scope)))))
                            (remem-set-scope-query new-scope query)
                            (remem-query client index name query)
                        )
                    )                
                    ) ;Todo: customize the amount of words to look back
                )
                
                ((string= scope-type remem-voice-type)
                
                    (remem-set-scope-timer new-scope
                    (run-at-time 1 period 
                        (lambda ()
                            (setq query (normalize-spaces-in-string (substring-no-properties (last-several-words current-voice-input (remem-scope-num-last-words new-scope)))))
                            (remem-set-scope-query new-scope query)
                            (remem-query client index name query)     
                    ) ;Todo: customize the amount of words to look back
                    )
                ))
            )
        )
        (setq remem-scopes (cons (list name new-scope) remem-scopes))
    )
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

(defun remem-scope-client (scope)
    (aref scope 5)) ; Most recent query searched

(defun remem-set-scope-client (scope value)
    (aset scope 5 value))

(defun remem-scope-num-last-words (scope)
    (aref scope 6)) ; Most recent query searched

(defun remem-set-scope-num-last-words (scope value)
    (aset scope 6 value))

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
                (car (cdr (cdr (cdr current-scope-config))))
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
                (read-only-mode 0)
                (erase-buffer)
                (let 
                    ((numlines 0))
                    (while (< numlines (- remem-display-buffer-height 1))
                        (insert "\n")
                        (setq numlines (1+ numlines))
                    )
                )
                (read-only-mode 1)
            )

            (setq w (split-window w))
            (set-window-buffer w new-buffer)
            (select-window w)
            (enlarge-window (- remem-display-buffer-height (window-displayed-height)))
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

(defun remem-query (client index name query-string)
    "Make a query on a particular string"
    (process-send-string client (concat query-string "\n"))
)

(defun get-point-words-back (count)
  "Return the position of the point 5 words back without moving the cursor."
  (save-excursion
    (backward-word count) 
    (point))) 

(defun get-point-words-forward (count)
  "Return the position of the point 5 words back without moving the cursor."
  (save-excursion
    (forward-word count) 
    (point))) 

(defun remem-last-several-words (count)
  "String from count words back to current point. "
  (let ((orig-window (get-buffer-window (current-buffer)))
        (end) (beg) (retval) (realbufname))
    (save-excursion
        ; place start of sample count words before current position
        ; or as far back as possible
        (if (null count)
            (setq beg point-min)
            (setq beg (get-point-words-back count))
        )
      (if (< beg (point-min)) 
          (setq beg (point-min)))
      ; and place end of sample count words ahead of beginning
      (setq end (get-point-words-forward count))
      (if (> end (point-max)) 
	  (setq end (point-max)))
      (setq retval (concat "" (buffer-substring beg end)))
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
    (setq current-voice-input string)

      (with-current-buffer remem-buffer-name   ;; Switch to the buffer

        (if (> (window-total-width) (string-width string))
            (setq earliest-point 0)
            (setq earliest-point (- 0 (window-total-width)))
        )
        (setq display-string (substring (normalize-spaces-in-string current-voice-input) earliest-point -1))
        (read-only-mode 0)
        (goto-line 1)         
        (delete-region (line-beginning-position) (line-end-position))
        (insert display-string)
        (read-only-mode 1)
    )

)

(defun voice-receiver-stop ()
  "Stop the server."
  (when (process-live-p voice-receiver-server)
    (delete-process voice-receiver-server)))

(defun remembrance-agent-query-results-filter (proc string scope-index scope)
  "Handle incoming data from PROC with STRING.
  Write incoming string into process buffer until we get '}', then handle as a JSON object and write remaining string into buffer, repeat.
  "

  (let ((buffer (process-buffer proc)))
    (when (and proc (process-live-p proc) buffer)
      (with-current-buffer buffer
        (progn 
            (read-only-mode 0)
            (insert string)
            (goto-char (point-min)) 
            (let* (
                    (json-object-type 'hash-table)
                    (current-json 
                        (condition-case nil
                            (json-read)
                        (error nil))
                    )
                )
                (while current-json
                (progn
                    (handle-ra-result current-json scope-index scope)
                    (setq current-json                         
                        (condition-case nil
                            (json-read)
                        (error nil))
                    )
                    (delete-region (point-min) (point))
                )
                )
            )
            (read-only-mode 1)
        )
        )     
    ))
) 


(defun handle-ra-result (parsed-result scope-index scope)
    ;;TODO with json.
    (let (
        (score (gethash "similarity_score" parsed-result))
        (doctitle (gethash "document_title" parsed-result))
        (scope-type (remem-scope-type scope))
        (scope-period (remem-scope-period scope))
        (scope-query (remem-scope-query scope))
        (scope-num-last-words (remem-scope-num-last-words scope))
    )
    
    (with-current-buffer remem-buffer-name   ;; Switch to the buffer
        (read-only-mode 0)
        (goto-line (+ 2 scope-index))         
        (delete-region (line-beginning-position) (line-end-position))
        (insert scope-type)
        (insert " | P: ")
        (insert (number-to-string scope-period))
        (insert " | W: ")
        (insert (number-to-string (or scope-num-last-words -1)))
        (insert " | S: ")
        (insert (number-to-string score))
        (insert " | \"")
        (insert doctitle)
        (insert "\"")
        (insert " | Q: \"")
        (insert scope-query)
        (insert "\"")
        (read-only-mode 1)
    )
    )
)

(defun remembrance-agent-query-client-stop (remembrance-agent-query-client)
  "Stop the client."
  (when (process-live-p remembrance-agent-query-client)
    (delete-process remembrance-agent-query-client)))

(defun client-connection-sentinel (proc event)
  "Handle connection events for PROC."
  (message "Connection event: %s" event))

(start-remem)
(provide 'remembrance)

