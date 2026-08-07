;;; rmsbolt-test.el --- Tests for rmsbolt  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for rmsbolt

;;; Code:

(require 'el-mock nil t)
(require 'rmsbolt)

(ert-deftest sanity-check-ert ()
  "Check if ERT is working. :)"
  (should t))

(defmacro rmsbolt-test-with-sources (sources &rest body)
  "Bind SOURCES to fresh source buffers while running BODY."
  (declare (indent 1))
  `(let ,(mapcar (lambda (source)
                   `(,source (generate-new-buffer ,(symbol-name source))))
                 sources)
     (unwind-protect
         (progn
           ,@(mapcar (lambda (source)
                       `(with-current-buffer ,source
                          (setq-local buffer-file-name
                                      (expand-file-name
                                       (concat (buffer-name) ".c")
                                       temporary-file-directory))))
                     sources)
           ,@body)
       ,@(mapcar (lambda (source) `(when (buffer-live-p ,source)
                                      (kill-buffer ,source)))
                 sources))))

(ert-deftest rmsbolt-sessions-use-independent-buffers-and-files ()
  "Each source buffer owns its output, compilation, and temporary files."
  (rmsbolt-test-with-sources (first-source second-source)
    (let ((first-session (rmsbolt--ensure-session first-source))
          (second-session (rmsbolt--ensure-session second-source)))
      (should-not (eq first-session second-session))
      (should-not (equal (rmsbolt-session-temp-dir first-session)
                         (rmsbolt-session-temp-dir second-session)))
      (should-not (equal (rmsbolt-output-filename first-source)
                         (rmsbolt-output-filename second-source)))
      (let ((first-output (rmsbolt--session-output-buffer first-session))
            (second-output (rmsbolt--session-output-buffer second-session)))
        (should-not (eq first-output second-output))
        (should-not (equal (buffer-name first-output)
                           (buffer-name second-output)))
        (should (eq (buffer-local-value 'rmsbolt--session first-output)
                    first-session))
        (should (eq (buffer-local-value 'rmsbolt--session second-output)
                    second-session))))))

(ert-deftest rmsbolt-session-cleanup-is-local-to-its-source ()
  "Cleaning one session leaves another source session available."
  (rmsbolt-test-with-sources (first-source second-source)
    (let* ((first-session (rmsbolt--ensure-session first-source))
           (second-session (rmsbolt--ensure-session second-source))
           (first-output (rmsbolt--session-output-buffer first-session))
           (second-output (rmsbolt--session-output-buffer second-session))
           (first-temp-dir (rmsbolt-session-temp-dir first-session)))
      (with-current-buffer first-source
        (rmsbolt--cleanup-session first-session))
      (should-not (buffer-live-p first-output))
      (should-not (file-exists-p first-temp-dir))
      (should (buffer-live-p second-output))
      (should (file-directory-p (rmsbolt-session-temp-dir second-session))))))

(ert-deftest rmsbolt-session-completions-update-their-own-output-buffers ()
  "Completion handlers must not replace another source's assembly output."
  (rmsbolt-test-with-sources (first-source second-source)
    (let* ((first-session (rmsbolt--ensure-session first-source))
           (second-session (rmsbolt--ensure-session second-source))
           (first-compilation (get-buffer-create " *rmsbolt-test-first*"))
           (second-compilation (get-buffer-create " *rmsbolt-test-second*"))
           (first-asm (generate-new-buffer " *rmsbolt-test-first-asm*"))
           (second-asm (generate-new-buffer " *rmsbolt-test-second-asm*")))
      (unwind-protect
          (progn
            (with-current-buffer first-compilation
              (setq-local rmsbolt--session first-session))
            (with-current-buffer second-compilation
              (setq-local rmsbolt--session second-session))
            (with-current-buffer first-asm (insert "first assembly"))
            (with-current-buffer second-asm (insert "second assembly"))
            (cl-letf (((symbol-function 'rmsbolt--process-asm-lines)
                       (lambda (_source lines) lines)))
              (rmsbolt--handle-finish-compile first-compilation "finished"
                                               :override-buffer first-asm)
              (rmsbolt--handle-finish-compile second-compilation "finished"
                                               :override-buffer second-asm))
            (should (with-current-buffer (rmsbolt-session-output-buffer first-session)
                      (string-match-p "first assembly" (buffer-string))))
            (should (with-current-buffer (rmsbolt-session-output-buffer second-session)
                      (string-match-p "second assembly" (buffer-string))))
            ;; `asm-mode' must not detach an output buffer from its source
            ;; session, otherwise overlay updates operate on a new session.
            (should (eq (buffer-local-value
                         'rmsbolt--session
                         (rmsbolt-session-output-buffer first-session))
                        first-session))
            (should (eq (buffer-local-value
                         'rmsbolt-src-buffer
                         (rmsbolt-session-output-buffer first-session))
                        first-source))
        (mapc (lambda (buffer)
                (when (buffer-live-p buffer) (kill-buffer buffer)))
              (list first-compilation second-compilation first-asm second-asm)))))))

(ert-deftest rmsbolt-session-overlay-update-keeps-source-and-output-highlights ()
  "A successful overlay update must retain both session highlights."
  (rmsbolt-test-with-sources (source)
    (let* ((session (rmsbolt--ensure-session source))
           (output (rmsbolt--session-output-buffer session))
           (mapping (make-hash-table :test #'eq)))
      (with-current-buffer source
        (insert "source line\n")
        (puthash 1 '((1 . 1)) mapping)
        (setq-local rmsbolt-line-mapping mapping)
        (setq-local rmsbolt-mode t)
        (set-buffer-modified-p nil)
        (goto-char (point-min)))
      (with-current-buffer output
        (insert "assembly line\n"))
      (with-current-buffer source
        (rmsbolt-update-overlays))
      (should (= 2 (length (rmsbolt-session-overlays session))))
      (should (cl-every #'overlay-buffer (rmsbolt-session-overlays session))))))

(ert-deftest remote-path-comparison-skips-file-truename ()
  "Do not canonicalize compiler debug paths from a remote source buffer."
  (with-temp-buffer
    (setq-local buffer-file-name "/ssh:test@example:/remote/source.rs")
    (setq-local rmsbolt-language-descriptor
                (make-rmsbolt-lang
                 :process-asm-custom-fn
                 (lambda (_src-buffer _asm-lines)
                   (rmsbolt--file-equal-p "/remote/source.rs"
                                           "/remote/std.rs"))))
    (cl-letf (((symbol-function 'file-equal-p)
               (lambda (&rest _)
                 (ert-fail "Remote path comparison called file-equal-p"))))
      (should-not (rmsbolt--process-asm-lines (current-buffer) nil)))))

(ert-deftest local-path-comparison-keeps-file-equal-p ()
  "Keep filesystem-aware path equivalence for local buffers."
  (let ((calls 0))
    (cl-letf (((symbol-function 'file-equal-p)
               (lambda (&rest _)
                 (setq calls (1+ calls))
                 t)))
      (should (rmsbolt--file-equal-p "/first/path" "/second/path"))
      (should (= calls 1)))))

(defun test-asm-preprocessor (pre post)
  "Tests the asm preprocessor on the current buffer."
  (insert-file-contents pre)
  (setq-local buffer-file-name (expand-file-name default-directory))
  (let
      ((source
        (string-trim
         (string-join
          (rmsbolt--process-asm-lines (current-buffer)
                                      (split-string (buffer-string) "\n" t))
          "\n")))
       (target
        (with-temp-buffer
          (insert-file-contents post)
          (string-trim
           (buffer-string)))))
    (should (string= source target))))

;;;; Filtration tests

(ert-deftest filter-tests-all-c ()
  "Test if assembly filteration in c is working."
  (with-temp-buffer
    (setq-local rmsbolt-disassemble nil)
    (setq-local rmsbolt-filter-comment-only t)
    (setq-local rmsbolt-filter-directives t)
    (setq-local rmsbolt-filter-labels t)
    (test-asm-preprocessor "test/rmsbolt-c-pre1.s" "test/rmsbolt-c-post1.s")))
(ert-deftest filter-tests-none-c ()
  "Test if assembly filteration in c is working."
  (with-temp-buffer
    (setq-local rmsbolt-disassemble nil)
    (setq-local rmsbolt-filter-comment-only nil)
    (setq-local rmsbolt-filter-directives nil)
    (setq-local rmsbolt-filter-labels nil)
    (test-asm-preprocessor "test/rmsbolt-c-pre1.s" "test/rmsbolt-c-post2.s")))
(ert-deftest filter-tests-dir-c ()
  "Test if assembly filteration in c is working."
  (with-temp-buffer
    (setq-local rmsbolt-disassemble nil)
    (setq-local rmsbolt-filter-comment-only nil)
    (setq-local rmsbolt-filter-directives t)
    (setq-local rmsbolt-filter-labels nil)
    (test-asm-preprocessor "test/rmsbolt-c-pre1.s" "test/rmsbolt-c-post3.s")))
(ert-deftest filter-tests-weak-ref-c ()
  "Test if assembly filteration in c is working."
  (with-temp-buffer
    (setq-local rmsbolt-disassemble nil)
    (setq-local rmsbolt-filter-comment-only nil)
    (setq-local rmsbolt-filter-directives t)
    (setq-local rmsbolt-filter-labels t)
    (test-asm-preprocessor "test/rmsbolt-c-pre2.s" "test/rmsbolt-c-post4.s")))

;;;; Demangler tests

(ert-deftest demangler-test-disabled ()
  (with-temp-buffer
    (setq-local rmsbolt-demangle nil)
    (should
     (string-empty-p
      (rmsbolt--demangle-command
       ""
       (make-rmsbolt-lang :demangler nil)
       (current-buffer))))))

(ert-deftest demangler-test-invalid-demangler ()
  (with-temp-buffer
    (setq-local rmsbolt-demangle t)
    (should
     (string-empty-p
      (rmsbolt--demangle-command
       ""
       (make-rmsbolt-lang :demangler nil)
       (current-buffer))))))

(ert-deftest demangler-test-not-path ()
  (with-temp-buffer
    (setq-local rmsbolt-demangle t)
    (should
     (string-empty-p
      (rmsbolt--demangle-command
       ""
       (make-rmsbolt-lang :demangler "nonsense-binary-name-not-on-path")
       (current-buffer))))))

(ert-deftest demangler-test-valid-demangler ()
  ;; Assumes test is on the path!
  (with-temp-buffer
    (setq-local rmsbolt-demangle t)
    (should
     (string-match-p
      (regexp-opt '("test"))
      (rmsbolt--demangle-command
       ""
       (make-rmsbolt-lang :demangler "test")
       (current-buffer))))))


;;; rmsbolt-test.el ends here
(provide 'rmsbolt-test)
