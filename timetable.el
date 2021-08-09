(provide 'timetable)

(require 'org-clock)
(require 'org-datetree)

;;; Used by convert functions
(defconst timetable-org-ts-regex (rx "[" (group (1+ (not (char "]")))) "]"))
(defconst timetable-week-regex (rx (| ?w ?W) "eek" (1+ blank) (group (1+ digit))))

;;; Date formatting used for headers
(defconst timetable-week-format "%YW%V"
  "Time format for week headlines (top level).")

(defconst timetable-day-format "%YW%V-%02u"
  "Time format for day headlines (second level).")

(defconst timetable-keys '(:week
                           :day
                           :date
                           :total
                           :work
                           :holiday
                           :sick
                           :vacation))

(defconst timetable-column-names
  (list :week "Week"
        :day "Day"
        :date "Date"
        :total "Total"
        :work "Work"
        :holiday "Holiday"
        :sick "Sick"
        :vacation "Vacation"))

;;; Customizable values
(defcustom timetable-hours-per-day 8
  "Number of hours in a regular work day.")

(defcustom timetable-vacation-days-fulltime 25
  "Number of vacation days for a fulltime job. This is multiplied by FTE to get the number of actual vacation days.")

(defcustom timetable-hours-per-week-fulltime 40
  "Number of hours in week in a fulltime job.")

(defcustom timetable-hours-per-week 32
  "Number of hours in your contract.")

(defun timetable--get-fte ()
  nil)

(defun timetable--add-row (fields &optional fill-existing-row)
  "Add a row to the timetable."
  (unless fill-existing-row
    (org-table-insert-row t))

  (let ((should-goto-next-field nil))
    (dolist (key timetable-keys)
      (if should-goto-next-field
          (org-table-next-field)
        (setq should-goto-next-field t))

      (let* ((field (or (plist-get fields key) ""))
             (formatted-field (format "%s" field)))
        (insert formatted-field)))))

(defun timetable--create-report-table (entries)
  "Creates new table with a header and one empty row."
  (let* ((header-length (number-to-string (length timetable-keys)))
         (size (concat header-length "x1")))
    (org-table-create size))
  ;; Go to start of table
  (org-table-next-field)

  ;; Header, hline and empty row
  (timetable--add-row timetable-column-names t)

  ;; ---------------------------------
  (org-table-insert-hline) (next-line)

  ;; Fill table
  (dolist (entry entries)
    (timetable--add-row entry))

  ;; sort date column
  (org-table-goto-column 3)
  (org-table-sort-lines nil ?a)

  ;; ---------------------------------
  (org-table-insert-hline) (next-line)

  (org-table-insert-row t)

  (org-table-align)

  (next-line))

(defun timetable--create-summary-table (summary)
  "Creates new table with a header and one empty row."
  (org-table-create "2x1")

  (dolist (item summary)
    (org-table-next-field)
    (insert (car item))
    (org-table-next-field)
    (org-table-eval-formula 4 (cdr item)))

  (org-table-align)

  (next-line))

(defun timetable--calc-day-type-minutes (work day-type)
  "Calculates the duration for DAY-TYPE, given the number of WORK minutes.

For holidays or vacation days, always report timetable-hours-per-day hours.

For sick days, report (timetable-hours-per-day - WORK) hours, so
 the sum of work and sick hours always equals a full workday. If
 the work hours is greater or equal to a full workday, it will
 report zero hours."
  (let ((minutes-per-day (* 60 timetable-hours-per-day)))
    (cond ((string= day-type "vacation")
           (- minutes-per-day))
          ((string= day-type "holiday")
           minutes-per-day)
          ((string= day-type "sick")
           (if (> work minutes-per-day)
               0
             (- minutes-per-day work))))))

(defun timetable--get-day-type-key ()
  "Get day-type for heading at point in key
 form (e.g. :holiday). If there is no day-type property or its
 valuenis unknown, return nil."
  (let* ((day-type (org-entry-get (point) "DAY-TYPE")))
    (if (member day-type '("vacation" "sick" "holiday"))
        (intern (concat ":" day-type))
      (unless (eq day-type nil)
        (message "Bad day-type '%s'" day-type))
      nil)))

(defun timetable--parse-entry ()
  (let* ((hc (org-heading-components))
         (heading (nth 4 hc))
         (heading+time (concat heading " 00:00"))
         (heading-time-encoded (encode-time (parse-time-string heading+time)))
         (day (format-time-string "%a" heading-time-encoded))
         (week (format-time-string "%V" heading-time-encoded))
         (date (format-time-string "%F" heading-time-encoded))
         (work-minutes (org-clock-sum-current-item))
         (work (org-duration-from-minutes work-minutes))
         (day-type (org-entry-get (point) "DAY-TYPE"))
         (day-type-key (intern (concat ":" day-type)))
         (day-type-minutes (timetable--calc-day-type-minutes work-minutes day-type))
         (day-type-value (when day-type-minutes (org-duration-from-minutes day-type-minutes)))
         (day-type-property (when day-type-minutes `(,day-type-key ,day-type-value))))
    `(:week ,week :day ,day :date ,date :work ,work ,@day-type-property)))

(defun timetable--parse-file (file)
  (org-map-entries
   (lambda ()
     (let* ((hc (org-heading-components))
            (heading (nth 4 hc))
            (level (nth 0 hc)))
       (cond
        ((= level 3) (timetable--parse-entry))
        (t (message "Skipping heading '%s'..." heading) nil))))
   nil
   (list file)))

(defun timetable--add-markings ()
  "Add markings column to the timetable."
  (save-excursion
    (org-table-goto-column 1)
    (org-table-insert-column)
    (org-table-move-column-left)

    (insert "#")

    (org-table-goto-line 1)
    (insert "!")))

(defun timetable--table-format ()
  (insert "#+TBLFM: ")
  (insert
   (string-join '("@>$2=vcount(@I..II)"
                  "@>$1=vcount(rdup(@I..II))"
                  "$4=vsum($5..$8);U"
                  "@>$4..$8=vsum(@I..II);U")
                "::"))
  (org-ctrl-c-ctrl-c))

(defun timetable--insert-constants (constants)
  (insert
   (format
    "#+CONSTANTS: %s\n"
    (string-join
     (mapcar
      (lambda (c)
        (format "%s=%s" (car c) (cdr c)))
      constants)
     " "))))

(defun org-dblock-write:timetable-report (args)
  (let* ((filename (format "%s" (plist-get args :file)))
         (tablename (plist-get args :tablename))
         (entries (remove nil (timetable--parse-file filename))))
    (when tablename (insert (format "#+NAME: %s\n" tablename)))
    (timetable--insert-constants
     `(("contract" . ,timetable-hours-per-week)))

    (timetable--create-report-table entries)

    (timetable--table-format)))


(defun org-dblock-write:timetable-summary (args)
  (let* ((tablename (plist-get args :tablename))
         (from-report (plist-get args :from-report))
         (math-additional-units
          '((wd (format "%d * hr" timetable-hours-per-day) "Workday")
            (ww (format "%d * hr" timetable-hours-per-week) "Workweek")
            (fte (format "%d * hr" timetable-hours-per-week-fulltime) "Full Time Equivalent"))))
    (when tablename (insert (format "#+NAME: %s\n" tablename)))
    (timetable--insert-constants
     `(("contract" . ,timetable-hours-per-week)
       ("vacation" . ,timetable-vacation-days-fulltime)))

    (timetable--create-summary-table
     `(("Contract hours per week" . "$contract hr")
       ("Number of weeks worked" . ,(format "remote(%s, @>$1) ww" from-report))
       ("FTE" . "uconvert(1 ww, fte)")
       ("Vacation days" . "uconvert(usimplify($vacation wd * @-1 / fte), ww + wd + hr)" )
       ("Hour total" . ,(format "uconvert(%d min, ww + wd + hr + min)"
                                (org-duration-to-minutes (org-table-get-remote-range (format "%s" from-report) "@>$4"))))
       ("Free time left" . "uconvert(usimplify(@-1 + @-2 - @-4), ww + wd + hr + min)")))))

(defmath workdays (hours)
  "Given the number of hours, compute number of workdays."
  (/ hours (float timetable-hours-per-day)))

(defmath uconvert (expr unit)
  (calc-eval
   (math-convert-units
    (calc-eval expr 'raw)
    (calc-eval unit 'raw))))

(defmath workweeks (hours)
    "Given the number of hours, compute number of workweekss."
    (/ hours (float timetable-hours-per-week)))

(defun timetable--calc-eval (expression)
  (let ((math-additional-units
         '((wd "8 * hr" "Workday")
           (ww "32 * hr" "Workweek")
           (fte "40 * hr" "Full Time Equivalent"))))
    (calc-eval expression)))

;;; Minor mode functions
(defvar timetable-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c t g t") 'timetable-goto-day)
    (define-key map (kbd "C-c t c h") 'timetable-convert-headline)
    (define-key map (kbd "C-c t c t") 'timetable-convert-to-datetree)
    map))

(define-minor-mode timetable-mode
  "Mode to manipulate timetables."
  nil " TimeTable")

(defun timetable-goto-day ()
  "Goto heading for today. It creates the subtree if it dies not
exist. You can select a day to jump to when a prefix argument is
used. "
  (interactive)
  (let* ((date (if current-prefix-arg
                   (parse-time-string (org-read-date))
                 (decode-time (current-time))))
         (day (nth 3 date))
         (month (nth 4 date))
         (year (nth 5 date))
         (datetree-date (list month day year)))
    (org-datetree-find-iso-week-create datetree-date)))

;;; Conversion functions
(defun timetable-convert-daytype-tag ()
  "Convert day type tag (freeday, holiday and sick) to property."
  (interactive)
  (let ((first t))
    (dolist (tag (org-get-tags))
      (when (member tag '("freeday" "holiday" "sick"))
        (unless first
          (error "Found second day type tag for heading: %s"
                 (nth 4 (org-heading-components))))

        (setq first nil)
        (org-toggle-tag tag)
        (when (equal tag "freeday")
          (setq tag "vacation"))
        (org-set-property "DAY-TYPE" tag)))))

(defun timetable-convert-headline ()
  "Convert headline to uniform dates in iso week notation."
  (interactive)
  (let* ((heading-components (org-heading-components))
         (level (nth 0 heading-components))
         (heading (nth 4 heading-components)))
    (cond
     ;; Convert top level headings
     ((and (= level 1)
           (string-match timetable-week-regex heading))
      (let* ((week (string-to-number (match-string 1 heading)))
             (year (file-name-sans-extension (file-name-base (buffer-file-name))))
             (new-headline (format "%sW%02d" year week)))
        (org-edit-headline new-headline)))
     ;; Convert day level headings
     ((and (= level 2)
           (string-match timetable-org-ts-regex heading))
      (let* ((date (match-string 1 heading))
             (date-time (concat date " 00:00"))
             (encoded-time (encode-time (parse-time-string date-time)))
             (new-headline (format-time-string timetable-day-format encoded-time)))
        (org-edit-headline new-headline)))
     ;; Report skipping
     (t (message "Skipping header '%s'..." heading)))))

(defun timetable-convert-to-datetree ()
  (interactive)
  (let ((year (file-name-sans-extension (file-name-base (buffer-file-name)))))
    (beginning-of-buffer)
    (org-insert-heading)
    (insert year)

    (org-map-entries
     (lambda ()
       (let* ((heading-components (org-heading-components))
              (level (nth 0 heading-components))
              (heading (nth 4 heading-components)))
         (cond ((and (= level 1)
                     (not (string= heading year)))
                (org-demote-subtree)
                (org-edit-headline (replace-regexp-in-string "W" "-W" heading)))
               ((= level 3)
                (let* ((date-time (concat heading "T00:00"))
                       (encoded-time (encode-time (iso8601-parse date-time)))
                       (new-headline (format-time-string "%F %A" encoded-time)))
                  (org-edit-headline new-headline)))))))))

(defun timetable-convert ()
  "Convert old to new format."
  (interactive)
  (org-map-entries
   (lambda ()
     (timetable-convert-headline)
     (timetable-convert-daytype-tag))))
