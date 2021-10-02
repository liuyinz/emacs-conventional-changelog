;;; conventional-changelog.el --- Conventional Changelog Generator for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2021 liuyinz

;; Author: liuyinz <liuyinz@gmail.com>
;; Created: 2021-09-18 23:45:09
;; Version: 1.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "27") (transient "0.3.6"))
;; Homepage: https://github.com/liuyinz/emacs-conventional-changelog

;; This file is not a part of GNU Emacsl.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Inspired by https://github.com/johnlepikhin/el-conventional-changelog

;;; Code:

(require 'transient)

(defgroup conventional-changelog nil
  "Generate CHANGELOG file in repository using standard-version."
  :group 'tools
  :tag "Conventional Changelog")

(defcustom conventional-changelog-default-mode 'markdown
  "The default filemode of changelog file.
`conventional-changelog' would select file like CHANGELOG.md or CHANGELOG.org
first if exists, otherwise create default file."
  :group 'conventional-changelog
  :type '(choice (const :tag "use CHANGELOG.md" markdown)
                 (const :tag "use CHANGELOG.org" org)))

(defcustom conventional-changelog-tmp-dir
  "/tmp/conventional-changelog"
  "Directory which stores temporary markdown file to generate org."
  :group 'conventional-changelog
  :type 'string)

(defcustom conventional-changelog-release-preset
  '(("--release-as" . ("major" "minor" "patch"))
    ("--prerelease" . ("dev" "alpha" "beta" "rc" "nightly" "next")))
  "Preset for --release-as and --prerelease option."
  :group 'conventional-changelog
  :type 'list)

(defvar conventional-changelog-versionrc nil
  "Config file for standard-version if exists.")

(defun conventional-changelog-versionrc ()
  "Return fullpath of CHANGELOG file in the current repository if exists."
  (unless conventional-changelog-versionrc
    (setq conventional-changelog-versionrc
          (car (directory-files
                (getenv "HOME")
                'full
                "\\`\\.versionrc\\(\\.js\\|\\.json\\)?\\'"))))
  conventional-changelog-versionrc)

(defun conventional-changelog-tmp-file ()
  "Full path of temporary CHANGELOG.md file to generate org."
  (let ((file (convert-standard-filename (expand-file-name "CHANGELOG.md"))))
    (make-directory conventional-changelog-tmp-dir :parents)
    (expand-file-name
     (subst-char-in-string
      ?/
      ?!
      (replace-regexp-in-string
       "!"
       "!!"
       (if (eq (aref file 1) ?:)
           (concat "/"
                   "drive_"
                   (char-to-string (downcase (aref file 0)))
                   (if (eq (aref file 2) ?/)
                       ""
                     "/")
                   (substring file 2))
         file)))
     conventional-changelog-tmp-dir)))

(defun conventional-changelog-file ()
  "Return fullpath of CHANGELOG file in the current repository if exists."
  (or (car (directory-files
            (conventional-changelog-get-rootdir)
            'full
            "\\`CHANGELOG\\.\\(org\\|md\\)\\'"))
      (concat (conventional-changelog-get-rootdir)
              "/"
              (pcase conventional-changelog-default-mode
                ('markdown "CHANGELOG.md")
                ('org "CHANGELOG.org")
                (_ (user-error
                    "Unsupported mode: %s, using markdown or org instead"
                    conventional-changelog-default-mode))))))

(defun conventional-changelog-get-latest-tag ()
  "Return name of latest tag info in current repository."
  (string-trim (shell-command-to-string
                "git describe --tags $(git rev-list --tags --max-count=1)")))

(defun conventional-changelog-get-rootdir ()
  "Return the absolute path to the toplevel of the current repository."
  (string-trim (shell-command-to-string "git rev-parse --show-toplevel")))

(defun conventional-changelog-menu--header ()
  "Header used in `conventional-changelog-menu'"
  (let ((conf (abbreviate-file-name (conventional-changelog-versionrc)))
        (name (file-name-nondirectory (conventional-changelog-file)))
        (tag (conventional-changelog-get-latest-tag)))
    (format (propertize "[%s]: %s\t[%s]: %s\t[%s]: %s\n" 'face 'bold)
            (propertize "infile" 'face 'font-lock-doc-face)
            (propertize name 'face 'font-lock-variable-name-face)
            (propertize "latest" 'face 'font-lock-doc-face)
            (propertize tag 'face 'font-lock-variable-name-face)
            (propertize "conf" 'face 'font-lock-doc-face)
            (propertize conf 'face 'font-lock-variable-name-face))))

(defun conventional-changelog-get-release-preset (prompt &optional default history)
  (let ((lst (cdr (assoc (substring prompt 0 -1) conventional-changelog-release-preset))))
    (completing-read prompt lst nil nil nil history (or default (car lst)))))

(transient-define-prefix conventional-changelog-menu ()
  "Invoke commands for `standard-version'."
  [:description conventional-changelog-menu--header
   :class transient-subgroups
   ["Preset"
    ("-H" "CHANGELOG header" "--header=")
    ("-M" "Premajor release"  "--preMajor")
    ("-F" "Release message" "--releaseCommitMessageFormat=")]
   ["Option"
    ("-k" "Select preset" "--preset="
     :choices ("angular" "atom" "codemirror" "ember"
               "eslint" "express" "jquery" "jscs" "jshint"))
    ("-r" "Specify release type manually" "--release-as="
     :reader conventional-changelog-get-release-preset)
    ("-p" "Make pre-release with tag id" "--prerelease="
     :reader conventional-changelog-get-release-preset)
    ("-i" "Read CHANGELOG from" "--infile=")
    ("-t" "Specify tag prefix" "--tag-prefix=")
    ("-P" "Populate commits under path only" "--path=")
    ("-f" "First release" "--first-release")
    ("-s" "Sign" "--sign")
    ("-n" "Disable hooks" "--no-verify")
    ("-a" "Commit all staged changes" "--commit-all")
    ("-D" "Dry run" "--dry-run")]
   ["Command"
    ("g" "Generate CHANGELOG" conventional-changelog-generate)
    ("t" "Transform CHANGELOG" conventional-changelog-transform)
    ("o" "Open CHANGELOG" conventional-changelog-open)
    ("e" "Open Config" conventional-changelog-edit)]])

(defun conventional-changelog-integrate-magit ()
  "Integrate command `conventional-changelog-menu' to `magit-tag'."
  (with-eval-after-load 'magit-tag
    (transient-append-suffix 'magit-tag
      '(1 0 -1)
      '("c" "changelog" conventional-changelog-menu))))

;;;###autoload
(defun conventional-changelog-generate (&optional working-directory)
  "Generate or update CHANGELOG file in `WORKING-DIRECTORY'."
  (interactive)
  (or working-directory (setq working-directory (conventional-changelog-get-rootdir)))
  (let* ((file (conventional-changelog-file))
         (org-ext (string= "org" (file-name-extension file)))
         (cmd (executable-find "standard-version"))
         (flags (or (mapconcat #'identity (transient-get-value) " ") ""))
         (shell-command-dont-erase-buffer 'beg-last-out))

    (unless cmd (user-error "Cannot find standard-version in PATH"))

    (when org-ext (conventional-changelog-transform))

    (shell-command
     (format "%s %s" (shell-quote-argument cmd) (shell-quote-argument flags)))

    (when org-ext
      (conventional-changelog-transform)
      (let ((default-directory working-directory)
            (tag (conventional-changelog-get-latest-tag)))
        (shell-command
         (format
          "git tag -d %1$s;git add CHANGELOG.{md,org};git commit --amend --no-edit;git tag %1$s"
          (shell-quote-argument tag)))))

    (switch-to-buffer (find-file-noselect file t))))

;;;###autoload
(defun conventional-changelog-open (&optional working-directory)
  "Open CHANGELOG file in `WORKING-DIRECTORY'."
  (interactive)
  (or working-directory (setq working-directory (conventional-changelog-get-rootdir)))
  (let ((path (conventional-changelog-file)))
    (if (file-exists-p path)
        (find-file path)
      (message "File: %s not exists!" path))))

;;;###autoload
(defun conventional-changelog-edit ()
  "Edit config file in `conventional-changelog-versionrc'."
  (interactive)
  (let ((versionrc (conventional-changelog-versionrc)))
    (if versionrc
        (find-file versionrc)
      (find-file
       (completing-read
        "No config under ~/, add one?"
        '("~/.versionrc" "~/.versionrc.json" "~/.versionrc.js"))))))

;;;###autoload
(defun conventional-changelog-transform ()
  "Transform CHANGELOG file between org and markdown."
  (interactive)
  (let* ((file (conventional-changelog-file))
         (tmp-file (conventional-changelog-tmp-file))
         (org-ext (string= "org" (file-name-extension file)))
         (md-file (concat (file-name-sans-extension file) ".md"))
         (org-file (concat (file-name-sans-extension file) ".org"))
         (pandoc (executable-find "pandoc")))

    (unless (file-exists-p file)
      (user-error "%s doesn't exist!" (file-name-nondirectory file)))

    (when (null pandoc)
      (user-error "Cannot find pandoc in PATH"))

    (if org-ext
        (progn
          (if (file-exists-p tmp-file)
              (copy-file tmp-file md-file t)
            (shell-command
             (format "pandoc -f org-auto_identifiers -t markdown_strict -o %s %s"
                     (shell-quote-argument md-file)
                     (shell-quote-argument org-file))))
          (when (get-file-buffer org-file)
            (kill-buffer (get-file-buffer org-file)))
          (delete-file org-file))
      (shell-command
       (format "pandoc -f markdown_strict -t org -o %s %s"
               (shell-quote-argument org-file)
               (shell-quote-argument md-file)))
      (when (get-file-buffer md-file)
        (kill-buffer (get-file-buffer md-file)))
      (rename-file md-file tmp-file t))))

(provide 'conventional-changelog)
;;; conventional-changelog.el ends here
