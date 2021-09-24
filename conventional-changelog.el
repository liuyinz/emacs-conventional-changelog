;;; conventional-changelog.el --- Conventional Changelog Generator for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2021 食無魚

;; Author: 食無魚 <liuyinz@gmail.com>
;; Created: 2021-09-18 23:45:09
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "27.0"))
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
;; default config: https://github.com/conventional-changelog/conventional-changelog-config-spec

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'transient)

(defgroup conventional-changelog nil
  "Generate CHANGELOG file in repository using standard-version."
  :group 'tools
  :tag "Conventional Changelog")

(defcustom conventional-changelog-default-mode 'markdown
  "The default filemode of changelog file.
`conventional-changelog' would search file like CHANGELOG.md or CHANGELOG.org
first if exists, otherwise create default file."
  :group 'conventional-changelog
  :type '(choice (const :tag "use CHANGELOG.md" markdown)
                 (const :tag "use CHANGELOG.org" org)))

(defcustom conventional-changelog-fix-missing nil
  "If non-nil, add changelog info for missing tags in CHANGELOG file."
  :group 'conventional-changelog
  :type 'boolean)

(defcustom conventional-changelog-tmp-dir
  "/tmp/conventional-changelog"
  "Directory which stores temporary markdown file to generate org."
  :group 'conventional-changelog
  :type 'string)

(defvar conventional-changelog-versionrc nil
  "Config file for standard-version if exists.")

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

(defun conventional-changelog-get-rootdir ()
  "Return the absolute path to the toplevel of the current repository."
  (string-trim (shell-command-to-string "git rev-parse --show-toplevel")))

(transient-define-prefix conventional-changelog-menu ()
  "Show menu buffer for standard-version commands."
  ["Preset"
   ("-S" "Select preset" "--preset="
    :choices ("angular" "atom" "codemirror" "ember"
              "eslint" "express" "jquery" "jscs" "jshint"))
   ("-H" "CHANGELOG header" "--header=")
   ("-M" "Premajor release"  "--preMajor=")
   ("-F" "Release message" "--releaseCommitMessageFormat=")]
  ["Options"
   ("-r" "Specify release type manually" "--release-as="
    :choices ("major" "minor" "patch"))
   ("-p" "Make pre-release with tag id" "--prerelease=")
   ;; ("-i" "Read CHANGELOG from" "--infile=")
   ("-t" "Specify tag prefix" "--tag-prefix=")
   ("-P" "Populate commits under path only" "--path=")
   ("-e" "Specify commit preset" "--preset=")
   ("-l" "Extract package name" "--lerna-package=")]
  ["Toggle"
   ("-f" "First release" "--first-release")
   ("-s" "Sign" "--sign")
   ("-n" "Disable hooks" "--no-verify")
   ("-a" "Commit all staged changes" "--commit-all")
   ("-D" "Dry run" "--dry-run")]
  ["Command"
   ("r" "Generate CHANGELOG" conventional-changelog-generate)
   ("o" "Open CHANGELOG" conventional-changelog-open)
   ("e" "Edit Config" conventional-changelog-edit)
   ]
  )

;;;###autoload
(defun conventional-changelog-generate (&optional working-directory)
  "Generate or update changelog-file in `WORKING-DIRECTORY'."
  (interactive)
  (or working-directory (setq working-directory (conventional-changelog-get-rootdir)))
  (let* ((cmd (executable-find "standard-version"))
         (org-ext (string= "org" (file-name-extension (conventional-changelog-file))))
         (flags (or (mapconcat #'identity (transient-get-value) " ") ""))
         (tmp-file (conventional-changelog-tmp-file))
         (path (conventional-changelog-file))
         (md-path (concat (file-name-sans-extension path) ".md"))
         (shell-command-dont-erase-buffer 'beg-last-out))

    (unless cmd (user-error "Cannot find %s in PATH" cmd))

    (when (and org-ext (file-exists-p path))
      (if (file-exists-p tmp-file)
          (copy-file tmp-file md-path t)
        (shell-command
         (format "pandoc -f org-auto_identifiers -t markdown_strict -o %s %s"
                 (shell-quote-argument md-path)
                 (shell-quote-argument path)))))

    (shell-command
     (format "%s %s" (shell-quote-argument cmd) (shell-quote-argument flags)))

    (when org-ext
      (shell-command
       (format "pandoc -f markdown_strict -t org -o %s %s"
               (shell-quote-argument path)
               (shell-quote-argument md-path)))
      (copy-file md-path tmp-file t)
      (shell-command
       (format "git -C %1$s add %2$s;git -C %1$s rm %3$s;git -C %1$s commit --amend --no-edit"
               (shell-quote-argument working-directory)
               (shell-quote-argument path)
               (shell-quote-argument md-path))))

    (find-file-noselect path t)))

;;;###autoload
(defun conventional-changelog-open (&optional working-directory)
  "Open CHANGELOG file in `WORKING-DIRECTORY'."
  (interactive)
  (or working-directory (setq working-directory (conventional-changelog-get-rootdir)))
  (let ((path (conventional-changelog-file)))
    (if (file-exists-p path)
        (find-file path)
      (message "File: %s not exists!" path))))

(defun conventional-changelog-edit ()
  "Edit config file in `conventional-changelog-versionrc'."
  (interactive)
  (let (versionrc (conventional-changelog-versionrc))
    (if (and versionrc (file-exists-p versionrc))
        (find-file versionrc)
      (message "Versionrc not exists!"))))

(provide 'conventional-changelog)
;;; conventional-changelog.el ends here
