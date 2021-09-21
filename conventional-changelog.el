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
;; TODO write command

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

(defvar conventional-changelog-file nil
  "The name of CHANGELOG file.")

(defun conventional-changelog-file ()
  "Return the name of CHANGELOG file in the current repository if exists."
  (unless conventional-changelog-file
    (setq conventional-changelog-file
          (or (car (directory-files
                    (conventional-changelog-get-rootdir)
                    nil
                    "\\`CHANGELOG\\.\\(org\\|md\\)\\'"))
              (pcase conventional-changelog-default-mode
                ('markdown "CHANGELOG.md")
                ('org "CHANGELOG.org")
                (_ (user-error
                    "Unsupported mode: %s, using markdown or org instead"
                    conventional-changelog-default-mode))))))
  conventional-changelog-file)

(defun conventional-changelog-get-rootdir ()
  "Return the absolute path to the toplevel of the current repository."
  (string-trim (shell-command-to-string "git rev-parse --show-toplevel")))

(transient-define-prefix conventional-changelog-menu ()
  "Show menu buffer for standard-version commands."
  ["Options"
   ("-r" "Specify release type manually" "--release-as="
    :choices ("major" "minor" "patch"))
   ("-p" "Make pre-release with tag id" "--prerelease=")
   ("-i" "Read CHANGELOG from" "--infile=")
   ("-t" "Specify tag prefix" "--tag-prefix=")
   ("-P" "Populate commits under path only" "--path=")
   ("-e" "Specify commit preset" "--preset=")
   ("-l" "Extract package name" "--lerna-package=")
   ("-f" "First release" "--first-release")
   ("-s" "Sign" "--sign")
   ("-n" "Disable hooks" "--no-verify")
   ("-a" "Commit all staged changes" "--commit-all")
   ("-S" "Silent" "--silent")
   ("-D" "Dry run" "--dry-run")
   ]
  ["Command"
   ("r" "Generate or Update CHANGELOG" conventional-changelog)]
  )

;;;###autoload
(defun conventional-changelog (&optional working-directory)
  "Generate or update changelog-file in `WORKING-DIRECTORY'."
  (interactive)
  (or working-directory (setq working-directory (conventional-changelog-get-rootdir)))
  (let ((cmd (executable-find "standard-version"))
        (flags (or (mapconcat #'identity (transient-get-value) " ") "")))
    (unless cmd (user-error "Cannot find %s in PATH" cmd))
    (call-process cmd nil nil nil flags)
    (find-file-noselect (concat working-directory "/" (conventional-changelog-file)) t)
    )
  )

(provide 'conventional-changelog)
;;; conventional-changelog.el ends here
