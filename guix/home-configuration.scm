;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu home services dotfiles)
             (gnu services)
             (gnu packages)
             (gnu packages shells)
             (guix gexp))

(home-environment
  ;; Below is the list of packages that will show up in your
  ;; Home profile, under ~/.guix-home/profile.
  (packages (specifications->packages (list "tree" "guile"
                                            ;; "glibc-my-utf8-locales"
                                            "nss-certs" "fontconfig")))

  ;; Below is the list of Home services.  To search for available
  ;; services, run 'guix home search KEYWORD' in a terminal.
  (services
   (append (list (service home-dotfiles-service-type
                          (home-dotfiles-configuration
                           (directories '("../dot-home"))
                           (layout 'plain)
                           ;; (packages '("dot-home"))
                           (excluded '(".*~" ".*\\.swp" "\\.git" "\\.gitignore"
                                       "\\.profile")))
                          )
                 ;; (service home-bash-service-type
                 ;;          (home-bash-configuration
                 ;;           (bashrc       (list (local-file "../dot-home/.bashrc"       "bashrc")))
                 ;;           (bash-profile (list (local-file "../dot-home/.bash_profile" "bash_profile")))
                 ;;           (bash-logout  (list (local-file "../dot-home/.bash_logout"  "bash_logout")))))
                 )
           %base-home-services)))
