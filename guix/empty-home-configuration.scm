;; https://issues.guix.gnu.org/59220

(use-modules (gnu home)
             (gnu home services symlink-manager)
             (gnu services))

(home-environment
  (services
   (list (service home-symlink-manager-service-type))))
