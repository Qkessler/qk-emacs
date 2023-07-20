;;; qk-tramp.el -*- lexical-binding: t; -*-

(use-package tramp
  :config
  (setq tramp-ssh-controlmaster-options nil
        tramp-copy-size-limit nil
        tramp-default-remote-shell "/bin/bash"
        tramp-default-method "rsync"
        remote-file-name-inhibit-cache nil)
  (connection-local-set-profile-variables 'clouddesk-tramp-profile
                                          `((shell-file-name . ,tramp-default-remote-shell)))

  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/ssh:enrikes-clouddesk.aka.corp.amazon.com:")
                     "direct-async-process" t))
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/ssh:enrikes-clouddesk.aka.corp.amazon.com:")
                     "remote-shell" tramp-default-remote-shell))
  (connection-local-set-profiles '(:application tramp :protocol "ssh" :machine "enrikes-clouddesk.aka.corp.amazon.com") 'clouddesk-tramp-profile))

(provide 'qk-tramp)
;; qk-tramp.el ends here. 
