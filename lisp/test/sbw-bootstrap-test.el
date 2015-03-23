(require 'sbw-bootstrap)

(ert-deftest sbw/bootstrap-install-packages-given-all-packages-installed-then-no-action ()
  ;; Given
  (let* ( (installed-packages  '(pkg-one pkg-two pkg-three))
          (packages-to-install '(pkg-one pkg-two pkg-three)) )

    (cl-letf* ( ((symbol-function 'package-install)          (lambda (x) (error "Unexpected call to package-install")))
                ((symbol-function 'package-installed-p)      (lambda (x) (member x installed-packages)))
                ((symbol-function 'package-refresh-contents) (lambda ()  (error "Unexpected call to package-refresh-contents"))) )

      ;; When
      ;; Then
      (sbw/bootstrap-install-packages packages-to-install))))

(ert-deftest sbw/bootstrap-install-packages-given-packages-to-install-then-installs-packages ()
  ;; Given
  (let* ( (installed-packages      '(pkg-one pkg-six))
          (packages-to-install     '(pkg-one pkg-two pkg-three (pkg-four . "repo-four") (pkg-five . "repo-five") (pkg-six . "repo-six")))
          (package-pinned-packages '())
          (contains                (lambda (a b) (equal (sort a 'string-lessp) (sort b 'string-lessp))))
          (alist-lessp             (lambda (x y) (string-lessp (car x) (car y))))
          (alist-contains          (lambda (a b) (equal (sort a alist-lessp) (sort b alist-lessp)))) )

    (cl-letf* ( ((symbol-function 'package-install)          (lambda (x) (setq installed-packages (cons x installed-packages))))
                ((symbol-function 'package-installed-p)      (lambda (x) (member x installed-packages)))
                ((symbol-function 'package-refresh-contents) (lambda () nil)) )

      ;; When
      (sbw/bootstrap-install-packages packages-to-install)

      ;; Then
      (should (funcall contains       installed-packages      '(pkg-one pkg-two pkg-three pkg-four pkg-five pkg-six)))
      (should (funcall alist-contains package-pinned-packages '((pkg-four . "repo-four") (pkg-five . "repo-five") (pkg-six . "repo-six")))))))

(provide 'sbw-bootstrap-test)
