(asdf:defsystem vtryout
  :version "0.0.1"
  :build-operation "deploy-op"
  :build-pathname #+linux "vtryout-linux.run"
  #+darwin "vtryout-macos.o"
  #+win32 "vtryout-windows"
  #+(and bsd (not darwin)) "vtryout-bsd.run"
  #-(or linux bsd win32) "vtryout"
  :entry-point "org.shirakumo.fraf.vtryout::main"
  :components ((:file "package")
               (:file "toolkit")
               (:file "speech-detection")
               (:file "scene")
               (:file "main")
               (:module "ui"
                :components ((:file "laf"))))
  :serial T
  :defsystem-depends-on (:deploy)
  :depends-on (:trial-glfw
               :trial-alloy
               :trial-harmony
               :trial-png
               :trial-jpeg-turbo
               :trial-gltf
               :trial-notify
               :trial-hdr
               :alloy-constraint
               :cl-mixed-wav
               :cl-mixed-opus))
