(module help mzscheme
  (require (lib "startup-cm.ss" "framework"))
   
  (install-cm "PLTHDCM" "PLTHDDEBUG")
  
  ;; start help desk for real
  (dynamic-require '(lib "help-app-main.ss" "help" "private") #f))
