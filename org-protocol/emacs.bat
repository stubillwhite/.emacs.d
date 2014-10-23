@echo off
SET EMACSDIR=C:\Users\IBM_ADMIN\my_local_stuff\home\utils\bin\emacs-bin-w64-24.4\bin
start %EMACSDIR%\emacsclientw.exe -na %EMACSDIR%\runemacs.exe %*
