@echo off
SET EMACSDIR=C:\Users\IBM_ADMIN\my_local_stuff\home\utils\bin\emacs-24.4-bin-i686-pc-mingw32\bin
start %EMACSDIR%\emacsclientw.exe -na %EMACSDIR%\runemacs.exe %*
