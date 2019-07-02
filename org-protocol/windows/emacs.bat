@echo off
SET EMACSDIR=C:\cygwin64\bin
%EMACSDIR%\emacsclient-w32.exe -na %EMACSDIR%\emacs-w32.exe %*
