REM
REM start the vic with the HesMon machine language monitor
REM cartridge loaded
REM
REM note: HOME environment variable must be set, as the disk drive
REM #8 is set to point there
REM
WinVice-2.2-x86\xvic +cart -cartA HesMon.prg -fs8 %HOME%