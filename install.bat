:: Batch file to call view_asn.beam
@ECHO OFF
:: Determine erl location (gives latest installed OTP)
FOR /F "usebackq skip=1 tokens=1" %%I IN (`reg query HKLM\Software\Ericsson\Erlang`) DO SET KEY="%%I"
FOR /F "usebackq tokens=3*" %%I IN (`reg query %KEY% ^| find "(Default)"`) DO SET ERL=%%I %%J
SET ERLC="%ERL%\bin\erlc.exe"

:: Compile source
SET SOURCE=src/easn.erl src/easn_parse.erl
%ERLC% -o bin %SOURCE%

:: Create help file
:: Copy files

:: Create menu group