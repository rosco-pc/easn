:: Batch file to call view_asn.beam
@ECHO OFF
:: Determine erl location (gives latest installed OTP)
FOR /F "usebackq skip=1 tokens=1" %%I IN (`reg query HKLM\Software\Ericsson\Erlang`) DO SET KEY="%%I"
FOR /F "usebackq tokens=3*" %%I IN (`reg query %KEY% ^| find "(Default)"`) DO SET ERL=%%I %%J
SET ERL="%ERL%\bin\erl.exe"

:: Determine command line parameters
SET CMD=-pa ./bin/ -run view_asn
IF "%1" == "" GOTO Run
IF "%2" == "" GOTO Usage
IF "%3" == "" GOTO Usage
SET TYPE="asn"
IF NOT "%4" == "" SET TYPE=%4
SET CMD=%CMD% view $1 $2 $3

:Run
:: Run view_asn
%ERL% %CMD% -run init stop -noshell
GOTO :EOF

:Usage
ECHO Usage: view_asn [File ASN1 Version]
ECHO.
ECHO View an ASN.1 encoded file in humand readable form
ECHO. 
ECHO   File    - File to be decoded
ECHO   ASN1    - ASN.1 Specification to be used
ECHO   Version - Version of ASN.1 Specification
ECHO   Output  - ASN | XML
ECHO. 
ECHO With no parameters given a GUI will be started.
ECHO With ALL parameters given output will be shown
ECHO on the console.