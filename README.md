easn
====

ASN.1 Viewer &amp; Editor


Usage
=====

Install Erlang on your system. Downlaod from http://www.erlang.org/download.html if you can not install it from your package 
manager or you're on Windows (and in that case make sure to download the corect version, a 32 bit erlang version under a 64bit 
windows system will not be detected by the easn.cmd file).

There is no make file yet, but you only need to compile 2 files easn.erl and easn-gui.erl. 

Caveats
=======
This is the very first release of my ASN.1 viewer/editor. Not much is working yet

- Gui is working, but not everything is connected yet.   
- Code for asn.1 pretty print is working (some issues with indenting still),  
- Code for xml pretty print works.   
- Hexview works, but selection is not yet working (need to implement multiple selection, know how to do this, just not done yet).  
- Editing is not yet working  
